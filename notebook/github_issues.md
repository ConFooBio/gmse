# GitHub Issues

## Issue 1: `1:length(x)` pattern causes silent mis-iteration when `length(x) == 0`

**Files:** Widespread — every `for(i in 1:length(x))` pattern in the codebase.

**Affected locations:**

| File | Lines |
|------|-------|
| `R/gmse_summary.R` | 21, 44, 84, 167 |
| `R/gmse_apply.R` | 121, 1674, 1699, 1736, 1900, 1938, 1954, 2013, 2019, 2475, 2565, 2697, 2933 |
| `R/plotting.R` | 148, 364, 580, 609 |

**Description:** This is a well-documented R gotcha. The expression `1:length(x)` looks innocuous but silently misbehaves when `x` has length zero:

```r
x <- numeric(0)      # empty vector, length = 0
1:length(x)          # 1:0 → c(1, 0)  ← NOT what you expect
```

Instead of producing an empty integer vector, `1:0` produces `c(1, 0)`. The loop body then executes twice — once for `i = 1` and once for `i = 0` — rather than skipping entirely. This can silently write to invalid indices or operate on non-existent elements without raising an error.

**Why it matters:** If a GMSE simulation ever produces an empty resource type vector, an empty agent array, or an empty observation record, any loop using this pattern will run twice with incorrect indices rather than being skipped. The code will not crash, but it may silently corrupt array entries at index 0 (which in R assigns to a new element or overwrites internal structure in certain contexts). In practice, bugs introduced this way would manifest as hard-to-debug numerical errors in simulation outputs rather than explicit crashes, making them very difficult to track down.

**How to fix:** Use `seq_along(x)` or `seq_len(length(x))` instead:

```r
# Instead of:
for(i in 1:length(x)){ ... }

# Use:
for(i in seq_along(x)){ ... }
```

`seq_along(x)` safely returns an empty integer vector when `x` has length 0, so the loop body never executes. This is the idiomatic R pattern and should be used consistently throughout the codebase.

---

## Issue 2: Missing `malloc` NULL checks in C code

**Files:** `src/utilities.c`, `src/resource.c`, `src/observation.c`, `src/game.c`, `src/landscape.c`

**Affected locations (not exhaustive):**

| File | Lines |
|------|-------|
| `src/utilities.c` | 67 |
| `src/resource.c` | 414, 520, 522, 537, 539, 541, 555, 587, 589, 607, 609 |
| `src/observation.c` | 599, 616, 619, 639, 641, 643, 659, 661, 675, 677, 804, 806, 1002, 1005, 1020, 1022, 1024, 1031, 1033 |
| `src/landscape.c` | 134, 149, 150, 151, 153 |
| `src/game.c` | 380, 381, 573–586 |

**Description:** Throughout the C code, `malloc` is called without checking whether it returns `NULL`. For example, in `src/utilities.c`:

```c
sarray = (int *) malloc(length * sizeof(int));
// No check that sarray != NULL before dereferencing
```

On modern systems with adequate memory, `malloc` failure is rare, but it can happen under memory pressure, in constrained environments, or when processing very large arrays. If `malloc` returns `NULL`, the subsequent pointer dereference causes a segfault, crashing the R session and losing all unsaved work.

**Why it matters:** A segfault in an R package is a hard crash — no error message, no opportunity for `tryCatch`, no way to save the workspace. This is especially problematic in simulation-intensive workflows where a user might be running a long GMSE simulation. R packages distributed via CRAN are expected to handle allocation failures gracefully.

**How to fix:** After every `malloc` call, check the return value and call `error()` if it is `NULL`:

```c
sarray = (int *) malloc(length * sizeof(int));
if(sarray == NULL){
    error("Memory allocation failed in find_descending_order");
}
```

R's `error()` function will safely unwind the C stack and return control to the R interpreter, producing a proper error message instead of a segfault.

---

## Issue 3: Single `|` and `&` used in `if()` conditions instead of `||` and `&&`

**Files:** Widespread.

**Affected locations include:**

| File | Lines |
|------|-------|
| `R/user.R` | 51 |
| `R/manager.R` | 47 |
| `R/observation.R` | 52 |
| `R/resource.R` | 32 |
| `R/anecdotal.R` | 49 |
| `R/gmse.R` | 182, 188, 191, 194, 656 |
| `R/gmse_apply.R` | ~70+ instances across 895–1132 |
| `R/initialise.R` | 145 |
| `R/plotting.R` | 224, 356, 415, 421, 457, 601, 626, 643, 649, 660, 687 |

**Description:** In R, `|` and `&` are vectorised operators that return a vector of the same length as their inputs. `||` and `&&` are scalar operators that evaluate left-to-right and short-circuit (stop evaluating as soon as the result is determined). Using `|` and `&` inside `if()` conditions — which require a single logical value — works by accident when both operands are length 1, but is fragile for several reasons:

1. **No short-circuiting:** Both sides are always evaluated, even when the first condition already determines the result. If the second condition involves an expensive computation or one that might error on certain inputs, this is wasteful or dangerous.

2. **Vectorised result:** If either operand is unexpectedly length > 1, the `if()` condition will produce a warning (only the first element is used) and may silently test the wrong thing.

3. **Misleading to readers:** The intent is clearly a scalar logical check, so `||` and `&&` better communicate that intention.

**Example from `R/user.R:51`:**

```r
if(!is.vector(PARAS) | !is.numeric(PARAS)){
    stop("Warning: Parameters must be in a numeric vector");
}
```

This works for scalar `PARAS` but would produce a warning and test only `PARAS[1]` if `PARAS` were somehow a multi-element non-vector.

**Why it matters:** In a simulation package, inputs might come from user-defined functions or previous model outputs where the structure is not guaranteed. A vector slipping through could cause the validation check to silently pass when it should fail, leading to downstream errors that are difficult to trace back to the root cause.

**How to fix:** Replace `|` with `||` and `&` with `&&` in all `if()` conditions:

```r
if(!is.vector(PARAS) || !is.numeric(PARAS)){
    stop("Parameters must be in a numeric vector");
}
```

---

## Issue 4: `find_descending_order` modifies the caller's `by_array` in place

**File:** `src/utilities.c`, lines 63–94 (function `find_descending_order`)

**Description:** This function sorts the elements of `order_array` into descending order based on the values in `by_array`. However, it modifies `by_array` in place during the sorting process:

```c
void find_descending_order(int *order_array, double *by_array, int length){
    int i, k, max_index, *sarray;
    double max_val, min_val;

    sarray = (int *) malloc(length * sizeof(int));
    for(i = 0; i < length; i++){
        sarray[i] = order_array[i];          // Save original order_array
    }

    // ... find min_val ...

    while(k < length){
        // ... find max_index ...
        by_array[max_index] = min_val - 1;   // ← MODIFIES by_array in place
        order_array[k]      = sarray[max_index];
        k++;
    }

    free(sarray);
}
```

At line 88, `by_array[max_index] = min_val - 1` destructively overwrites entries in `by_array` so they won't be selected again in subsequent iterations. This means the caller's data is silently destroyed. The function does not document this side effect, and the name `find_descending_order` gives no indication that the input array will be modified.

**Why it matters:** If a caller relies on `by_array` retaining its values after the call (e.g., for further computation or logging), the data will be silently corrupted. In a simulation context, this could introduce numerical errors that manifest differently depending on the order in which functions are called, making them extremely difficult to debug.

**How to fix:** Either:
- Make a copy of `by_array` inside the function (similar to how `sarray` copies `order_array`), or
- Rename the function to clearly indicate the side effect (e.g., `descending_order_inplace`), or
- Restructure the algorithm to not need in-place mutation (e.g., compute the order via an auxiliary index array and sort that instead).
