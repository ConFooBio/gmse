### Proposed wrapper function for gmse_apply() and test runs
### 
### See this Github issue: https://github.com/ConFooBio/gmse/issues/54
### 
### The code below works but only when the function place_args() in gmse_apply() is
###  adapted as per the [latest version in brach 'jeroen']
###  https://github.com/ConFooBio/gmse/blob/jeroen/R/gmse_apply.R.
### See comments in that file; original respecification of code 
### https://github.com/ConFooBio/gmse/commit/cc1f80cfd8ac910cd557c54941e3e955c067961b.

rm(list=ls())

library(GMSE)

### This is the semi-working wrapper. Note the need to include ... arguments in both gmse_apply function calls
###  as well as the wrapper itself, to ensure the custom parameters are passed on correctly.
gmse_sims = function(s, y, ...) {
    res = list()
    for(sim in 1:s) {
        res_year = as.list(rep(NA, y))
        sim_old = gmse_apply(get_res = "Full", ...)
        for(year in 1:y) {
            sim_new = gmse_apply(get_res = "Full", old_list = sim_old, ...)
            res_year[[year]] = sim_new
            sim_old <- sim_new
            print(sprintf("Sim %d, year %d", sim, year))
        }
        res[[sim]] = res_year
    }
    return(res)
}

### The following uses the above wrapper to run with a set of custom parameters contained in a list.
### Create the list:
gmse_paras = list(observe_type = 3, res_consume = 0.5)

### Set number of replicates (sims) and time steps (years).
sims = 2
years = 2

### Run the wrapper:
fun_res = gmse_sims(s = 2, y = 2, 
                    observe_type = gmse_paras[["observe_type"]], 
                    res_consume = gmse_paras[["res_consume"]])

### Check the parameters have been passed on as expected (for an arbitrary sim/year combination):
fun_res[[1]][[2]][["get_res"]]
fun_res[[1]][[2]][["observe_type"]]
fun_res[[1]][[2]][["res_consume"]]
### All seems fine.

### Now to re-run the above, but including land_dim_1 as a parameter:
gmse_paras = list(observe_type = 3, res_consume = 0.5, land_dim_1 = 120)

### Set number of replicates (sims) and time steps (years).
sims = 2
years = 2

### Run the wrapper THIS CRASHES MY R/RStudio INSTALLATION
fun_res = gmse_sims(s = 2, y = 2, 
                    observe_type = gmse_paras[["observe_type"]], 
                    res_consume = gmse_paras[["res_consume"]],
                    land_dim_1 = gmse_paras[["land_dim_1"]])


