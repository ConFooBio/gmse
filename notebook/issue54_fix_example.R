### With proposed change in jeroen_issue54_fix branch, 5 may 2021

rm(list=ls())

global_ld1 = 111 # "global" parameter

# This obviously works fine:
m = gmse_apply(get_res = "Full", land_dim_1 = global_ld1)
dim(m$LAND[,,1]) # Check of para used
### OK.

wrapper1 = function() {
    print(global_ld1)
    gmse_apply(get_res = "Full", land_dim_1 = global_ld1)
}
m2 = wrapper1()
dim(m2$LAND[,,1])
### OK.

### Now we try to use a "local" para defined inside the wrapper:
wrapper2 = function() {
    local_ld1 = 222
    print(local_ld1)
    gmse_apply(get_res = "Full", land_dim_1 = local_ld1)
}
m3 = wrapper2()
dim(m3$LAND[,,1])
### OK.

wrapper3 = function(ld1) {
    print(ld1)
    gmse_apply(get_res = "Full", land_dim_1 = ld1)
}
m4 = wrapper3(ld1 = 321)
dim(m4$LAND[,,1])
### OK.

### Testing the above with some 'old_list' loops inside the wrapper:

wrapper4 = function(ld1) {
    M = list()
    M[[1]] = gmse_apply(get_res = "Full", land_dim_1 = ld1)
    for(i in 2:5) {
        M[[i]] = gmse_apply(get_res = "Full", old_list = M[[i-1]])
    }
    return(M)
}
m5 = wrapper4(ld1 = 112)
lapply(m5, function(x) dim(x$LAND[,,1])[1])
### OK!

### Testing some more with multiple varying parameters for the wrapper:

wrapper5 = function(ld1, STAKEHOLDERS, LAND_OWNERSHIP) {
    M = list()
    M[[1]] = gmse_apply(get_res = "Full", land_dim_1 = ld1, stakeholders = STAKEHOLDERS, land_ownership = LAND_OWNERSHIP)
    for(i in 2:5) {
        M[[i]] = gmse_apply(get_res = "Full", old_list = M[[i-1]])
    }
    return(M)
}
m6 = wrapper5(ld1 = 113, STAKEHOLDERS = 3, LAND_OWNERSHIP = TRUE)
lapply(m6, function(x) x$stakeholders) # OK
lapply(m6, function(x) x$land_ownership) # OK
lapply(m6, function(x) x$land_dim_1) # OK

### Multiple wrappers:

## Two:
rm(list=ls())

f1 = function(init_stakeholders) {
    S = init_stakeholders
    f2(STAKEHOLDERS = S)
}

f2 = function(STAKEHOLDERS) {
    print(STAKEHOLDERS)
    gmse_apply(get_res = "Full", stakeholders = STAKEHOLDERS)
}

m = f1(5)
m$stakeholders
# OK!

## Three:
f2 = function(STAKEHOLDERS) {
    sholders = STAKEHOLDERS
    f3(S = sholders)
}

f3 = function(S) {
    gmse_apply(get_res = "Full", stakeholders = S)
}

m = f1(6)
m$stakeholders
# OK

## With looping in inside wrapper:
f3 = function(S) {
    M = list()
    M[[1]] = gmse_apply(get_res = "Full", stakeholders = S)
    for(i in 2:5) {
        M[[i]] = gmse_apply(get_res = "Full", old_list = M[[i-1]])
    }
    return(M)
}
M = f1(3)
lapply(M, function(x) x$stakeholders)
# OK

### Does it work when paras are in lists?
rm(list=ls())

f10 = function(x) {
    GMSE_PARAS = list(land_dim_2 = 123)
    gmse_apply(get_res = "Full", land_dim_2 = GMSE_PARAS$land_dim_2)
}
m = f10()
m$land_dim_2
# OK

rm(list=ls())
f10 = function(gmse_paras) {
    f11(pars = gmse_paras)
}
f11 = function(pars) {
    gmse_apply(get_res = "Full", land_dim_2 = pars$land_dim_2)
}
GMSE_PARAS = list(land_dim_2 = 111)
m = f10(gmse_paras = GMSE_PARAS)
m$land_dim_2
# OK


### Does passing custom model functions work as expected?
rm(list=ls())

### Custom resource function specified in "global" environment
alt_res <- function(X, K = 2000, rate = 1){
    X_1 <- X + rate*X*(1 - X/K);
    return(X_1);
}

### Wrapper function, one argument of which includes the alternate resource model function, as well as parameter X for that function:
w = function(resource_function, stakeholders, x) {
    # Note the call to gmse_apply includes the given resource function, as well as parameter X for that function:
    gmse_apply(get_res = "Full", res_mod = resource_function, X = x)
}
w(resource_function = alt_res, x = 1111)
# OK

### Note that if we re-specify the wrapper with X named as X we get the my_way_or_the_highway warning, which shows the latter works really well!
w = function(resource_function, stakeholders, X) {
    # Note the call to gmse_apply includes the given resource function, as well as parameter X for that function:
    gmse_apply(get_res = "Full", res_mod = resource_function, X = X)
}
w(resource_function = alt_res, X = 1234)

stakeholders = 5
gmse_apply(get_res = "Full", res_mod = alt_res, X = 1000, stakeholders = stakeholders, my_way_or_the_highway = TRUE)

### It is very interesting though that this proposed fix also appears to circumvent the problems caused by re-using variable names 
###  (i.e. the reason why the my_way_or_the_highway safety catch was put in place)!
### Examples:

### This causes the warning, as expected:
another_wrapper = function(stakeholders = 5) {
    gmse_apply(get_res = "Full", stakeholders = stakeholders)
}
another_wrapper()

### Whereas this does not, nor does it cause a crash:
yet_another_wrapper = function(stakeholders = 5) {
    gmse_apply(get_res = "Full", stakeholders = stakeholders, my_way_or_the_highway = TRUE)
}
yet_another_wrapper()
### If the last wrapper is run using the current `master` version of GMSE, it does cause serious problems (bomb!)
### So, perhaps the memory-related issues caused by re-using parameter names is circumvented by the proposed fix?
### Another example that would crash R using the current CRAN version, but seems to behave OK when using this fix:
rm(list=ls())
stakeholders = 5
gmse_apply(stakeholders = stakeholders, my_way_or_the_highway = TRUE)
