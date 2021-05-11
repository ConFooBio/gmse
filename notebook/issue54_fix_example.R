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

rm(list=ls())

alt_res <- function(X, K = 2000, rate = 1){
    X_1 <- X + rate*X*(1 - X/K);
    return(X_1);
}

w = function(ALT_RES, stakeholders) {
    Y = 1111
    gmse_apply(get_res = "Full", res_mod = ALT_RES, X = Y, stakeholders = stakeholders, my_way_or_the_highway = TRUE)
}

w(ALT_RES = alt_res, stakeholders = 5)


stakeholders = 5
gmse_apply(get_res = "Full", res_mod = alt_res, X = 1000, stakeholders = stakeholders, my_way_or_the_highway = TRUE)
