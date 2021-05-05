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


