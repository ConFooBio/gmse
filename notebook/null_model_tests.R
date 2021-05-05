### Default GMSE models, test run:
sim_old = gmse_apply(get_res = "Full", manage_target = 700)
### Note some culling occurs because initial population likely above target:
sim_old$basic_output

### Create "null" models.
### Each of these mirrors the default models manager.R and user.R but strips out the call to the default models,
###  and simply returns what was passed to the function.
manager_NULL = function(RESOURCES   = NULL,
                        AGENTS      = NULL,
                        LAND        = NULL, 
                        PARAS       = NULL,
                        COST        = NULL,
                        ACTION      = NULL
                        ) {
    MANAGER_OUT = list(RESOURCES, AGENTS, LAND, ACTION, COST, PARAS)
    names(MANAGER_OUT) = c("RESOURCES", "AGENTS", "LAND", "ACTION", "COST", "PARAS")
    return(MANAGER_OUT)
}

user_NULL <- function(RESOURCES  = NULL,
                 AGENTS     = NULL,
                 LAND       = NULL, 
                 PARAS      = NULL,
                 COST       = NULL,
                 ACTION     = NULL){
    
    USER_OUT = list(RESOURCES, AGENTS, LAND, ACTION, COST, PARAS)
    names(USER_OUT) <- c("RESOURCES", "AGENTS", "LAND", "ACTION", "COST","PARAS");
    return(USER_OUT);
}

### This repeats the test above, but using the custom "null" models above
sim_old = gmse_apply(get_res = "Full", manage_target = 500, man_mod = manager_NULL, use_mod = user_NULL)
### Note lack of any culling in spite of population being over target:
sim_old$basic_output


### Looping tests. Just to verify that (1) gmse_apply() looping `old_list` works as expected with the above null models,
### and (2) that repeated simulation runs of these give the same approximate trajectories to the default models where no
### actions can be taken.

### Number of iterations:
J = 10

### Number of time steps:
K = 10 
### Default model but ensuring no culling can happen:

OUT_nulls = list()
OUT_default = list()

for(j in 1:J) {
    
    sim_old_default = gmse_apply(get_res = "Full", manage_target = 1000, user_budget = 1, scaring = TRUE)
    sim_old_nulls = gmse_apply(get_res = "Full", manage_target = 1000, user_budget = 1000, scaring = TRUE, man_mod = manager_NULL, use_mod = user_NULL)
    
    out_default = matrix(NA, nrow = K, ncol = 4)
    out_nulls = matrix(NA, nrow = K, ncol = 4)
    colnames(out_default) = c("res","obs","culls","scares")
    colnames(out_nulls) = c("res","obs","culls","scares")
    
    out_default[1,1] = sim_old_default$basic_output$resource_results
    out_default[1,2] = sim_old_default$basic_output$observation_results
    out_default[1,3] = sum(sim_old_default$basic_output$user_results[,"culling"], na.rm=T)
    out_default[1,4] = sum(sim_old_default$basic_output$user_results[,"scaring"], na.rm=T)
    
    out_nulls[1,1] = sim_old_nulls$basic_output$resource_results
    out_nulls[1,2] = sim_old_nulls$basic_output$observation_results
    out_nulls[1,3] = sum(sim_old_nulls$basic_output$user_results[,"culling"], na.rm=T)
    out_nulls[1,4] = sum(sim_old_nulls$basic_output$user_results[,"scaring"], na.rm=T)
    
    for(i in 2:K) {
        sim_new_default = gmse_apply(get_res = "Full", old_list= sim_old_default)
        sim_new_nulls = gmse_apply(get_res = "Full", man_mod = manager_NULL, use_mod = user_NULL, old_list= sim_old_nulls)
        
        out_default[i,1] = sim_new_default$basic_output$resource_results
        out_default[i,2] = sim_new_default$basic_output$observation_results
        out_default[i,3] = sum(sim_new_default$basic_output$user_results[,"culling"], na.rm=T)
        out_default[i,4] = sum(sim_new_default$basic_output$user_results[,"scaring"], na.rm=T)
        
        out_nulls[i,1] = sim_new_nulls$basic_output$resource_results
        out_nulls[i,2] = sim_new_nulls$basic_output$observation_results
        out_nulls[i,3] = sum(sim_new_nulls$basic_output$user_results[,"culling"], na.rm=T)
        out_nulls[i,4] = sum(sim_new_nulls$basic_output$user_results[,"scaring"], na.rm=T)
        
        sim_old_default = sim_new_default
        sim_old_nulls = sim_new_nulls
    }
    OUT_default[[j]] = out_default
    OUT_nulls[[j]] = out_nulls
}

lo1 = unlist(lapply(OUT_default, function(x) min(x[,1], na.rm=T)))
lo2 = unlist(lapply(OUT_nulls, function(x) min(x[,1], na.rm=T)))
hi1 = unlist(lapply(OUT_default, function(x) max(x[,1], na.rm=T)))
hi2 = unlist(lapply(OUT_nulls, function(x) max(x[,1], na.rm=T)))
ylo = floor(min(lo1, lo2)*0.95)
yhi = ceiling(max(hi1, hi2)*1.05)

plot(OUT_default[[1]][,1], type = "n", ylim = c(ylo,yhi))
lapply(OUT_default, function(x) lines(x[,1], col = "red", lwd = 2))
lapply(OUT_nulls, function(x) lines(x[,1], col = "blue", lwd = 1.5))

