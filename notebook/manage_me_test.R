rm(list=ls())

K = 20 ### Number of GMSE time steps

### This first set is using `gmse_apply()` with default settings

### Initial run and set up output:
sim_old = gmse_apply(get_res = "Full")
out = matrix(NA, nrow = K, ncol = 4)
colnames(out) = c("res","obs","cull_cost","cull_acts")
out[1, "res"] = sim_old$basic_output$resource_results
out[1, "obs"] = sim_old$basic_output$observation_results
out[1, "cull_cost"] = unique(sim_old$COST[1,9,2:5])
out[1, "cull_acts"] = sum(sim_old$basic_output$user_results[,"culling"])

### Loop ssquent runs:
for(i in 2:K) {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
    
    out[i, "res"] = sim_new$basic_output$resource_results
    out[i, "obs"] = sim_new$basic_output$observation_results
    out[i, "cull_cost"] = unique(sim_new$COST[1,9,2:5])
    out[i, "cull_acts"] = sum(sim_new$basic_output$user_results[,"culling"])
    
    sim_old = sim_new
}


### Now repeat, but asking for manual set of culling cost in each time step:

### Initial run. This is in a clumsy non-loop to avoid code being executed before user input is complete.
for(i in 1:1) {
    
    sim_old = gmse_apply(get_res = "Full", manage_me = TRUE)
    
    ### Set up output and add initial run:
    out_man = matrix(NA, nrow = K, ncol = 4)
    colnames(out_man) = c("res","obs","cull_cost","cull_acts")
    out_man[1, "res"] = sim_old$basic_output$resource_results
    out_man[1, "obs"] = sim_old$basic_output$observation_results
    out_man[1, "cull_cost"] = unique(sim_old$COST[1,9,2:5])
    out_man[1, "cull_acts"] = sum(sim_old$basic_output$user_results[,"culling"])
    
}

for(i in 2:K) {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old, manage_me = TRUE)
    
    out_man[i, "res"] = sim_new$basic_output$resource_results
    out_man[i, "obs"] = sim_new$basic_output$observation_results
    out_man[i, "cull_cost"] = unique(sim_new$COST[1,9,2:5])
    out_man[i, "cull_acts"] = sum(sim_new$basic_output$user_results[,"culling"])

    sim_old = sim_new
}

if(K==i) {
    ylo = min(out[,1],out_man[,2])*0.95
    yhi = max(out[,1],out_man[,2])*1.05
    plot(out[,1], type = "l", lwd = 2, ylim = c(ylo,yhi))
    lines(out_man[,1], col = "red", lwd = 2)
    
}
