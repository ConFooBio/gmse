rm(list=ls())

# Alternative manager sub-model
alt_man <- function(observation_vector){
    return(observation_vector)
}

# Alternative user sub-model
alt_usr <- function(manager_vector){
    return(manager_vector)
}

K = 10
out = matrix(NA, nrow = K, ncol = 4)
colnames(out) = c("res","obs","cull_cost","cull_acts")

sim_old = gmse_apply(get_res = "Full", man_mod = alt_man, use_mod = alt_usr)
out[1, "res"] = sim_old$basic_output$resource_results
out[1, "obs"] = sim_old$basic_output$observation_results

for(i in 2:10) {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old, man_mod = alt_man, use_mode = alt_usr)
    out[i, "res"] = sim_new$basic_output$resource_results
    out[i, "obs"] = sim_new$basic_output$observation_results
    sim_old = sim_new
}

out2 = matrix(NA, nrow = K, ncol = 4)
colnames(out2) = c("res","obs","cull_cost","cull_acts")

sim_old = gmse_apply(get_res = "Full", user_budget = 1, culling = FALSE)
out2[1, "res"] = sim_old$basic_output$resource_results
out2[1, "obs"] = sim_old$basic_output$observation_results
out2[1, "cull_cost"] = unique(sim_old$COST[1,9,2:5])
out2[1, "cull_acts"] = sum(sim_old$basic_output$user_results[,"culling"])

for(i in 2:10) {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old, man_mod = alt_man, use_mode = alt_usr)
    out2[i, "res"] = sim_new$basic_output$resource_results
    out2[i, "obs"] = sim_new$basic_output$observation_results
    out2[1, "cull_cost"] = unique(sim_old$COST[1,9,2:5])
    out2[1, "cull_acts"] = sum(sim_old$basic_output$user_results[,"culling"])
    sim_old = sim_new
}
