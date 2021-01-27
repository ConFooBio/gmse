###
source("notebook/gmse_apply_CONTROL.R")
source("R/gmse_apply.R")
rm(gmse_apply)

K1 = 10
K2 = 20

sim_old = gmse_apply(get_res = "Full", land_ownership = TRUE, scaring = FALSE, culling = TRUE, 
                     manage_target = 1500, RESOURCE_ini = 2500, res_death_K = 2500, lambda = 0.2)
output = gmse_apply_summary(sim_old, include = c("res","obs","culls","scares", "cull_cost", "scare_cost", "RES_CULLS"))
for(i in 2:K1) {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
    output = gmse_apply_summary(sim_new, output)
    sim_old = sim_new
}
output 

#sim_old$COST[1,8:12,2:5]

#### gmse_apply_INTERIM() essentially runs gmse_apply() but skipping the user model altogether, i.e. no actions are
#### taken, evem though costs have been set and retained in the output list.
#### So the following should lead to resource population growth, provided the population is below CC:
for(i in 1:K2) {
    sim_new = gmse_apply_INTERIM(get_res = "Full", old_list = sim_old)
    output = gmse_apply_summary(sim_new, output)
    sim_old = sim_new
}
output

