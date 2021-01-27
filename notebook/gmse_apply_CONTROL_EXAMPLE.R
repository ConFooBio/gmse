###
source("notebook/gmse_apply_CONTROL.R")
source("R/gmse_apply.R")
rm(gmse_apply)

K1 = 5
K2 = 15

sim_old = gmse_apply(get_res = "Full", land_ownership = TRUE, scaring = FALSE, culling = TRUE, manage_target = 1500, res_death_K = 3000, lambda = 0.2)
output = gmse_apply_summary(sim_old, include = c("res","obs","culls","scares", "cull_cost", "scare_cost", "RES_CULLS"))

#### 1. First K1-1 time steps as normal to set up population run:
for(i in 2:K1) {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
    output = gmse_apply_summary(sim_new, output)
    sim_old = sim_new
}
output 

#### 2. Run "interim" time step where user actions as surpressed:
sim_new = gmse_apply_INTERIM(get_res = "Full", old_list = sim_old)
output = gmse_apply_summary(sim_new, output) 
# Reset selected output for interim time step (as no actions have been taken)
output[nrow(output),c("culls","scares","cull_cost","scare_cost","RES_CULLS")] = NA
output

#### 3. Returned observed population size and suggested action costs:
observed_suggested(sim_new)

#### 4. Set desired action costs (user input), and reset sim_old:
sim_new = set_man_costs(sim_new, newcost = list(culling = 99999))
sim_old = sim_new

#### 5. Run UROM gmse_apply():
sim_new = gmse_apply_UROM(get_res = "Full", old_list = sim_old)
# Add output to previous time step as relevant.
# This is the MANUALLY set costs, i.e. those retained in sim_old, but the actions taken from sim_new:
output[nrow(output),"cull_cost"] = mean(observed_suggested(sim_old)$culling)
output[nrow(output),"scare_cost"] = mean(observed_suggested(sim_old)$scaring)
output[nrow(output),"culls"] = sum(sim_new$ACTION[1,9,2:dim(sim_new$ACTION)[3]])
output[nrow(output),"scares"] = sum(sim_new$ACTION[1,8,2:dim(sim_new$ACTION)[3]])
output[nrow(output),"RES_CULLS"] = sum(sim_new$RESOURCES[,17]!=0)
# Add next output line, and append new resource and observed:
output = rbind(output, output[nrow(output),])
output[nrow(output),] = NA
output[nrow(output),c("res","obs")] = gmse_apply_summary(sim_new, include = c("res","obs"))
output

#### 6. Report new observed_suggsted:
observed_suggested(sim_new)



######## RETURN TO STEP 4


### First run of UROM model:
sim_new = gmse_apply_UROM(get_res = "Full", old_list = sim_old)
### Add output. Actions returned are those taken in the "previous" time step, and costs are those set by the user in the
### last step; so add these to the previous output line:
output[nrow(output),"culls"] = sum(sim_new$ACTION[1,9,2:dim(sim_new$ACTION)[3]])
output[nrow(output),"scares"] = sum(sim_new$ACTION[1,8,2:dim(sim_new$ACTION)[3]])
output[nrow(output),"cull_cost"] = mean(manual_costs[2,])     # These are still from the ones set previously
output[nrow(output),"scare_cost"] = mean(manual_costs[1,])    # These are still from the ones set previously
### Add a new line for the next time step.
output = rbind(output,output[nrow(output),]) 
output[nrow(output),] = NA
output[nrow(output), c("res","obs")] = gmse_apply_summary(sim_new, include = c("res","obs"))
output

sim_new$resource_vector                # What is the real population?
sim_new$observation_vector             # What is the observed population?
sim_new$COST[1,8:12,2:5]           # What are the suggested costs?
manual_costs = sim_new$COST[1,8:12,2:5]  # Vector to keep new manually set costs
manual_costs[2,] = 10               # Set cull cost to max
sim_new$COST[1,8:12,2:5]  = manual_costs  # Reset costs to manual
sim_new$COST[1,8:12,2:5]                  # Double check
sim_new$manager_array = sim_new$COST  # Ensure manager_array and COST are the same.
### Reset - UROM time step finished:
sim_old = sim_new

