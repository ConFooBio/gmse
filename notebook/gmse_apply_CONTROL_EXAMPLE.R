###
source("notebook/gmse_apply_CONTROL.R")
source("R/gmse_apply.R")
rm(gmse_apply)

K1 = 5

sim_old = gmse_apply(get_res = "Full", scaring = TRUE)

output = gmse_apply_summary(sim_old, include = c("res","obs","culls","scares"))
output

for(i in 2:K1) {
    sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
    output = gmse_apply_summary(sim_new, output)
    sim_old = sim_new
}
output 

### Done the first K1 steps.

### Now run interim model: this executes everything but the USER model - i.e. the user actions contained in 
###  'interim' DO NOT APPLY, and the manager costs can/will be overridden.
interim = gmse_apply(get_res = "Full", old_list=sim_old, scaring = FALSE, culling = FALSE)
#interim = gmse_apply(get_res = "Full", use_mod = user_NULL, old_list=sim_old)

###### At this point, apparently there ARE culled individuals in the resource array?
interim$RESOURCES[,17]
interim$resource_array[,17]


### Add RESOURCE and OBSERVATION values from interim to output, as these will not change:
output = rbind(output, NA)
output[nrow(output),c("res","obs")] = gmse_apply_summary(interim, include = c("res","obs"))

### Ask for user input
observed_suggested(interim)
print("Enter desired culling cost for all users (blank/Enter accepts suggested cost):")
new_culling <- scan(n=1);
#################################################################################################
interim = set_man_costs(interim, newcost = list(culling = new_culling))


### Now start re-looping gmse_apply, but starting from USER model, with new costs.
### Note that in each loop, the user actions in a given iterations are actually those taken in the "previous"
###  line of `output`:
n = gmse_apply_UROM(get_res = "Full", old_list = interim, scaring = TRUE, culling = TRUE)

### Add previous actions taken. This is on the last line of output, which is essentially t-1:
output[nrow(output),c("culls","scares")] = gmse_apply_summary(n, include = c("culls","scares"))

### Add new population/observation:
output = rbind(output, NA)
output[nrow(output),c("res","obs")] = gmse_apply_summary(n, include = c("res","obs"))
output

### New becomes old:
sim_old = n


n = gmse_apply_UROM(get_res = "Full", old_list = sim_old)

### Add previous actions taken. This is on the last line of output, which is essentially t-1:
output[nrow(output),c("culls","scares")] = gmse_apply_summary(n, include = c("culls","scares"))

### Add new population/observation:
output = rbind(output, NA)
output[nrow(output),c("res","obs")] = gmse_apply_summary(n, include = c("res","obs"))
output

### New becomes old:
sim_old = n




### New time step:
n = gmse_apply_UROM(get_res = "Full", old_list = sim_old)
### Add new resource/obs:
output = rbind(output, NA)
output[nrow(output),c("res","obs")] = gmse_apply_summary(n, include = c("res","obs"))



output = gmse_apply_summary(n, output)
observed_suggested(n)
new_culling <- scan(n=1);
sim_old = set_man_costs(n, newcost = list(culling = new_culling))

n = gmse_apply_UROM(get_res = "Full", old_list = sim_old)
output[nrow(output)-1,c("culls","scares")] = gmse_apply_summary(n, include = c("culls","scares"))
