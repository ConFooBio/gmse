### This is a short example of the use and utility of gmse_apply_summary().

### This function can be used to extract chosen summary statistics from a single run of gmse_apply() in much the same
### way as gmse_summary produces a summary of gmse() results.

### The function can be used on a single run of gmse_apply() if desired:
sim_old <- gmse_apply(get_res = "Full", scaring = TRUE, land_ownership = T)
gmse_apply_summary(data = sim_old, include = c("res","obs","culls","yield"))

### The values summarised can be controlled by `input`.
### Currently, the values for actions and yield is equal to the sum across all users.
gmse_apply_summary(data = sim_old, include = c("res","culls","scares","tend_crops"))
gmse_apply_summary(data = sim_old, include = c("obs","yield"))

### If no values are supplied for `input`, all possible outputs are reported:
gmse_apply_summary(data = sim_old)

### The argument `output` can use used to append the output of gmse_apply_summary() to a previous output.
### For example, this can be usd when looping gmse_apply() over a number of iterations.
### When doing this, it is not needed to specify values for `include` over subsequent calls to gmse_apply_summary(); 
### in such cases, only the output columns already present in `output` are appended.

K <- 3 # Number of iterations
sim_old <- gmse_apply(get_res = "Full", scaring = TRUE, culling = TRUE, land_ownership = TRUE)
output  <- gmse_apply_summary(data = sim_old, include = c("res","obs","culls","scares"))

for(i in 2:K) {
    sim_new = gmse_apply(get_res = "Full", old_list=sim_old)
    output = gmse_apply_summary(data = sim_new, output = output)
    sim_old = sim_new
}
output

### Just to show that the above produces the same output when manually adding these results to an output matrix,
###  and to show how the for-loop is shortened by using gmse_apply_summary():

sim_old = gmse_apply(get_res = "Full", scaring = TRUE, culling = TRUE, land_ownership = TRUE)
out = matrix(NA, ncol = 4, nrow = K)
out[1,1] = sim_old$basic_output$resource_results
out[1,2] = sim_old$basic_output$observation_results
out[1,3] = sum(sim_old$basic_output$user_results[,"culling"],na.rm=T)
out[1,4] = sum(sim_old$basic_output$user_results[,"scaring"],na.rm=T)
for(i in 2:K) {
  sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
  out[i,1] = sim_new$basic_output$resource_results
  out[i,2] = sim_new$basic_output$observation_results
  out[i,3] = sum(sim_new$basic_output$user_results[,"culling"],na.rm=T)
  out[i,4] = sum(sim_new$basic_output$user_results[,"scaring"],na.rm=T)
  sim_old = sim_new
}
out


