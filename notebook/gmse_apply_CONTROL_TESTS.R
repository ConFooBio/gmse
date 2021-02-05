rm(list=ls())
source("notebook/gmse_apply_CONTROL.R")
source("R/gmse_apply.R")
rm(gmse_apply)

LAND_OWNERSHIP = TRUE
OBSERVE_TYPE = 1
RES_MOVE_OBS = TRUE
RES_DEATH_K = 3000
LAMBDA = 0.3
MANAGE_TARGET = 1500
STAKEHOLDERS = 4
USER_BUDGET = 1500

### Initial time steps:
init_steps = init_man_control()
# Appends output:
output = init_steps$summary 
# Extracts last time step (last `old_list`)
prev = init_steps$gmse_list[[length(init_steps$gmse_list)]]
# Print output:
init_steps$summary
init_steps$observed_suggested

### User input
costs_as_input = list(culling = 10, scaring = 100001)
prev = set_man_costs(prev, newcost = costs_as_input)

### Run next time step:
nxt = gmse_apply_UROM(get_res = "Full", old_list = prev)
# Add appropriate outputs.
output = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = output)
# Reset time step
prev = nxt

### Output new results:
output
observed_suggested(prev)
