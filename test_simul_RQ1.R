#### The new feature seems to work fine, first try for systematic exploration of the results ####

#################################################################################################
# rm(list=ls())

# packages
library("GMSE", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.5")

# one test simulation
simtest_at <- gmse(land_ownership = TRUE, stakeholders = 2, observe_type = 0,
                   res_death_K = 2000, manage_target = 1000, RESOURCE_ini = 1000,
                   user_budget = 1000, manager_budget = 1000, res_consume = 1,
                   scaring = F, plotting = T, time_max = 20, action_thres = 0.1, budget_bonus = 0.1)

# check if manager are actually waiting sometimes
# the last two variables in $paras are pol_updated and number of time step since last policy update
simtest_at$paras[,107:108]

# a priori its working
# still don't understand why, even without the new features, managers can't stop the pop