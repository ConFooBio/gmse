library(GMSE)

regular <- gmse(time_max = 5, mem_prv_observ = FALSE, traj_pred = FALSE, plotting = FALSE, land_ownership = TRUE, stakeholders = 5)
# regular

#pred <- gmse_replicates(replicates = 10, time_max = 10, mem_prv_observ = TRUE, traj_pred = TRUE, plotting = FALSE, land_ownership = TRUE, stakeholders = 20)
pred <- gmse(time_max = 5, mem_prv_observ = TRUE, traj_pred = TRUE, plotting = FALSE, land_ownership = TRUE, stakeholders = 5, scaring = FALSE)

# Check abundance estimations
pred$paras[,100]

# Check previous time step estimation (Should be the same as pred$paras[,100])
pred$paras[,130]

# Check predicion (Should be pred$paras[1,100] at the first time step and a linear extrapolation of pred$paras[t-1,100] and pred$paras[t,100] onward)
pred$paras[,136]
