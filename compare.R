library(GMSE)

# regular <- gmse_replicates(replicates = 10, time_max = 10, mem_prv_observ = FALSE, traj_pred = FALSE, plotting = FALSE, land_ownership = TRUE, stakeholders = 20)
# regular

#pred <- gmse_replicates(replicates = 10, time_max = 10, mem_prv_observ = TRUE, traj_pred = TRUE, plotting = FALSE, land_ownership = TRUE, stakeholders = 20)
pred <- gmse(time_max = 10, mem_prv_observ = TRUE, traj_pred = TRUE, plotting = FALSE, land_ownership = TRUE, stakeholders = 20)

# Check abundance estimations
pred$paras[,100]

# Check previous time step estimation (Should be the same as pred$paras[,100])
pred$paras[,130]

# Check predicion (Should be a linear extrapolation of pred$paras[t-1,100] and pred$paras[t,100])
pred$paras[,136]
# on my computer it's the same as abundances, I don't understant why still