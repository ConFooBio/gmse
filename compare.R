library(GMSE)

regular <- gmse_replicates(replicates = 10, time_max = 10, mem_prv_observ = FALSE, traj_pred = FALSE, plotting = FALSE, land_ownership = TRUE, stakeholders = 20)
regular

pred <- gmse_replicates(replicates = 10, time_max = 10, mem_prv_observ = TRUE, traj_pred = TRUE, plotting = FALSE, land_ownership = TRUE, stakeholders = 20)
pred