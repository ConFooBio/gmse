install.packages("devtools");
devtools::install_github("confoobio/gmse", ref = "rev");

sim.pop <- gmse(consume_surv = 3, consume_repr = 4.5, times_feeding = 12, res_consume = 0.41,
                res_birth_type = 0, res_death_type = 0, land_ownership = TRUE,
                stakeholders = 10, scaring = FALSE, time_max = 100, manager_budget = 1, user_budget = 1,
                RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE);

rep.pop <- gmse_replicates(replicates = 10,
                           consume_surv = 3, consume_repr = 4.5, times_feeding = 12, res_consume = 0.41,
                           res_birth_type = 0, res_death_type = 0, land_ownership = TRUE,
                           stakeholders = 10, scaring = FALSE, time_max = 100, manager_budget = 1, user_budget = 1,
                           RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE)

sim.use <- gmse(consume_surv = 3, consume_repr = 4.5, times_feeding = 12, res_consume = 0.41,
                res_birth_type = 0, res_death_type = 0, land_ownership = TRUE,
                stakeholders = 10, scaring = FALSE, time_max = 100, manager_budget = 1, user_budget = 1000,
                RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE);

rep.use <- gmse_replicates(replicates = 10,
                           consume_surv = 3, consume_repr = 4.5, times_feeding = 12, res_consume = 0.41,
                           res_birth_type = 0, res_death_type = 0, land_ownership = TRUE,
                           stakeholders = 10, scaring = FALSE, time_max = 20, manager_budget = 1, user_budget = 1000,
                           RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE)

sim.man <- gmse(consume_surv = 3, consume_repr = 4.5, times_feeding = 12, res_consume = 0.41,
                res_birth_type = 0, res_death_type = 0, land_ownership = TRUE,
                stakeholders = 10, scaring = FALSE, time_max = 20, manager_budget = 500, user_budget = 1000,
                RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE);

rep.man <- gmse_replicates(replicates = 10,
                           consume_surv = 3, consume_repr = 4.5, times_feeding = 12, res_consume = 0.41,
                           res_birth_type = 0, res_death_type = 0, land_ownership = TRUE,
                           stakeholders = 10, scaring = FALSE, time_max = 20, manager_budget = 500, user_budget = 1000,
                           RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE)
rep.man
# IDEA: Sum the square of the deviation at each time step, and use it as a measure of stability