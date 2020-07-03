library(devtools)
library(gmse)
devtools::install_github("AdrianBach/gmse", ref = "master")

sim <- gmse(consume_surv = 4.75, consume_repr = 5, times_feeding = 12, 
            res_consume = 0.5, res_birth_type = 0, res_death_type = 0, 
            land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
            stakeholders = 40, scaring = FALSE, time_max = 20, 
            group_think = FALSE, manager_budget = 1000, user_budget = 1000, 
            manager_sense = 0.15, RESOURCE_ini = 1000, observe_type = 3, 
            res_move_obs = FALSE, plotting = FALSE,
            manage_target = 2000)
p.def <- plot_gmse_results(sim)

sim.upt <- gmse(action_thres = 0.2, budget_bonus = 0.5, mem_prv_observ = FALSE,
                consume_surv = 4.75, consume_repr = 5, times_feeding = 12, 
                res_consume = 0.5, res_birth_type = 0, res_death_type = 0, 
                land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
                stakeholders = 40, scaring = FALSE, time_max = 20, 
                group_think = FALSE, manager_budget = 1000, user_budget = 1000, 
                manager_sense = 0.15, RESOURCE_ini = 1000, observe_type = 3, 
                res_move_obs = FALSE, plotting = FALSE,
                manage_target = 2000)
p.ati <- plot_gmse_results(sim.upt)

sim.mem <- gmse(action_thres = 0.2, budget_bonus = 0.5, mem_prv_observ = TRUE,
                consume_surv = 4.75, consume_repr = 5, times_feeding = 12, 
                res_consume = 0.5, res_birth_type = 0, res_death_type = 0, 
                land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
                stakeholders = 40, scaring = FALSE, time_max = 20, 
                group_think = FALSE, manager_budget = 1000, user_budget = 1000, 
                manager_sense = 0.15, RESOURCE_ini = 1000, observe_type = 3, 
                res_move_obs = FALSE, plotting = FALSE, 
                manage_target = 2000)
p.mem <- plot_gmse_results(sim.mem)
