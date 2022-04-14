
################################################################################
count <- 1;
while(count < 125){

time_steps <- 200;
ga_min     <- 25 + count;
kmx        <- 250 + (count * 10);

alt_res <- function(X, K = 10000, rate = 1){
    X_1 <- X + rate*X*(1 - X/K);
    return(X_1);
}
# Alternative observation sub-model
alt_obs <- function(resource_vector){
    X_obs <- resource_vector;
    return(X_obs);
}

# Alternative manager sub-model
alt_man <- function(observation_vector){
    policy <- 10;
    if(policy < 0){
        policy <- 0;
    }
    return(policy);
}

# Alternative user sub-model
alt_usr <- function(manager_vector){
    harvest <- manager_vector + manager_vector * 0.1;
    return(harvest);
}

################################################################################
# Genetic algorithm
################################################################################
st_time   <- Sys.time();
gal_count <- 0;
ann_count <- 0;
sim_old   <- gmse_apply(res_mod = alt_res, obs_mod = alt_obs, man_mod = alt_man,
                        get_res = "Full", stakeholders = 1, X = 10000,
                        ga_seedrep = 0, ga_mingen = ga_min, 
                        user_annealing = FALSE, mana_annealing = FALSE, 
                        kmax_annealing = kmx, user_budget = 10000);
gal_count <- gal_count + sim_old$PARAS[[141]];
ann_count <- ann_count + sim_old$PARAS[[142]];
sim_sum_1 <- matrix(data = NA, nrow = time_steps, ncol = 9);
for(time_step in 1:time_steps){
    sim_new                 <- gmse_apply(get_res = "Full", old_list = sim_old,
                                          res_mod = alt_res, obs_mod = alt_obs,
                                          man_mod = alt_man);
    sim_sum_1[time_step, 1] <- time_step;
    sim_sum_1[time_step, 2] <- sim_new$basic_output$resource_results[1];
    sim_sum_1[time_step, 3] <- sim_new$basic_output$observation_results[1];
    sim_sum_1[time_step, 4] <- sim_new$basic_output$manager_results[3];
    sim_sum_1[time_step, 5] <- sum(sim_new$basic_output$user_results[,3]);
    sim_sum_1[time_step, 6] <- sim_old$PARAS[[141]];
    sim_sum_1[time_step, 7] <- sim_old$PARAS[[142]];
    sim_sum_1[time_step, 8] <- sim_new$COST[1,9,2];
    sim_sum_1[time_step, 9] <- sim_new$user_budget;
    gal_count               <- gal_count + sim_old$PARAS[[141]];
    ann_count               <- ann_count + sim_old$PARAS[[142]];
}
colnames(sim_sum_1) <- c("Time", "Pop_size", "Pop_est", "Cull_cost", 
                         "Cull_count", "GA_count", "AN_count", "Cull_cost",
                         "User_budget");

end_time <- Sys.time();
tot_time <- end_time - st_time;
gal_fcal <- sum(sim_sum_1[,6]);
GA_stats <- c(time_steps, tot_time, gal_fcal, mean(sim_sum_1[101:200,5]));
################################################################################

################################################################################
# Simulated Annealing
################################################################################
st_time   <- Sys.time();
gal_count <- 0;
ann_count <- 0;
sim_old   <- gmse_apply(res_mod = alt_res, obs_mod = alt_obs, man_mod = alt_man,
                        get_res = "Full", stakeholders = 1, X = 10000,
                        ga_seedrep = 0, ga_mingen = ga_min, 
                        user_annealing = TRUE, mana_annealing = FALSE, 
                        kmax_annealing = kmx, user_budget = 10000);
gal_count <- gal_count + sim_old$PARAS[[141]];
ann_count <- ann_count + sim_old$PARAS[[142]];
sim_sum_2 <- matrix(data = NA, nrow = time_steps, ncol = 9);
for(time_step in 1:time_steps){
    sim_new                 <- gmse_apply(get_res = "Full", old_list = sim_old,
                                          res_mod = alt_res, obs_mod = alt_obs,
                                          man_mod = alt_man);
    sim_sum_2[time_step, 1] <- time_step;
    sim_sum_2[time_step, 2] <- sim_new$basic_output$resource_results[1];
    sim_sum_2[time_step, 3] <- sim_new$basic_output$observation_results[1];
    sim_sum_2[time_step, 4] <- sim_new$basic_output$manager_results[3];
    sim_sum_2[time_step, 5] <- sum(sim_new$basic_output$user_results[,3]);
    sim_sum_2[time_step, 6] <- sim_old$PARAS[[141]];
    sim_sum_2[time_step, 7] <- sim_old$PARAS[[142]];
    sim_sum_2[time_step, 8] <- sim_new$COST[1,9,2];
    sim_sum_2[time_step, 9] <- sim_new$user_budget;
    gal_count               <- gal_count + sim_old$PARAS[[141]];
    ann_count               <- ann_count + sim_old$PARAS[[142]];
}
colnames(sim_sum_2) <- c("Time", "Pop_size", "Pop_est", "Cull_cost", 
                         "Cull_count", "GA_count", "AN_count", "Cull_cost",
                         "User_budget");
end_time <- Sys.time();
tot_time <- end_time - st_time;
ann_fcal <- sum(sim_sum_2[,7]);
AN_stats <- c(time_steps, tot_time, ann_fcal, mean(sim_sum_2[101:200,5]));
################################################################################



AN_summary <- rbind(AN_summary, AN_stats);
GA_summary <- rbind(GA_summary, GA_stats);

count <- count + 1;
}


# Run simulations and get fitness by getting number of culls made in last gen



################################################################################
# Side-by-side fair comparison
################################################################################
sim <- gmse(time_max = 40, land_ownership = TRUE, scaring = TRUE, 
            culling = TRUE, castration = TRUE, help_offspring = TRUE, 
            tend_crops = TRUE, kill_crops = TRUE, plotting = FALSE,
            ga_seedrep = 0);


sim <- gmse(time_max = 40, land_ownership = TRUE, scaring = TRUE, 
            culling = TRUE, castration = TRUE, help_offspring = TRUE, 
            tend_crops = TRUE, kill_crops = TRUE, plotting = FALSE,
            ga_seedrep = 0, user_annealing = TRUE, mana_annealing = TRUE,
            kmax_annealing = 100);


end_time <- Sys.time();
tot_time <- end_time - st_time;
gal_fcal <- sum(sim_sum_1[,6]);
GA_stats <- c(time_steps, tot_time, gal_fcal, mean(sim_sum_1[101:200,5]));







