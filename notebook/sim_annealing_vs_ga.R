# Simple function for bootstrapping
simpleboot <- function(freqs, repli = 1000, alpha = 0.05){                           
    vals  <- NULL;                                                              
    i     <- 0;                                                             
    while(i < repli){                                                      
        boot  <- sample(x = freqs, size = length(freqs), replace = TRUE);           
        strap <- mean(boot);                                              
        vals  <- c(vals,strap);                                          
        i     <- i + 1;                                                    
    }                                                                      
    vals   <- sort(x = vals, decreasing = FALSE);                               
    lowCI  <- vals[round((alpha*0.5)*repli)];                               
    highCI <- vals[round((1-(alpha*0.5))*repli)];                         
    CIs    <- c(lowCI,highCI);                                             
    return(CIs);                                                            
} 

## RUN SIMULATIONS FOR THE ANALYSIS OF SIMULATED ANNEALING VS GENETIC ALGORITHM
################################################################################
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

count      <- 1;
AN_summary <- NULL;
GA_summary <- NULL;
while(count < 200){
  time_steps <- 40;
  ga_min     <- 1 * count;
  kmx        <- 1 * count;
  ##############################################################################
  # Genetic algorithm
  ##############################################################################
  st_time   <- Sys.time();
  gal_count <- 0;
  ann_count <- 0;
  sim_old   <- gmse_apply(res_mod = alt_res, obs_mod = alt_obs, man_mod = 
                          alt_man, get_res = "Full", stakeholders = 1, 
                          X = 10000, ga_seedrep = 0, ga_mingen = ga_min, 
                          user_annealing = FALSE, mana_annealing = FALSE, 
                          kmax_annealing = kmx, user_budget = 10000, mu_magnitude = 1000);
  gal_count <- gal_count + sim_old$PARAS[[141]];
  ann_count <- ann_count + sim_old$PARAS[[142]];
  sim_sum_1 <- matrix(data = NA, nrow = time_steps, ncol = 9);
  for(time_step in 1:time_steps){
      sim_new                 <- gmse_apply(get_res = "Full", 
                                            old_list = sim_old,
                                            res_mod = alt_res, 
                                            obs_mod = alt_obs,
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
  GA_stats <- c(time_steps, tot_time, gal_fcal, mean(sim_sum_1[31:40,5]));
  ##############################################################################

  ##############################################################################
  # Simulated Annealing
  ##############################################################################
  st_time   <- Sys.time();
  gal_count <- 0;
  ann_count <- 0;
  sim_old   <- gmse_apply(res_mod = alt_res, obs_mod = alt_obs, 
                          man_mod = alt_man, get_res = "Full", stakeholders = 1, 
                          X = 10000, ga_seedrep = 0, ga_mingen = ga_min, 
                          mu_magnitude = 1000, user_annealing = TRUE, 
                          mana_annealing = FALSE, kmax_annealing = kmx, 
                          user_budget = 10000);
  gal_count <- gal_count + sim_old$PARAS[[141]];
  ann_count <- ann_count + sim_old$PARAS[[142]];
  sim_sum_2 <- matrix(data = NA, nrow = time_steps, ncol = 9);
  for(time_step in 1:time_steps){
      sim_new                 <- gmse_apply(get_res = "Full", 
                                            old_list = sim_old,
                                            res_mod = alt_res, 
                                            obs_mod = alt_obs,
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
  AN_stats <- c(time_steps, tot_time, ann_fcal, mean(sim_sum_2[31:40,5]));
  ##############################################################################
  AN_summary <- rbind(AN_summary, AN_stats);
  GA_summary <- rbind(GA_summary, GA_stats);
  count <- count + 1;
  print(count);
}



file.remove("sa_fitness.txt");

# Run simulations and get fitness by getting number of culls made in last gen
reps <- 100;
while(reps > 0){
    sim <- gmse(land_ownership = TRUE, kmax_annealing = 2000, ga_popsize = 100,
                user_annealing = FALSE, mana_annealing = FALSE, ga_seedrep = 0, 
                plotting = FALSE, scaring = FALSE, culling = FALSE, 
                castration = FALSE, feeding = FALSE, help_offspring = FALSE, 
                tend_crops = FALSE, kill_crops = FALSE, mu_magnitude = 1000);
    reps <- reps - 1;
    print(reps);
}
# Run the above for all desired combinations of actions and annealing vs GA
# Need to print off in the game.c function
# Create the following files:
#
# ga_fitness_user_cull.txt
# sa_fitness_user_cull.txt
# ga_fitness_user_all.txt
# sa_fitness_user_all.txt
# ga_fitness_mana_cull.txt
# sa_fitness_mana_cull.txt
# ga_fitness_mana_all.txt
# sa_fitness_mana_all.txt
#
# These files will be used to create the CSV files below with the same names


## GA User cull
ga_fitness_user_cull    <- read.csv("ga_fitness_user_cull.txt", sep = "\t",
                                    header = FALSE);
fitcalls <- unique(ga_fitness_user_cull[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- ga_fitness_user_cull[ga_fitness_user_cull[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "ga_fitness_user_cull.csv", row.names = FALSE);


## SA User cull
sa_fitness_user_cull    <- read.csv("sa_fitness_user_cull.txt", sep = "\t",
                                    header = FALSE);
sa_fitness_user_cull[,4] <- sa_fitness_user_cull[,4] + 100;
fitcalls <- unique(sa_fitness_user_cull[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- sa_fitness_user_cull[sa_fitness_user_cull[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "sa_fitness_user_cull.csv", row.names = FALSE);

## GA User all
ga_fitness_user_all    <- read.csv("ga_fitness_user_all.txt", sep = "\t",
                                   header = FALSE);
fitcalls <- unique(ga_fitness_user_all[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- ga_fitness_user_all[ga_fitness_user_all[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "ga_fitness_user_all.csv", row.names = FALSE);

## SA User all
sa_fitness_user_all    <- read.csv("sa_fitness_user_all.txt", sep = "\t",
                                   header = FALSE);
sa_fitness_user_all[,4] <- sa_fitness_user_all[,4] + 100;
fitcalls <- unique(sa_fitness_user_all[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- sa_fitness_user_all[sa_fitness_user_all[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "sa_fitness_user_all.csv", row.names = FALSE);


## GA Mana cull
ga_fitness_mana_cull    <- read.csv("ga_fitness_mana_cull.txt", sep = "\t",
                                    header = FALSE);
fitcalls <- unique(ga_fitness_mana_cull[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- ga_fitness_mana_cull[ga_fitness_mana_cull[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "ga_fitness_mana_cull.csv", row.names = FALSE);


## SA Mana cull
sa_fitness_mana_cull    <- read.csv("sa_fitness_mana_cull.txt", sep = "\t",
                                    header = FALSE);
sa_fitness_mana_cull[,4] <- sa_fitness_mana_cull[,4] + 100;
fitcalls <- unique(sa_fitness_mana_cull[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- sa_fitness_mana_cull[sa_fitness_mana_cull[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "sa_fitness_mana_cull.csv", row.names = FALSE);

## GA Mana all
ga_fitness_mana_all    <- read.csv("ga_fitness_mana_all.txt", sep = "\t",
                                   header = FALSE);
fitcalls <- unique(ga_fitness_mana_all[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- ga_fitness_mana_all[ga_fitness_mana_all[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "ga_fitness_mana_all.csv", row.names = FALSE);

## SA Mana all
sa_fitness_mana_all    <- read.csv("sa_fitness_mana_all.txt", sep = "\t",
                                   header = FALSE);
sa_fitness_mana_all[,4] <- sa_fitness_mana_all[,4] + 100;
fitcalls <- unique(sa_fitness_mana_all[,4]);
FIT_data <- NULL;
for(i in fitcalls){
    calls    <- sa_fitness_mana_all[sa_fitness_mana_all[,4] == i,];
    callmn   <- mean(calls[,3]);
    callCI   <- simpleboot(freqs = calls[,3]);
    crow     <- c(i, callmn, callCI);
    FIT_data <- rbind(FIT_data, crow);
}
colnames(FIT_data) <- c("Fitness_calls", "Mean_Fitness", "LCI", "UCI");
write.csv(FIT_data, file = "sa_fitness_mana_all.csv", row.names = FALSE);


