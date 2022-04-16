

## REDO WITH A GA POPULATION SIZE OF 100

################################################################################
count      <- 1;
AN_summary <- NULL;
GA_summary <- NULL;
while(count < 200){

time_steps <- 40;
ga_min     <- 1 * count;
kmx        <- 100 * count;

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
GA_stats <- c(time_steps, tot_time, gal_fcal, mean(sim_sum_1[31:40,5]));
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
AN_stats <- c(time_steps, tot_time, ann_fcal, mean(sim_sum_2[31:40,5]));
################################################################################



AN_summary <- rbind(AN_summary, AN_stats);
GA_summary <- rbind(GA_summary, GA_stats);

count <- count + 1;
print(count);
}


# Run simulations and get fitness by getting number of culls made in last gen



################################################################################
# Side-by-side fair comparison
################################################################################
sim <- gmse(time_max = 40, land_ownership = TRUE, scaring = FALSE, 
            culling = TRUE, castration = FALSE, help_offspring = FALSE, 
            tend_crops = FALSE, kill_crops = FALSE, feeding = FALSE, 
            plotting = FALSE, ga_seedrep = 0);


sim <- gmse(time_max = 40, land_ownership = TRUE, scaring = FALSE, 
            culling = TRUE, castration = FALSE, help_offspring = FALSE, 
            tend_crops = FALSE, kill_crops = FALSE, feeding = FALSE,
            plotting = FALSE, ga_seedrep = 0, user_annealing = TRUE, 
            mana_annealing = FALSE, kmax_annealing = 2000);


end_time <- Sys.time();
tot_time <- end_time - st_time;
gal_fcal <- sum(sim_sum_1[,6]);
GA_stats <- c(time_steps, tot_time, gal_fcal, mean(sim_sum_1[101:200,5]));




################################################################################
################################################################################

# Genetic algorithm
gafit      <- read.csv("ga_fitness.txt", sep = "\t", header = FALSE);
stk1cga    <- gafit[gafit[,1] == 1 & gafit[,2] == 0 & gafit[,3] == 1 & 
                    gafit[,4] == 0 & gafit[,5] == 0 & gafit[,6] == 0 & 
                    gafit[,7] == 0 & gafit[,8] == 0,];

stkfit_ga  <- tapply(X = stk1cga[,12], INDEX = stk1cga[,10], FUN = mean);
stkch_ga   <- tapply(X = stk1cga[,13], INDEX = stk1cga[,10], FUN = mean);

# Simulated annealing
safit      <- read.csv("sa_fitness.txt", sep = "\t", header = FALSE);
stk1csa    <- safit[safit[,1] == 1 & safit[,2] == 0 & safit[,3] == 1 & 
                    safit[,4] == 0 & safit[,5] == 0 & safit[,6] == 0 & 
                    safit[,7] == 0 & safit[,8] == 0,];

stkfit_sa  <- tapply(X = stk1csa[,12], INDEX = stk1csa[,10], FUN = mean);
stkch_sa   <- tapply(X = stk1csa[,13], INDEX = stk1csa[,10], FUN = mean);

# Only culling
plot(x = 1:39, y = stkfit_ga[1:39], type = "l", lwd = 2, cex = 0.5,
     col = "black", pch = 20, ylim = c(1800, 3600), xlab = "Iteration",
     ylab = "User fitness", xlim = c(1, 39));
points(x = 1:39, y = stkfit_sa[1:39], type = "l", lwd = 2, cex = 0.5,
     col = "red", pch = 20);
legend(x = 0, y = 3600, fill = c("black", "red"),
       legend = c("Genetic Algorithm", "Simulated Anealing"));
text(x = 30, y = 3500, labels = "Only Culling", cex = 1.5);

# All actions
stk6cga    <- gafit[gafit[,1] == 1 & gafit[,2] == 1 & gafit[,3] == 1 & 
                        gafit[,4] == 1 & gafit[,5] == 0 & gafit[,6] == 1 & 
                        gafit[,7] == 1 & gafit[,8] == 1,];

stkfit_ga6 <- tapply(X = stk6cga[,12], INDEX = stk6cga[,10], FUN = mean);
stkch_ga6  <- tapply(X = stk6cga[,13], INDEX = stk6cga[,10], FUN = mean);


stk6csa    <- safit[safit[,1] == 1 & safit[,2] == 1 & safit[,3] == 1 & 
                        safit[,4] == 1 & safit[,5] == 0 & safit[,6] == 1 & 
                        safit[,7] == 1 & safit[,8] == 1,];

stkfit_sa6 <- tapply(X = stk6csa[,12], INDEX = stk6csa[,10], FUN = mean);
stkch_sa6  <- tapply(X = stk6csa[,13], INDEX = stk6csa[,10], FUN = mean);

# All possible actions 
plot(x = 1:39, y = stkfit_ga6[1:39], type = "l", lwd = 2, cex = 0.5,
     col = "black", pch = 20, ylim = c(1000, 5400), xlab = "Iteration",
     ylab = "User fitness", xlim = c(1, 49));
points(x = 1:39, y = stkfit_sa6[1:39], type = "l", lwd = 2, cex = 0.5,
       col = "red", pch = 20);
legend(x = 0, y = 5400, fill = c("black", "red"),
       legend = c("Genetic Algorithm", "Simulated Anealing"));
text(x = 35, y = 5200, labels = "All possible user actions", cex = 1.5);

##################
# Managers
#################
man1cga    <- gafit[gafit[,1] == 0 & gafit[,2] == 0 & gafit[,3] == 1 & 
                    gafit[,4] == 0 & gafit[,5] == 0 & gafit[,6] == 0 & 
                    gafit[,7] == 0 & gafit[,8] == 0,];

manfit_ga  <- tapply(X = man1cga[,12], INDEX = man1cga[,10], FUN = mean);
manch_ga   <- tapply(X = man1cga[,13], INDEX = man1cga[,10], FUN = mean);
manfit_ga  <- manfit_ga - 9790000;

man1csa    <- safit[safit[,1] == 0 & safit[,2] == 0 & safit[,3] == 1 & 
                        safit[,4] == 0 & safit[,5] == 0 & safit[,6] == 0 & 
                        safit[,7] == 0 & safit[,8] == 0,];

manfit_sa  <- tapply(X = man1csa[,12], INDEX = man1csa[,10], FUN = mean);
manch_sa   <- tapply(X = man1csa[,13], INDEX = man1csa[,10], FUN = mean);
manfit_sa  <- manfit_sa - 9790000;

plot(x = 1:39, y = manfit_ga[1:39], type = "l", lwd = 2, cex = 0.5,
     col = "black", pch = 20, ylim = c(0, 80000), xlab = "Iteration",
     ylab = "Manager scaled fitness", xlim = c(1, 39));
points(x = 1:39, y = manfit_sa[1:39], type = "l", lwd = 2, cex = 0.5,
       col = "red", pch = 20);
legend(x = 0, y = 82000, fill = c("black", "red"),
       legend = c("Genetic Algorithm", "Simulated Anealing"));
text(x = 30, y = 80000, labels = "Only Culling", cex = 1.5);

##########
#################
man7cga    <- gafit[gafit[,1] == 0 & gafit[,2] == 1 & gafit[,3] == 1 & 
                        gafit[,4] == 1 & gafit[,5] == 0 & gafit[,6] == 1 & 
                        gafit[,7] == 1 & gafit[,8] == 1,];

manfit_ga7 <- tapply(X = man7cga[,12], INDEX = man7cga[,10], FUN = mean);
manch_ga7  <- tapply(X = man7cga[,13], INDEX = man7cga[,10], FUN = mean);
manfit_ga7 <- manfit_ga7 - 8000000;

man7csa    <- safit[safit[,1] == 0 & safit[,2] == 1 & safit[,3] == 1 & 
                        safit[,4] == 1 & safit[,5] == 0 & safit[,6] == 1 & 
                        safit[,7] == 1 & safit[,8] == 1,];

manfit_sa7 <- tapply(X = man7csa[,12], INDEX = man7csa[,10], FUN = mean);
manch_sa7  <- tapply(X = man7csa[,13], INDEX = man7csa[,10], FUN = mean);
manfit_sa7 <- manfit_sa7 - 8000000;

plot(x = 1:39, y = manfit_ga7[1:39], type = "l", lwd = 2, cex = 0.5,
     col = "black", pch = 20, ylim = c(0, 2000000), xlab = "Iteration",
     ylab = "Manager scaled fitness", xlim = c(1, 39));
points(x = 1:39, y = manfit_sa7[1:39], type = "l", lwd = 2, cex = 0.5,
       col = "red", pch = 20);
legend(x = 0, y = 2070000, fill = c("black", "red"), 
       legend = c("Genetic Algorithm", "Simulated Anealing"));
text(x = 30, y = 2000000, labels = "All possible user actions", cex = 1.5);



#################
ga_repl <- read.csv("genetic_algorithm_reps.csv");
sa_repl <- read.csv("sim_annealing_reps.csv");

GA_summ <- read.csv("GA_summary.csv");
SA_summ <- read.csv("SA_summary.csv");



plot(x = SA_summ[,2], SA_summ[,4], xlim = c(7, 19), ylim = c(0, 1000),
     xlab = "Simulation time (sec)", ylab = "Stakeholder culls", cex = 1.5,
     pch = 20, col = "red");
points(x = GA_summ[,2], GA_summ[,4], cex = 1.5, pch = 20, col = "black");
legend(x = 14, y = 90, fill = c("black", "red"), cex = 1.25,
       legend = c("Genetic Algorithm", "Simulated Anealing"));









calls_GA <- rep(x = NA, times = dim(GA_summ)[1]);
for(i in 1:dim(GA_summ)[1]){
    calls_GA[i] <- GA_summ[i, 3] / i;
}

calls_SA <- rep(x = NA, times = dim(SA_summ)[1]);
for(i in 1:dim(SA_summ)[1]){
    calls_SA[i] <- SA_summ[i, 3] / i;
}






dat <- read.csv("ga_fitness_mana_all.txt", sep = "\t", header = FALSE);
smr <- tapply(X = dat[,12], INDEX = dat[,10], FUN = mean);


reps <- 100;
while(reps > 0){
    sim <- gmse(land_ownership = TRUE, kmax_annealing = 2000, 
                user_annealing = FALSE, ga_seedrep = 0, plotting = FALSE,
                scaring = TRUE, culling = TRUE, castration = TRUE,
                feeding = TRUE, help_offspring = TRUE, tend_crops = TRUE,
                kill_crops = TRUE, mana_annealing = FALSE);
    reps <- reps - 1;
    print(reps);
}

simpleboot <- function(freqs,repli=1000,alpha=0.05){                           
    vals  <- NULL;                                                              
    i     <- 0;                                                             
    while(i < repli){                                                      
        boot  <- sample(x=freqs,size=length(freqs),replace=TRUE);           
        strap <- mean(boot);                                              
        vals  <- c(vals,strap);                                          
        i     <- i + 1;                                                    
    }                                                                      
    vals   <- sort(x=vals,decreasing=FALSE);                               
    lowCI  <- vals[round((alpha*0.5)*repli)];                               
    highCI <- vals[round((1-(alpha*0.5))*repli)];                         
    CIs    <- c(lowCI,highCI);                                             
    return(CIs);                                                            
} 


########################################

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



