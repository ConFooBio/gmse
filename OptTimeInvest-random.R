## Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy update timing
# Simulations + Results + Figures

#### Update GMSE with the new features ####

# Make sure this script is opened within the gmse_forkRQ1.Rproj project
# Update GMSE clicking Build > Clean and Rebuild


#### Simulations ####

## Set parameters

## Parameters: random cases, managers want to maintain it close to their carrying capacity

# Array of Budget Bonus values to explore: 0.5 because preliminary results shows no improvement passed this fraction 
bb <- 0.5

# Number of simulation time steps
ts <- 10

# Number of different cases
rep <- 10

# Budget
bdgt <- 1000

# number of updating threshold values
ut_nb <- 5

# Other parameters to GMSE default


## Create empty structures to gather simulation results

columns <- c("case_nb", "popinit", "K", "MT", "nb_stkh", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "overK")

# Empty 2D array of correct size 
# Dimensions(lines = simulations, columns = measures)
OTI_random_results <- array(data=NA, dim = c(rep*ut_nb, length(columns)), dimnames = list(NULL,columns))                 

## Create 3 structures to follow up on the costs, the actions, and the population along the simulations
flw_cos <- NULL
flw_act <- NULL
flw_pop <- NULL

# # how often do we want to save them? Every 10 replicates?
# freq <- 5

## Simulations loop

# Loop
start <- Sys.time()

# For every different case
for (k in 1:rep) {
  
  ## Randomly choose case parameters 
  # Initial Resource population: draw an initial population in a uniform law (every value has the same probability)
  popini <- floor(runif(n = 1, min = 10, max = 10000))
  
  # Carrying capacity: draw a K reasonably close to popini
  K <- floor(rnorm(n = 1, mean = popini, sd = 3000))
  
  # Make sure it is not negative
  K <- ifelse(K<0, -K, K)
  
  # Manager target: which will probably be close to but under K 
  MT <- signif(rnorm(n = 1, mean = 0.75, sd = 0.1)*K, digits = 2)
  
  # Make sure it is under K
  MT <- ifelse(MT > K, signif(MT-2*(MT-K), digits = 2), MT)
  
  # Number of stakeholders : draw a number of stakeholders between 2 and 50
  nbstk <- floor(runif(n = 1, min = 2, max = 50))
  
  # Array of Action Threshold values to explore
  # Make sure AT values includes 0 (updating only if extinction is suspected by monitoring data) and K (updating only when pop overshoots carrying capacity)
  at <- sort(c(0, 0.1, abs(K/MT-1), 0.5, 0.7, 1))
  
  # for each ut value
  for (i in 1:length(at)) {
    
    # Run GMSE for the parameter combo
    sim <- gmse(time_max = ts,
                stakeholders = nbstk, RESOURCE_ini = popini, res_death_K = K, manage_target = MT,
                action_thres = at[i], budget_bonus = bb,
                land_ownership = TRUE, plotting = F)
    
    # Store the last time step number (for extinction-related bugs)
    final_ts <- length(which(sim$paras[,1] != 0))
    
    # Pick up values for simulation results and store them in OTI_random_results
    
    # case number
    OTI_random_results[k,1] <- k
    
    # Initial Resource population
    OTI_random_results[k,2] <- popini
    
    # Carrying capacity
    OTI_random_results[k,3] <- K
    
    # Manager target
    OTI_random_results[k,4] <- MT
    
    # Number of stakeholders
    OTI_random_results[k,5] <- nbstk
    
    # Updating threshold value
    OTI_random_results[k,6] <- at[i]
    
    # Budget bonus value
    OTI_random_results[k,7] <- bb
    
    # Has extinction occured? (yes = 1, no = 0)
    OTI_random_results[k,8] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
    
    ## save costs, actions and population in flw structures
    # if (k %% freq == 0) {
    # print("Saving costs, actions and pop")
    para <- OTI_random_results[k,2:8]
    
    pop <- rep(0, ts)
    cos <- rep(0, ts)
    act <- rep(0, ts)
    
    for (t in 1:final_ts) {
      pop[t] <- dim(sim$resource[[t]])[1]
      cos[t] <- sim$cost[[t]][1,9,2]
      act[t] <- mean(sim$action[[t]][1,9,2:nbstk])
    }
    
    flw_pop <- rbind(flw_pop, c(para, pop))
    flw_cos <- rbind(flw_cos, c(para, cos))
    flw_act <- rbind(flw_act, c(para, act))
    # }
    
    # Next measures involve calculus that can be disturbed if extinction occured
    
    # If exctinction occured
    if (OTI_random_results[k,8] != 0) {
      
      # Resource actual pop deviation from target
      OTI_random_results[k,9] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
      
      # absolute value
      OTI_random_results[k,10] <- abs(OTI_random_results[k,6])
      
      # Users total final yield
      OTI_random_results[k,11] <- sum(sim$agents[[final_ts-1]][,16])
      
      # Maximum difference between Users yield (in percentage of the highest yield)
      OTI_random_results[k,12] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
      
      # Number of timesteps during which Manager chose not to update policy
      OTI_random_results[k,13] <- 1-sum(sim$paras[,107])/final_ts
      
      # Number of K exceedings
      OTI_random_results[k,14] <- sum(sim$paras[,109])/final_ts
    }
    
    # If extinction did not occured
    else {
      
      # Resource actual pop deviation from target
      OTI_random_results[k,9] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
      
      # absolute value
      OTI_random_results[k,10] <- abs(OTI_random_results[k,6])
      
      # Users total final yield
      OTI_random_results[k,11] <- sum(sim$agents[[final_ts-1]][,16])
      
      # Maximum difference between Users yield (in percentage of the highest yield)
      OTI_random_results[k,12] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
      
      # Number of timesteps during which Manager chose not to update policy
      OTI_random_results[k,13] <- 1-sum(sim$paras[,107])/final_ts
      
      # Number of K exceedings
      OTI_random_results[k,14] <- sum(sim$paras[,109])/final_ts
    }    
  } # end at for loop      
  
  # keep track of the simulations
  if (k %% 5 == 0) {
    print(paste("parameter set number", k, "out of", rep, "at", Sys.time(), sep = " "))
  }
  
} # end rep for loop

# Simulation time
end <- Sys.time()
print(paste("Batch started", start, "and ended", end, sep = " "))

write.csv(OTI_random_results, file = "OTI_random_batch3.csv")
write.csv(flw_pop, file = "flw_pop_batch3.csv")
write.csv(flw_cos, file = "flw_cos_batch3.csv")
write.csv(flw_act, file = "flw_act_batch3.csv")
