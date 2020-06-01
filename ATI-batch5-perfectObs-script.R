# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy updating timing with a fictional default case 
# Simulations + saving results


#### Update GMSE with the new features ####

# Update GMSE clicking Build > Clean and Rebuild
# batch 5 ==> with perfect observation (obs_type = 3 & res_move_obs = FALSE & agent_view = land_dim_1 to make counting as fast as possible)


#### Simulations ####

ATI_replicate <- function(UTrange = seq(0,1,0.5), BBrange = seq(0,1,0.5), ts = 10, rep = 2, bdgt = 1000, popinit = 1000, tf = 12, cons = 0.41, surv = 3, repr = 0.4, trgt = 1000, stkh = 10, freq = 10, obstype = 3) {
  
  # Array of Action Threshold values to explore
  at <- UTrange
  
  # Array of Budget Bonus values to explore
  bb <- BBrange
  
  ## Create empty structures to gather simulation results
  
  columns <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "overK")
  
  # Empty 3D array of correct size 
  # Dimensions(lines = replicates, columns = measures, layer = parameter combination)
  OTI_default_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)-(length(bb)-1)), dimnames = list(NULL,columns,NULL))                 
  
  ## Create structures to follow up on the costs, the actions, the budget and the population along the simulations
  flw_bgt <- NULL
  flw_cos <- NULL
  flw_act <- NULL
  flw_pop <- NULL
  # flw_obs <- NULL
  
  ## Simulations loop
  
  # Initialize an index of parameter combination
  param_set <- 1
  
  # Loop
  start <- Sys.time()
  
  # For every AT values in 'at'
  for (i in 1:length(at)) {
    
    # avoid simul for different bb values for at = 0
    if (at[i] == 0) {
      
      # With 'rep' number of replicate per parameter combo
      for (k in 1:rep) {
        
        # Print info
        if (k == 1 | k%%10 == 0) {
          print(paste("AT = 0, replicate", k, "of", rep))
        }
        
        # Run GMSE for the parameter combo
        sim <- gmse(time_max = ts, 
                    consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
                    res_birth_type = 0, res_death_type = 0, group_think = TRUE,
                    land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200, stakeholders = stkh,
                    scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, manager_sense = 0.15,
                    action_thres = 0, budget_bonus = 0, manage_target = trgt,
                    RESOURCE_ini = 1000, observe_type = obstype, res_move_obs = FALSE, plotting = FALSE)
        
        # Store the last time step number (for extinction-related bugs)
        final_ts <- length(which(sim$paras[,1] != 0))
        
        # Pick up values for simulation results and store them in OTI_default_results
        
        # Replicate number
        OTI_default_results[k,1,param_set] <- k
        
        # Budget
        OTI_default_results[k,2,param_set] <- bdgt
        
        # AT value
        OTI_default_results[k,3,param_set] <- at[i]
        
        # BB value
        OTI_default_results[k,4,param_set] <- 0
        
        # Has extinction occured? (yes = 1, no = 0)
        OTI_default_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
        
        ## save costs, actions and population in flw structures
        if (k %% freq == 0) {
          print("Saving budget, costs, actions, actual and observed pop")
          para <- OTI_default_results[k,2:5, param_set]
          para <- c(para, K, trgt, popinit)
          
          bdg <- rep(0, ts)
          pop <- rep(0, ts)
          cos <- rep(0, ts)
          act <- rep(0, ts)
          # obs <- rep(0, ts)
          
          for (t in 1:final_ts) {
            bdg[t] <- sim$agents[[t]][1,17]
            pop[t] <- dim(sim$resource[[t]])[1]
            cos[t] <- sim$cost[[t]][1,9,2]
            act[t] <- mean(sim$action[[t]][1,9,2:stkh])
            # obs[t] <- dim(sim$observation[[t]])[1]
          }
          
          flw_bgt <- rbind(flw_bgt, c(para, bdg))
          flw_pop <- rbind(flw_pop, c(para, pop))
          flw_cos <- rbind(flw_cos, c(para, cos))
          flw_act <- rbind(flw_act, c(para, act))
          # flw_obs <- rbind(flw_obs, c(para, obs))
        }
        
        # Next measures involve calculus that can be disturbed if extinction occured
        
        # If exctinction occured
        if (OTI_default_results[k,5,param_set] != 0) {
          
          # Resource actual pop deviation from target
          OTI_default_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
          
          # Absolute value
          OTI_default_results[k,7,param_set] <- abs(OTI_default_results[k,6,param_set])
          
          # Users total final yield
          OTI_default_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OTI_default_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OTI_default_results[k,10,param_set] <- (final_ts-sum(sim$paras[,107]))/final_ts
          
          # Number of K exceedings
          OTI_default_results[k,11,param_set] <- sum(sim$paras[,109])/final_ts
        }
        
        # If extinction did not occured
        else {
          
          # Resource actual pop deviation from target
          OTI_default_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
          
          # Absolute value
          OTI_default_results[k,7,param_set] <- abs(OTI_default_results[k,6,param_set])
          
          # Users total final yield
          OTI_default_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OTI_default_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OTI_default_results[k,10,param_set] <- (final_ts-sum(sim$paras[,107]))/final_ts
          
          # Number of K exceedings
          OTI_default_results[k,11,param_set] <- sum(sim$paras[,109])/final_ts
        }
      } # end rep for loop
      
      # Increment parameter combo index
      param_set <- param_set + 1
    } # end at if loop
    
    # for every other at value
    else {
      
      # For every BB values in 'bb'
      for (j in 1:length(bb)) {
        
        # With 'rep' number of replicate per parameter combo
        for (k in 1:rep) {
          
          # Print info
          if (k == 1 | k%%10 == 0) {
            print(paste("AT =", at[i], ", BB =", bb[j], "replicate", k, "of", rep))
          }
          
          # Run GMSE for the parameter combo
          sim <- gmse(time_max = ts, 
                      consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
                      res_birth_type = 0, res_death_type = 0, land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
                      stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, manager_sense = 0.15,
                      action_thres = at[i], budget_bonus = bb[j], manage_target = trgt, group_think = TRUE,
                      RESOURCE_ini = 1000, observe_type = obstype, res_move_obs = FALSE, plotting = FALSE)
          
          # Store the last time step number (for extinction-related bugs)
          final_ts <- length(which(sim$paras[,1] != 0))
          
          # Pick up values for simulation results and store them in OTI_default_results
          
          # Replicate number
          OTI_default_results[k,1,param_set] <- k
          
          # Budget
          OTI_default_results[k,2,param_set] <- bdgt
          
          # AT value
          OTI_default_results[k,3,param_set] <- sim$paras[1,106]
          
          # BB value
          OTI_default_results[k,4,param_set] <- bb[j]
          
          # Has extinction occured? (yes = 1, no = 0)
          OTI_default_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
          
          ## save costs, actions and population in flw structures
          if (k %% freq == 0) {
            print("Saving budget, costs, actions, actual and observed pop")
            para <- OTI_default_results[k,2:5, param_set]
            para <- c(para, k, trgt, popinit)
            
            bdg <- rep(0, ts)
            pop <- rep(0, ts)
            cos <- rep(0, ts)
            act <- rep(0, ts)
            # obs <- rep(0, ts)
            
            for (t in 1:final_ts) {
              bdg[t] <- sim$agents[[t]][1,17]
              pop[t] <- dim(sim$resource[[t]])[1]
              cos[t] <- sim$cost[[t]][1,9,2]
              act[t] <- mean(sim$action[[t]][1,9,2:stkh])
              # obs[t] <- dim(sim$observation[[t]])[1]
            }
            
            flw_bgt <- rbind(flw_bgt, c(para, bdg))
            flw_pop <- rbind(flw_pop, c(para, pop))
            flw_cos <- rbind(flw_cos, c(para, cos))
            flw_act <- rbind(flw_act, c(para, act))
            # flw_obs <- rbind(flw_obs, c(para, obs))
          }
          
          # Next measures involve calculus that can be disturbed if extinction occured
          
          # If exctinction occured
          if (OTI_default_results[k,5,param_set] != 0) {
            
            # Resource actual pop deviation from target
            OTI_default_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OTI_default_results[k,7,param_set] <- abs(OTI_default_results[k,6,param_set])
            
            # Users total final yield
            OTI_default_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
            
            # Maximum difference between Users yield (in percentage of the highest yield)
            OTI_default_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OTI_default_results[k,10,param_set] <- 1-sum(sim$paras[,107])/final_ts
            
            # Number of K exceedings
            OTI_default_results[k,11,param_set] <- sum(sim$paras[,109])/final_ts
          }
          
          # If extinction did not occured
          else {
            
            # Resource actual pop deviation from target
            OTI_default_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OTI_default_results[k,7,param_set] <- abs(OTI_default_results[k,6,param_set])
            
            # Users total final yield
            OTI_default_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
            
            # Maximum difference between Users yield (in percentage of the highest yield)
            OTI_default_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OTI_default_results[k,10,param_set] <- 1-sum(sim$paras[,107])/final_ts
            
            # Number of K exceedings
            OTI_default_results[k,11,param_set] <- sum(sim$paras[,109])/final_ts
          }
        } # end rep for loop
        
        # keep track of the simulations
        if (param_set %% 10 == 0) {
          print(paste("parameter set number", param_set, "out of", dim(OTI_default_results)[3], "at", Sys.time(), sep = " "))
        }
        
        # Increment parameter combo index
        param_set <- param_set + 1
        gc();
      } # end bb for loop
      gc();
    } # end at else loop
    gc();
  } # end at for loop
  gc();
  
  # Simulation time
  end <- Sys.time()
  print(paste("Batch started", start, "and ended", end, sep = " "))
  
  # rbind the layers
  if (param_set > 2) {
    
    tab_OTI_default_results <- OTI_default_results[,,1]
    
    for (i in 2:dim(OTI_default_results)[3]) {
      tab_OTI_default_results <- rbind(tab_OTI_default_results, OTI_default_results[,,i])
    }
  } else {
    tab_OTI_default_results <- OTI_default_results
  }
  
  return(list(tab_OTI_default_results, flw_pop, flw_cos, flw_bgt, flw_act))#, flw_obs
  
} # end function

# Run the simulations
batch5 <- ATI_replicate(UTrange = c(0, 0.1), BBrange = seq(0,1,0.1), ts = 5, rep = 2, stkh = 40)

# Save the results on GitHub
write.csv(batch5[[1]], file = "AT_results/tab_ATI_batch5.csv")
write.csv(batch5[[2]], file = "AT_results/pop_batch5.csv")
write.csv(batch5[[3]], file = "AT_results/cos_batch5.csv")
write.csv(batch5[[4]], file = "AT_results/bgt_batch5.csv")
write.csv(batch5[[5]], file = "AT_results/act_batch5.csv")
# write.csv(batch5[[6]], file = "case_obs_batch5.csv")

# Population only
#rep.pop <- gmse_replicates(replicates = 100, time_max = 20,
#                           consume_surv = 4.75, consume_repr = 5, times_feeding = 12, res_consume = 0.5,
#                           res_birth_type = 0, res_death_type = 0,
#                           land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
#                           stakeholders = 40, scaring = FALSE, manager_budget = 1, user_budget = 1, manager_sense = 0.15,
#                           RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = FALSE)
#write.csv(rep.pop, file = "batch5-popOnly.csv")

# pop and users only
#rep.use <- gmse_replicates(replicates = 100, time_max = 20,
#                           consume_surv = 4.75, consume_repr = 5, times_feeding = 12, res_consume = 0.5,
#                           res_birth_type = 0, res_death_type = 0,
#                           land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
#                           stakeholders = 40, scaring = FALSE, manager_budget = 1, user_budget = 1000, manager_sense = 0.15,
#                           RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = FALSE)
#write.csv(rep.use, file = "batch5-popAndUsers.csv")
