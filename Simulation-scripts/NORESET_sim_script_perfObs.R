# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy updating timing with a fictional default case 
# Simulations + saving results


#### Update GMSE with the new features ####

# batch 5 ==> with perfect observation (obs_type = 3 & res_move_obs = FALSE)

#### Simulations ####

ATI_replicate <- function(UTrange = 0, BBrange = 0,
                          ts = 20, rep = 100, freq = 10,
                          bdgt = 1000, trgt = 2000, stkh = 40, obstype = 3,
                          popinit = 1000, tf = 12, cons = 0.5, surv = 4.75, repr = 5, 
                          ldim = 200,
                          out_file = "test-immense.csv") {
  
  file.create(out_file); # Initialise a file in the directory.
  
  out_head <- as.character(strsplit(out_file, split = ".csv"));
  
  # Now initialise other files to print 
  bgt_file <- paste(out_head, "-flw_bgt", ".csv", sep = ""); 
  pop_file <- paste(out_head, "-flw_pop", ".csv", sep = "");
  cos_file <- paste(out_head, "-flw_cos", ".csv", sep = "");
  act_file <- paste(out_head, "-flw_act", ".csv", sep = "");
  
  # Now create the other new files
  file.create(bgt_file);
  file.create(pop_file);
  file.create(cos_file);
  file.create(act_file);
  
  # Array of Action Threshold values to explore
  at <- UTrange
  
  # Array of Budget Bonus values to explore
  bb <- BBrange
  
  ## Create empty structures to gather simulation results
  
  columns <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "SSD", "final_ts") #, "overK", "param_set"
  
  # Empty 3D array of correct size 
  # Dimensions(lines = replicates, columns = measures, layer = parameter combination)
  OTI_default_results <- array(data=NA, dim = c(rep, length(columns)), dimnames = list(NULL,columns))                 
  
  ## Create structures to follow up on the costs, the actions, the budget and the population along the simulations
  flw_bgt <- NULL
  flw_cos <- NULL
  flw_act <- NULL
  flw_pop <- NULL
  # flw_obs <- NULL
  
  ## Simulations loop
  
  # # Initialize an index of parameter combination
  # param_set <- 1
  
  # Loop
  start <- Sys.time()
  
  # # For every AT values in 'at'
  # for (i in 1:length(at)) {
    
    # avoid simul for different bb values for at = 0
    if (at == 0) {
      
      # With 'rep' number of replicate per parameter combo
      for (k in 1:rep) {
        
        # Run GMSE for the parameter combo
        sim <- gmse(time_max = ts, mem_prv_observ = FALSE, bgt_bonus_reset = FALSE, 
                    RESOURCE_ini = popinit, res_birth_type = 0, res_death_type = 0, 
                    consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
                    land_ownership = TRUE, land_dim_1 = ldim, land_dim_2 = ldim,
                    stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, 
                    manager_sense = 0.15, manage_target = trgt, observe_type = obstype, res_move_obs = FALSE,
                    action_thres = 0, budget_bonus = 0, 
                    plotting = FALSE)
        
        # Store the last time step number (for extinction-related bugs)
        final_ts <- length(which(sim$paras[,1] != 0))
        
        # Pick up values for simulation results and store them in OTI_default_results
        
        # Replicate number
        OTI_default_results[k,1] <- k
        
        # Budget
        OTI_default_results[k,2] <- bdgt
        
        # AT value
        OTI_default_results[k,3] <- 0
        
        # BB value
        OTI_default_results[k,4] <- 0
        
        # Has extinction occured? (yes = 1, no = 0)
        OTI_default_results[k,5] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
        
        ## save costs, actions and population in flw structures
        if (k %% freq == 0) {
          print("Saving budget, costs, actions, actual and observed pop")
          para <- OTI_default_results[k,2:5]
          para <- c(para, k, trgt, popinit)
          
          bdg <- rep(0, ts)
          pop <- rep(0, ts)
          cos <- rep(0, ts)
          act <- rep(0, ts)
          # obs <- rep(0, ts)
          
          for (t in 1:final_ts) {
            bdg[t] <- sim$paras[t,132]
            pop[t] <- dim(sim$resource[[t]])[1]
            cos[t] <- sim$cost[[t]][1,9,2]
            act[t] <- mean(sim$action[[t]][1,9,2:stkh])
            # obs[t] <- dim(sim$observation[[t]])[1]
          }
          
          # Append the files now
          write.table(t(c(para, bdg)), file = bgt_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
          write.table(t(c(para, pop)), file = pop_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
          write.table(t(c(para, cos)), file = cos_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
          write.table(t(c(para, act)), file = act_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
          
          #flw_bgt <- rbind(flw_bgt, c(para, bdg))
          #flw_pop <- rbind(flw_pop, c(para, pop))
          #flw_cos <- rbind(flw_cos, c(para, cos))
          # flw_act <- rbind(flw_act, c(para, act))
          #flw_obs <- rbind(flw_obs, c(para, obs))
        }
        
        # Next measures involve calculus that can be disturbed if extinction occured
        
        # If exctinction occured
        if (OTI_default_results[k,5] != 0) {
          
          # Resource actual pop deviation from target
          OTI_default_results[k,6] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
          
          # Absolute value
          OTI_default_results[k,7] <- abs(OTI_default_results[k,6])
          
          # Users total final yield
          OTI_default_results[k,8] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OTI_default_results[k,9] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OTI_default_results[k,10] <- (final_ts-sum(sim$paras[,107]))/final_ts
          
          # # Number of K exceedings
          # OTI_default_results[k,11] <- sum(sim$paras[,109])/final_ts
          
          # Sum of squared deviation from target along time steps
          ssd <- 0
          for (i in 1:final_ts){
            ssd <- ssd + abs(dim(sim$resource[[i]])[1]-sim$paras[i,7])
          }
          OTI_default_results[k,11] <- ssd
          
          # Param set
          OTI_default_results[k,12] <- final_ts
        }
        
        # If extinction did not occured
        else {
          
          # Resource actual pop deviation from target
          OTI_default_results[k,6] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
          
          # Absolute value
          OTI_default_results[k,7] <- abs(OTI_default_results[k,6])
          
          # Users total final yield
          # OTI_default_results[k,8] <- sum(sim$agents[[final_ts-1]][,16])
          OTI_default_results[k,8] <- sum(sim$agents[[final_ts]][,16])
          
          # Maximum difference between Users yield
          # OTI_default_results[k,9] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          OTI_default_results[k,9] <- (max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OTI_default_results[k,10] <- (final_ts-sum(sim$paras[,107]))/final_ts
          
          # # Number of K exceedings
          # OTI_default_results[k,11] <- sum(sim$paras[,109])/final_ts
          
          # Sum of squared deviation from target along time steps
          ssd <- 0
          for (i in 1:final_ts){
            ssd <- ssd + abs(dim(sim$resource[[i]])[1]-sim$paras[i,7])
          }
          OTI_default_results[k,11] <- ssd
          
          # Param set
          OTI_default_results[k,12] <- final_ts
        }
        
        gc(); # Run a garbage collect here
        
      } # end rep for loop
      
      # # Increment parameter combo index
      # param_set <- param_set + 1
    } # end at if loop
    
    # for every other at value
    else {
      
      # # For every BB values in 'bb'
      # for (j in 1:length(bb)) {
        
        # With 'rep' number of replicate per parameter combo
        for (k in 1:rep) {
          
          # Run GMSE for the parameter combo
          sim <- gmse(time_max = ts, mem_prv_observ = FALSE, bgt_bonus_reset = FALSE, 
                      RESOURCE_ini = popinit, res_birth_type = 0, res_death_type = 0, 
                      consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
                      land_ownership = TRUE, land_dim_1 = ldim, land_dim_2 = ldim,
                      stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, 
                      manager_sense = 0.15, manage_target = trgt, observe_type = obstype, res_move_obs = FALSE,
                      action_thres = at, budget_bonus = bb, 
                      plotting = FALSE)
          
          # Store the last time step number (for extinction-related bugs)
          final_ts <- length(which(sim$paras[,1] != 0))
          
          # Pick up values for simulation results and store them in OTI_default_results
          
          # Replicate number
          OTI_default_results[k,1] <- k
          
          # Budget
          OTI_default_results[k,2] <- bdgt
          
          # AT value
          OTI_default_results[k,3] <- at #sim$paras[1,106]
          
          # BB value
          OTI_default_results[k,4] <- bb
          
          # Has extinction occured? (yes = 1, no = 0)
          OTI_default_results[k,5] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
          
          ## save costs, actions and population in flw structures
          if (k %% freq == 0) {
            print("Saving budget, costs, actions, actual and observed pop")
            para <- OTI_default_results[k,2:5]
            para <- c(para, k, trgt, popinit)
            
            bdg <- rep(0, ts)
            pop <- rep(0, ts)
            cos <- rep(0, ts)
            act <- rep(0, ts)
            # obs <- rep(0, ts)
            
            for (t in 1:final_ts) {
              bdg[t] <- sim$paras[t,132]
              pop[t] <- dim(sim$resource[[t]])[1]
              cos[t] <- sim$cost[[t]][1,9,2]
              act[t] <- mean(sim$action[[t]][1,9,2:stkh])
              # obs[t] <- dim(sim$observation[[t]])[1]
            }
            
            # Append the files now
            write.table(t(c(para, bdg)), file = bgt_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
            write.table(t(c(para, pop)), file = pop_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
            write.table(t(c(para, cos)), file = cos_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
            write.table(t(c(para, act)), file = act_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
            
            #flw_bgt <- rbind(flw_bgt, c(para, bdg))
            #flw_pop <- rbind(flw_pop, c(para, pop))
            #flw_cos <- rbind(flw_cos, c(para, cos))
            #flw_act <- rbind(flw_act, c(para, act))
            #flw_obs <- rbind(flw_obs, c(para, obs))
          }
          
          # Next measures involve calculus that can be disturbed if extinction occured
          
          # If exctinction occured
          if (OTI_default_results[k,5] != 0) {
            
            # Resource actual pop deviation from target
            OTI_default_results[k,6] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OTI_default_results[k,7] <- abs(OTI_default_results[k,6])
            
            # Users total final yield
            OTI_default_results[k,8] <- sum(sim$agents[[final_ts-1]][,16])
            
            # Maximum difference between Users yield (in percentage of the highest yield)
            OTI_default_results[k,9] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OTI_default_results[k,10] <- 1-sum(sim$paras[,107])/final_ts
            
            # # Number of K exceedings
            # OTI_default_results[k,11] <- sum(sim$paras[,109])/final_ts
            
            # Sum of squared deviation from target along time steps
            ssd <- 0
            for (i in 1:final_ts){
              ssd <- ssd + abs(dim(sim$resource[[i]])[1]-sim$paras[i,7])
            }
            OTI_default_results[k,11] <- ssd
            
            # Param set
            OTI_default_results[k,12] <- final_ts
          }
          
          # If extinction did not occured
          else {
            
            # Resource actual pop deviation from target
            OTI_default_results[k,6] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OTI_default_results[k,7] <- abs(OTI_default_results[k,6])
            
            # Users total final yield
            # OTI_default_results[k,8] <- sum(sim$agents[[final_ts-1]][,16])
            OTI_default_results[k,8] <- sum(sim$agents[[final_ts]][,16])
            
            # Maximum difference between Users yield (in percentage of the highest yield)
            # OTI_default_results[k,9] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
            OTI_default_results[k,9] <- (max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OTI_default_results[k,10] <- 1-sum(sim$paras[,107])/final_ts
            
            # # Number of K exceedings
            # OTI_default_results[k,11] <- sum(sim$paras[,109])/final_ts
            
            # Sum of squared deviation from target along time steps
            ssd <- 0
            for (i in 1:final_ts){
              ssd <- ssd + abs(dim(sim$resource[[i]])[1]-sim$paras[i,7])
            }
            OTI_default_results[k,11] <- ssd
            
            # Param set
            OTI_default_results[k,12] <- final_ts
          }
        } # end rep for loop
        
        # # keep track of the simulations
        # if (param_set %% 10 == 0) {
        #   print(paste("parameter set number", param_set, "out of", dim(OTI_default_results)[3], "at", Sys.time(), sep = " "))
        # }
        
        # # Increment parameter combo index
        # param_set <- param_set + 1
        
        gc(); # Run a garbage collect here
      # } # end bb for loop
      
    } # end at else loop
    
    write.table(x = OTI_default_results[,], file = out_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE);
    gc(); # Run a garbage collect here
  # } # end at for loop
  
  # Simulation time
  end <- Sys.time()
  
  print(paste("Batch started", start, "and ended", end, sep = " "))
  
  #rbind the layers
  # if (param_set > 2) {
  #   
  #   tab_OTI_default_results <- OTI_default_results[,,1]
  #   
  #   for (i in 2:dim(OTI_default_results)[3]) {
  #    tab_OTI_default_results <- rbind(tab_OTI_default_results, OTI_default_results[,,i])
  #   }
  #} else {
  #   tab_OTI_default_results <- OTI_default_results
  #}
  
  #return(list(tab_OTI_default_results, flw_pop, flw_cos, flw_bgt, flw_act))
  
  return(OTI_default_results);
  
} # end function

# Run the simulations
#batch5_1 <- ATI_replicate(UTrange = c(0.0, 0.1), out_file = "AT0pt0-0pt1_OTI_default_results.csv");
#batch5_2 <- ATI_replicate(UTrange = c(0.2, 0.3), out_file = "AT0pt2-0pt3_OTI_default_results.csv");
#batch5_3 <- ATI_replicate(UTrange = c(0.4, 0.5), out_file = "AT0pt4-0pt5_OTI_default_results.csv");
#batch5_4 <- ATI_replicate(UTrange = c(0.6, 0.7), out_file = "AT0pt6-0pt7_OTI_default_results.csv");
#batch5_5 <- ATI_replicate(UTrange = c(0.8, 1.0), out_file = "AT0pt8-1pt0_OTI_default_results.csv");

# Save the results on GitHub
#write.csv(batch5[[1]], file = "tab_ATI_batch5.csv")
#write.csv(batch5[[2]], file = "case_pop_batch5.csv")
#write.csv(batch5[[3]], file = "case_cos_batch5.csv")
#write.csv(batch5[[4]], file = "case_bgt_batch5.csv")
#write.csv(batch5[[5]], file = "case_act_batch5.csv")
# write.csv(batch5[[6]], file = "case_obs_batch5.csv")

# Population only
#rep.pop <- gmse_replicates(replicates = 100,
#                           consume_surv = 4.75, consume_repr = 5, times_feeding = 12, res_consume = 0.5,
#                           res_birth_type = 0, res_death_type = 0,
#                           land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
#                           stakeholders = 40, scaring = FALSE, time_max = 20, manager_budget = 1, user_budget = 1, manager_sense = 0.15,
#                           RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE)
#write.csv(rep.pop, file = "batch5-popOnly.csv")

# pop and users only
#rep.use <- gmse_replicates(replicates = 100,
#                           consume_surv = 4.75, consume_repr = 5, times_feeding = 12, res_consume = 0.5,
#                           res_birth_type = 0, res_death_type = 0,
#                           land_ownership = TRUE, land_dim_1 = 200, land_dim_2 = 200,
#                           stakeholders = 40, scaring = FALSE, time_max = 20, manager_budget = 1, user_budget = 1000, manager_sense = 0.15,
#                           RESOURCE_ini = 1000, observe_type = 3, res_move_obs = FALSE, plotting = TRUE)
#write.csv(rep.use, file = "batch5-popAndUsers.csv")



