# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy updating timing with a fictional default case 
# Simulations + Results + Figures


#### Update GMSE with the new features ####

# Make sure this script is opened within the gmse_forkRQ1.Rproj project
# Update GMSE clicking Build > Clean and Rebuild


#### Simulations ####

## Set parameters

# Array of Action Threshold values to explore
at <- seq(0,1,0.1)

# Array of Budget Bonus values to explore
bb <- seq(0,1,0.1)

# Number of simulation time steps
ts <- 20

# Number of replicates
rep <- 100

# Budget
bdgt <- 1000

## Case parameters: a standard case, a pop is endangered by culling, put under conservation, managers want to maintain it close to their carrying capacity
# initial Resource population
popinit <- 700

# carrying capacity
K <- 1500

# Manager target
trgt <- 1000

# number of stakeholders
stkh <- 10

# Other parameters to GMSE default


## Create empty structures to gather simulation results

columns <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "overK")

# Empty 3D array of correct size 
# Dimensions(lines = replicates, columns = measures, layer = parameter combination)
OTI_default_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)-(length(bb)-1)), dimnames = list(NULL,columns,NULL))                 

## Create 3 structures to follow up on the costs, the actions, and the population along the simulations
flw_bgt <- NULL
flw_cos <- NULL
flw_act <- NULL
flw_pop <- NULL

# how often do we want to save them? Every 10 replicates?
freq <- 10

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
      
      # Run GMSE for the parameter combo
      sim <- gmse(stakeholders = stkh, time_max = ts, land_ownership = TRUE,
                  RESOURCE_ini = popinit, res_death_K = K,
                  action_thres = 0, budget_bonus = 0, manage_target = trgt,
                  plotting = F)
      
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
        print("Saving budget, costs, actions and pop")
        para <- OTI_default_results[k,2:5, param_set]
        para <- c(para, K, trgt)
        
        bdg <- rep(0, ts)
        pop <- rep(0, ts)
        cos <- rep(0, ts)
        act <- rep(0, ts)
        
        for (t in 1:final_ts) {
          bdg[t] <- sim$agents[[1]][1,17]
          pop[t] <- dim(sim$resource[[t]])[1]
          cos[t] <- sim$cost[[t]][1,9,2]
          act[t] <- mean(sim$action[[t]][1,9,2:stkh])
        }
        
        flw_bgt <- rbind(flw_bgt, c(para, bdg))
        flw_pop <- rbind(flw_pop, c(para, pop))
        flw_cos <- rbind(flw_cos, c(para, cos))
        flw_act <- rbind(flw_act, c(para, act))
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
        OTI_default_results[k,11,param_set] <- sum(sim$paras[,109])
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
        OTI_default_results[k,11,param_set] <- sum(sim$paras[,109])
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
        
        # Run GMSE for the parameter combo
        sim <- gmse(stakeholders = stkh, time_max = ts, land_ownership = TRUE,
                    RESOURCE_ini = popinit, res_death_K = K,
                    action_thres = at[i], budget_bonus = bb[j], manage_target = trgt,
                    plotting = F)
        
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
          print("Saving costs, actions and pop")
          para <- OTI_default_results[k,2:5, param_set]
          para <- c(para, K, trgt)
          
          bdg <- rep(0, ts)
          pop <- rep(0, ts)
          cos <- rep(0, ts)
          act <- rep(0, ts)
          
          for (t in 1:final_ts) {
            bdg[t] <- sim$agents[[1]][1,17]
            pop[t] <- dim(sim$resource[[t]])[1]
            cos[t] <- sim$cost[[t]][1,9,2]
            act[t] <- mean(sim$action[[t]][1,9,2:stkh])
          }
          
          flw_bgt <- rbind(flw_bgt, c(para, bdg))
          flw_pop <- rbind(flw_pop, c(para, pop))
          flw_cos <- rbind(flw_cos, c(para, cos))
          flw_act <- rbind(flw_act, c(para, act))
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
    } # end bb for loop
  } # end at else loop
} # end at for loop

# Simulation time
end <- Sys.time()
print(paste("Batch started", start, "and ended", end, sep = " "))

## save the 3D array of results?

# rbind the layers

tab_OTI_default_results <- OTI_default_results[,,1]

for (i in 2:dim(OTI_default_results)[3]) {
  tab_OTI_default_results <- rbind(tab_OTI_default_results, OTI_default_results[,,i])
}

write.csv(tab_OTI_default_results, file = "tab_OTI_default_batch3.csv")
write.csv(flw_bgt, file = "flw_bgt_batch3")
write.csv(flw_pop, file = "flw_pop_batch3.csv")
write.csv(flw_cos, file = "flw_cos_batch3.csv")
write.csv(flw_act, file = "flw_act_batch3.csv")

#### Results ####
# # Array of column names
# stats_columns <- c("rep", "budget", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci", "overK_tot", "overK_sd", "overK_95ci")
# 
# # Empty 2D array of correct size
# # Dimensions(lines = parameter combo index, columns = measures)
# stats_OTI_default_results <- matrix(data = NA, nrow = dim(OTI_default_results)[3], ncol = length(stats_columns), dimnames = list(NULL,stats_columns))
# 
# # for each parameter combo
# for (i in 1:dim(OTI_default_results)[3]) {
#   
#   # Store number of replicates for this combo
#   stats_OTI_default_results[i,1] <- dim(OTI_default_results)[1]
#   
#   # Next 3 columns just take values from batch_results
#   for (j in 2:4) {
#     stats_OTI_default_results[i,j] <- OTI_default_results[1,j,i]
#   }
#   
#   # Extinction probability (number of extinctions / number of replicates)
#   stats_OTI_default_results[i,5] <- round(sum(OTI_default_results[,5,i])/dim(OTI_default_results)[1],3)
#   
#   # Next are systematically mean, sd and 95CI of the mesures from batch_results
#   zz <- 0
#   for (k in 6:dim(OTI_default_results)[2]) {
#     stats_OTI_default_results[i,k+zz] <- round(mean(OTI_default_results[,k,i]),3)
#     stats_OTI_default_results[i,k+zz+1] <- sd(OTI_default_results[,k,i])
#     stats_OTI_default_results[i,k+zz+2] <- 1.86*stats_OTI_default_results[i,k+zz+1]/sqrt(rep)
#     zz <- zz + 2
#   }
#   
#   ## need to find a sd calculus more adapted to each measure
#   # uniform [-100;+inf] for actdev
#   # uniform [0; +inf] for absactdev
#   # uniform [0;100] for finyie
#   # uniform [0;+inf] for maxdiff 
#   # binomial for inacts and overK sd = sqrt(rep*mean(inacts)(1-rep))
# }
# 
# # Visualise the table to check for inconsistencies
# View(stats_OTI_default_results)
# 
# # Save the table in a csv file
# write.csv(stats_OTI_default_results, file = "stats_OTI_default_batch3.csv", row.names = F)

OTI_stats <- function(df, omit.extinction = FALSE) {
  
  df <- as.data.frame(df)
  
  # levels of OTI parameters
  upd_thr <- levels(as.factor(df$at))
  bud_bon <- levels(as.factor(df$bb))
  
  if (omit.extinction == "TRUE") { 
    
    # a loop to calculate extinction freq
    sub <- subset(df, at == 0 & bb == 0)
    ext_freq <- sum(sub$extinct)/dim(sub)[1]
    
    for (i in 2:length(upd_thr)) {
      for (j in 1:length(bud_bon)) {
        sub <- subset(df, at == upd_thr[i] & bb == bud_bon[j])
        ext_freq <- c(ext_freq, sum(sub$extinct)/dim(sub)[1])
      }
    }
    
    print("Ommiting replicates that resulted in Resource extinction")
    df <- subset(df, extinct == 0)
    
    # levels of OTI parameters
    upd_thr <- levels(as.factor(df$at))
    bud_bon <- levels(as.factor(df$bb))
    
    # initiate a count for later
    zz <- 1
    
  } # end if loop on omit.extinction 
  
  # create empty tab
  res_tab <- NULL
  
  ## Special subset for UT = 0, for which there was only one BB
  # initiate a string
  res_str <- NULL
  
  # subset
  sub <- subset(df, at == upd_thr[1] & bb == bud_bon[1])
  
  # number of replicates
  nbrep <- dim(sub)[1]
  
  # extinction frequency
  if (omit.extinction == TRUE) {
    ext <- ext_freq[zz]
  } else {
    ext <- sum(sub$extinct)/dim(sub)[1]
  }
  
  # start filling the string in
  res_str <- c(nbrep,mean(sub$budget),upd_thr[1],bud_bon[1], ext)
  
  ## mean, sd and 95ci for each proxy
  # Actual Resource population deviation from Manager's target
  res_sd <- sd(sub$act_dev)
  
  res_str <- c(res_str, mean(sub$act_dev), res_sd, 1.86*res_sd/sqrt(nbrep))
  
  # Absolute value of the Actual Resource population deviation from Manager's target
  res_sd <- sd(sub$abs_act_dev)
  
  res_str <- c(res_str, mean(sub$abs_act_dev), res_sd, 1.86*res_sd/sqrt(nbrep))
  
  # Users' total final yield
  res_sd <- sd(sub$fin_yield)
  
  res_str <- c(res_str, mean(sub$fin_yield), res_sd, 1.86*res_sd/sqrt(nbrep))
  
  # Difference between the highest and the lowest yield
  res_sd <- sd(sub$max_diff_yield)
  
  res_str <- c(res_str, mean(sub$max_diff_yield), res_sd, 1.86*res_sd/sqrt(nbrep))
  
  # Percentage of time steps of non-updating
  res_sd <- sd(sub$inac_ts)
  
  res_str <- c(res_str, mean(sub$inac_ts), res_sd, 1.86*res_sd/sqrt(nbrep))
  
  # Percentage of time steps of K overshooting
  res_sd <- sd(sub$overK)
  
  res_str <- c(res_str, mean(sub$overK), res_sd, 1.86*res_sd/sqrt(nbrep))
  
  # binding the string to the tab
  res_tab <- rbind(res_tab, as.numeric(res_str))
  
  # loop over the other OTI parameters
  # for each at value
  for (i in 2:length(upd_thr)) {
    
    # for each bb value
    for (j in 1:length(bud_bon)) {
      
      # increment tracker
      if (omit.extinction == TRUE) {
        zz <- zz + 1
      }
      
      # initiate a string
      res_str <- NULL
      
      # subset
      sub <- subset(df, at == upd_thr[i] & bb == bud_bon[j])
      
      # number of replicates
      nbrep <- dim(sub)[1]
      
      # extinction frequency
      if (omit.extinction == TRUE) {
        ext <- ext_freq[zz]
      } else {
        ext <- sum(sub$extinct)/dim(sub)[1]
      }
      
      # start filling the string in
      res_str <- c(nbrep,mean(sub$budget),upd_thr[i],bud_bon[j], ext)
      
      # avoid problems if there is only one replicate
      if (nbrep >= 2) {
        
        ## mean, sd and 95ci for each proxy
        # Actual Resource population deviation from Manager's target
        res_sd <- sd(sub$act_dev)
        
        res_str <- c(res_str, mean(sub$act_dev), res_sd, 1.86*res_sd/sqrt(nbrep))
        
        # Absolute value of the Actual Resource population deviation from Manager's target
        res_sd <- sd(sub$abs_act_dev)
        
        res_str <- c(res_str, mean(sub$abs_act_dev), res_sd, 1.86*res_sd/sqrt(nbrep))
        
        # Users' total final yield
        res_sd <- sd(sub$fin_yield)
        
        res_str <- c(res_str, mean(sub$fin_yield), res_sd, 1.86*res_sd/sqrt(nbrep))
        
        # Difference between the highest and the lowest yield
        res_sd <- sd(sub$max_diff_yield)
        
        res_str <- c(res_str, mean(sub$max_diff_yield), res_sd, 1.86*res_sd/sqrt(nbrep))
        
        # Percentage of time steps of non-updating
        res_sd <- sd(sub$inac_ts)
        
        res_str <- c(res_str, mean(sub$inac_ts), res_sd, 1.86*res_sd/sqrt(nbrep))
        
        # Percentage of time steps of K overshooting
        res_sd <- sd(sub$overK)
        
        res_str <- c(res_str, mean(sub$overK), res_sd, 1.86*res_sd/sqrt(nbrep))
        
      } else {
        
        # Actual Resource population deviation from Manager's target
        res_str <- c(res_str, sub$act_dev, 0, 0)
        
        # Absolute value of the Actual Resource population deviation from Manager's target
        res_str <- c(res_str, sub$abs_act_dev, 0, 0)
        
        # Users' total final yield
        res_str <- c(res_str, sub$fin_yield, 0, 0)
        
        # Difference between the highest and the lowest yield
        res_str <- c(res_str, sub$max_diff_yield, 0, 0)
        
        # Percentage of time steps of non-updating
        res_str <- c(res_str, sub$inac_ts, 0, 0)
        
        # Percentage of time steps of K overshooting
        res_str <- c(res_str, sub$overK, 0, 0)
      } # end else loop on nbrep
      
      # binding the string to the tab
      res_tab <- rbind(res_tab, as.numeric(res_str))
      
    } # end for loop on budget bonus
  } # end for loop on update threshold
  
  # Array of column names
  colnames(res_tab) <- c("rep", "budget", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci", "overK_tot", "overK_sd", "overK_95ci")
  
  res_tab <- as.data.frame(res_tab)
  
  return(res_tab)
  
} # end function

# import data
tab_OTI_default_results <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/Default-case/tab_OTI_default_batch3.csv")
brut <- as.data.frame(tab_OTI_default_results)

stats_OTI_default_results <- OTI_stats(df = brut, omit.extinction = F) 
we_stats_OTI_default_results <- OTI_stats(df = brut, omit.extinction = T)

# Save the table in a csv file
write.csv(stats_OTI_default_results, file = "stats_OTI_default_batch4.csv", row.names = F)

#### Plotting ####

# import data
tab_OTI_default_results <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/Default-case/tab_OTI_default_batch3.csv")
brut <- as.data.frame(tab_OTI_default_results)

stats_OTI_default_results <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/Default-case/stats_OTI_default_batch3.csv")
stat <- as.data.frame(stats_OTI_default_results)

## Extinction probability

attach(stat)

gg_extprob <- ggplot(stat, aes(x=as.factor(bb), y=ext_prob)) +
  facet_wrap(~at, ncol = 4) +
  geom_point(colour = "red") +     
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y = "Extinction probability") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#gg_extprob

## box plot

attach(brut)

# Resource population actual deviation from Manager's target
# the closer to zero the better
bp_actdev <- ggplot(brut, aes(x=as.factor(bb), y=act_dev)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target \n (in % of Manager's target)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#bp_actdev

# Absolute value of Resource population actual deviation from target
# the closer to zero the better
bp_absactdev <- ggplot(brut, aes(x=as.factor(bb), y=abs_act_dev*100)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  # geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -100, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute value of deviation from target \n (in % of Manager's target)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#bp_absactdev

# Users final yield
# Absolute value of Resource population actual deviation from target
# the closer to 100 the better
bp_finyie <- ggplot(brut, aes(x=as.factor(bb), y=fin_yield/100)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Users final yield (in k-budget units)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#bp_finyie

# Equity between users final yields
# the closer to zero the better
bp_maxdif <- ggplot(brut, aes(x=as.factor(bb), y=max_diff_yield)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Maximum difference between Users personal yields \n (in budget units)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#bp_maxdif

# Number of time steps of non updating
# (maximum value of time_max-1)
bp_inacts <- ggplot(brut, aes(x=as.factor(bb), y=inac_ts)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Mean number of non-updating policy events") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#bp_inacts

# Number of time steps when Resource population went over its K
# the closer to zero the better
bp_overK <- ggplot(brut, aes(x=as.factor(bb), y=overK)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Number of Resource K-overshooting events") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#bp_overK

## mean +- sd / mean +- 95ci

attach(stat)

# act_dev
msd_actdev <- ggplot(stat, aes(x=as.factor(bb), y=act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_sd, ymax=act_dev+act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target\n(fraction of MT, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#msd_actdev

mci_actdev <- ggplot(stat, aes(x=as.factor(bb), y=act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_95ci, ymax=act_dev+act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target\n(fraction of MT, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#mci_actdev

# abs_act_dev
msd_absactdev <- ggplot(stat, aes(x=as.factor(bb), y=abs_act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_sd, ymax=abs_act_dev+abs_act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute deviation from target\n(fraction of MT, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#msd_absactdev

mci_absactdev <- ggplot(stat, aes(x=as.factor(bb), y=abs_act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_95ci, ymax=abs_act_dev+abs_act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute deviation from target\n(fraction of MT, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#mci_absactdev

# fin_yield
msd_finyie <- ggplot(stat, aes(x=as.factor(bb), y=fin_yield/100)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=(fin_yield-fin_yield_sd)/100, ymax=(fin_yield+fin_yield_sd)/100),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Users final yield\n(in k-budget units, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#msd_finyie

mci_finyie <- ggplot(stat, aes(x=as.factor(bb), y=fin_yield/100)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=(fin_yield-fin_yield_95ci)/100, ymax=(fin_yield+fin_yield_95ci)/100),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Users final yield\n(in k-budget units, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#mci_finyie

# max diff between users yield
msd_maxdif <- ggplot(stat, aes(x=as.factor(bb), y=max_diff_yield*100)) +
  facet_wrap(~at, ncol = 4) +
  geom_errorbar(aes(ymin=(max_diff_yield-max_diff_yield_sd)*100, ymax=(max_diff_yield+max_diff_yield_sd)*100), 
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour="red") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Maximum difference between Users yields\n(in percentage of the highest yield, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#msd_maxdif

mci_maxdif <- ggplot(stat, aes(x=as.factor(bb), y=max_diff_yield*100)) +
  facet_wrap(~at, ncol = 4) +
  geom_errorbar(aes(ymin=(max_diff_yield-max_diff_yield_95ci)*100, ymax=(max_diff_yield+max_diff_yield_95ci)*100), 
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour="red") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Maximum difference between Users yields\n(in percentage of the highest yield, mean +/- 95CI)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#mci_maxdif

# overK 
msd_overK <- ggplot(stat, aes(x=as.factor(bb), y=overK_tot)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=overK_tot-overK_sd, ymax=overK_tot+overK_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Number of K-overshooting events (mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#msd_overK

mci_overK <- ggplot(stat, aes(x=as.factor(bb), y=overK_tot)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=overK_tot-overK_95ci, ymax=overK_tot+overK_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Number of K-overshooting events (mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
#mci_overK

# all of them
gg_extprob

bp_actdev
bp_absactdev
bp_finyie
bp_maxdif
bp_inacts
bp_overK

msd_actdev # sometimes goes under -1 which is impossible
msd_absactdev
msd_finyie # sometime goes over 100 which is impossible
msd_maxdif # sometime goes under 0 which is impossible
msd_overK # sometime goes under 0 which is impossible

mci_actdev
mci_absactdev
mci_finyie
mci_maxdif
mci_overK

#### A figure for extinction probability ~ UT : BB ####

stat <- stats_OTI_default_results

# points
plot(1, type="n", xlab="Update threshold \n (in % of Manager's target)", ylab="Extinction frequency", xlim=c(0, 135), ylim=c(0, 1))

# a dotted line to show the null, K, and extinction UT
abline(v=c(0,50,100), lty = 2, lwd = 1.5, col = c("black","blue","red"))
# to how the extinction probability of 1
abline(h=1, lty = 2, lwd = 1.5, col = "red")

bud_bon <- levels(as.factor(stat$bb))

cl <- rainbow(length(bud_bon))

for (i in 2:length(bud_bon)) {
  # # debug
  # print(i)
  # print(subset(stat, bb == bud_bon[i])[,4])
  dd <- subset(stat, bb == bud_bon[i])
  dd$at <- dd$at*100
  points(dd$ext_prob ~ dd$at, col = cl[i-1], type = "b", pch = 16)
}

dd <- subset(stat, bb == 0)
dd$at <- dd$at*100
points(data = dd,
       ext_prob ~ at, col = "black", 
       type = "b", pch = 16)

bud_bon <- as.numeric(bud_bon)*100
legend(105, 1.01,
       legend=bud_bon, col=c("black", cl),
       pch = 16, pt.cex = 1, cex=0.6, #horiz = T,
       title = "Budget bonus\n(in % of\ninitial budget)")

#### plots by isolating replicates without extinction ####

## box plot

attach(wo_ext)

# Resource population actual deviation from Manager's target
# the closer to zero the better
WE_bp_actdev <- ggplot(wo_ext, aes(x=as.factor(bb), y=act_dev)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target \n (in % of Manager's target)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# Absolute value of Resource population actual deviation from target
# the closer to zero the better
WE_bp_absactdev <- ggplot(wo_ext, aes(x=as.factor(bb), y=abs_act_dev*100)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  # geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -100, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute value of deviation from target \n (in % of Manager's target)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# Users final yield
# Absolute value of Resource population actual deviation from target
# the closer to 100 the better
WE_bp_finyie <- ggplot(wo_ext, aes(x=as.factor(bb), y=fin_yield/100)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Users final yield (in k-budget units)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# Equity between users final yields
# the closer to zero the better
WE_bp_maxdif <- ggplot(wo_ext, aes(x=as.factor(bb), y=max_diff_yield)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Maximum difference between Users personal yields \n (in budget units)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# Number of time steps of non updating
# (maximum value of time_max-1)
WE_bp_inacts <- ggplot(wo_ext, aes(x=as.factor(bb), y=inac_ts)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Mean number of non-updating policy events") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# Number of time steps when Resource population went over its K
# the closer to zero the better
WE_bp_overK <- ggplot(wo_ext, aes(x=as.factor(bb), y=overK)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Number of Resource K-overshooting events") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

## mean +- sd / mean +- 95ci

attach(stats_wo_ext)

# act_dev
WE_msd_actdev <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_sd, ymax=act_dev+act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target\n(fraction of MT, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

WE_mci_actdev <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_95ci, ymax=act_dev+act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target\n(fraction of MT, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# abs_act_dev
WE_msd_absactdev <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=abs_act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_sd, ymax=abs_act_dev+abs_act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute deviation from target\n(fraction of MT, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

WE_mci_absactdev <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=abs_act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_95ci, ymax=abs_act_dev+abs_act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute deviation from target\n(fraction of MT, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# fin_yield
WE_msd_finyie <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=fin_yield/100)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=(fin_yield-fin_yield_sd)/100, ymax=(fin_yield+fin_yield_sd)/100),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Users final yield\n(in k-budget units, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

WE_mci_finyie <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=fin_yield/100)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=(fin_yield-fin_yield_95ci)/100, ymax=(fin_yield+fin_yield_95ci)/100),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Users final yield\n(in k-budget units, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# max diff between users yield
WE_msd_maxdif <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=max_diff_yield*100)) +
  facet_wrap(~at, ncol = 4) +
  geom_errorbar(aes(ymin=(max_diff_yield-max_diff_yield_sd)*100, ymax=(max_diff_yield+max_diff_yield_sd)*100), 
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour="red") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Maximum difference between Users yields\n(in percentage of the highest yield, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

WE_mci_maxdif <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=max_diff_yield*100)) +
  facet_wrap(~at, ncol = 4) +
  geom_errorbar(aes(ymin=(max_diff_yield-max_diff_yield_95ci)*100, ymax=(max_diff_yield+max_diff_yield_95ci)*100), 
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour="red") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Maximum difference between Users yields\n(in percentage of the highest yield, mean +/- 95CI)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# inact ts 
WE_msd_inacts <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=inac_ts)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=inac_ts-inac_ts_sd, ymax=inac_ts+inac_ts_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Frequency of non-updating events (in %, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

WE_mci_inac_ts <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=inac_ts)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=inac_ts-inac_ts_95ci, ymax=inac_ts+inac_ts_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  #geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Frequency of non-updating events (in %, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# overK 
WE_msd_overK <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=overK_tot)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=overK_tot-overK_sd, ymax=overK_tot+overK_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Number of K-overshooting events (mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

WE_mci_overK <- ggplot(stats_wo_ext, aes(x=as.factor(bb), y=overK_tot)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=overK_tot-overK_95ci, ymax=overK_tot+overK_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Number of K-overshooting events (mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))

# all of them
WE_bp_actdev
WE_bp_absactdev
WE_bp_finyie
WE_bp_maxdif
WE_bp_inacts
WE_bp_overK

WE_msd_actdev
WE_msd_absactdev
WE_msd_finyie # sometime goes over 100 which is impossible
WE_msd_maxdif # sometime goes under 0 which is impossible
WE_msd_overK # sometime goes under 0 which is impossible

WE_mci_actdev
WE_mci_absactdev
WE_mci_finyie
WE_mci_maxdif
WE_mci_overK

## Plot what happens at UT=0.1
costs <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/Default-case/flw_cos_batch3.csv")
popul <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/Default-case/flw_pop_batch3.csv")
actio <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/Default-case/flw_act_batch3.csv")

# only without extinction
we_costs <- subset(costs, extinct == 0)
we_popul <- subset(popul, extinct == 0)
we_actio <- subset(actio, extinct == 0)

# function returning ymax, ymin and ymean and plot
maxminmean <- function(df, acth, bubo, tmax, color, yaxis) {
  # subsetting
  dd <- subset(df, at == acth & bb == bubo)  
  
  # max
  ymax <- max(dd[,6])
  # ymax <- max(dd[,8])
  for (k in 7:dim(dd)[2]) {
  # for (k in 9:dim(dd)[2]) {
    ymax <- c(ymax,max(dd[,k]))
  }
  
  # min
  ymin <- min(dd[,6])
  # ymax <- max(dd[,8])
  for (k in 7:dim(dd)[2]) {
    # for (k in 9:dim(dd)[2]) 
    ymin <- c(ymin,min(dd[,k]))
  }
  
  # mean
  ymean <- mean(dd[,6])
  # ymax <- max(dd[,8])
  for (k in 7:dim(dd)[2]) {
    # for (k in 9:dim(dd)[2]) 
    ymean <- c(ymean,mean(dd[,k]))
  }
  
  # plot
  xrange <- seq(1,tmax,1)
  # lightcol <- paste("light",color,sep = "")
  lightcol <- lighten(color, amount = 0.4)
  trans <- adjustcolor(lightcol,alpha.f=0.5)
  plot(1, type = "n", xlab = "time", ylab = yaxis, ylim = c(0,max(ymax)), xlim = c(0,20), main = paste("UT = ", acth, " BB = ",bubo))
  polygon(c(xrange,rev(xrange)),c(ymax,rev(ymin)),col=trans, border = lightcol)
  
  # max cost with FLI strategy
  if (str_detect(yaxis, "Cost")){
    abline(h = (dd[1,2]/10)+10, lty = 2, pch = 2, col = "black")
  }
  
  # initial budget
  if (str_detect(yaxis, "budget")){
    abline(h = dd[1,2], lty = 2, pch = 2, col = "black")
  }
  
  # maximum culling capacity
  if (str_detect(yaxis, "action")){
    abline(h = dd[1,2]/10, lty = 2, pch = 2, col = "black")
  }
  
  # # K and manager target
  # if (str_detect(yaxis, "population")){
  #   abline(h = dd[1,5], lty = 2, pch = 2, col = "red")
  #   abline(h = dd[1,6], lty = 2, pch = 2, col = "blue")
  # }
  
  lines(x = xrange, y = ymean, type = "b", pch = 20, col = color)
}

# max min and all the different trajectories
maxminmulti <- function(df, acth, bubo, tmax, color, yaxis) {
  # subsetting
  dd <- subset(df, at == acth & bb == bubo)  
  
  # max
  ymax <- max(dd[,6])
  # ymax <- max(dd[,8])
  for (k in 7:dim(dd)[2]) {
    # for (k in 9:dim(dd)[2]) {
    ymax <- c(ymax,max(dd[,k]))
  }
  
  # min
  ymin <- min(dd[,6])
  # ymax <- max(dd[,8])
  for (k in 7:dim(dd)[2]) {
    # for (k in 9:dim(dd)[2]) 
    ymin <- c(ymin,min(dd[,k]))
  }
  
  # mean
  ymean <- mean(dd[,6])
  # ymax <- max(dd[,8])
  for (k in 7:dim(dd)[2]) {
    # for (k in 9:dim(dd)[2]) 
    ymean <- c(ymean,mean(dd[,k]))
  }
  
  # plot
  xrange <- seq(1,tmax,1)
  # lightcol <- paste("light",color,sep = "")
  lightcol <- lighten(color, amount = 0.4)
  trans <- adjustcolor(lightcol,alpha.f=0.5)
  plot(1, type = "n", xlab = "time", ylab = yaxis, ylim = c(0,max(ymax)), xlim = c(0,20), main = paste("UT = ", acth, " BB = ",bubo))
  polygon(c(xrange,rev(xrange)),c(ymax,rev(ymin)),col=trans, border = lightcol)
  
  # max cost with FLI strategy
  if (str_detect(yaxis, "Cost")){
    abline(h = (dd[1,2]/10)+10, lty = 2, pch = 2, col = "black")
  }
  
  # initial budget
  if (str_detect(yaxis, "budget")){
    abline(h = dd[1,2], lty = 2, pch = 2, col = "black")
  }
  
  # maximum culling capacity
  if (str_detect(yaxis, "action")){
    abline(h = dd[1,2]/10, lty = 2, pch = 2, col = "black")
  }
  
  # # K and manager target
  # if (str_detect(yaxis, "population")){
  #   abline(h = dd[1,5], lty = 2, pch = 2, col = "red")
  #   abline(h = dd[1,6], lty = 2, pch = 2, col = "blue")
  # }
  
  # all the trajectories
  for (i in 1:(dim(dd)[1])) {
    points(x = xrange, y = dd[i,6:dim(dd)[2]], type = "b", pch = i, col = color)
  }
}

#for (i in seq(0,1,0.1)) {maxminmean(we_costs, 0.1, i, 20, "blue", "Cost of culling")}

#### confronting at = 0 and at = 0.1 ####

OTI_vs_FLI_plot <- function(df, upth, goal = c(0:4), variance = c("sd","ci")) {
  
  # subsetting
  fli <- subset(df, at == 0)
  oti <- subset(df, at == upth)
  
  if (goal == 0) {
    
    # get means, sd, and 95ci and plot
    fli_avg <- rep(fli$ext_prob, dim(oti)[1])
    oti_avg <- oti$ext_prob
  
    # plotting
    xoti <- oti$bb*100
    
    # barplot
    moyennes = c(fli_avg,oti_avg) 
    moyennes = matrix(moyennes,nc=11, nr=2, byrow=T) # nc : nombre de tests - nr : nombre de barres accolées (ici par paire) 
    colnames(moyennes) = xoti 
    barplot(moyennes,beside=T, col = c("black","blue"),
            xlab = "Budget bonus\n(in % of initial budget)", ylab = "Extinction frequency (N = 100)",
            legend.text = T)
            # main = paste("UT = ", upth*100,"%")) #; box()
    
    # legend("topright", legend = c("0%",paste(upth*100,"%")), fill = c("black","blue"), title = "UT", cex = 0.7, bty = "n")
           
  }
  
  if (goal == 1) {
  
    # get means, sd, and 95ci and plot
    fli_avg <- fli$act_dev*100
    oti_avg <- oti$act_dev*100
    
    if (variance == "sd") {
      
      # FLI strat
      fli_sd <- fli$act_dev_sd*100
      # prevent the sd range to go over the borders
      fli_sd_neg <- ifelse(test = fli_avg-fli_sd < -100, fli_sd+(fli_avg-fli_sd+100), fli_sd)
      
      # OTI strat
      oti_sd <- oti$act_dev_sd*100
      # prevent the sd range to go over the borders
      oti_sd_neg <- ifelse(test = oti_avg-oti_sd < -100, oti_sd+(oti_avg-oti_sd+100), oti_sd)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_avg+fli_sd, length(xtendrange))
      flimin <- rep(fli_avg-fli_sd_neg, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Resource population deviation from MT\n(in %, mean +/- SD)",
           ylim = c(-100,max(max(fli_avg+fli_sd),max(oti_avg+oti_sd))+10), xlim = c(0,100)) # ,
           # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "lightgrey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 0, lty = 2, lwd = 1.5, col = "darkgreen")
      abline(h = -100, lty = 2, lwd = 1.2, col = "red")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "blue")
    }
    
    if (variance == "ci") {
      
      # FLI strat
      fli_95ci <- fli$act_dev_95ci*100
      # prevent the 95ci range to go over the borders
      fli_95ci_neg <- ifelse(test = fli_avg-fli_95ci < -100, fli_95ci+(fli_avg-fli_95ci+100), fli_95ci)
      
      # OTI strat
      oti_95ci <- oti$act_dev_95ci*100
      # prevent the 95ci range to go over the borders
      oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < -100, oti_95ci+(oti_avg-oti_95ci+100), oti_95ci)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_avg+fli_95ci, length(xtendrange))
      flimin <- rep(fli_avg-fli_95ci_neg, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Resource population deviation from MT\n(in %, mean +/- 95CI)",
           ylim = c(-100,max(max(fli_avg+fli_95ci),max(oti_avg+oti_95ci))+5), xlim = c(0,100)) # ,
           # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "lightgrey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 0, lty = 2, lwd = 1.5, col = "darkgreen")
      abline(h = -100, lty = 2, lwd = 1.2, col = "red")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_avg-oti_95ci_neg, xoti, oti_avg+oti_95ci, length=0.05, angle=90, code=3, col = "blue")
    }
  }
  
  if (goal == 2) {
    
    # get means, sd, and 95ci and plot
    fli_avg <- fli$fin_yield/100
    oti_avg <- oti$fin_yield/100
    
    if (variance == "sd") {
      
      # FLI strat
      fli_sd <- fli$fin_yield_sd/100
      # prevent the sd range to go over the borders
      fli_sd_neg <- ifelse(test = fli_avg-fli_sd < 0, fli_sd+(fli_avg-fli_sd), fli_sd)
      fli_sd_pos <- ifelse(test = fli_avg-fli_sd > 100, fli_sd-(fli_avg+fli_sd-100), fli_sd)
      
      # OTI strat
      oti_sd <- oti$fin_yield_sd/100
      # prevent the sd range to go over the borders
      oti_sd_neg <- ifelse(test = oti_avg-oti_sd < 0, oti_sd+(oti_avg-oti_sd+100), oti_sd)
      oti_sd_pos <- ifelse(test = oti_avg-oti_sd > 100, oti_sd-(oti_avg+oti_sd-100), oti_sd)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_avg+fli_sd_pos, length(xtendrange))
      flimin <- rep(fli_avg-fli_sd_neg, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Sum of Users final budgets\n(in k-a.b.u, mean +/- SD)",
           ylim = c(min(min(fli_avg+fli_sd),min(oti_avg+oti_sd))-5,100), xlim = c(0,100)) # ,
           # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "lightgrey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 100, lty = 2, lwd = 1.5, col = "darkgreen")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd_pos, length=0.05, angle=90, code=3, col = "blue")
    }
    
    if (variance == "ci") {
      
      # FLI strat
      fli_95ci <- fli$fin_yield_95ci/100
      # prevent the 95ci range to go over the borders
      fli_95ci_neg <- ifelse(test = fli_avg-fli_95ci < 0, fli_95ci+(fli_avg-fli_95ci), fli_95ci)
      fli_95ci_pos <- ifelse(test = fli_avg-fli_95ci > 100, fli_95ci-(fli_avg+fli_95ci-100), fli_95ci)
      
      # OTI strat
      oti_95ci <- oti$fin_yield_95ci/100
      # prevent the 95ci range to go over the borders
      oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < 0, oti_95ci+(oti_avg-oti_95ci), oti_95ci)
      oti_95ci_pos <- ifelse(test = oti_avg-oti_95ci > 100, oti_95ci-(oti_avg+oti_95ci-100), oti_95ci)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_avg+fli_95ci_pos, length(xtendrange))
      flimin <- rep(fli_avg-fli_95ci_neg, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Sum of Users final budgets\n(in k-a.b.u, mean +/- 95CI)",
           ylim = c(min(min(fli_avg+fli_95ci),min(oti_avg+oti_95ci))-2,100), xlim = c(0,100)) # , 
           # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "lightgrey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 100, lty = 2, lwd = 1.5, col = "darkgreen")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_avg-oti_95ci_neg, xoti, oti_avg+oti_95ci_pos, length=0.05, angle=90, code=3, col = "blue")
    }
  }
  
  if (goal == 3) {
    
    # get means, sd, and 95ci and plot
    fli_avg <- fli$max_diff_yield*100
    oti_avg <- oti$max_diff_yield*100
    
    if (variance == "sd") {
      
      # FLI strat
      fli_sd <- fli$max_diff_yield_sd*100
      # prevent the sd range to go over the borders
      fli_sd_neg <- ifelse(test = fli_avg-fli_sd < 0, fli_sd+(fli_avg-fli_sd), fli_sd)
      fli_sd_pos <- ifelse(test = fli_avg-fli_sd > 100, fli_sd-(fli_avg+fli_sd-100), fli_sd)
      
      # OTI strat
      oti_sd <- oti$max_diff_yield_sd*100
      # prevent the sd range to go over the borders
      oti_sd_neg <- ifelse(test = oti_avg-oti_sd < 0, oti_sd+(oti_avg-oti_sd), oti_sd)
      oti_sd_pos <- ifelse(test = oti_avg-oti_sd > 100, oti_sd-(oti_avg+oti_sd-100), oti_sd)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_avg+fli_sd_pos, length(xtendrange))
      flimin <- rep(fli_avg-fli_sd_neg, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Maximum difference between Users yields\n(in % of the highest yield, mean +/- SD)",
           ylim = c(0,max(max(fli_avg+fli_sd),max(oti_avg+oti_sd))+5), xlim = c(0,100)) #,
           # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "lightgrey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 0, lty = 2, lwd = 1.5, col = "darkgreen")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd_pos, length=0.05, angle=90, code=3, col = "blue")
    }
    
    if (variance == "ci") {
      
      # FLI strat
      fli_95ci <- fli$max_diff_yield_95ci*100
      # prevent the 95ci range to go over the borders
      fli_95ci_neg <- ifelse(test = fli_avg-fli_95ci < 0, fli_95ci+(fli_avg-fli_95ci), fli_95ci)
      fli_95ci_pos <- ifelse(test = fli_avg-fli_95ci > 100, fli_95ci-(fli_avg+fli_95ci-100), fli_95ci)
      
      # OTI strat
      oti_95ci <- oti$max_diff_yield_95ci*100
      # prevent the 95ci range to go over the borders
      oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < 0, oti_95ci+(oti_avg-oti_95ci), oti_95ci)
      oti_95ci_pos <- ifelse(test = oti_avg-oti_95ci > 100, oti_95ci-(oti_avg+oti_95ci-100), oti_95ci)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_avg+fli_95ci_pos, length(xtendrange))
      flimin <- rep(fli_avg-fli_95ci_neg, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Maximum difference between Users yields\n(in % of the highest yield, mean +/- 95CI)",
           ylim = c(0,max(max(fli_avg+fli_sd),max(oti_avg+oti_sd))+5), xlim = c(0,100)) # ,
           # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "lightgrey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 100, lty = 2, lwd = 1.5, col = "darkgreen")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_avg-oti_95ci_neg, xoti, oti_avg+oti_95ci_pos, length=0.05, angle=90, code=3, col = "blue")
    }
  }
  
  if (goal == 4) {
    
    # get means, sd, 95ci and plot
    oti_avg <- oti$inac_ts*100
    
    if (variance == "sd") {
      
      # OTI strat
      oti_sd <- oti$inac_ts_sd*100
      # prevent the sd range to go over the borders
      oti_sd_neg <- ifelse(test = oti_avg-oti_sd < 0, oti_sd+(oti_avg-oti_sd), oti_sd)
      oti_sd_pos <- ifelse(test = oti_avg-oti_sd > 100, oti_sd-(oti_avg+oti_sd-100), oti_sd)
      
      # plotting
      xoti <- oti$bb*100
      
      # barplot
      diag = barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
                     xlab = "Budget bonus\n(in % of initial budget)", ylab = "Time steps without updating\n(in %, mean +/- SD)",
                     ylim = c(0,100), xlim = c(0,100)) # ,
                     # main = paste("UT = ", upth*100,"%"))
      arrows(diag, oti_avg-oti_sd_neg, diag, oti_avg+oti_sd_pos, length=0.03, angle=90, code=3, col = "black")
    }
    
    if (variance == "ci") {
      
      # OTI strat
      oti_95ci <- oti$inac_ts_95ci*100
      # prevent the 95ci range to go over the borders
      oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < 0, oti_95ci+(oti_avg-oti_95ci), oti_95ci)
      oti_95ci_pos <- ifelse(test = oti_avg-oti_95ci > 100, oti_95ci-(oti_avg+oti_95ci-100), oti_95ci)
      
      # plotting
      xoti <- oti$bb*100
      
      # barplot
      diag = barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
                     xlab = "Budget bonus\n(in % of initial budget)", ylab = "Time steps without updating\n(in %, mean +/- 95CI)",
                     ylim = c(0,100), xlim = c(0,100)) # ,
                     # main = paste("UT = ", upth*100,"%"))
      arrows(diag, oti_avg-oti_95ci_neg, diag, oti_avg+oti_95ci_pos, length=0.03, angle=90, code=3, col = "black")
    }
  }
}

layout(matrix(c(1,2,3,4), nrow = 2), widths = c(3,3))

# Pour créer un espace pour le titre global
par(oma = c(0, 0, 3, 0))
layout.show(n = 4)

OTI_vs_FLI_plot(df = we_stats_OTI_default_results, upth = 0.1, goal = 0, variance = "sd")
OTI_vs_FLI_plot(df = we_stats_OTI_default_results, upth = 0.1, goal = 4, variance = "sd")
OTI_vs_FLI_plot(df = we_stats_OTI_default_results, upth = 0.1, goal = 1, variance = "sd")
OTI_vs_FLI_plot(df = we_stats_OTI_default_results, upth = 0.1, goal = 2, variance = "sd")

# Ajout du titre global
mtext("UT = 10%",
      outer = TRUE, cex = 1.2, line = 1.5)

