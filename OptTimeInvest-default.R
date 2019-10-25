# Adrian Bach
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

# Create an empty structure for basic stats on OTI_default_results

# Array of column names
stats_columns <- c("rep", "budget", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci", "overK_tot", "overK_sd", "overK_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
stats_OTI_default_results <- matrix(data = NA, nrow = dim(OTI_default_results)[3], ncol = length(stats_columns), dimnames = list(NULL,stats_columns))


## Create 3 structures to follow up on the costs, the actions, and the population along the simulations
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
        print("Saving costs, actions and pop")
        para <- OTI_default_results[k,2:5, param_set]
        
        pop <- rep(0, ts)
        cos <- rep(0, ts)
        act <- rep(0, ts)
        
        for (t in 1:final_ts) {
          pop[t] <- dim(sim$resource[[t]])[1]
          cos[t] <- sim$cost[[t]][1,9,2]
          act[t] <- mean(sim$action[[t]][1,9,2:stkh])
        }
        
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
          
          pop <- rep(0, ts)
          cos <- rep(0, ts)
          act <- rep(0, ts)
          
          for (t in 1:final_ts) {
            pop[t] <- dim(sim$resource[[t]])[1]
            cos[t] <- sim$cost[[t]][1,9,2]
            act[t] <- mean(sim$action[[t]][1,9,2:stkh])
          }
          
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
write.csv(flw_pop, file = "flw_pop_batch3.csv")
write.csv(flw_cos, file = "flw_cos_batch3.csv")
write.csv(flw_act, file = "flw_act_batch3.csv")

#### Results ####

# for each parameter combo
for (i in 1:dim(OTI_default_results)[3]) {
  
  # Store number of replicates for this combo
  stats_OTI_default_results[i,1] <- dim(OTI_default_results)[1]
  
  # Next 3 columns just take values from batch_results
  for (j in 2:4) {
    stats_OTI_default_results[i,j] <- OTI_default_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  stats_OTI_default_results[i,5] <- round(sum(OTI_default_results[,5,i])/dim(OTI_default_results)[1],3)
  
  # Next are systematically mean, sd and 95CI of the mesures from batch_results
  zz <- 0
  for (k in 6:dim(OTI_default_results)[2]) {
    stats_OTI_default_results[i,k+zz] <- round(mean(OTI_default_results[,k,i]),3)
    stats_OTI_default_results[i,k+zz+1] <- sd(OTI_default_results[,k,i])
    stats_OTI_default_results[i,k+zz+2] <- 1.86*stats_OTI_default_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
  
  ## need to find a sd calculus more adapted to each measure
  # uniform [-100;+inf] for actdev
  # uniform [0; +inf] for absactdev
  # uniform [0;100] for finyie
  # uniform [0;+inf] for maxdiff 
  # binomial for inacts and overK sd = sqrt(rep*mean(inacts)(1-rep))
}

# Visualise the table to check for inconsistencies
View(stats_OTI_default_results)

# Save the table in a csv file
write.csv(stats_OTI_default_results, file = "stats_OTI_default_batch3.csv", row.names = F)

#### plotting ####

# import data
tab_OTI_default_results <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/tab_OTI_default_batch2.csv")
brut <- as.data.frame(tab_OTI_default_results)

stats_OTI_default_results <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/stats_OTI_default_batch2.csv")
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

msd_actdev
msd_absactdev
msd_finyie # sometime goes over 100 which is impossible
msd_maxdif # sometime goes under 0 which is impossible
msd_overK # sometime goes under 0 which is impossible

mci_actdev
mci_absactdev
mci_finyie
mci_maxdif
mci_overK