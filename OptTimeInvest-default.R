# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy update timing
# Simulations + Results + Figures


#### Initialization ####

## Packages 

# GMSE
install.packages("GMSE")

# ggplot
install.packages("ggplot2")
install.packages("dplyr")

## Libraries

# GMSE
library("GMSE")

# plotting
library("ggplot2")
library(grid)
library(dplyr)

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
popinit <- 500

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
batch4_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)-(length(bb)-1)), dimnames = list(NULL,columns,NULL))                 

# Create an empty structure for basic stats on batch4_results

# Array of column names
stats_columns <- c("rep", "budget", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci", "overK_tot", "overK_sd", "overK_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
stats_batch4_results <- matrix(data = NA, nrow = dim(batch4_results)[3], ncol = length(stats_columns), dimnames = list(NULL,stats_columns))


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
      
      # Pick up values for simulation results and store them in batch4_results
      
      # Replicate number
      batch4_results[k,1,param_set] <- k
      
      # Budget
      batch4_results[k,2,param_set] <- bdgt
      
      # AT value
      batch4_results[k,3,param_set] <- at[i]
      
      # BB value
      batch4_results[k,4,param_set] <- 0
      
      # Has extinction occured? (yes = 1, no = 0)
      batch4_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      # Next measures involve calculus that can be disturbed if extinction occured
      
      # If exctinction occured
      if (batch4_results[k,5,param_set] != 0) {
        
        # Resource actual pop deviation from target
        batch4_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
        
        # Absolute value
        batch4_results[k,7,param_set] <- abs(batch4_results[k,6,param_set])
        
        # Users total final yield
        batch4_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # Maximum difference between Users yield
        batch4_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
        
        # Number of timesteps during which Manager chose not to update policy
        batch4_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
        
        # Number of K exceedings
        batch4_results[k,11,param_set] <- sum(sim$paras[,109])
      }
      
      # If extinction did not occured
      else {
        
        # Resource actual pop deviation from target
        batch4_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
        
        # Absolute value
        batch4_results[k,7,param_set] <- abs(batch4_results[k,6,param_set])
        
        # Users total final yield
        batch4_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # Maximum difference between Users yield
        batch4_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
        
        # Number of timesteps during which Manager chose not to update policy
        batch4_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
        
        # Number of K exceedings
        batch4_results[k,11,param_set] <- sum(sim$paras[,109])
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
        
        # Pick up values for simulation results and store them in batch4_results
        
        # Replicate number
        batch4_results[k,1,param_set] <- k
        
        # Budget
        batch4_results[k,2,param_set] <- bdgt
        
        # AT value
        batch4_results[k,3,param_set] <- sim$paras[1,106]
        
        # BB value
        batch4_results[k,4,param_set] <- bb[j]
        
        # Has extinction occured? (yes = 1, no = 0)
        batch4_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
        
        # Next measures involve calculus that can be disturbed if extinction occured
        
        # If exctinction occured
        if (batch4_results[k,5,param_set] != 0) {
          
          # Resource actual pop deviation from target
          batch4_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
          
          # absolute value
          batch4_results[k,7,param_set] <- abs(batch4_results[k,6,param_set])
          
          # Users total final yield
          batch4_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield (in percentage of the highest yield)
          batch4_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          batch4_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
          
          # Number of K exceedings
          batch4_results[k,11,param_set] <- sum(sim$paras[,109])
        }
        
        # If extinction did not occured
        else {
          
          # Resource actual pop deviation from target
          batch4_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
          
          # absolute value
          batch4_results[k,7,param_set] <- abs(batch4_results[k,6,param_set])
          
          # Users total final yield
          batch4_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield (in percentage of the highest yield)
          batch4_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          batch4_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
          
          # Number of K exceedings
          batch4_results[k,11,param_set] <- sum(sim$paras[,109])
        }
      } # end rep for loop
      
      # keep track of the simulations
      if (param_set %% 5 == 0) {
        print(paste("parameter set number", param_set, "out of", dim(batch4_results)[3], "at", Sys.time(), sep = " "))
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

tab_batch4_results <- batch4_results[,,1]

for (i in 2:dim(batch4_results)[3]) {
  tab_batch4_results <- rbind(tab_batch4_results, batch4_results[,,i])
}

write.csv(tab_batch4_results, file = "tab_batch4_results.csv")


#### Results ####

# for each parameter combo
for (i in 1:dim(batch4_results)[3]) {
  
  # Store number of replicates for this combo
  stats_batch4_results[i,1] <- dim(batch4_results)[1]
  
  # Next 3 columns just take values from batch_results
  for (j in 2:4) {
    stats_batch4_results[i,j] <- batch4_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  stats_batch4_results[i,5] <- round(sum(batch4_results[,5,i])/dim(batch4_results)[1],3)
  
  # Next are systematically mean, sd and 95CI of the meaures from batch_results
  zz <- 0
  for (k in 6:dim(batch4_results)[2]) {
    stats_batch4_results[i,k+zz] <- round(mean(batch4_results[,k,i]),3)
    stats_batch4_results[i,k+zz+1] <- sd(batch4_results[,k,i])
    stats_batch4_results[i,k+zz+2] <- 1.86*stats_batch4_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}

# Visualise the table to check for inconsistencies
View(stats_batch4_results)

# Save the table in a csv file
write.csv(stats_batch4_results, file = "stats_batch1.csv", row.names = F)

#### plotting ####

brut <- as.data.frame(tab_batch4_results)
stat <- as.data.frame(stats_batch4_results)

## Extinction probability

attach(stat)

gg_extprob <- ggplot(stat, aes(x=as.factor(bb), y=ext_prob)) +
  facet_wrap(~at, ncol = 4) +
  geom_point(colour = "red") +     
  labs(x="Budget Bonus (in fraction of Initial Budget)", y = "Extinction probability") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg_extprob

## box plot

attach(brut)

# Resource population actual deviation from Manager's target
# the closer to zero the better
bp_actdev <- ggplot(brut, aes(x=as.factor(bb), y=act_dev*100)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at, ncol = 4) +            
  # geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -100, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target \n (in % of Manager's target)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
bp_actdev

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
bp_absactdev

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
bp_finyie

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
bp_maxdif

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
bp_inacts

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
bp_overK

## mean +- sd / mean +- 95ci

attach(stat)

# act_dev
msd_actdev <- ggplot(stat, aes(x=as.factor(bb), y=act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_sd, ymax=act_dev+act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target\n(fraction of MT, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
msd_actdev

mci_actdev <- ggplot(stat, aes(x=as.factor(bb), y=act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_95ci, ymax=act_dev+act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Resource population deviation from target\n(fraction of MT, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
mci_actdev

# abs_act_dev
msd_absactdev <- ggplot(stat, aes(x=as.factor(bb), y=abs_act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_sd, ymax=abs_act_dev+abs_act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute deviation from target\n(fraction of MT, mean +/- sd)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
msd_absactdev

mci_absactdev <- ggplot(stat, aes(x=as.factor(bb), y=abs_act_dev)) +
  facet_wrap(~at, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_95ci, ymax=abs_act_dev+abs_act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(colour = "red") +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Budget Bonus (in fraction of Initial Budget)", y= "Absolute deviation from target\n(fraction of MT, mean +/- 95ci)") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
mci_absactdev

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
msd_finyie

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
mci_finyie

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
msd_maxdif

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
mci_maxdif
