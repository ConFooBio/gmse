# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy update timing
# Simulations + Results + Figures


######## Initialization ########

#### Packages ####

# GMSE
install.packages("GMSE")

# ggplot
install.packages("ggplot2")
install.packages("dplyr")

#### Libraries ####

# GMSE
library("GMSE")

# ggplot
library("ggplot2")
library(grid)
library(dplyr)

#### Update GMSE with the new features ####

# Make sure this script is opened within the gmse_forkRQ1.Rproj project
# Update GMSE clicking Build > Clean and Rebuild


############ Simulations and Figures ############

######## IAPETUS Conference May 2019 - poster figure ########

## Set parameters

# Array of Action Threshold values to explore
at <- seq(0,0.2,0.05)

# Array of Budget Bonus values to explore
bb <- seq(0,0.1,0.1)

# Number of simulation time steps
ts <- 20

# Number of replicates
rep <- 50

# Initial budget
bud_ini <- 1000       # (here default value)

# Resource initial population
res_ini <- 1000       # (here default value)

# Manager's target for Resource population
man_tar <- 1000       # (here default value)

# Resource growth rate
lbd <- 0.27

# Number of stakeholders
stkh <- 3              # Manager and 2 users for sake of computing time


## Create empty structures to gather simulation results

# Array of column names (the measures of interest for the question)
columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "extinct", "act_dev", "fin_yield", "max_diff_yield", "inac_ts")

# Empty 3D array of correct size 
# Dimensions(lines = replicates, columns = measures, layer = parameter combination)
OYA_batch1_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)), dimnames = list(NULL,columns,NULL))


# Create an empty structure for basic stats on OYA_batch1_results

# Array of column names
stats_columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
stats_OYA_batch1_results <- matrix(data = NA, nrow = length(at)*length(bb), ncol = length(stats_columns), dimnames = list(NULL,stats_columns))


## Simulations loop

# Initialize an index of parameter combination
param_set <- 1

# Loop
# (Takes approx. 20min on my desk computer)

# For every AT values in 'at'
for (i in 1:length(at)) {
  
  # For every BB values in 'bb'
  for (j in 1:length(bb)) {
    
    # With 'rep' number of replicate per parameter combo
    for (k in 1:rep) {
      
      # Run GMSE for the parameter combo
      sim <- gmse(stakeholders = stkh, time_max = ts, land_ownership = TRUE,
                  manage_target = man_tar, manager_budget = bud_ini,
                  user_budget = bud_ini, 
                  RESOURCE_ini = res_ini, lambda = lbd, 
                  action_thres = at[i], budget_bonus = bb[j],
                  plotting = F)
      
      # Store the last time step number (for extinction-related bugs)
      final_ts <- length(which(sim$paras[,1] != 0))
      
      # Pick up values for simulation results and store them in OYA_batch1_results
      
      # Replicate number
      OYA_batch1_results[k,1,param_set] <- k
      
      # AT value
      OYA_batch1_results[k,2,param_set] <- at[i]
      
      # BB value
      OYA_batch1_results[k,3,param_set] <- bb[j]
      
      # Initial budget (same for both for now)
      OYA_batch1_results[k,4,param_set] <- bud_ini
      
      # Resource initial pop
      OYA_batch1_results[k,5,param_set] <- res_ini
      
      # Resource growth rate
      OYA_batch1_results[k,6, param_set] <- lbd
      
      # Has extinction occured? (yes = 1, no = 0)
      OYA_batch1_results[k,7,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      # Next measures involve calculus that can be disturbed if extinction occured
      
      # If exctinction occured
      if (OYA_batch1_results[k,7,param_set] != 0) {
        
        # Resource actual pop deviation from target
        OYA_batch1_results[k,8,param_set] <- abs(dim(sim$resource[[final_ts-1]])[1]/man_tar - 1)
        
        # Users total final yield
        OYA_batch1_results[k,9,param_set] <- sum(sim$agents[[final_ts-1]][,16])
        
        # Maximum difference between Users yield
        OYA_batch1_results[k,10,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),2)
        
        # Number of timesteps during which Manager chose not to update policy
        OYA_batch1_results[k,11,param_set] <- round(final_ts-sum(sim$paras[,107]),1)
      }
      # If extinction did not occured
      else {
        
        # Resource actual pop deviation from target
        OYA_batch1_results[k,8,param_set] <- abs(dim(sim$resource[[final_ts]])[1]/man_tar - 1)
        
        # Users total final yield
        OYA_batch1_results[k,9,param_set] <- sum(sim$agents[[final_ts]][,16])
        
        # Maximum difference between Users yield
        OYA_batch1_results[k,10,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
        
        # Number of timesteps during which Manager chose not to update policy
        OYA_batch1_results[k,11,param_set] <- round(length(sim$paras[,107])-sum(sim$paras[,107]),1)
      }
    }
    
    # Increment parameter combo index
    param_set <- param_set + 1
  }
}


## Basic stats

# for each parameter combo
for (i in 1:dim(OYA_batch1_results)[3]) {
  
  # Store number of replicates for this combo
  stats_OYA_batch1_results[i,1] <- dim(OYA_batch1_results)[1]
  
  # Next 5 columns just take values from batch_results
  for (j in 2:6) {
    stats_OYA_batch1_results[i,j] <- OYA_batch1_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  stats_OYA_batch1_results[i,7] <- round(sum(OYA_batch1_results[,7,i])/dim(OYA_batch1_results)[1],2)
  
  # Next are systematically mean, sd and 95CI of the meaures from batch_results
  zz <- 0
  for (k in 8:dim(OYA_batch1_results)[2]) {
    stats_OYA_batch1_results[i,k+zz] <- mean(OYA_batch1_results[,k,i])
    stats_OYA_batch1_results[i,k+zz+1] <- sd(OYA_batch1_results[,k,i])
    stats_OYA_batch1_results[i,k+zz+2] <- 1.86*stats_OYA_batch1_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}

# Visualise the table to check for inconsistencies
View(stats_OYA_batch1_results)

# Save the table in a csv file
write.csv(stats_OYA_batch1_results, file = "stats_batch1.csv", row.names = F)


## Plot actual Resource population deviation from target and Users final yield according to AT and BB values

# Relevant subset of results
# exclude the combo at = 0 & bb = 0.1 which makes no sense and might be confusing
fig_tab <- stats_OYA_batch1_results[-2,]

# Figure without labels and big text for inclusion in the poster

# Plot actual Resource population deviation from target according to AT and BB values (both in %)
p1 <- ggplot(as.data.frame(fig_tab), aes(x=as.factor(at), y=act_dev*100, group=as.factor(bb), fill = as.factor(bb))) +       # Define data set, colour according to BB values
      geom_errorbar(aes(ymin=act_dev*100-act_dev_sd*100/2, ymax=act_dev*100+act_dev_sd*100/2, group = as.factor(bb)),          # Define error bars values, grouped by BB values 
                    position=position_dodge(0.6),                                                                              # Avoid superposition of the bars
                    colour = "grey40", width=0.4) +
      geom_point(size = 6, alpha = 1, colour="black", stroke = 1, shape = 21,                                                  # Define representation of data
                 position = position_dodge(width = 0.6)) +                                                                     # Avoid superposition of the points
      theme_gray(base_size = 50) +                                                                                             # Define theme and feature sizes
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),                     # x axis features, here no tittle, no text, suppress tick for better superposition
            axis.title.y = element_blank(),                                                                                    # y axis features, here no text
            legend.position='none',                                                                                            # legend features, here none, done in poster ppt
            axis.line.y = element_line(size = 1, colour = "grey50"))                                                           # big lines for style

# Plot actual Users total final yield (in kilo-budget units) according to AT and BB values (in %)
p2 <- ggplot(as.data.frame(fig_tab), aes(x=as.factor(at), y=fin_yield/100, group=as.factor(bb), fill = as.factor(bb))) +
      geom_errorbar(aes(ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2, group = as.factor(bb)),  
                    position=position_dodge(0.6),
                    colour = "grey40", width=0.4) +
      geom_point(size = 6, alpha = 1, colour="black", stroke = 1, shape = 21,
                 position = position_dodge(width = 0.6)) +
      theme_gray(base_size = 50) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.position='none',
            axis.line = element_line(size = 1, colour = "grey50"))

# New plot window
grid.newpage()

# Stack graphs
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

# Save the figure as pdf with explicit name



######## 1 year assessment (due 1st of July) ########

## interests
# same figure as poster but spanning a larger scale of at and bb values
# w/wo scaring + plotting equity measure
# 100 replicates

#### fixed nb of stakeholders, varying at and bb ####

## Set parameters

# Array of Action Threshold values to explore
at <- seq(0,1,0.1)

# Array of Budget Bonus values to explore
bb <- seq(0,1,0.1)

# Scaring allowed?
scar <- c(F,T)

# Number of simulation time steps
ts <- 20

# Number of replicates
rep <- 100

# Other parameters to GMSE default


## Create empty structures to gather simulation results

# Array of column names (the measures of interest for the question)
columns <- c("rep", "scar", "at", "bb", "extinct", "act_dev", "fin_yield", "max_diff_yield", "inac_ts")

# Empty 3D array of correct size 
# Dimensions(lines = replicates, columns = measures, layer = parameter combination)
OYA_batch1_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)*length(scar)-20), dimnames = list(NULL,columns,NULL))                 # -20 not to waste computing time for different bb values for at = 0


# Create an empty structure for basic stats on OYA_batch1_results

# Array of column names
stats_columns <- c("rep", "scar", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
stats_OYA_batch1_results <- matrix(data = NA, nrow = dim(OYA_batch1_results)[3], ncol = length(stats_columns), dimnames = list(NULL,stats_columns))


## Simulations loop

# Initialize an index of parameter combination
param_set <- 1

# Loop
# (Takes approx. 20min on my desk computer)

start <- Sys.time()

# without and with scaring
for (s in scar) {

  # For every AT values in 'at'
  for (i in 1:length(at)) {
    
    # avoid simul for different bb values for at = 0
    if (at[i] == 0) {
      
      # With 'rep' number of replicate per parameter combo
      for (k in 1:rep) {
        
        # Run GMSE for the parameter combo
        sim <- gmse(stakeholders = 10, time_max = ts, land_ownership = TRUE,
                    scaring = s,
                    action_thres = at[i], budget_bonus = 0,
                    plotting = F)
        
        # Store the last time step number (for extinction-related bugs)
        final_ts <- length(which(sim$paras[,1] != 0))
        
        # Pick up values for simulation results and store them in OYA_batch1_results
        
        # Replicate number
        OYA_batch1_results[k,1,param_set] <- k
        
        # Scarring allowed
        OYA_batch1_results[k,2,param_set] <- s
        
        # AT value
        OYA_batch1_results[k,3,param_set] <- at[i]
        
        # BB value
        OYA_batch1_results[k,4,param_set] <- bb[j]
        
        # Has extinction occured? (yes = 1, no = 0)
        OYA_batch1_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
        
        # Next measures involve calculus that can be disturbed if extinction occured
        
        # If exctinction occured
        if (OYA_batch1_results[k,5,param_set] != 0) {
          
          # Resource actual pop deviation from target
          OYA_batch1_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
          
          # Users total final yield
          OYA_batch1_results[k,7,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OYA_batch1_results[k,8,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),2)
          
          # Number of timesteps during which Manager chose not to update policy
          OYA_batch1_results[k,9,param_set] <- final_ts-sum(sim$paras[,107])
        }
        
        # If extinction did not occured
        else {
          
          # Resource actual pop deviation from target
          OYA_batch1_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
          
          # Users total final yield
          OYA_batch1_results[k,7,param_set] <- sum(sim$agents[[final_ts]][,16])
          
          # Maximum difference between Users yield
          OYA_batch1_results[k,8,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),2)
          
          # Number of timesteps during which Manager chose not to update policy
          OYA_batch1_results[k,9,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
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
          sim <- gmse(stakeholders = 10, time_max = ts, land_ownership = TRUE,
                      scaring = s,
                      action_thres = at[i], budget_bonus = bb[j],
                      plotting = F)
          
          # Store the last time step number (for extinction-related bugs)
          final_ts <- length(which(sim$paras[,1] != 0))
          
          # Pick up values for simulation results and store them in OYA_batch1_results
          
          # Replicate number
          OYA_batch1_results[k,1,param_set] <- k
          
          # Scarring allowed
          OYA_batch1_results[k,2,param_set] <- s
          
          # AT value
          OYA_batch1_results[k,3,param_set] <- at[i]
          
          # BB value
          OYA_batch1_results[k,4,param_set] <- bb[j]
          
          # Has extinction occured? (yes = 1, no = 0)
          OYA_batch1_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
          
          # Next measures involve calculus that can be disturbed if extinction occured
          
          # If exctinction occured
          if (OYA_batch1_results[k,5,param_set] != 0) {
            
            # Resource actual pop deviation from target
            OYA_batch1_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
            
            # Users total final yield
            OYA_batch1_results[k,7,param_set] <- sum(sim$agents[[final_ts-1]][,16])
            
            # Maximum difference between Users yield
            OYA_batch1_results[k,8,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),1)
            
            # Number of timesteps during which Manager chose not to update policy
            OYA_batch1_results[k,9,param_set] <- final_ts-sum(sim$paras[,107])
          }
          
          # If extinction did not occured
          else {
            
            # Resource actual pop deviation from target
            OYA_batch1_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
            
            # Users total final yield
            OYA_batch1_results[k,7,param_set] <- sum(sim$agents[[final_ts]][,16])
            
            # Maximum difference between Users yield
            OYA_batch1_results[k,8,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),1)
          
            # Number of timesteps during which Manager chose not to update policy
            OYA_batch1_results[k,9,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
          }
        } # end rep for loop
        
        # keep track of the simulation
        if (param_set % 20) {
          print(paste("parameter set number", param_set, "out of", dim(OYA_batch1_results)[3], sep = " "))           
        }
        
        # Increment parameter combo index
        param_set <- param_set + 1
      } # end bb for loop
    } # end at else loop
  } # end at for loop
} # end scar for loop

# Simulation time
end <- Sys.time()

print("sim started", start, "and ended", end, sep =" ")

## Basic stats

# for each parameter combo
for (i in 1:dim(OYA_batch1_results)[3]) {
  
  # Store number of replicates for this combo
  stats_OYA_batch1_results[i,1] <- dim(OYA_batch1_results)[1]
  
  # Next 3 columns just take values from batch_results
  for (j in 2:4) {
    stats_OYA_batch1_results[i,j] <- OYA_batch1_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  stats_OYA_batch1_results[i,5] <- round(sum(OYA_batch1_results[,5,i])/dim(OYA_batch1_results)[1],2)
  
  # Next are systematically mean, sd and 95CI of the meaures from batch_results
  zz <- 0
  for (k in 6:dim(OYA_batch1_results)[2]) {
    stats_OYA_batch1_results[i,k+zz] <- mean(OYA_batch1_results[,k,i])
    stats_OYA_batch1_results[i,k+zz+1] <- sd(OYA_batch1_results[,k,i])
    stats_OYA_batch1_results[i,k+zz+2] <- 1.86*stats_OYA_batch1_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}

# Visualise the table to check for inconsistencies
View(stats_OYA_batch1_results)

# Save the table in a csv file
write.csv(stats_OYA_batch1_results, file = "stats_batch1.csv", row.names = F)
# write.csv(stats_OYA_batch1_results, file = "stats_batch1_extprob.csv", row.names = F)


#### Plot actual Resource population deviation from target and Users final yield according to AT and BB values w and wo scaring ####

# Line for subset of results if needed
OYA_fig1_tab <- subset(tab_OYA_batch1_results, scar == 0)

# Actual Resource population deviation from target
gg1 <- ggplot(as.data.frame(tab_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=act_dev)) +
       geom_boxplot(position=position_dodge(1)) +
       facet_wrap(~at) +
       geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
       geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
       geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
    labs(x="Budget Bonus value", y= "Resource density deviation from target") +
    scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
       theme_gray() +
       theme(strip.background=element_rect(fill="grey"),
             strip.text=element_text(color="white", face="bold"),
             axis.title=element_text(size=18),
             legend.text=element_text(size=15),
             legend.title = element_text(size = 18))
gg1

# Absolute actual Resource population deviation from target
gg2 <- ggplot(as.data.frame(tab_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=abs(act_dev))) +
       geom_boxplot(position=position_dodge(1)) +
       facet_wrap(~at) +
       geom_hline(yintercept = 1, linetype = "dashed", color = "red") +      # show carrying capacity
  labs(x="Budget Bonus value", y= "Resource density absolute deviation from target") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg2

colnames(tab_OYA_batch1_results) <- columns

# Users final yield
gg3 <- ggplot(as.data.frame(tab_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=fin_yield/100)) +
       geom_boxplot(position=position_dodge(1)) +
       facet_wrap(~at) +
       geom_hline(yintercept = 95, linetype = "dashed", color = "red") +  
  labs(x="Budget Bonus value", y= "Users final total yield") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg3

## mean +- sd

# act_dev
gg4 <- ggplot(as.data.frame(stats_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=act_dev)) +
       facet_wrap(~at) +
       geom_errorbar(aes(ymin=act_dev-act_dev_sd/2, ymax=act_dev+act_dev_sd/2, group = as.factor(scar)),  
                     position=position_dodge(1),
                     colour = "grey40", width=0.5) +
       geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
                  position = position_dodge(width = 1)) +
       geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
       geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
       geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +  
  labs(x="Budget Bonus value", y= "Resource density deviation from target\n(mean +/- SD)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg4

# abs_act_dev

# stats
# Array of column names
new_stats_columns <- c("rep", "scar", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
new_stats_OYA_batch1_results <- matrix(data = NA, nrow = dim(OYA_batch1_results)[3], ncol = length(new_stats_columns), dimnames = list(NULL,new_stats_columns))

# for each parameter combo
for (i in 1:dim(new_OYA_batch1_results)[3]) {
  
  # Store number of replicates for this combo
  new_stats_OYA_batch1_results[i,1] <- dim(new_OYA_batch1_results)[1]
  
  # Next 3 columns just take values from batch_results
  for (j in 2:4) {
    new_stats_OYA_batch1_results[i,j] <- new_OYA_batch1_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  new_stats_OYA_batch1_results[i,5] <- round(sum(new_OYA_batch1_results[,5,i])/dim(new_OYA_batch1_results)[1],2)
  
  # Next are systematically mean, sd and 95CI of the meaures from batch_results
  zz <- 0
  for (k in 6:dim(new_OYA_batch1_results)[2]) {
    new_stats_OYA_batch1_results[i,k+zz] <- mean(new_OYA_batch1_results[,k,i])
    new_stats_OYA_batch1_results[i,k+zz+1] <- sd(new_OYA_batch1_results[,k,i])
    new_stats_OYA_batch1_results[i,k+zz+2] <- 1.86*new_stats_OYA_batch1_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}

View(new_stats_OYA_batch1_results)

 gg5 <- ggplot(as.data.frame(new_stats_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=abs_act_dev)) +
   facet_wrap(~at) +
   geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_sd/2, ymax=abs_act_dev+abs_act_dev_sd/2, group = as.factor(scar)),  
                 position=position_dodge(1),
                 colour = "grey40", width=0.5) +
   geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
              position = position_dodge(width = 1)) +
   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +         # show carrying capacity
   labs(x="Budget Bonus value", y= "Resource density absolute deviation from target\n(mean +/- SD)") +
   scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
   theme_gray() +
   theme(strip.background=element_rect(fill="grey"),
         strip.text=element_text(color="white", face="bold"),
         axis.title=element_text(size=18),
         legend.text=element_text(size=15),
         legend.title = element_text(size = 18))
gg5

gg6 <- ggplot(as.data.frame(stats_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=fin_yield/100)) +
      facet_wrap(~at) +
      geom_errorbar(aes(ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2, group = as.factor(scar)),  
                    position=position_dodge(1),
                    colour = "grey40", width=0.5) +
      geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
                 position = position_dodge(width = 1)) +
      geom_hline(yintercept = 95, linetype = "dashed", color = "red") + 
  labs(x="Budget Bonus value", y= "Users final total yield\n(mean +/- SD)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg6

# mean +- ic 95

# act_dev
gg7 <- ggplot(as.data.frame(stats_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=act_dev)) +
      facet_wrap(~at) +
      geom_errorbar(aes(ymin=act_dev-act_dev_95ci, ymax=act_dev+act_dev_95ci, group = as.factor(scar)),  
                    position=position_dodge(1),
                    colour = "grey40", width=0.5) +
      geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
                 position = position_dodge(width = 1)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +       
  labs(x="Budget Bonus value", y= "Resource density deviation from target\n(mean +/- 95%CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg7

# abs_act_dev
gg8 <- ggplot(as.data.frame(new_stats_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=abs_act_dev)) +
      facet_wrap(~at) +
      geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_95ci, ymax=abs_act_dev+abs_act_dev_95ci, group = as.factor(scar)),  
                    position=position_dodge(1),
                    colour = "grey40", width=0.5) +
      geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
                 position = position_dodge(width = 1)) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +         # show carrying capacity
  labs(x="Budget Bonus value", y= "Resource density absolute deviation from target\n(mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg8

gg9 <- ggplot(as.data.frame(stats_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=fin_yield/100)) +
      facet_wrap(~at) +
      geom_errorbar(aes(ymin=fin_yield/100-fin_yield_95ci/100, ymax=fin_yield/100+fin_yield_95ci/100, group = as.factor(scar)),  
                    position=position_dodge(1),
                    colour = "grey40", width=0.5) +
      geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
                 position = position_dodge(width = 1)) +
      geom_hline(yintercept = 95, linetype = "dashed", color = "red") +   
  labs(x="Budget Bonus value", y= "Users final total yield\n(mean +/- 95%CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg9

# extinction probability according to varying at and bb

gg10 <- ggplot(as.data.frame(stats_OYA_batch1_results), aes(x=as.factor(bb), fill = as.factor(scar), y=ext_prob)) +
       facet_wrap(~at) +
       geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
                  position = position_dodge(width = 1)) +
       geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +     
  labs(x="Budget Bonus value", y= "Extinction probability") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg10

# Figure without labels and big text for inclusion in the poster

# Save the figure as pdf with clear name

######## new batch : with scaring, varying less widely at, bb, and investigating the sensibility to initial budget ########

## Set parameters

# Array of Action Threshold values to explore
at <- seq(0,1.2,0.4)

# Array of Budget Bonus values to explore
bb <- seq(0,1.5,0.5)

# Array of initial budget
budget <- c(100, 400, 800, 1200)

# # Scaring allowed?
# scar <- T

# Number of simulation time steps
ts <- 20

# Number of replicates
rep <- 100

# number of stakeholder
stkh <- 10

# Other parameters to GMSE default


## Create empty structures to gather simulation results

# Array of column names (the measures of interest for the question)
columns <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts")

# Empty 3D array of correct size 
# Dimensions(lines = replicates, columns = measures, layer = parameter combination)
OYA_batch2_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)*length(budget)-12), dimnames = list(NULL,columns,NULL))                 # -8 not to waste computing time for different bb values for at = 0


# Create an empty structure for basic stats on OYA_batch2_results

# Array of column names
stats_columns <- c("rep", "budget", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
stats_OYA_batch2_results <- matrix(data = NA, nrow = dim(OYA_batch2_results)[3], ncol = length(stats_columns), dimnames = list(NULL,stats_columns))


## Simulations loop

# Initialize an index of parameter combination
param_set <- 1

# Store start time
start <- Sys.time()

# For every budget values
for (b in 1:length(budget)) {
  
  # For every AT values in 'at'
  for (i in 1:length(at)) {
    
    # avoid simul for different bb values for at = 0
    if (at[i] == 0) {
      
      # With 'rep' number of replicate per parameter combo
      for (k in 1:rep) {
        
        # Run GMSE for the parameter combo
        sim <- gmse(stakeholders = stkh, time_max = ts, land_ownership = TRUE,
                    scaring = T,
                    manager_budget = budget[b],
                    action_thres = at[i], budget_bonus = 0,
                    plotting = F)
        
        # Store the last time step number (for extinction-related bugs)
        final_ts <- length(which(sim$paras[,1] != 0))
        
        # Pick up values for simulation results and store them in OYA_batch2_results
        
        # Replicate number
        OYA_batch2_results[k,1,param_set] <- k
        
        # Manager initial budget
        OYA_batch2_results[k,2,param_set] <- budget[b]
        
        # AT value
        OYA_batch2_results[k,3,param_set] <- at[i]
        
        # BB value
        OYA_batch2_results[k,4,param_set] <- 0
        
        # Has extinction occured? (yes = 1, no = 0)
        OYA_batch2_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
        
        # Next measures involve calculus that can be disturbed if extinction occured
        
        # If exctinction occured
        if (OYA_batch2_results[k,5,param_set] != 0) {
          
          # Resource actual pop deviation from target
          OYA_batch2_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
          
          # absolute value
          OYA_batch2_results[k,7,param_set] <- abs(OYA_batch2_results[k,6,param_set])
          
          # Users total final yield
          OYA_batch2_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OYA_batch2_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OYA_batch2_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
        }
        
        # If extinction did not occured
        else {
          
          # Resource actual pop deviation from target
          OYA_batch2_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
          
          # absolute value
          OYA_batch2_results[k,7,param_set] <- abs(OYA_batch2_results[k,6,param_set])
          
          # Users total final yield
          OYA_batch2_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OYA_batch2_results[k,9,param_set] <- (max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OYA_batch2_results[k,10,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
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
          sim <- gmse(stakeholders = 10, time_max = ts, land_ownership = TRUE,
                      scaring = T,
                      manager_budget = budget[b],
                      action_thres = at[i], budget_bonus = bb[j],
                      plotting = F)
          
          # Store the last time step number (for extinction-related bugs)
          final_ts <- length(which(sim$paras[,1] != 0))
          
          # Pick up values for simulation results and store them in OYA_batch2_results
          
          # Replicate number
          OYA_batch2_results[k,1,param_set] <- k
          
          # Manager initial budget
          OYA_batch2_results[k,2,param_set] <- budget[b]
          
          # AT value
          OYA_batch2_results[k,3,param_set] <- at[i]
          
          # BB value
          OYA_batch2_results[k,4,param_set] <- bb[j]
          
          # Has extinction occured? (yes = 1, no = 0)
          OYA_batch2_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
          
          # Next measures involve calculus that can be disturbed if extinction occured
          
          # If exctinction occured
          if (OYA_batch2_results[k,5,param_set] != 0) {
            
            # Resource actual pop deviation from target
            OYA_batch2_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OYA_batch2_results[k,7,param_set] <- abs(OYA_batch2_results[k,6,param_set])
            
            # Users total final yield
            OYA_batch2_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
            
            # Maximum difference between Users yield
            OYA_batch2_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OYA_batch2_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
          }
          
          # If extinction did not occured
          else {
            
            # Resource actual pop deviation from target
            OYA_batch2_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OYA_batch2_results[k,7,param_set] <- abs(OYA_batch2_results[k,6,param_set])
            
            # Users total final yield
            OYA_batch2_results[k,8,param_set] <- sum(sim$agents[[final_ts]][,16])
            
            # Maximum difference between Users yield
            OYA_batch2_results[k,9,param_set] <- (max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OYA_batch2_results[k,10,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
          }
        } # end rep for loop
        
        # keep track of the simulations
        if (param_set %% 10 == 0) {
          print(paste("parameter set number", param_set, "out of", dim(OYA_batch2_results)[3], "at", Sys.time(), sep = " "))
        }
        
        # Increment parameter combo index
        param_set <- param_set + 1
      } # end bb for loop
    } # end at else loop
  } # end at for loop
} # end budget for loop

# end of sim
end <- Sys.time()

print(paste("Batch started", start, "and ended", end, sep = " "))

## save the 3D array of results?

# rbind the layers

tab_OYA_batch2_results <- OYA_batch2_results[,,1]

for (i in 2:dim(OYA_batch2_results)[3]) {
  tab_OYA_batch2_results <- rbind(tab_OYA_batch2_results, OYA_batch2_results[,,i])
}

write.csv(tab_OYA_batch2_results, file = "tab_OYA_batch2_results.csv")


## Basic stats

# for each parameter combo
for (i in 1:dim(OYA_batch2_results)[3]) {
  
  # Store number of replicates for this combo
  stats_OYA_batch2_results[i,1] <- dim(OYA_batch2_results)[1]
  
  # Next 3 columns just take values from batch_results
  for (j in 2:4) {
    stats_OYA_batch2_results[i,j] <- OYA_batch2_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  stats_OYA_batch2_results[i,5] <- round(sum(OYA_batch2_results[,5,i])/dim(OYA_batch2_results)[1],2)
  
  # Next are systematically mean, sd and 95CI of the meaures from batch_results
  zz <- 0
  for (k in 6:dim(OYA_batch2_results)[2]) {
    stats_OYA_batch2_results[i,k+zz] <- mean(OYA_batch2_results[,k,i])
    stats_OYA_batch2_results[i,k+zz+1] <- sd(OYA_batch2_results[,k,i])
    stats_OYA_batch2_results[i,k+zz+2] <- 1.86*stats_OYA_batch2_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}

# Visualise the table to check for inconsistencies
View(stats_OYA_batch2_results)

# Save the table in a csv file
write.csv(stats_OYA_batch2_results, file = "stats_batch2.csv", row.names = F)

######## new batch : without scaring, varying less widely at, bb, and investigating the sensibility to initial budget ########

## Set parameters

# Array of Action Threshold values to explore
at <- seq(0,1.2,0.4)

# Array of Budget Bonus values to explore
bb <- seq(0,1.5,0.5)

# Array of initial budget
budget <- c(100, 400, 800, 1200)

# # Scaring allowed?
# scar <- T

# Number of simulation time steps
ts <- 20

# Number of replicates
rep <- 100

# number of stakeholder
stkh <- 10

# Other parameters to GMSE default


## Create empty structures to gather simulation results

# Array of column names (the measures of interest for the question)
columns <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts")

# Empty 3D array of correct size 
# Dimensions(lines = replicates, columns = measures, layer = parameter combination)
OYA_batch3_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)*length(budget)-12), dimnames = list(NULL,columns,NULL))                 # -8 not to waste computing time for different bb values for at = 0


# Create an empty structure for basic stats on OYA_batch3_results

# Array of column names
stats_columns <- c("rep", "budget", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
stats_OYA_batch3_results <- matrix(data = NA, nrow = dim(OYA_batch3_results)[3], ncol = length(stats_columns), dimnames = list(NULL,stats_columns))


## Simulations loop

# Initialize an index of parameter combination
param_set <- 1

# Store start time
start <- Sys.time()

# For every budget values
for (b in 1:length(budget)) {
  
  # For every AT values in 'at'
  for (i in 1:length(at)) {
    
    # avoid simul for different bb values for at = 0
    if (at[i] == 0) {
      
      # With 'rep' number of replicate per parameter combo
      for (k in 1:rep) {
        
        # Run GMSE for the parameter combo
        sim <- gmse(stakeholders = stkh, time_max = ts, land_ownership = TRUE,
                    scaring = F,
                    manager_budget = budget[b],
                    action_thres = at[i], budget_bonus = 0,
                    plotting = F)
        
        # Store the last time step number (for extinction-related bugs)
        final_ts <- length(which(sim$paras[,1] != 0))
        
        # Pick up values for simulation results and store them in OYA_batch3_results
        
        # Replicate number
        OYA_batch3_results[k,1,param_set] <- k
        
        # Manager initial budget
        OYA_batch3_results[k,2,param_set] <- budget[b]
        
        # AT value
        OYA_batch3_results[k,3,param_set] <- at[i]
        
        # BB value
        OYA_batch3_results[k,4,param_set] <- 0
        
        # Has extinction occured? (yes = 1, no = 0)
        OYA_batch3_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
        
        # Next measures involve calculus that can be disturbed if extinction occured
        
        # If exctinction occured
        if (OYA_batch3_results[k,5,param_set] != 0) {
          
          # Resource actual pop deviation from target
          OYA_batch3_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
          
          # absolute value
          OYA_batch3_results[k,7,param_set] <- abs(OYA_batch3_results[k,6,param_set])
          
          # Users total final yield
          OYA_batch3_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OYA_batch3_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OYA_batch3_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
        }
        
        # If extinction did not occured
        else {
          
          # Resource actual pop deviation from target
          OYA_batch3_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
          
          # absolute value
          OYA_batch3_results[k,7,param_set] <- abs(OYA_batch3_results[k,6,param_set])
          
          # Users total final yield
          OYA_batch3_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
          
          # Maximum difference between Users yield
          OYA_batch3_results[k,9,param_set] <- (max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16])
          
          # Number of timesteps during which Manager chose not to update policy
          OYA_batch3_results[k,10,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
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
          sim <- gmse(stakeholders = 10, time_max = ts, land_ownership = TRUE,
                      scaring = F,
                      manager_budget = budget[b],
                      action_thres = at[i], budget_bonus = bb[j],
                      plotting = F)
          
          # Store the last time step number (for extinction-related bugs)
          final_ts <- length(which(sim$paras[,1] != 0))
          
          # Pick up values for simulation results and store them in OYA_batch3_results
          
          # Replicate number
          OYA_batch3_results[k,1,param_set] <- k
          
          # Manager initial budget
          OYA_batch3_results[k,2,param_set] <- budget[b]
          
          # AT value
          OYA_batch3_results[k,3,param_set] <- at[i]
          
          # BB value
          OYA_batch3_results[k,4,param_set] <- bb[j]
          
          # Has extinction occured? (yes = 1, no = 0)
          OYA_batch3_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
          
          # Next measures involve calculus that can be disturbed if extinction occured
          
          # If exctinction occured
          if (OYA_batch3_results[k,5,param_set] != 0) {
            
            # Resource actual pop deviation from target
            OYA_batch3_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OYA_batch3_results[k,7,param_set] <- abs(OYA_batch3_results[k,6,param_set])
            
            # Users total final yield
            OYA_batch3_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
            
            # Maximum difference between Users yield
            OYA_batch3_results[k,9,param_set] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OYA_batch3_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
          }
          
          # If extinction did not occured
          else {
            
            # Resource actual pop deviation from target
            OYA_batch3_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
            
            # absolute value
            OYA_batch3_results[k,7,param_set] <- abs(OYA_batch3_results[k,6,param_set])
            
            # Users total final yield
            OYA_batch3_results[k,8,param_set] <- sum(sim$agents[[final_ts]][,16])
            
            # Maximum difference between Users yield
            OYA_batch3_results[k,9,param_set] <- (max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16])
            
            # Number of timesteps during which Manager chose not to update policy
            OYA_batch3_results[k,10,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
          }
        } # end rep for loop
        
        # keep track of the simulations
        if (param_set %% 10 == 0) {
          print(paste("parameter set number", param_set, "out of", dim(OYA_batch3_results)[3], "at", Sys.time(), sep = " "))
        }
        
        # Increment parameter combo index
        param_set <- param_set + 1
      } # end bb for loop
    } # end at else loop
  } # end at for loop
} # end budget for loop

# end of sim
end <- Sys.time()

print(paste("Batch started", start, "and ended", end, sep = " "))

## save the 3D array of results?

# rbind the layers

tab_OYA_batch3_results <- OYA_batch3_results[,,1]

for (i in 2:dim(OYA_batch3_results)[3]) {
  tab_OYA_batch3_results <- rbind(tab_OYA_batch3_results, OYA_batch3_results[,,i])
}

write.csv(tab_OYA_batch3_results, file = "tab_OYA_batch3_results.csv")


## Basic stats

# for each parameter combo
for (i in 1:dim(OYA_batch3_results)[3]) {
  
  # Store number of replicates for this combo
  stats_OYA_batch3_results[i,1] <- dim(OYA_batch3_results)[1]
  
  # Next 3 columns just take values from batch_results
  for (j in 2:4) {
    stats_OYA_batch3_results[i,j] <- OYA_batch3_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  stats_OYA_batch3_results[i,5] <- round(sum(OYA_batch3_results[,5,i])/dim(OYA_batch3_results)[1],2)
  
  # Next are systematically mean, sd and 95CI of the meaures from batch_results
  zz <- 0
  for (k in 6:dim(OYA_batch3_results)[2]) {
    stats_OYA_batch3_results[i,k+zz] <- mean(OYA_batch3_results[,k,i])
    stats_OYA_batch3_results[i,k+zz+1] <- sd(OYA_batch3_results[,k,i])
    stats_OYA_batch3_results[i,k+zz+2] <- 1.86*stats_OYA_batch3_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}

# Visualise the table to check for inconsistencies
View(stats_OYA_batch3_results)

# Save the table in a csv file
write.csv(stats_OYA_batch3_results, file = "stats_batch3.csv", row.names = F)

#### Merge batch1 and 2, and plot results ####

## Import results files
tab_OYA_batch2_results <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/tab_OYA_batch2_results.csv")
tab_OYA_batch3_results <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/tab_OYA_batch3_results.csv")

## Create a column for Scaring parameter
tab_OYA_batch2_results$scar <- rep(1, dim(tab_OYA_batch2_results)[1])
tab_OYA_batch3_results$scar <- rep(0, dim(tab_OYA_batch3_results)[1])

## Merge the tables
tab_OYA_batch2 <- rbind(tab_OYA_batch3_results, tab_OYA_batch2_results)

## Import stats results files
stats_OYA_batch2 <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/stats_batch2.csv")
stats_OYA_batch3 <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/data/stats_batch3.csv")

## Create a column for Scaring parameter
stats_OYA_batch2$scar <- rep(1, dim(stats_OYA_batch2)[1])
stats_OYA_batch3$scar <- rep(0, dim(stats_OYA_batch3)[1])

## Merge the tables
stats_batch2 <- rbind(stats_OYA_batch2, stats_OYA_batch3)

## Plots
gg1 <- ggplot(subset(tab_OYA_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=act_dev)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at+bb, ncol = 4) +             # find a way to split by the at:bb combinaison of this data set
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Manager initial budget", y= "Resource population deviation from target") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg1

gg1control <- ggplot(subset(tab_OYA_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=act_dev)) +
  geom_boxplot(position=position_dodge()) +
  #facet_wrap(~at+bb, ncol = 4) +             # find a way to split by the at:bb combinaison of this data set
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Manager initial budget", y= "Resource population deviation from target") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg1control

# Absolute actual Resource population deviation from target
gg2 <- ggplot(subset(tab_OYA_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=abs_act_dev)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at+bb, ncol = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +      # show carrying capacity
  labs(x="Manager initial budget", y= "Resource population absolute deviation from target") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg2

gg2control <- ggplot(subset(tab_OYA_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=abs_act_dev)) +
  geom_boxplot(position=position_dodge()) +
  #facet_wrap(~at+bb, ncol = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +      # show carrying capacity
  labs(x="Manager initial budget", y= "Resource population absolute deviation from target") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg2control

# Users final yield
gg3 <- ggplot(subset(tab_OYA_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=fin_yield/100)) +
  geom_boxplot(position=position_dodge()) +
  facet_wrap(~at+bb, ncol=4) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +     
  labs(x="Manager initial budget", y= "Users final total yield") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg3

gg3control <- ggplot(subset(tab_OYA_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=fin_yield/100)) +
  geom_boxplot(position=position_dodge()) +
  #facet_wrap(~at+bb, ncol=4) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +    
  labs(x="Manager initial budget", y= "Users final total yield") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg3control

## mean +- sd

# act_dev
gg4 <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=act_dev)) +
  facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_sd/2, ymax=act_dev+act_dev_sd/2, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Manager initial budget", y= "Resource population deviation from target\n(mean +/- sd)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg4

gg4control <- ggplot(subset(stats_batch2, at== 0), aes(x=as.factor(budget), fill = as.factor(scar), y=act_dev)) +
  #facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_sd/2, ymax=act_dev+act_dev_sd/2, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") + 
  labs(x="Manager initial budget", y= "Resource population deviation from target\n(mean +/- sd)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg4control

# abs_act_dev
gg5 <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=abs_act_dev)) +
  facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_sd/2, ymax=abs_act_dev+abs_act_dev_sd/2, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +    
  labs(x="Manager initial budget", y= "Resource population absolute deviation from target\n(mean +/- sd)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg5

gg5control <- ggplot(subset(stats_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=abs_act_dev)) +
  #facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_sd/2, ymax=abs_act_dev+abs_act_dev_sd/2, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +    
  labs(x="Manager initial budget", y= "Resource population absolute deviation from target\n(mean +/- sd)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg5control

# fin_yield
gg6 <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=fin_yield/100)) +
  facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +  
  labs(x="Manager initial budget", y= "Users final total yield\n(mean +/- sd)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg6

gg6control <- ggplot(subset(stats_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=fin_yield/100)) +
  #facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=fin_yield/100-fin_yield_sd/100/2, ymax=fin_yield/100+fin_yield_sd/100/2, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +     
  labs(x="Manager initial budget", y= "Users final total yield\n(mean +/- sd)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg6control

# mean +- ic 95

# act_dev
gg7 <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=act_dev)) +
  facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_95ci, ymax=act_dev+act_dev_95ci, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +   
  labs(x="Manager initial budget", y= "Resource population deviation from target\n(in fraction, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg7

gg7control <- ggplot(subset(stats_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=act_dev)) +
  #facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=act_dev-act_dev_95ci, ymax=act_dev+act_dev_95ci, group = as.factor(scar)),  
                position=position_dodge(0.5),
                colour = "grey40", width=0.25) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +   
  labs(x="Manager initial budget", y= "Resource population deviation from target\n(in fraction, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg7control

# abs_act_dev
gg8 <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=abs_act_dev)) +
  facet_wrap(~at+bb, ncol=4) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_95ci, ymax=abs_act_dev+abs_act_dev_95ci, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  
  labs(x="Manager initial budget", y= "Resource population absolute deviation from target\n(in fraction, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18)) #+
  #ylim(0.2, 1.3)
gg8

gg8control <- ggplot(subset(stats_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=abs_act_dev)) +
  #facet_wrap(~at+bb, ncol=5) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_95ci, ymax=abs_act_dev+abs_act_dev_95ci, group = as.factor(scar)),  
                position=position_dodge(0.5),
                colour = "grey40", width=0.25) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  
  labs(x="Manager initial budget", y= "Resource population absolute deviation from target\n(in fraction, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg8control

# fin_yield

gg9 <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=fin_yield/100)) +
  facet_wrap(~at+bb, ncol = 4) +
  geom_errorbar(aes(ymin=fin_yield/100-fin_yield_95ci/100, ymax=fin_yield/100+fin_yield_95ci/100, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +      
  labs(x="Manager initial budget", y= "Users final total yield\n(k-money units, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg9

gg9control <- ggplot(subset(stats_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=fin_yield/100)) +
  #facet_wrap(~at+bb, ncol = 4) +
  geom_errorbar(aes(ymin=fin_yield/100-fin_yield_95ci/100, ymax=fin_yield/100+fin_yield_95ci/100, group = as.factor(scar)),  
                position=position_dodge(0.5),
                colour = "grey40", width=0.25) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 95, linetype = "dashed", color = "red") +   
  labs(x="Manager initial budget", y= "Users final total yield\n(k-money units, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg9control

# max diff between users yield
gg10 <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=max_diff_yield*100)) +
  facet_wrap(~at+bb, ncol = 4) +
  geom_errorbar(aes(ymin=max_diff_yield*100-max_diff_yield_95ci*100, ymax=max_diff_yield*100+max_diff_yield_95ci*100, group = as.factor(scar)),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  labs(x="Manager initial budget", y= "Maximum difference between Users yields"\n"(in % of the highest yield, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg10

gg10control <- ggplot(subset(stats_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=max_diff_yield*100)) +
  #facet_wrap(~at+bb, ncol = 4) +
  geom_errorbar(aes(ymin=max_diff_yield*100-max_diff_yield_95ci*100, ymax=max_diff_yield*100+max_diff_yield_95ci*100, group = as.factor(scar)),  
                position=position_dodge(0.5),
                width=0.25, colour = "grey40") +
  geom_point(size = 2, alpha = 1, stroke = 1, shape = 21, colour="black",
             position = position_dodge(width = 0.5)) 
  labs(x="Manager initial budget", y= "Maximum difference between Users yields\n(in % of the highest yield, mean +/- 95CI)") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg10control

 # extinction probability according to varying at and bb

gg_extprob <- ggplot(subset(stats_batch2, at != 0), aes(x=as.factor(budget), fill = as.factor(scar), y=ext_prob)) +
  facet_wrap(~at+bb, ncol = 4) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +      
  labs(x="Managers initial budget", y = "Extinction probability") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg_extprob

gg_extprob_control <- ggplot(subset(stats_batch2, at == 0), aes(x=as.factor(budget), fill = as.factor(scar), y=ext_prob)) +
  #facet_wrap(~at+bb, ncol = 4) +
  geom_point(size = 2, alpha = 1, colour="black", stroke = 1, shape = 21,
             position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +      
  labs(x="Managers initial budget", y = "Extinction probability") +
  scale_fill_discrete(name="Scaring Option", labels=c("Not allowed", "Allowed")) +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
gg_extprob_control




#### Simulations to isolate BB effect ####

## Set parameters

# Array of Action Threshold values to explore, here a value for which managers perform better than acting every time step
at = 0.1;

# Array of Budget Bonus values to explore
bb <- seq(0,2,0.2);

# Array of initial budget, here lower than the users
budget <- 500

# Scaring allowed? yes because favorable to a good management
scar <- T

# Number of simulation time steps
ts <- 20;

# Number of replicates
rep <- 20;

# number of stakeholders
stkh <- 10;

# Other parameters to GMSE default

## Create empty structures to gather simulation results

# Array of column names (the measures of interest for the question)
columns <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "overK_freq")

# Empty 3D array of correct size 
# Dimensions(lines = replicates, columns = measures, layer = parameter combination)
batchBB1_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)*length(budget)), dimnames = list(NULL,columns,NULL))                 

# Create an empty structure for basic stats on batchBB1_results

# Array of column names
stats_columns <- c("rep", "budget", "at", "bb", "ext_prob", "act_dev", "act_dev_sd", "act_dev_95ci", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci", "fin_yield", "fin_yield_sd", "fin_yield_95ci", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci", "inac_ts", "inac_ts_sd", "inac_ts_95ci", "overK_tot", "overK_sd", "overK_95ci")

# Empty 2D array of correct size
# Dimensions(lines = parameter combo index, columns = measures)
stats_batchBB1_results <- matrix(data = NA, nrow = dim(batchBB1_results)[3], ncol = length(stats_columns), dimnames = list(NULL,stats_columns))

## Simulations loop

# Initialize an index of parameter combination
param_set <- 1

# Store start time
start <- Sys.time()

# For every BB values in 'bb'
for (j in 1:length(bb)) {
  
  # With 'rep' number of replicate per parameter combo
  for (k in 1:rep) {
    
    # Run GMSE for the parameter combo
    sim <- gmse(stakeholders = stkh, time_max = ts, land_ownership = TRUE,
                RESOURCE_ini = 1500, res_death_K = 2000,
                scaring = scar, manager_budget = budget, action_thres = at, budget_bonus = bb[j],
                plotting = F)
    
    # Store the last time step number (for extinction-related bugs)
    final_ts <- length(which(sim$paras[,1] != 0))
    
    # Pick up values for simulation results and store them in batchBB1_results
    
    # Replicate number
    batchBB1_results[k,1,param_set] <- k
    
    # Manager initial budget
    batchBB1_results[k,2,param_set] <- budget
    
    # AT value
    batchBB1_results[k,3,param_set] <- at
    
    # BB value
    batchBB1_results[k,4,param_set] <- bb[j]
    
    # Has extinction occured? (yes = 1, no = 0)
    batchBB1_results[k,5,param_set] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
    
    # Next measures involve calculus that can be disturbed if extinction occured
    
    # If exctinction occured
    if (batchBB1_results[k,5,param_set] != 0) {
      
      # Resource actual pop deviation from target
      batchBB1_results[k,6,param_set] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
      
      # absolute value
      batchBB1_results[k,7,param_set] <- abs(batchBB1_results[k,6,param_set])
      
      # Users total final yield
      batchBB1_results[k,8,param_set] <- sum(sim$agents[[final_ts-1]][,16])
      
      # Maximum difference between Users yield
      batchBB1_results[k,9,param_set] <- round((max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16]),3)
      
      # Number of timesteps during which Manager chose not to update policy
      batchBB1_results[k,10,param_set] <- final_ts-sum(sim$paras[,107])
    }
    
    # If extinction did not occured
    else {
      
      # Resource actual pop deviation from target
      batchBB1_results[k,6,param_set] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
      
      # absolute value
      batchBB1_results[k,7,param_set] <- abs(batchBB1_results[k,6,param_set])
      
      # Users total final yield
      batchBB1_results[k,8,param_set] <- sum(sim$agents[[final_ts]][,16])
      
      # Maximum difference between Users yield
      batchBB1_results[k,9,param_set] <- round((max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16]),3)
      
      # Number of timesteps during which Manager chose not to update policy
      batchBB1_results[k,10,param_set] <- length(sim$paras[,107])-sum(sim$paras[,107])
    }
  
    # Number of K exceedings
    batchBB1_results[k,11,param_set] <- sum(sim$paras[,109])
  
  } # end rep for loop
  
  # keep track of the simulations
  if (param_set %% 5 == 0) {
    print(paste("parameter set number", param_set, "out of", dim(batchBB1_results)[3], "at", Sys.time(), sep = " "))
  }
  
  # Increment parameter combo index
  param_set <- param_set + 1
} # end bb for loop

# end of sim
end <- Sys.time()

print(paste("Batch started", start, "and ended", end, sep = " "))

## save the 3D array of results?

# rbind the layers

tab_batchBB1_results <- batchBB1_results[,,1]

for (i in 2:dim(batchBB1_results)[3]) {
  tab_batchBB1_results <- rbind(tab_batchBB1_results, batchBB1_results[,,i])
}

write.csv(tab_batchBB1_results, file = "tab_batchBB1_results.csv")


## Basic stats

# for each parameter combo
for (i in 1:dim(batchBB1_results)[3]) {
  
  # Store number of replicates for this combo
  stats_batchBB1_results[i,1] <- dim(batchBB1_results)[1]
  
  # Next 3 columns just take values from batch_results
  for (j in 2:4) {
    stats_batchBB1_results[i,j] <- batchBB1_results[1,j,i]
  }
  
  # Extinction probability (number of extinctions / number of replicates)
  stats_batchBB1_results[i,5] <- round(sum(batchBB1_results[,5,i])/dim(batchBB1_results)[1],2)
  
  # Next are systematically mean, sd and 95CI of the meaures from batch_results
  zz <- 0
  for (k in 6:dim(batchBB1_results)[2]) {
    stats_batchBB1_results[i,k+zz] <- mean(batchBB1_results[,k,i])
    stats_batchBB1_results[i,k+zz+1] <- sd(batchBB1_results[,k,i])
    stats_batchBB1_results[i,k+zz+2] <- 1.86*stats_batchBB1_results[i,k+zz+1]/sqrt(rep)
    zz <- zz + 2
  }
}

## Plots

# Resource population actual deviation from manager's target
bb1.bp <- ggplot(data = as.data.frame(tab_batchBB1_results), aes(x=as.factor(bb), y=act_dev)) +
  geom_boxplot(position=position_dodge()) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget bonus value (in fraction of initial budget)", y= "Resource population deviation from target") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
bb1.bp

bb1.msd <- ggplot(data = as.data.frame(stats_batchBB1_results), aes(x=as.factor(bb), y=act_dev)) +
  geom_errorbar(aes(ymin=act_dev-act_dev_sd, ymax=act_dev+act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", fill = "white", stroke = 1, shape = 21,
             position = position_dodge(1)) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget bonus value (in fraction of initial budget)", y= "Resource population deviation from target") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
bb1.msd

bb1.ci <- ggplot(data = as.data.frame(stats_batchBB1_results), aes(x=as.factor(bb), y=act_dev)) +
  geom_errorbar(aes(ymin=act_dev-act_dev_95ci, ymax=act_dev+act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", fill = "white", stroke = 1, shape = 21,
             position = position_dodge(1)) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget bonus value (in fraction of initial budget)", y= "Resource population deviation from target") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
bb1.ci

# Resource population absolute deviation from manager's target
bb2.bp <- ggplot(data = as.data.frame(tab_batchBB1_results), aes(x=as.factor(bb), y=abs_act_dev)) +
  geom_boxplot(position=position_dodge()) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget bonus value (in fraction of initial budget)", y= "Resource population deviation from target") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
bb2.bp

bb2.msd <- ggplot(data = as.data.frame(stats_batchBB1_results), aes(x=as.factor(bb), y=abs_act_dev)) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_sd, ymax=abs_act_dev+abs_act_dev_sd),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", fill = "white", stroke = 1, shape = 21,
             position = position_dodge(1)) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget bonus value (in fraction of initial budget)", y= "Resource population deviation from target") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
bb2.msd

bb2.ci <- ggplot(data = as.data.frame(stats_batchBB1_results), aes(x=as.factor(bb), y=abs_act_dev)) +
  geom_errorbar(aes(ymin=abs_act_dev-abs_act_dev_95ci, ymax=abs_act_dev+abs_act_dev_95ci),  
                position=position_dodge(1),
                colour = "grey40", width=0.5) +
  geom_point(size = 2, alpha = 1, colour="black", fill = "white", stroke = 1, shape = 21,
             position = position_dodge(1)) +
  # geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # geom_hline(yintercept = -1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x="Budget bonus value (in fraction of initial budget)", y= "Resource population deviation from target") +
  theme_gray() +
  theme(strip.background=element_rect(fill="grey"),
        strip.text=element_text(color="white", face="bold"),
        axis.title=element_text(size=18),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 18))
bb2.ci


######## New batch of simulations with functional action threshold and better chosen case parameters, without scaring for now ########

## Set parameters

# Array of Action Threshold values to explore
at <- seq(0,1,0.25)

# Array of Budget Bonus values to explore
bb <- seq(0,1,0.25)

# Number of simulation time steps
ts <- 10

# Number of replicates
rep <- 10

# Budget
bdgt <- 1000

## Case parameters: a standard case, a pop is endangered by culling, put under conservation, managers want to maintain it close to their carrying capacity
# initial Resource population
popinit <- 1000

# carrying capacity
K <- 3000

# Manager target
trgt <- 2000

# number of stakeholders
stkh <- 2

# Other parameters to GMSE default


## Create empty structures to gather simulation results

columns <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "overK")

# Empty 3D array of correct size 
# Dimensions(lines = replicates, columns = measures, layer = parameter combination)
batch4_results <- array(data=NA, dim = c(rep, length(columns), length(at)*length(bb)-4), dimnames = list(NULL,columns,NULL))                 

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

          # absolute value
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


## Basic stats

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