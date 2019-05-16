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
columns <- c("rep", "at", "bb", "init_budg", "init_res", "lambda", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts")

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
# effect of growing number of stakeholders
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
columns <- c("rep", "scar", "at", "bb", "extinct", "act_dev", "final yield", "max_diff_yield", "inac_ts")

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

# store start time
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
        OYA_batch1_results[k,4,param_set] <- 0
        
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
        
        # keep track of the simulations
        if (param_set %/% 20 != 0) {
          print(paste("parameter set number", param_set, "out of", dim(OYA_batch1_results)[3], "at", Sys.time(), sep = " "))
        }
        
        # Increment parameter combo index
        param_set <- param_set + 1
      } # end bb for loop
    } # end at else loop
  } # end at for loop
} # end scar for loop

# end of sim
end <- Sys.time()

print(paste("Batch started", start, "and ended", end, sep = " "))

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
  stats_OYA_batch1_results[i,5] <- round(sum(OYA_batch1_results[,7,i])/dim(OYA_batch1_results)[1],2)
  
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


## Plot actual Resource population deviation from target and Users final yield according to AT and BB values

# Line for subset of results if needed
fig_tab <- stats_OYA_batch1_results

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