# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy updating timing with a fictional case
# 4 strategies: default, memory of previous observation of population size, bonus reset only after raisong the costs, a combination of the latter two

#### libraries ####
# library("colorspace")
# library("ggplot2")
# library("stringr")
library(plotly)

#### functions needed ####

boot_sd_ci <- function(x, confidence = 95, itr = 1000) {
  
  # init iterations and sample
  i <- itr
  bt_avg <- NULL
  
  # loop over iterations
  while (i > 0) {
    # sample randomly from x
    spl <- sample(x, length(x), replace = TRUE)
    
    # store mean
    bt_avg <- c(bt_avg, sum(spl)/length(spl))
    
    # decrement i
    i <- i-1
  }
  
  # mean over the bootstrapped samples
  bt_est <- sum(bt_avg)/itr
  
  # compute standard deviation
  bt_sd <- sqrt((1/(length(x)-1)) * sum((bt_avg-bt_est)^2))
  
  # compute confidence interval
  # sorting bt_avg numerically
  st_avg <- sort(bt_avg)
  
  # get the first value after the 2.5 first centiles
  bt_95ci_inf <- st_avg[floor(0.5*(1-0.01*confidence)*itr)+1]
  
  # get the last value before the 2.5 last centiles
  bt_95ci_sup <- st_avg[floor((0.01*confidence+0.5*(1-0.01*confidence))*itr)-1]
  
  res <- c(bt_sd, bt_95ci_inf, bt_95ci_sup)
  return(res) 
  
}

OTI_stats <- function(df, ts, omit.extinction = FALSE) {
  
  df <- as.data.frame(df)
  
  # levels of ratio, UT and BB parameters 
  bgt_rat <- levels(as.factor(df$ratio))
  upd_thr <- levels(as.factor(df$at))
  bud_bon <- levels(as.factor(df$bb))
  
  # number of samples for bootstrap 
  nbs <- 1000
  
  if (omit.extinction == "TRUE") { 
    
    # a loop to calculate extinction freq
    sub <- subset(df, at == 0 & bb == 0 & ratio == bgt_rat[1])
    
    # NULL tab
    ext_freq <- NULL
    sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
    res <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2],sd_ci[3])
    ext_freq <- rbind(ext_freq, res)
    
    # loop over BRs just for control strat
    for (i in 2:length(bgt_rat)) {
      sub <- subset(df, at == upd_thr[1] & bb == bud_bon[1] & ratio == bgt_rat[i])
      sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
      res <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2],sd_ci[3])
      ext_freq <- rbind(ext_freq, res)
    }
    
    # loop over the rest
    for (i in 1:length(bgt_rat)) {
      for (j in 2:length(upd_thr)) {
        for (k in 1:length(bud_bon)) {
          sub <- subset(df, at == upd_thr[j] & bb == bud_bon[k] & ratio == bgt_rat[i])
          sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
          res <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2],sd_ci[3])
          ext_freq <- rbind(ext_freq, res)
        }
      }
    }
    
    print("Ommiting replicates that resulted in Resource extinction")
    df <- subset(df, extinct == 0)
    
    # levels after ommission
    bgt_rat <- levels(as.factor(df$ratio))
    upd_thr <- levels(as.factor(df$at))
    bud_bon <- levels(as.factor(df$bb))
    
    # initiate a count for later
    zz <- 1
    
  } # end if loop on omit.extinction 
  
  # create empty tab
  res_tab <- NULL
  
  ## Special subset for UT = 0, for which there was only BB = 0
  # initiate a string
  res_str <- NULL
  
  # subset
  sub <- subset(df, at == upd_thr[1] & bb == bud_bon[1] & ratio == bgt_rat[1])
  
  # number of replicates
  nbrep <- dim(sub)[1]
  
  # extinction frequency
  if (omit.extinction == TRUE) {
    ext <- ext_freq[zz,]
  } else {
    sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
    ext <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2], sd_ci[3])
  }
  
  # start filling the string in
  res_str <- c(nbrep, mean(sub$memory), mean(sub$budget), bgt_rat[1], upd_thr[1], bud_bon[1], ext)
  
  ## mean, sd and 95ci for each proxy
  
  # Actual Resource population deviation from Manager's target
  sd_ci <- boot_sd_ci(sub$act_dev, itr = nbs)
  res_str <- c(res_str, mean(sub$act_dev), sd_ci[1], sd_ci[2], sd_ci[3])
  
  # Absolute value of the actual Resource population deviation from Manager's target
  sd_ci <- boot_sd_ci(sub$abs_act_dev, itr = nbs)
  res_str <- c(res_str, mean(sub$abs_act_dev), sd_ci[1], sd_ci[2], sd_ci[3])
  
  # Users' total final yield
  sd_ci <- boot_sd_ci(sub$fin_yield, itr = nbs)
  res_str <- c(res_str, mean(sub$fin_yield), sd_ci[1], sd_ci[2], sd_ci[3])
  
  # Difference between the highest and the lowest yield
  sd_ci <- boot_sd_ci(sub$max_diff_yield, itr = nbs)
  res_str <- c(res_str, mean(sub$max_diff_yield), sd_ci[1], sd_ci[2], sd_ci[3])
  
  # Percentage of time steps of non-updating
  sd_ci <- boot_sd_ci(sub$inac_ts, itr = nbs)
  res_str <- c(res_str, mean(sub$inac_ts), sd_ci[1], sd_ci[2], sd_ci[3])
  
  # Sum of absolute deviation from target
  sd_ci <- boot_sd_ci(sub$SumAbsDev/sub$final_ts, itr = nbs)
  res_str <- c(res_str, mean(sub$SumAbsDev/sub$final_ts), sd_ci[1], sd_ci[2], sd_ci[3])
  
  # binding the string to the tab
  res_tab <- rbind(res_tab, as.numeric(res_str))
  
  # loop over BRs just for control strat
  for (i in 2:length(bgt_rat)) {
    
    # increment tracker
    if (omit.extinction == TRUE) {
      zz <- zz + 1
    }
    
    # initiate a string
    res_str <- NULL
    
    # subset
    sub <- subset(df, ratio == bgt_rat[i] & at == upd_thr[1] & bb == bud_bon[1])
    
    # number of replicates
    nbrep <- dim(sub)[1]
    
    # extinction frequency
    if (omit.extinction == TRUE) {
      ext <- ext_freq[zz,]
    } else {
      sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
      ext <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2], sd_ci[3])
    }
    
    # start filling the string in
    res_str <- c(nbrep,mean(sub$memory), mean(sub$budget), bgt_rat[i], upd_thr[1], bud_bon[1], ext)
    
    # avoid problems if there is only one replicate
    if (nbrep >= 2) {
      
      ## mean, sd and 95ci for each proxy
      
      # Actual Resource population deviation from Manager's target
      sd_ci <- boot_sd_ci(sub$act_dev, itr = nbs)
      res_str <- c(res_str, mean(sub$act_dev), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Absolute value of the Actual Resource population deviation from Manager's target
      sd_ci <- boot_sd_ci(sub$abs_act_dev, itr = nbs)
      res_str <- c(res_str, mean(sub$abs_act_dev), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Users' total final yield
      sd_ci <- boot_sd_ci(sub$fin_yield, itr = nbs)
      res_str <- c(res_str, mean(sub$fin_yield), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Difference between the highest and the lowest yield
      sd_ci <- boot_sd_ci(sub$max_diff_yield, itr = nbs)
      res_str <- c(res_str, mean(sub$max_diff_yield), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Percentage of time steps of non-updating
      sd_ci <- boot_sd_ci(sub$inac_ts, itr = nbs)
      res_str <- c(res_str, mean(sub$inac_ts), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Sum of absolute deviation from target
      sd_ci <- boot_sd_ci(sub$SumAbsDev/sub$final_ts, itr = nbs)
      res_str <- c(res_str, mean(sub$SumAbsDev/sub$final_ts), sd_ci[1], sd_ci[2], sd_ci[3])
      
    } else {
      
      print(paste("parameter set with budget ratio = ", as.numeric(bgt_rat)[i]*100, "%, UT = ", as.numeric(upd_thr[j])*100, "% and BB = ", as.numeric(bud_bon[k])*100, "% has less than 2 replicates"))
      
      # Actual Resource population deviation from Manager's target
      res_str <- c(res_str, sub$act_dev, 0, sub$act_dev, sub$act_dev)
      
      # Absolute value of the Actual Resource population deviation from Manager's target
      res_str <- c(res_str, sub$abs_act_dev, 0, sub$abs_act_dev, sub$abs_act_dev)
      
      # Users' total final yield
      res_str <- c(res_str, sub$fin_yield, 0, sub$fin_yield, sub$fin_yield)
      
      # Difference between the highest and the lowest yield
      res_str <- c(res_str, sub$max_diff_yield, 0, sub$max_diff_yield, sub$max_diff_yield)
      
      # Percentage of time steps of non-updating
      res_str <- c(res_str, sub$inac_ts, 0, sub$inac_ts, sub$inac_ts)
      
      # Sum of squared deviation from target
      res_str <- c(res_str, sub$SumAbsDev/sub$final_ts, 0, sub$SumAbsDev, sub$SumAbsDev)
    } # end else loop on nbrep
    
    # binding the string to the tab
    res_tab <- rbind(res_tab, as.numeric(res_str))
    
  }
  
  ## loop over the other OTI parameters
  # for each ratio value
  for (i in 1:length(bgt_rat)) {
    
    # for each positive at value
    for (j in 2:length(upd_thr)) {
      
      # for each bb value
      for (k in 1:length(bud_bon)) {
        
        # increment tracker
        if (omit.extinction == TRUE) {
          zz <- zz + 1
        }
        
        # initiate a string
        res_str <- NULL
        
        # subset
        sub <- subset(df, ratio == bgt_rat[i] & at == upd_thr[j] & bb == bud_bon[k])
        
        # number of replicates
        nbrep <- dim(sub)[1]
        
        # extinction frequency
        if (omit.extinction == TRUE) {
          ext <- ext_freq[zz,]
        } else {
          sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
          ext <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2], sd_ci[3])
        }
        
        # start filling the string in
        res_str <- c(nbrep,mean(sub$memory), mean(sub$budget), bgt_rat[i], upd_thr[j], bud_bon[k], ext)
        
        # avoid problems if there is only one replicate
        if (nbrep >= 2) {
          
          ## mean, sd and 95ci for each proxy
          
          # Actual Resource population deviation from Manager's target
          sd_ci <- boot_sd_ci(sub$act_dev, itr = nbs)
          res_str <- c(res_str, mean(sub$act_dev), sd_ci[1], sd_ci[2], sd_ci[3])
          
          # Absolute value of the Actual Resource population deviation from Manager's target
          sd_ci <- boot_sd_ci(sub$abs_act_dev, itr = nbs)
          res_str <- c(res_str, mean(sub$abs_act_dev), sd_ci[1], sd_ci[2], sd_ci[3])
          
          # Users' total final yield
          sd_ci <- boot_sd_ci(sub$fin_yield, itr = nbs)
          res_str <- c(res_str, mean(sub$fin_yield), sd_ci[1], sd_ci[2], sd_ci[3])
          
          # Difference between the highest and the lowest yield
          sd_ci <- boot_sd_ci(sub$max_diff_yield, itr = nbs)
          res_str <- c(res_str, mean(sub$max_diff_yield), sd_ci[1], sd_ci[2], sd_ci[3])
          
          # Percentage of time steps of non-updating
          sd_ci <- boot_sd_ci(sub$inac_ts, itr = nbs)
          res_str <- c(res_str, mean(sub$inac_ts), sd_ci[1], sd_ci[2], sd_ci[3])
          
          # Sum of absolute deviation from target
          sd_ci <- boot_sd_ci(sub$SumAbsDev/sub$final_ts, itr = nbs)
          res_str <- c(res_str, mean(sub$SumAbsDev/sub$final_ts), sd_ci[1], sd_ci[2], sd_ci[3])
          
        } else {
          
          print(paste("parameter set with budget ratio = ", as.numeric(bgt_rat)[i]*100, "%, UT = ", as.numeric(upd_thr[j])*100, "% and BB = ", as.numeric(bud_bon[k])*100, "% has less than 2 replicates"))
          
          # Actual Resource population deviation from Manager's target
          res_str <- c(res_str, sub$act_dev, 0, sub$act_dev, sub$act_dev)
          
          # Absolute value of the Actual Resource population deviation from Manager's target
          res_str <- c(res_str, sub$abs_act_dev, 0, sub$abs_act_dev, sub$abs_act_dev)
          
          # Users' total final yield
          res_str <- c(res_str, sub$fin_yield, 0, sub$fin_yield, sub$fin_yield)
          
          # Difference between the highest and the lowest yield
          res_str <- c(res_str, sub$max_diff_yield, 0, sub$max_diff_yield, sub$max_diff_yield)
          
          # Percentage of time steps of non-updating
          res_str <- c(res_str, sub$inac_ts, 0, sub$inac_ts, sub$inac_ts)
          
          # Sum of squared deviation from target
          res_str <- c(res_str, sub$SumAbsDev/sub$final_ts, 0, sub$SumAbsDev, sub$SumAbsDev)
        } # end else loop on nbrep
        
        # binding the string to the tab
        res_tab <- rbind(res_tab, as.numeric(res_str))

      } # end for loop on budget bonus
    } # end for loop on update threshold
  } # end for loop on budget ratio
  
  # Array of column names
  colnames(res_tab) <- c("rep", "memory", "budget", "ratio", "at", "bb", "ext_prob", "ext_prob_sd", "ext_prob_95ci_inf", "ext_prob_95ci_sup", "act_dev", "act_dev_sd", "act_dev_95ci_inf", "act_dev_95ci_sup", "abs_act_dev", "abs_act_dev_sd", "abs_act_dev_95ci_inf", "abs_act_dev_95ci_sup", "fin_yield", "fin_yield_sd", "fin_yield_95ci_inf", "fin_yield_95ci_sup", "max_diff_yield", "max_diff_yield_sd", "max_diff_yield_95ci_inf", "max_diff_yield_95ci_sup", "inac_ts", "inac_ts_sd", "inac_ts_95ci_inf", "inac_ts_95ci_sup", "SumAbsDev", "SumAbsDev_sd", "SumAbsDev_95ci_inf", "SumAbsDev_95ci_sup")
  
  res_tab <- as.data.frame(res_tab)
  
  return(res_tab)
  
} # end function

#### import data ####

path <- "~/Desktop/PhD/GitKraken/gmse_fork_RQ1/Budget-ratio-large-batch/"

setwd(path)

dir.name <- "noMem-res"

brut <- as.data.frame(read.csv(paste(dir.name, "/merged-res/noMem-budget-ratio-merged.csv", sep = "")))

stat <- OTI_stats(df = brut, ts = 20, omit.extinction = F) 
woe_stat <- OTI_stats(df = brut, ts = 20, omit.extinction = T)

# in case of problem
stat2 <- stat[-which(is.na(stat$at)),]
stat2 <- stat2[-c(1:6),]

write.csv(stat, file = paste(dir.name, "/merged-res/bgt-ratio-mem-stats.csv", sep=""))
write.csv(woe_stat, file = paste(dir.name,"/bgt-ratio-noMem-woExt-stats.csv", sep=""))

# # eliminate 100% extinction parameter sets if necessary
# woe_stat <- woe_stat[-which(as.numeric(as.character(woe_stat$rep)) < 1),]

# evolution of some replicates along simulation time
costs <- read.csv(paste(path, dir.name, "/merged-res/cos-mem-budget-ratio-merged.csv", sep = ""))
popul <- read.csv(paste(path, dir.name, "/merged-res/pop-mem-budget-ratio-merged.csv", sep = ""))
actio <- read.csv(paste(path, dir.name, "/merged-res/act-mem-budget-ratio-merged.csv", sep = ""))
budge <- read.csv(paste(path, dir.name, "/merged-res/bgt-mem-budget-ratio-merged.csv", sep = ""))

# only without extinction
we_costs <- subset(costs, Extinct == 0)
we_popul <- subset(popul, Extinct == 0)
we_actio <- subset(actio, Extinct == 0)
we_budge <- subset(budge, Extinct == 0)

#### effect of BR on extfreq for control strat ####

{d <- subset(stat, at == 0)

  d$ratio <- d$ratio*100
  
  bura <- levels(as.factor(d$ratio))
  
  # # with the same budget
  # same.bgt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/mem-noreset-results/mem-noreset-merged-results/ATI-mem-noreset-stats.csv", header = T)[,-1]
  # same.bgt.var <- same.bgt[1,5] 
  # same.bgt.inf <- same.bgt[1,7] 
  # same.bgt.sup <- same.bgt[1,8] 
  
  # Without management?
  no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  no.mgmt.var <- sum(no.mgmt[,5])/dim(no.mgmt)[1]
  
  # Without humans?
  no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  no.hum.var <- sum(no.hum[,5])/dim(no.hum)[1]
  
  # plot and export in pdf
  {pdf(file = "MEM-bgt-ratio-control-extfreq.pdf", width = par('din')[1], height = par('din')[2])
    
    {# # enlarge margins
      # par(mar = c(5, 5, 1, 1))
      
      # points cex
      pts <- 0.5
      # pts <- 1
      
      # plot base
      plot(1, type = "n",
           ylim = c(0, 1),
           xlim = c(10, 100),
           ylab = "Extinction frequency", #
           xlab = "Budget ratio (%)", #cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
           cex.lab = pts + 0.2, cex.axis = pts + 0.2)
      
      # Plot the results
      arrows(d$ratio, d$ext_prob_95ci_inf, d$ratio, d$ext_prob_95ci_sup, length=0.02, angle=90, code=3, col = "black")
      points(d$ratio, d$ext_prob, type = "b", pch = 15, col = "black", cex = pts, lwd = pts)
      
      # # When they have the same budget
      # arrows(100, same.bgt.inf, 100, same.bgt.sup, length=0.02, angle=90, code=3, col = "violet")
      # points(100, same.bgt.var, pch = 15, col = "violet", cex = pts)
      
      # No management
      abline(h = no.mgmt.var, lty = 2, col = "grey", lwd = pts)
      
      # No humans
      abline(h = no.hum.var, lty = 3, col = "gray", lwd = pts)
      
      # legend
      legend( # 110, 0.5,             # Location of legend 
        "bottomleft",
        # xpd = TRUE,                          # Allow drawing outside plot area
        # ncol = 2,
        # xjust = 0,                           # Left justify legend box on x
        # yjust = 0.5,                          # Center legend box on y
        legend = c("No humans",
                   "No management"),
                   # "Equal budget"),
        col = c("grey",                 # Legend Element Colors
                "grey"),
                # "violet"),          
        pch = c(NA_integer_,
                NA_integer_),
                # 15),                      # Legend Element Styles          
        lty = c(3,
                2),
                # NA_integer_),       
        cex = pts-0.2) # ,
        # cex = 0.6,
        # title = "Strategies") #,                  # Legend Title
      # title.col = gray(.2) ,                # Legend title color
      # box.lty = 1,                         # Legend box line type
      # box.lwd = 1)                         # Legend box line width
    }
    dev.off()
  }
}

#### effect of BR on deviation for control strat ####

{d <- subset(stat, at == 0)

d$ratio <- d$ratio*100
d$act_dev <- d$act_dev*100
d$act_dev_95ci_inf <- d$act_dev_95ci_inf*100
d$act_dev_95ci_sup <- d$act_dev_95ci_sup*100

bura <- levels(as.factor(d$ratio))

# # with the same budget
# same.bgt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/mem-noreset-results/mem-noreset-merged-results/ATI-mem-noreset-stats.csv", header = T)[,-1]
# same.bgt.var <- same.bgt[1,5] 
# same.bgt.inf <- same.bgt[1,7] 
# same.bgt.sup <- same.bgt[1,8] 

# Without management?
no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
no.mgmt.var <- 100*sum(no.mgmt[,6])/dim(no.mgmt)[1]

# Without humans?
no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
no.hum.var <- 100*sum(no.hum[,6])/dim(no.hum)[1]

# plot and export in pdf
{pdf(file = "MEM-bgt-ratio-control-dev.pdf", width = par('din')[1], height = par('din')[2])
  
  {# # enlarge margins
    # par(mar = c(5, 5, 1, 1))
    
    # points cex
    pts <- 0.5
    # pts <- 1
    
    # plot base
    plot(1, type = "n",
         ylim = c(-100, 100),
         xlim = c(10, 100),
         ylab = "Population deviation from target (%)", #
         xlab = "Budget ratio (%)", #cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
         cex.lab = pts + 0.2, cex.axis = pts + 0.2)
    
    # best res
    abline(h = 0, col = "darkgreen", lty = 2, lwd = pts)
    
    # No management
    abline(h = no.mgmt.var, lty = 2, col = "grey", lwd = pts)
    
    # No humans
    abline(h = no.hum.var, lty = 3, col = "gray", lwd = pts)
    
    # Plot the results
    arrows(d$ratio, d$act_dev_95ci_inf, d$ratio, d$act_dev_95ci_sup, length=0.02, angle=90, code=3, col = "black")
    points(d$ratio, d$act_dev, type = "b", pch = 15, col = "black", cex = pts, lwd = pts)
    
    # # When they have the same budget
    # arrows(100, same.bgt.inf, 100, same.bgt.sup, length=0.02, angle=90, code=3, col = "violet")
    # points(100, same.bgt.var, pch = 15, col = "violet", cex = pts)
    
    # legend
    legend( # 110, 0.5,             # Location of legend 
      "bottomleft",
      # xpd = TRUE,                          # Allow drawing outside plot area
      # ncol = 2,
      # xjust = 0,                           # Left justify legend box on x
      # yjust = 0.5,                          # Center legend box on y
      legend = c("No humans",
                 "No management"),
                 # "Equal budget"),
      col = c("grey",                 # Legend Element Colors
              "grey"),
              # "violet"),          
      pch = c(NA_integer_,
              NA_integer_),
              # 15),                      # Legend Element Styles          
      lty = c(3,
              2),
              # NA_integer_),       
      cex = pts-0.2) #,
      # cex = 0.6,
      # title = "Strategies") #,                  # Legend Title
    # title.col = gray(.2) ,                # Legend title color
    # box.lty = 1,                         # Legend box line type
    # box.lwd = 1)                         # Legend box line width
}
  dev.off()
}
}

######## Contour figures ########

#### Extinction frequency ####

# build results matrix

d <- subset(stat, at == 0.5)

d$bb <- d$bb*100

bubo <- levels(as.factor(d$bb))
bura <- levels(as.factor(d$ratio))

# debut <- length(d$ext_prob)-length(bubo)+1
# fin <- length(d$ext_prob)
# res <- d$ext_prob[fin:debut]
# 
# for (i in 2:length(bura)) {
#   debut <- length(d$ext_prob)-i*length(bubo)+1
#   fin <- length(d$ext_prob)-(i-1)*length(bubo)
#   res <- c(res, d$ext_prob[debut:fin])
# }

# resmat <- matrix(data = seq(1:(length(bubo)*length(bura))), ncol = length(bura), nrow = length(bubo))
resmat <- matrix(data = d$ext_prob, ncol = length(bura), nrow = length(bubo))

# resfin <- resmat[,9]
# for (i in (length(bura)-1):1) {
#   resfin <- rbind(resfin, resmat[,i])
# }

fig <- plot_ly(
  x = bubo, 
  y = bura, 
  # z = matrix(data = d$ext_prob, ncol = length(bubo), nrow = length(bura)), 
  z = t(resmat),
  type = "contour",
  colorscale = list(c(0, 0.5, 1), c('green', 'orange', 'red')),
  autocontour = F,
  # contours = list(showlabels = TRUE),
  contours = list(
    start = 0,
    end = 1,
    size = 0.1,
    showlabels = T
  )
)

xlab <- list(
  title = "Budget bonus (%)"#,
  # titlefont = f
)
ylab <- list(
  title = "Budget ratio"#,
  # titlefont = f
)

fig <- fig %>% colorbar(title = "Extinction \n frequency")
fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)
fig

#### Deviation from target ####

# build results matrix

d <- subset(stat, at == 0.3)

d$bb <- d$bb*100

bubo <- levels(as.factor(d$bb))
bura <- levels(as.factor(d$ratio))

resmat <- matrix(data = 100*d$act_dev, nrow = length(bubo), ncol = length(bura))
# resmat <- t(resmat)

fig <- plot_ly(
  x = bubo, 
  y = bura, 
  z = t(resmat), 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = F,
  # contours = list(showlabels = TRUE)#,
  contours = list(
    start = -100,
    end = 20,
    size = 10,
    showlabels = T
  )
)
# 
# xlab <- list(
#   title = "Budget bonus (%)"#,
#   # titlefont = f
# )
# ylab <- list(
#   title = "Budget ratio"#,
#   # titlefont = f
# )

fig <- fig %>% colorbar(title = "Dev. from \n target (%)")
fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)
fig

#### users yield ####

# build results matrix

d <- subset(stat, at == 0.5)

d$bb <- d$bb*100

bubo <- levels(as.factor(d$bb))
bura <- levels(as.factor(d$ratio))

resmat <- matrix(data = d$fin_yield/1000, ncol = length(bubo), nrow = length(bura))

fig <- plot_ly(
  x = bubo, 
  y = bura, 
  z = t(resmat), 
  type = "contour",
  colorscale = list(c(0, 1), c('orange', 'green')),
  autocontour = F,
  # contours = list(showlabels = TRUE),
  contours = list(
    start = 20,
    end = 40,
    size = 2,
    showlabels = T
  )
)

fig <- fig %>% colorbar(title = "Final yield \n (in k)")
fig <- fig %>% layout(xaxis = xlab, yaxis = ylab)
fig

#### Yiel inequity ####

# build results matrix

d <- subset(stat, at == 0.5)

d$bb <- d$bb*100

bubo <- levels(as.factor(d$bb))
bura <- levels(as.factor(d$ratio))

fig <- plot_ly(
  x = bubo, 
  y = bura, 
  z = matrix(data = d$max_diff_yield, ncol = length(bubo), nrow = length(bura)), 
  type = "contour",
  colorscale = 'Viridis',
  autocontour = F,
  contours = list(showlabels = TRUE)
)

fig <- fig %>% colorbar(title = "Yield \n inequity")
fig

#### effect of BB:ratio on extinction frequency according to UT ####

{d <- subset(stat, at == 0.5)
  
  d$at <- d$at*100
  d$bb <- d$bb*100
  
  # get max, min and average of each UT
  upth <- levels(as.factor(d$at))
  bubo <- levels(as.factor(d$bb))
  bura <- levels(as.factor(d$ratio))
  
  sub <- as.data.frame(subset(d, ratio == bura[1]))
  ext <- sub$ext_prob
  sd <- sub$ext_prob_sd
  ci_inf <- sub$ext_prob_95ci_inf
  ci_sup <- sub$ext_prob_95ci_sup
  
  for (i in 2:length(bura)) {
    sub <- as.data.frame(subset(d, ratio == bura[i]))
    ext <- rbind(ext, sub$ext_prob)
    sd <- rbind(sd, sub$ext_prob_sd)
    ci_inf <- rbind(ci_inf, sub$ext_prob_95ci_inf)
    ci_sup <- rbind(ci_sup, sub$ext_prob_95ci_sup)
  }
  
  # Without management?
  no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  no.mgmt.var <- sum(no.mgmt[,5])/dim(no.mgmt)[1]
  
  # Without humans?
  no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  no.hum.var <- sum(no.hum[,5])/dim(no.hum)[1]
  
  # # plotting convenience
  # xadj1 <- as.numeric(upth[-1]) - 1;
  # xadj2 <- as.numeric(upth[-1]) + 1;
  # xtendrange <- seq(-10,110,1)
}

# plot and export in pdf
{pdf(file = "mem-UT50-bgtRatio-extfreq.pdf", width = par('din')[1], height = par('din')[2])

  {# # enlarge margins
  # par(mar = c(5, 5, 1, 1))
  
  # points cex
  pts <- 0.5
  # pts <- 1
    
  # trunk ext if necessary
  ext <- ext[-(1:2),]
  ci_inf <- ci_inf[-(1:2),]
  ci_sup <- ci_sup[-(1:2),]
  
  xadj <- seq(-2,2,length.out=dim(ext)[1])
  colo <- rainbow(dim(ext)[1])
  
  # plot base
  plot(1, type = "n",
       ylim = c(0, 1),
       xlim = c(0, 105),
       ylab = "Extinction frequency", #
       xlab = "Budget Bonus (%)", #cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
       cex.lab = pts + 0.2, cex.axis = pts + 0.2)
  
  # # Control band
  # polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
  
  k <- 1
  for (i in 1:dim(ext)[1]) {
    arrows(x0 = as.numeric(bubo) + xadj[k], y0 = ci_inf[i,], x1 = as.numeric(bubo) + xadj[k], y1 = ci_sup[i,], length=0.02, angle=90, code=3, col = colo[i])
    points(x = as.numeric(bubo) + xadj[k], y = ext[i,], type = "b", cex = pts, lwd = pts-0.2, col = colo[i], pch = 20);
    k <- k+1
  }
  
  # # Plot the results
  # arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.02, angle=90, code=3, col = "black")
  # arrows(xadj1, ci_inf[-1,2], xadj1, ci_sup[-1,2], length=0.02, angle=90, code=3, col = "darkred")
  # arrows(xadj2, ci_inf[-1,11], xadj2, ci_sup[-1,11], length=0.02, angle=90, code=3, col = "darkred")
  # points(x = xadj1, y = ext[-1,2], type = "b", pch = 16, col = "darkred", cex = pts, lwd = pts)
  # points(x = xadj2, y = ext[-1,11], type = "b", pch = 21, lty = "dashed", col = "darkred", cex = pts, lwd = pts)
  # points(x = 0, y = ext[1,1], pch = 15, cex = pts, lwd = pts)
  # abline(h=ext[1,1], lty = 1, col = "black", lwd = pts)
  
  # # No management
  # points(y = no.mgmt.var, x = 0, pch = 17, col = "black", cex = pts)
  # abline(h = no.mgmt.var, lty = 2, col = "black", lwd = pts)
  # 
  # # No humans
  # points(y = no.hum.var, x = 0, pch = 23, col = "black", cex = pts)
  # abline(h = no.hum.var, lty = 3, col = "black", lwd = pts)
  
  # legend
  legend( # 110, 0.5,             # Location of legend 
         "topright",
         # xpd = TRUE,                          # Allow drawing outside plot area
         ncol = 2,
         # xjust = 0,                           # Left justify legend box on x
         # yjust = 0.5,                          # Center legend box on y
         legend = bura[3:6],
           # c(paste("BR = ", bura[1]),
           #          paste("BR = ", bura[2]),
           #          paste("BR = ", bura[3]),
           #          paste("BR = ", bura[4]),
           #          paste("BR = ", bura[5]),
           #          paste("BR = ", bura[6]),
           #          paste("BR = ", bura[7]),
           #          paste("BR = ", bura[8]),
           #          paste("BR = ", bura[9])),
         col = colo,
           # c(colo[1],
           #       colo[2],
           #       colo[3],
           #       colo[4],
           #       colo[5],
           #       colo[6],
           #       colo[7],
           #       colo[8],
           #       colo[9]),          
         pch = rep(16, dim(ext)[1]),
           # c(23,
           #       17,
           #       15,
           #       19,
           #       21,
           #       20),                      # Legend Element Styles          
         lty = rep(1, dim(ext)[1]),
           # c(3,
           #       2,
           #       1,
           #       1,
           #       2,
           #       1),       
         cex = pts-0.2,
         # cex = 0.6,
         title = "Ratios") #,                  # Legend Title
         # title.col = gray(.2) ,                # Legend title color
         # box.lty = 1,                         # Legend box line type
         # box.lwd = 1)                         # Legend box line width
  }
dev.off()
}
  

#### effect of BB:ratio on deviation from target according to UT ####

{d <- subset(stat, at == 0.3)

d$at <- d$at*100
d$bb <- d$bb*100
d$act_dev <- d$act_dev*100
d$act_dev_sd <- d$act_dev_sd*100
d$act_dev_95ci_inf <- d$act_dev_95ci_inf*100
d$act_dev_95ci_sup <- d$act_dev_95ci_sup*100

# get max, min and average of each UT
upth <- levels(as.factor(d$at))
bubo <- levels(as.factor(d$bb))
bura <- levels(as.factor(d$ratio))

sub <- as.data.frame(subset(d, ratio == bura[1]))
ext <- sub$act_dev
sd <- sub$act_dev_sd
ci_inf <- sub$act_dev_95ci_inf
ci_sup <- sub$act_dev_95ci_sup

for (i in 2:length(bura)) {
  sub <- as.data.frame(subset(d, ratio == bura[i]))
  ext <- rbind(ext, sub$act_dev)
  sd <- rbind(sd, sub$act_dev_sd)
  ci_inf <- rbind(ci_inf, sub$act_dev_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$act_dev_95ci_sup)
}

# # Without management?
# no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
# no.mgmt.var <- sum(no.mgmt[,5])/dim(no.mgmt)[1]
# 
# # Without humans?
# no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
# no.hum.var <- sum(no.hum[,5])/dim(no.hum)[1]

# # plotting convenience
# xadj1 <- as.numeric(upth[-1]) - 1;
# xadj2 <- as.numeric(upth[-1]) + 1;
# xtendrange <- seq(-10,110,1)
}

# plot and export in pdf
{pdf(file = "mem-UT30-bgtRatio-dev.pdf", width = par('din')[1], height = par('din')[2])
  
  {# # enlarge margins
    # par(mar = c(5, 5, 1, 1))
    
    # points cex
    pts <- 0.5
    # pts <- 1
    
    # trunk ext if necessary
    ext <- ext[-(1:2),]
    ci_inf <- ci_inf[-(1:2),]
    ci_sup <- ci_sup[-(1:2),]
    
    xadj <- seq(-2,2,length.out=dim(ext)[1])
    colo <- rainbow(dim(ext)[1])
    
    # plot base
    plot(1, type = "n",
         ylim = c(-100, 20),
         xlim = c(0, 100),
         ylab = "Deviation from\ntarget (%)", #
         xlab = "Budget Bonus (%)", #cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
         cex.lab = pts + 0.2, cex.axis = pts + 0.2)
    
    # best possible
    abline(h = 0, lty = 2, lwd = pts-0.2, col = "black")
    
    # # Control band
    # polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
    
    k <- 1
    for (i in 1:dim(ext)[1]) {
      arrows(x0 = as.numeric(bubo) + xadj[k], y0 = ci_inf[i,], x1 = as.numeric(bubo) + xadj[k], y1 = ci_sup[i,], length=0.02, angle=90, code=3, col = colo[i])
      points(x = as.numeric(bubo) + xadj[k], y = ext[i,], type = "b", cex = pts, lwd = pts-0.2, col = colo[i], pch = 20);
      k <- k+1
    }
    
    # # Plot the results
    # arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.02, angle=90, code=3, col = "black")
    # arrows(xadj1, ci_inf[-1,2], xadj1, ci_sup[-1,2], length=0.02, angle=90, code=3, col = "darkred")
    # arrows(xadj2, ci_inf[-1,11], xadj2, ci_sup[-1,11], length=0.02, angle=90, code=3, col = "darkred")
    # points(x = xadj1, y = ext[-1,2], type = "b", pch = 16, col = "darkred", cex = pts, lwd = pts)
    # points(x = xadj2, y = ext[-1,11], type = "b", pch = 21, lty = "dashed", col = "darkred", cex = pts, lwd = pts)
    # points(x = 0, y = ext[1,1], pch = 15, cex = pts, lwd = pts)
    # abline(h=ext[1,1], lty = 1, col = "black", lwd = pts)
    
    # # No management
    # points(y = no.mgmt.var, x = 0, pch = 17, col = "black", cex = pts)
    # abline(h = no.mgmt.var, lty = 2, col = "black", lwd = pts)
    # 
    # # No humans
    # points(y = no.hum.var, x = 0, pch = 23, col = "black", cex = pts)
    # abline(h = no.hum.var, lty = 3, col = "black", lwd = pts)
    
    # legend
    legend( # 110, 0.5,             # Location of legend 
      "bottomright",
      # xpd = TRUE,                          # Allow drawing outside plot area
      ncol = 2,
      # xjust = 0,                           # Left justify legend box on x
      # yjust = 0.5,                          # Center legend box on y
      legend = bura[3:6],
      # c(paste("BR = ", bura[1]),
      #          paste("BR = ", bura[2]),
      #          paste("BR = ", bura[3]),
      #          paste("BR = ", bura[4]),
      #          paste("BR = ", bura[5]),
      #          paste("BR = ", bura[6]),
      #          paste("BR = ", bura[7]),
      #          paste("BR = ", bura[8]),
      #          paste("BR = ", bura[9])),
      col = colo,
      # c(colo[1],
      #       colo[2],
      #       colo[3],
      #       colo[4],
      #       colo[5],
      #       colo[6],
      #       colo[7],
      #       colo[8],
      #       colo[9]),          
      pch = rep(16, dim(ext)[1]),
      # c(23,
      #       17,
      #       15,
      #       19,
      #       21,
      #       20),                      # Legend Element Styles          
      lty = rep(1, dim(ext)[1]),
      # c(3,
      #       2,
      #       1,
      #       1,
      #       2,
      #       1),       
      cex = pts-0.2,
      # cex = 0.6,
      title = "Ratios") #,                  # Legend Title
    # title.col = gray(.2) ,                # Legend title color
    # box.lty = 1,                         # Legend box line type
    # box.lwd = 1)                         # Legend box line width
  }
  dev.off()
}

#### Details of BB effect and waiting strategy on dev according to ratio ####

{d <- subset(stat, at == 0 & ratio == 0.8 | at == 0.3 & ratio == 0.8 | at == 0.5 & ratio == 0.8)

d$at <- d$at*100
d$bb <- d$bb*100

bubo <- levels(as.factor(d$bb))

sub <- subset(d, at != 50)
ext <- sub$act_dev*100
sd <- sub$act_dev_sd*100
ci_inf <- sub$act_dev_95ci_inf*100
ci_sup <- sub$act_dev_95ci_sup*100

sub <- subset(d, at != 30)
ext <- cbind(ext, sub$act_dev*100)
sd <- cbind(sd, sub$act_dev_sd*100)
ci_inf <- cbind(ci_inf, sub$act_dev_95ci_inf*100)
ci_sup <- cbind(ci_sup, sub$act_dev_95ci_sup*100)
}

# plot and export in pdf
{pdf(file = "mem-BR08-bgtRatio-dev.pdf", width = par('din')[1], height = par('din')[2])
  
  { # points cex
    pts <- 0.5
    
    # plot base
    plot(1, type = "n",
         ylim = c(-100, 20),
         xlim = c(0, 100),
         ylab = "Deviation from target (%)", #
         xlab = "Budget Bonus (%)", #cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
         cex.lab = pts + 0.2, cex.axis = pts + 0.2)
    
    # Control band
    xtendrange <- seq(-10,110,1)
    
    polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey")
    abline(h = ext[1,1], lwd = pts)
    
    # best possible
    abline(h = 0, lty = 2, lwd = pts, col = "darkgreen")
    
    # UT30
    arrows(x0 = as.numeric(bubo)-1, y0 = ci_inf[-1,1], x1 = as.numeric(bubo)-1, y1 = ci_sup[-1,1], length=0.02, angle=90, code=3, col = 'blue')
    points(x = as.numeric(bubo)-1, y = ext[-1,1], type = "b", cex = pts, lwd = pts, col = 'blue', pch = 20);
    
    # UT50
    arrows(x0 = as.numeric(bubo)+1, y0 = ci_inf[-1,2], x1 = as.numeric(bubo)+1, y1 = ci_sup[-1,2], length=0.02, angle=90, code=3, col = 'violet')
    points(x = as.numeric(bubo)+1, y = ext[-1,2], type = "b", cex = pts, lwd = pts, col = 'violet', pch = 4);
    
    # legend
    legend( # 110, 0.5,             # Location of legend 
      "topright",
      # xpd = TRUE,                          # Allow drawing outside plot area
      # ncol = 2,
      # xjust = 0,                           # Left justify legend box on x
      # yjust = 0.5,                          # Center legend box on y
      legend = c("Control",
                 "UT 30%",
                 "UT 50%"),
      col = c("black",
              "blue",
              "violet"),        
      pch = c(NA_integer_,
              20,
              4),                    # Legend Element Styles          
      lty = c(1, 
              1,
              1),     
      cex = pts-0.2,
      # cex = 0.6,
      title = "Strategies - 0.8 ratio") #,                  # Legend Title
    # title.col = gray(.2) ,                # Legend title color
    # box.lty = 1,                         # Legend box line type
    # box.lwd = 1)                         # Legend box line width
}
  dev.off()
}

#### Details of BB effect and waiting strategy on ext freq according to ratio ####

{d <- subset(stat, at == 0 & ratio == 0.8 | at == 0.5 & ratio == 0.8) # | at == 0.5 & ratio == 0.7

d$at <- d$at*100
d$bb <- d$bb*100

bubo <- levels(as.factor(d$bb))

sub <- subset(d, at != 50)
ext <- sub$ext_prob
sd <- sub$ext_prob_sd
ci_inf <- sub$ext_prob_95ci_inf
ci_sup <- sub$ext_prob_95ci_sup

# sub <- subset(d, at != 30)
# ext <- cbind(ext, sub$ext_prob)
# sd <- cbind(sd, sub$ext_prob_sd)
# ci_inf <- cbind(ci_inf, sub$ext_prob_95ci_inf)
# ci_sup <- cbind(ci_sup, sub$ext_prob_95ci_sup)

sub <- subset(d, at != 0)
ext <- c(ext, sub$ext_prob)
sd <- c(sd, sub$ext_prob_sd)
ci_inf <- c(ci_inf, sub$ext_prob_95ci_inf)
ci_sup <- c(ci_sup, sub$ext_prob_95ci_sup)
}

# plot and export in pdf
{pdf(file = "mem-BR08-UT50-bgtRatio-extfreq.pdf", width = par('din')[1], height = par('din')[2])
  
  { # points cex
    pts <- 0.5
    
    # plot base
    plot(1, type = "n",
         ylim = c(0, 1),
         xlim = c(0, 100),
         ylab = "Extinction frequency", #
         xlab = "Budget Bonus (%)", #cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
         cex.lab = pts + 0.2, cex.axis = pts + 0.2)
    
    # Control band
    xtendrange <- seq(-10,110,1)
    
    # polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey")
    # abline(h = ext[1,1], lwd = pts)
    
    polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1], length(xtendrange)),rev(rep(ci_inf[1], length(xtendrange)))),col="lightgrey", border = "grey")
    abline(h = ext[1], lwd = pts)
    
    # best possible
    abline(h = 0, lty = 2, lwd = pts, col = "darkgreen")
    
    # # UT30
    # arrows(x0 = as.numeric(bubo)-1, y0 = ci_inf[-1,1], x1 = as.numeric(bubo)-1, y1 = ci_sup[-1,1], length=0.02, angle=90, code=3, col = 'blue')
    # points(x = as.numeric(bubo)-1, y = ext[-1,1], type = "b", cex = pts, lwd = pts, col = 'blue', pch = 20);
    
    # UT50
    # arrows(x0 = as.numeric(bubo)+1, y0 = ci_inf[-1,2], x1 = as.numeric(bubo)+1, y1 = ci_sup[-1,2], length=0.02, angle=90, code=3, col = 'violet')
    # points(x = as.numeric(bubo)+1, y = ext[-1,2], type = "b", cex = pts, lwd = pts, col = 'violet', pch = 4);
    arrows(x0 = as.numeric(bubo), y0 = ci_inf[-1], x1 = as.numeric(bubo), y1 = ci_sup[-1], length=0.02, angle=90, code=3, col = 'violet')
    points(x = as.numeric(bubo), y = ext[-1], type = "b", cex = pts, lwd = pts, col = 'violet', pch = 4);
    
    # legend
    legend( # 110, 0.5,             # Location of legend 
      "topright",
      # xpd = TRUE,                          # Allow drawing outside plot area
      # ncol = 2,
      # xjust = 0,                           # Left justify legend box on x
      # yjust = 0.5,                          # Center legend box on y
      legend = c("Control",
                 # "UT 30%",
                 "UT 50%"),
      col = c("black",
              # "blue",
              "violet"),        
      pch = c(NA_integer_,
              # 20,
              4),                    # Legend Element Styles          
      lty = c(1, 
              # 1,
              1),     
      cex = pts-0.2,
      # cex = 0.6,
      title = "Strategies") #,          - 0.7 ratio         # Legend Title
    # title.col = gray(.2) ,                # Legend title color
    # box.lty = 1,                         # Legend box line type
    # box.lwd = 1)                         # Legend box line width
}
  dev.off()
}

#### effect of UT:BB on poplation deviation from target ####

{d <- stat

d$at <- d$at*100
d$bb <- d$bb*100
d$act_dev <- d$act_dev*100
d$act_dev_95ci_inf <- d$act_dev_95ci_inf*100
d$act_dev_95ci_sup <- d$act_dev_95ci_sup*100

# get max, min and average of each UT
upth <- levels(as.factor(d$at))
bubo <- levels(as.factor(d$bb))

sub <- as.data.frame(subset(d, at == upth[1]))
var <- c(sub$act_dev, rep(NA,length(upth)-1))
sd <- c(sub$act_dev_sd, rep(NA,length(upth)-1))
ci_inf <- c(sub$act_dev_95ci_inf, rep(NA,length(upth)-1))
ci_sup <- c(sub$act_dev_95ci_sup, rep(NA,length(upth)-1))

for (i in 2:length(upth)) {
  sub <- as.data.frame(subset(d, at == upth[i]))
  var <- rbind(var, sub$act_dev)
  sd <- rbind(sd, sub$act_dev_sd)
  ci_inf <- rbind(ci_inf, sub$act_dev_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$act_dev_95ci_sup)
}

# Without management?
no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
no.mgmt.var <- sum(no.mgmt[,6])/dim(no.mgmt)[1]

# Without humans?
no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
no.hum.var <- sum(no.hum[,6])/dim(no.hum)[1]

# plotting convenience
xadj1 <- as.numeric(upth[-1]) - 1;
xadj2 <- as.numeric(upth[-1]) + 1;
xtendrange <- seq(-10,110,1)
}

# plot and export in pdf
{pdf(file = "T1Q1-dev-small.pdf", width = par('din')[1], height = par('din')[2])
  
  {# # enlarge margins
    # par(mar = c(5, 5, 1, 1))
    
    # points cex
    pts <- 0.5
    # pts <- 1
    
    # plot base
    plot(1, type = "n",
         ylim = c(-100, 100),
         xlim = c(0, 100),
         ylab = "Average deviation from target", #
         xlab = "Update threshold (%)", #cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
         cex.lab = pts + 0.2, cex.axis = pts + 0.2)
    
    # Control band
    polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
    
    # Put the other budget bonuses in a faint grey to show but de-emphasise
    for (i in 3:(dim(var)[2]-1)) {
      points(x = as.numeric(upth[-1]), y = var[-1,i], type = "b", cex = pts-0.2, lwd = pts-0.2, lty = "solid",
             col = "grey", pch = 20);
    }
    
    # Plot the results
    arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.02, angle=90, code=3, col = "black")
    arrows(xadj1, ci_inf[-1,2], xadj1, ci_sup[-1,2], length=0.02, angle=90, code=3, col = "darkred")
    arrows(xadj2, ci_inf[-1,11], xadj2, ci_sup[-1,11], length=0.02, angle=90, code=3, col = "darkred")
    points(x = xadj1, y = var[-1,2], type = "b", pch = 16, col = "darkred", cex = pts, lwd = pts)
    points(x = xadj2, y = var[-1,11], type = "b", pch = 21, lty = "dashed", col = "darkred", cex = pts, lwd = pts)
    points(x = 0, y = var[1,1], pch = 15, cex = pts, lwd = pts)
    abline(h=var[1,1], lty = 1, col = "black", lwd = pts)
    
    # best possible
    abline(h = 0, col = "darkgreen", lty = 2)
    
    # No management
    points(y = no.mgmt.var, x = 0, pch = 17, col = "black", cex = pts)
    abline(h = no.mgmt.var, lty = 2, col = "black", lwd = pts)
    
    # No humans
    points(y = no.hum.var, x = 0, pch = 23, col = "black", cex = pts)
    abline(h = no.hum.var, lty = 3, col = "black", lwd = pts)
    
    # legend
    legend( # 110, 0.5,             # Location of legend 
      "bottomright",
      # xpd = TRUE,                          # Allow drawing outside plot area
      # ncol = 2,
      # xjust = 0,                           # Left justify legend box on x
      # yjust = 0.5,                          # Center legend box on y
      legend = c("No humans",
                 "No management",
                 "Null deviation",
                 "Control", 
                 "ATI - 0% BB",
                 "ATI - 100% BB", 
                 "other BB values"),
      col = c("black",                 # Legend Element Colors
              "black",
              "darkgreen",
              "black",
              "darkred",
              "darkred",
              "lightgrey"),          
      pch = c(23,
              17,
              NA_integer_,
              15,
              19,
              21,
              20),                      # Legend Element Styles          
      lty = c(3,
              2,
              2,
              1,
              1,
              2,
              1),       
      cex = pts-0.2,
      # cex = 0.6,
      title = "Strategies") #,                  # Legend Title
    # title.col = gray(.2) ,                # Legend title color
    # box.lty = 1,                         # Legend box line type
    # box.lwd = 1)                         # Legend box line width
}
  dev.off()
}

# {d <- stat
# 
# d$at <- d$at*100
# d$bb <- d$bb*100
# d$act_dev <- d$act_dev*100
# d$act_dev_95ci_inf <- d$act_dev_95ci_inf*100
# d$act_dev_95ci_sup <- d$act_dev_95ci_sup*100
# 
# # get max, min and average of each UT
# upth <- levels(as.factor(d$at))
# bubo <- levels(as.factor(d$bb))
# 
# sub <- as.data.frame(subset(d, at == upth[1]))
# dev <- c(sub$act_dev, rep(NA,length(upth)-1))
# sd <- c(sub$act_dev_sd, rep(NA,length(upth)-1))
# ci_inf <- c(sub$act_dev_95ci_inf, rep(NA,length(upth)-1))
# ci_sup <- c(sub$act_dev_95ci_sup, rep(NA,length(upth)-1))
# 
# for (i in 2:length(upth)) {
#   sub <- as.data.frame(subset(d, at == upth[i]))
#   dev <- rbind(dev, sub$act_dev)
#   sd <- rbind(sd, sub$act_dev_sd)
#   ci_inf <- rbind(ci_inf, sub$act_dev_95ci_inf)
#   ci_sup <- rbind(ci_sup, sub$act_dev_95ci_sup)
#   
#   # Without management?
#   no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
#   
#   # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
#   no.mgmt.extfreq <- sum(no.mgmt[,5])/dim(no.mgmt)[1]
#   points(y = no.mgmt.extfreq, x = 0, pch = 17, col = "black")
#   abline(h = no.mgmt.extfreq, lty = 2, lwd = 1, col = "black")
#   
#   # Without humans?
#   no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
#   
#   no.hum.extfreq <- sum(no.hum[,5])/dim(no.hum)[1]
#   points(y = no.hum.extfreq, x = 5, pch = 23, col = "black")
#   abline(h = no.hum.extfreq, lty = 3, lwd = 1, col = "black")
# }
# 
# xadj1 <- as.numeric(upth[-1]) - 1;
# xadj2 <- as.numeric(upth[-1]) + 1;
# xtendrange <- seq(-10,110,1)
# 
# # par(mar = c(5, 5, 1, 1))
# plot(1, xlab = "Update threshold", type = "n", pch = 20,
#      ylab = "Population deviation from target", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
#      ylim = c(-100, 100), xlim = c(0, 100), lwd = 2, col = "darkred")
# polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
# 
# # Put the other budget bonuses in a faint grey to show but de-emphasise
# for (i in 3:(dim(dev)[2]-1)) {
#   points(x = as.numeric(upth[-1]), y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
#          col = "grey", pch = 20);
# }
# abline(h = 0, col = "darkgreen", lty = 2)
# 
# arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.05, angle=90, code=3, col = "black")
# arrows(xadj1, ci_inf[-1,2], xadj1, ci_sup[-1,2], length=0.05, angle=90, code=3, col = "darkblue")
# arrows(xadj2, ci_inf[-1,11], xadj2, ci_sup[-1,11], length=0.05, angle=90, code=3, col = "darkblue")
# points(x = xadj1, y = dev[-1,2], xlab = "Update threshold", type = "b", pch = 20,
#        ylab = "Population deviation from target (+/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
#        ylim = c(0, 1), xlim = c(0, 100), lwd = 2, col = "blue");
# points(x = xadj2, y = dev[-1,11], type = "b", cex = 1, lwd = 2, lty = "dashed", col = "blue")
# points(x = 0, y = dev[1,1], pch = 15)
# abline(h=dev[1,1], lty = 1, col = "black")
# 
# # Without management?
# no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
# 
# # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
# no.mgmt.extfreq <- 100*sum(no.mgmt[,6])/dim(no.mgmt)[1]
# boot <- 100*boot_sd_ci(no.mgmt[,6])
# arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
# points(y = no.mgmt.extfreq, x = 0, pch = 17, col = "black")
# abline(h = no.mgmt.extfreq, lty = 2, lwd = 1, col = "black")
# 
# # Without humans?
# no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
# 
# no.hum.extfreq <- 100*sum(no.hum[,6])/dim(no.hum)[1]
# boot <- 100*boot_sd_ci(no.hum[,6])
# arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
# points(y = no.hum.extfreq, x = 0, pch = 23, col = "black")
# abline(h = no.hum.extfreq, lty = 3, lwd = 1, col = "black")

# # legend
# legend("topright",             # Location of legend 
#        # ncol = 2,
#        # xpd = TRUE,                          # Allow drawing outside plot area
#        # xjust = 0,                           # Left justify legend box on x
#        # yjust = .5,                          # Center legend box on y
#        legend = c("No humans",
#                   "No management",
#                   "Null deviation",
#                   "Control", 
#                   "ATI - 0% BB",
#                   "ATI - 100% BB", 
#                   "other BB values"),
#        col = c("black",                 # Legend Element Colors
#                "black",
#                "darkgreen",
#                "black",
#                "blue",
#                "blue",
#                "lightgrey"),          
#        pch = c(23,
#                17,
#                NA_integer_,
#                15,
#                20,
#                21,
#                20),                      # Legend Element Styles          
#        lty = c(3,
#                2,
#                2,
#                1,
#                1,
#                2,
#                1),       
#        cex = 0.7,
#        title = "Strategies",                  # Legend Title
#        title.col = gray(.2),                # Legend title color
#        box.lty = 1,                         # Legend box line type
#        box.lwd = 1)                         # Legend box line width  
# }

#### effect of UT:BB on the users final yield ####

{d <- stat

d$at <- d$at*100
d$bb <- d$bb*100
d$fin_yield <- d$fin_yield/1000
d$fin_yield_95ci_inf <- d$fin_yield_95ci_inf/1000
d$fin_yield_95ci_sup <- d$fin_yield_95ci_sup/1000

# get max, min and average of each UT
upth <- levels(as.factor(d$at))
bubo <- levels(as.factor(d$bb))

sub <- as.data.frame(subset(d, at == upth[1]))
dev <- c(sub$fin_yield, rep(NA,length(upth)-1))
sd <- c(sub$fin_yield_sd, rep(NA,length(upth)-1))
ci_inf <- c(sub$fin_yield_95ci_inf, rep(NA,length(upth)-1))
ci_sup <- c(sub$fin_yield_95ci_sup, rep(NA,length(upth)-1))

for (i in 2:length(upth)) {
  sub <- as.data.frame(subset(d, at == upth[i]))
  dev <- rbind(dev, sub$fin_yield)
  sd <- rbind(sd, sub$fin_yield_sd)
  ci_inf <- rbind(ci_inf, sub$fin_yield_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$fin_yield_95ci_sup)
}

xadj1 <- as.numeric(upth[-1]) - 3;
xadj2 <- as.numeric(upth[-1]) - 1;
xtendrange <- seq(-10,110,1)

par(mar = c(5, 5, 1, 1))
plot(1, type = "n",
     ylab = "Sum of users' final yield (10^3 a.b.u +/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
     ylim = c(20, 40), xlim = c(0, 100), lwd = 2)
polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 

# Put the other budget bonuses in a faint grey to show but de-emphasise
for (i in 3:(dim(dev)[2]-1)) {
  points(x = as.numeric(upth[-1])-2, y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
         col = "grey", pch = 20);
}
abline(h = 40, col = "darkgreen", lty = 2)

arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.05, angle=90, code=3, col = "black")
arrows(xadj1, ci_inf[-1,2], xadj1, ci_sup[-1,2], length=0.05, angle=90, code=3, col = "darkblue")
arrows(xadj2, ci_inf[-1,11], xadj2, ci_sup[-1,11], length=0.05, angle=90, code=3, col = "darkblue")
points(x = xadj1, y = dev[-1,2], xlab = "Update threshold", type = "b", pch = 20,
       cex = 1.5,
       lwd = 2, col = "blue");
points(x = xadj2, y = dev[-1,11], type = "b", cex = 1, lwd = 2, lty = "dashed", col = "blue")
points(x = 0, y = dev[1,1], pch = 15)
abline(h=dev[1,1], lty = 1, col = "black")

# Without management?
no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)

# no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
no.mgmt.finyie <- (sum(no.mgmt[,8])/dim(no.mgmt)[1])/1000
boot <- boot_sd_ci(no.mgmt[,8])/1000
arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
points(y = no.mgmt.finyie, x = 0, pch = 17, col = "black")
# abline(h = no.mgmt.finyie, lty = 2, lwd = 1, col = "black")

# Without humans?
no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)

no.hum.finyie <- sum(no.hum[,8])/dim(no.hum)[1] * 1/1000
boot <- boot_sd_ci(no.hum[,8]) * 1/1000
arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
points(y = no.hum.finyie, x = 0, pch = 23, col = "black")
abline(h = no.hum.finyie, lty = 3, lwd = 1, col = "black")

# legend
legend("bottomright",             # Location of legend 
       # ncol = 2,
       # xpd = TRUE,                          # Allow drawing outside plot area
       # xjust = 0,                           # Left justify legend box on x
       # yjust = .5,                          # Center legend box on y
       legend = c("No humans",
                  "No management",
                  "Maximum yield",
                  "Control", 
                  "ATI - 0% BB",
                  "ATI - 100% BB", 
                  "other BB values"),
       col = c("black",                 # Legend Element Colors
               "black",
               "darkgreen",
               "black",
               "blue",
               "blue",
               "lightgrey"),          
       pch = c(23,
               17,
               NA_integer_,
               15,
               20,
               21,
               20),                      # Legend Element Styles          
       lty = c(3,
               NA_integer_,
               2,
               1,
               2,
               1),       
       cex = 0.7,
       title = "Strategies",                  # Legend Title
       title.col = gray(.2),                # Legend title color
       box.lty = 1,                         # Legend box line type
       box.lwd = 1)                         # Legend box line width  
}


#### Figure to compare ATI strategies against FLI strategy for each management goal ####

OTI_vs_FLI_plot <- function(df, upth, goal = c(0:5), variance = c("sd","ci"), nb_replicates, nemar = FALSE) {
  
  # subsetting
  fli <- subset(df, at == 0)
  oti <- subset(df, at == upth)
  
  trans <- adjustcolor("lightgrey",alpha.f=0.7)
  
  if (goal == 0) {
    
    # get values
    fli_avg <- fli$ext_prob
    oti_avg <- oti$ext_prob
    
    ## macnemar test for comparison of the frequencies for paired samples, for each BB value to the FLI
    if (nemar == TRUE) {
      
      # initiate vector for pvalues
      pv <- NULL
      for (i in 1:length(oti_avg)) {
        # table of contingency
        tc <- matrix(c(fli$ext_prob+oti_avg[i], (1-fli$ext_prob)+oti_avg[i], fli$ext_prob+(1-oti_avg[i]), (1-fli$ext_prob)+(1-oti_avg[i]))*nb_replicates, nrow = 2, ncol = 2)
        if (tc[1,1]>10 & tc[1,2]>10 & tc[2,1]>10 & tc[2,2]>10) {
          pv <- c(pv, mcnemar.test(tc, correct = F)$p.value)
        } else {
          pv <- c(pv, mcnemar.test(tc, correct = T)$p.value)
        }      
      }
      
      # convert p-values in stars
      # initiate a vector for the stars
      st <- rep(NA, length(pv))
      for (i in 1:length(pv)) {
        st[i] <- ""
        if(pv[i] < 0.05 & pv[i] >= 0.01) {
          st[i] <- "*"
        }
        if (pv[i] < 0.01 & pv[i] >= 0.001) {
          st[i] <- "**"
        }
        if (pv[i] < 0.001) {
          st[i] <- "***"
        }
      }
    }
    
    if (variance == "sd") {
      
      # FLI strat
      fli_sd <- fli$ext_prob_sd
      
      # OTI strat
      oti_sd <- oti$ext_prob_sd
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110,1)
      flimax <- rep(fli_avg+fli_sd, length(xtendrange))
      flimin <- rep(fli_avg-fli_sd, length(xtendrange))
      xoti <- oti$bb*100
      
      # diag <- barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
      #                 xlab = "Budget bonus\n(in % of initial budget)", ylab = paste("Extinction frequency (N =", nb_replicates,")"), ylim = c(0, max(fli$ext_prob, max(oti_avg))+0.1)) 
      #                 # ylim = c(0,1), xlim = c(0,100)) # ,
      #                 # main = paste("UT = ", upth*100,"%"))
      # abline(h = fli$ext_prob, lty = 1, lwd = 2, col = "black")
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Extinction frequency (+/- SD)",
           ylim = c(0,1), xlim = c(0,100)) # , max(max(fli_avg+fli_sd),max(oti_avg+oti_sd))
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h=fli_avg, lwd=2)
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      # arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "blue")
      arrows(xoti, oti_avg-oti_sd, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "darkblue")
      
      if (nemar == TRUE) {
        # add the stars above the bars
        text(diag,oti_avg+oti_sd+0.05,as.character(st),cex=1)
      }
    }  
    
    if (variance == "ci") {
      
      # FLI strat
      fli_ci_inf <- fli$ext_prob_95ci_inf
      fli_ci_sup <- fli$ext_prob_95ci_sup
      
      # OTI strat
      oti_ci_inf <- oti$ext_prob_95ci_inf
      oti_ci_sup <- oti$ext_prob_95ci_sup
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_ci_sup, length(xtendrange))
      flimin <- rep(fli_ci_inf, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # diag <- barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
      #                 xlab = "Budget bonus\n(in % of initial budget)", ylab = paste("Extinction frequency (N =", nb_replicates,")"), ylim = c(0, max(fli$ext_prob, max(oti_avg))+0.1)) 
      #                 # ylim = c(0,1), xlim = c(0,100)) # ,
      #                 # main = paste("UT = ", upth*100,"%"))
      # abline(h = fli$ext_prob, lty = 1, lwd = 2, col = "black")
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Extinction frequency (+/- 95CI)",
           ylim = c(0, 1), xlim = c(0,100)) # ,max(fli_ci_sup,max(oti_ci_sup)
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h=fli_avg, lwd=2)
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      # arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "blue")
      arrows(xoti, oti_ci_inf, xoti, oti_ci_sup, length=0.05, angle=90, code=3, col = "darkblue")
      
      if (nemar == TRUE) {
        # add the stars above the bars
        text(diag,oti_avg+oti_sd+0.05,as.character(st),cex=1)
      }
    }  
  }
  
  if (goal == 1) {
    
    # get means, sd, and 95ci and plot
    fli_avg <- fli$act_dev*100
    oti_avg <- oti$act_dev*100
    
    if (variance == "sd") {
      
      # FLI strat
      fli_sd <- fli$act_dev_sd*100
      # # prevent the sd range to go over the borders
      # fli_sd_neg <- ifelse(test = fli_avg-fli_sd < -100, fli_sd+(fli_avg-fli_sd+100), fli_sd)
      
      # OTI strat
      oti_sd <- oti$act_dev_sd*100
      # # prevent the sd range to go over the borders
      # oti_sd_neg <- ifelse(test = oti_avg-oti_sd < -100, oti_sd+(oti_avg-oti_sd+100), oti_sd)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_avg+fli_sd, length(xtendrange))
      # flimin <- rep(fli_avg-fli_sd_neg, length(xtendrange))
      flimin <- rep(fli_avg-fli_sd, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Resource population deviation from MT\n(in %, mean +/- SD)",
           ylim = c(-100,ifelse(max(max(fli_avg+fli_sd)+10,max(oti_avg+oti_sd)+10)<0, 0, max(max(fli_avg+fli_sd)+10,max(oti_avg+oti_sd)+10))), xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 0, lty = 2, lwd = 1.5, col = "darkgreen")
      abline(h = -100, lty = 2, lwd = 1.2, col = "red")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      # arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "blue")
      arrows(xoti, oti_avg-oti_sd, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "darkblue")
      
    }
    
    if (variance == "ci") {
      
      # FLI strat
      # fli_95ci <- fli$act_dev_95ci*100
      # # prevent the 95ci range to go over the borders
      # fli_95ci_neg <- ifelse(test = fli_avg-fli_95ci < -100, fli_95ci+(fli_avg-fli_95ci+100), fli_95ci)
      fli_95ci_inf <- fli$act_dev_95ci_inf*100
      fli_95ci_sup <- fli$act_dev_95ci_sup*100
      
      # OTI strat
      # oti_95ci <- oti$act_dev_95ci*100
      # # prevent the 95ci range to go over the borders
      # oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < -100, oti_95ci+(oti_avg-oti_95ci+100), oti_95ci)
      oti_95ci_inf <- oti$act_dev_95ci_inf*100
      oti_95ci_sup <- oti$act_dev_95ci_sup*100
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_95ci_sup, length(xtendrange))
      flimin <- rep(fli_95ci_inf, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Resource population deviation from MT\n(in %, mean +/- 95CI)",
           ylim = c(-100,ifelse(max(max(fli_95ci_sup)+5,max(oti_95ci_sup)+5)<0, 0, max(max(fli_95ci_sup)+5,max(oti_95ci_sup)+5))), xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 0, lty = 2, lwd = 1.5, col = "darkgreen")
      abline(h = -100, lty = 2, lwd = 1.2, col = "red")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_95ci_inf, xoti, oti_95ci_sup, length=0.05, angle=90, code=3, col = "darkblue")
    }
  }
  
  if (goal == 2) {
    
    # get means, sd, and 95ci and plot
    fli_avg <- fli$fin_yield/100
    oti_avg <- oti$fin_yield/100
    
    if (variance == "sd") {
      
      # FLI strat
      fli_sd <- fli$fin_yield_sd/100
      # # prevent the sd range to go over the borders
      # fli_sd_neg <- ifelse(test = fli_avg-fli_sd < 0, fli_sd+(fli_avg-fli_sd), fli_sd)
      # fli_sd_pos <- ifelse(test = fli_avg+fli_sd > 100, fli_sd-(fli_avg+fli_sd-100), fli_sd)
      
      # OTI strat
      oti_sd <- oti$fin_yield_sd/100
      # # prevent the sd range to go over the borders
      # oti_sd_neg <- ifelse(test = oti_avg-oti_sd < 0, oti_sd+(oti_avg-oti_sd+100), oti_sd)
      # oti_sd_pos <- ifelse(test = oti_avg+oti_sd > 100, oti_sd-(oti_avg+oti_sd-100), oti_sd)
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      # flimax <- rep(fli_avg+fli_sd_pos, length(xtendrange))
      # flimin <- rep(fli_avg-fli_sd_neg, length(xtendrange))
      flimax <- rep(fli_avg+fli_sd, length(xtendrange))
      flimin <- rep(fli_avg-fli_sd, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Sum of Users final budgets\n(in k-a.b.u, mean +/- SD)",
           ylim = c(min(min(fli_avg+fli_sd)-5,min(oti_avg+oti_sd))-5,400), xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 400, lty = 2, lwd = 1.5, col = "darkgreen")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      # arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd_pos, length=0.05, angle=90, code=3, col = "blue")
      arrows(xoti, oti_avg-oti_sd, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "darkblue")
    }
    
    if (variance == "ci") {
      
      # FLI strat
      # fli_95ci <- fli$fin_yield_95ci/100
      # # prevent the 95ci range to go over the borders
      # fli_95ci_neg <- ifelse(test = fli_avg-fli_95ci < 0, fli_95ci+(fli_avg-fli_95ci), fli_95ci)
      # fli_95ci_pos <- ifelse(test = fli_avg+fli_95ci > 100, fli_95ci-(fli_avg+fli_95ci-100), fli_95ci)
      fli_95ci_inf <- fli$fin_yield_95ci_inf/100
      fli_95ci_sup <- fli$fin_yield_95ci_sup/100
      
      # OTI strat
      # oti_95ci <- oti$fin_yield_95ci/100
      # # prevent the 95ci range to go over the borders
      # oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < 0, oti_95ci+(oti_avg-oti_95ci), oti_95ci)
      # oti_95ci_pos <- ifelse(test = oti_avg+oti_95ci > 100, oti_95ci-(oti_avg+oti_95ci-100), oti_95ci)
      oti_95ci_inf <- oti$fin_yield_95ci_inf/100
      oti_95ci_sup <- oti$fin_yield_95ci_sup/100
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      flimax <- rep(fli_95ci_sup, length(xtendrange))
      flimin <- rep(fli_95ci_inf, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Sum of Users final budgets\n(in k-a.b.u, mean +/- 95CI)",
           ylim = c(min(min(fli_95ci_inf)-2,min(oti_95ci_inf))-2,400), xlim = c(0,100)) # , 
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 400, lty = 2, lwd = 1.5, col = "darkgreen")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      arrows(xoti, oti_95ci_inf, xoti, oti_95ci_sup, length=0.05, angle=90, code=3, col = "darkblue")
    }
  }
  
  if (goal == 3) {
    
    # get means, sd, and 95ci and plot
    fli_avg <- fli$max_diff_yield*100
    oti_avg <- oti$max_diff_yield*100
    
    if (variance == "sd") {
      
      # FLI strat
      fli_sd <- fli$max_diff_yield_sd*100
      # # prevent the sd range to go over the borders
      # fli_sd_neg <- ifelse(test = fli_avg-fli_sd < 0, fli_sd+(fli_avg-fli_sd), fli_sd)
      # fli_sd_pos <- ifelse(test = fli_avg+fli_sd > 100, fli_sd-(fli_avg+fli_sd-100), fli_sd)
      
      # OTI strat
      oti_sd <- oti$max_diff_yield_sd*100
      # # prevent the sd range to go over the borders
      # oti_sd_neg <- ifelse(test = oti_avg-oti_sd < 0, oti_sd+(oti_avg-oti_sd), oti_sd)
      # oti_sd_pos <- ifelse(test = oti_avg+oti_sd > 100, oti_sd-(oti_avg+oti_sd-100), oti_sd)
      
      # plotting
      xrange <- seq(0,100,10)
      # xtendrange <- seq(-10,110)
      # flimax <- rep(fli_avg+fli_sd_pos, length(xtendrange))
      # flimin <- rep(fli_avg-fli_sd_neg, length(xtendrange))
      flimax <- rep(fli_avg+fli_sd, length(xtendrange))
      flimin <- rep(fli_avg-fli_sd, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Maximum difference between Users yields\n(in % of the highest yield, mean +/- SD)",
           ylim = c(0,max(max(fli_avg+fli_sd),max(oti_avg+oti_sd))+5), xlim = c(0,100)) #,
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 0, lty = 2, lwd = 1.5, col = "darkgreen")
      
      # points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      # arrows(xoti, oti_avg-oti_sd_neg, xoti, oti_avg+oti_sd_pos, length=0.05, angle=90, code=3, col = "blue")
      arrows(xoti, oti_avg-oti_sd, xoti, oti_avg+oti_sd, length=0.05, angle=90, code=3, col = "darkblue")
    }
    
    if (variance == "ci") {
      
      # FLI strat
      # fli_95ci <- fli$max_diff_yield_95ci*100
      # # prevent the 95ci range to go over the borders
      # fli_95ci_neg <- ifelse(test = fli_avg-fli_95ci < 0, fli_95ci+(fli_avg-fli_95ci), fli_95ci)
      # fli_95ci_pos <- ifelse(test = fli_avg+fli_95ci > 100, fli_95ci-(fli_avg+fli_95ci-100), fli_95ci)
      fli_95ci_inf <- fli$max_diff_yield_95ci_inf*100
      fli_95ci_sup <- fli$max_diff_yield_95ci_sup*100
      
      # OTI strat
      # oti_95ci <- oti$max_diff_yield_95ci*100
      # # prevent the 95ci range to go over the borders
      # oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < 0, oti_95ci+(oti_avg-oti_95ci), oti_95ci)
      # oti_95ci_pos <- ifelse(test = oti_avg+oti_95ci > 100, oti_95ci-(oti_avg+oti_95ci-100), oti_95ci)
      oti_95ci_inf <- oti$max_diff_yield_95ci_inf*100
      oti_95ci_sup <- oti$max_diff_yield_95ci_sup*100
      
      # plotting
      xrange <- seq(0,100,10)
      xtendrange <- seq(-10,110)
      # flimax <- rep(fli_avg+fli_95ci_pos, length(xtendrange))
      # flimin <- rep(fli_avg-fli_95ci_neg, length(xtendrange))
      flimax <- rep(fli_95ci_sup, length(xtendrange))
      flimin <- rep(fli_95ci_inf, length(xtendrange))
      trans <- adjustcolor("lightgrey",alpha.f=0.7)
      xoti <- oti$bb*100
      
      # plot base
      plot(1, type = "n", xlab = "Budget bonus\n(in % of initial budget)", ylab = "Maximum difference between Users yields\n(in % of the highest yield, mean +/- 95CI)",
           # ylim = c(0,max(max(fli_avg+fli_sd),max(oti_avg+oti_sd))+5), xlim = c(0,100)) # ,
           ylim = c(0,max(max(fli_95ci_sup)+1,max(oti_95ci_sup))+1), xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      polygon(c(xtendrange,rev(xtendrange)),c(flimax,rev(flimin)),col=trans, border = "grey")
      abline(h = fli_avg, lwd = 2)
      abline(h = 0, lty = 2, lwd = 1.5, col = "darkgreen")
      
      points(y = oti_avg, x = xoti, pch = 16, col = "blue")
      # arrows(xoti, oti_avg-oti_95ci_neg, xoti, oti_avg+oti_95ci_pos, length=0.05, angle=90, code=3, col = "blue")
      arrows(xoti, oti_95ci_inf, xoti, oti_95ci_sup, length=0.05, angle=90, code=3, col = "darkblue")
    }
  }
  
  if (goal == 4) {
    
    # get means, sd, 95ci and plot
    oti_avg <- oti$inac_ts*100
    
    if (variance == "sd") {
      
      # OTI strat
      oti_sd <- oti$inac_ts_sd*100
      # # prevent the sd range to go over the borders
      # oti_sd_neg <- ifelse(test = oti_avg-oti_sd < 0, oti_sd+(oti_avg-oti_sd), oti_sd)
      # oti_sd_pos <- ifelse(test = oti_avg+oti_sd > 100, oti_sd-(oti_avg+oti_sd-100), oti_sd)
      
      # plotting
      xoti <- oti$bb*100
      
      # barplot
      diag = barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
                     xlab = "Budget bonus\n(in % of initial budget)", ylab = "Time steps without updating\n(in %, mean +/- SD)",
                     ylim = c(0,max(oti_avg+oti_sd))) # ,
      # ylim = c(0,max(oti_avg+oti_sd_pos))) # ,
      # ylim = c(0,100), xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      # arrows(diag, oti_avg-oti_sd_neg, diag, oti_avg+oti_sd_pos, length=0.03, angle=90, code=3, col = "black")
      arrows(diag, oti_avg-oti_sd, diag, oti_avg+oti_sd, length=0.03, angle=90, code=3, col = "black")
    }
    
    if (variance == "ci") {
      
      # OTI strat
      # oti_95ci <- oti$inac_ts_95ci*100
      # # prevent the 95ci range to go over the borders
      # oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < 0, oti_95ci+(oti_avg-oti_95ci), oti_95ci)
      # oti_95ci_pos <- ifelse(test = oti_avg+oti_95ci > 100, oti_95ci-(oti_avg+oti_95ci-100), oti_95ci)
      oti_95ci_inf <- oti$inac_ts_95ci_inf*100
      oti_95ci_sup <- oti$inac_ts_95ci_sup*100
      
      # plotting
      xoti <- oti$bb*100
      
      # barplot
      diag = barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
                     xlab = "Budget bonus\n(in % of initial budget)", ylab = "Time steps without updating\n(in %, mean +/- 95CI)",
                     ylim = c(0,max(oti_95ci_sup)))
      # ylim = c(0,max(oti_avg+oti_95ci_pos))) 
      # , xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      arrows(diag, oti_95ci_inf, diag, oti_95ci_sup, length=0.03, angle=90, code=3, col = "black")
      # arrows(diag, oti_avg-oti_95ci_neg, diag, oti_avg+oti_95ci_pos, length=0.03, angle=90, code=3, col = "black")
    }
  }
  
  if (goal == 5) {
    
    # get means, sd, 95ci and plot
    oti_avg <- oti$SumAbsDev/10^6
    
    if (variance == "sd") {
      
      # OTI strat
      oti_sd <- oti$SumAbsDev_sd/10^6
      # # prevent the sd range to go over the borders
      # oti_sd_neg <- ifelse(test = oti_avg-oti_sd < 0, oti_sd+(oti_avg-oti_sd), oti_sd)
      # oti_sd_pos <- ifelse(test = oti_avg+oti_sd > 100, oti_sd-(oti_avg+oti_sd-100), oti_sd)
      
      # plotting
      xoti <- oti$bb*100
      
      # barplot
      diag = barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
                     xlab = "Budget bonus\n(in % of initial budget)", ylab = "Sum of sq. deviation from target\n(ind.10^6 +/- SD)",
                     ylim = c(0,max(oti_avg+oti_sd))) # ,
      # ylim = c(0,max(oti_avg+oti_sd_pos))) # ,
      # ylim = c(0,100), xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      # arrows(diag, oti_avg-oti_sd_neg, diag, oti_avg+oti_sd_pos, length=0.03, angle=90, code=3, col = "black")
      arrows(diag, oti_avg-oti_sd, diag, oti_avg+oti_sd, length=0.03, angle=90, code=3, col = "black")
    }
    
    if (variance == "ci") {
      
      # OTI strat
      # oti_95ci <- oti$inac_ts_95ci*100
      # # prevent the 95ci range to go over the borders
      # oti_95ci_neg <- ifelse(test = oti_avg-oti_95ci < 0, oti_95ci+(oti_avg-oti_95ci), oti_95ci)
      # oti_95ci_pos <- ifelse(test = oti_avg+oti_95ci > 100, oti_95ci-(oti_avg+oti_95ci-100), oti_95ci)
      oti_95ci_inf <- oti$SumAbsDev_95ci_inf/10^6
      oti_95ci_sup <- oti$SumAbsDev_95ci_sup/10^6
      
      # plotting
      xoti <- oti$bb*100
      
      # barplot
      diag = barplot(oti_avg, col = "lightblue", space = 1, width = 4, names.arg = xoti,
                     xlab = "Budget bonus\n(in % of initial budget)", ylab = "Sum of sq. deviation from target\n (ind.10^6 +/- 95%CI)",
                     ylim = c(0,max(oti_95ci_sup)))
      # ylim = c(0,max(oti_avg+oti_95ci_pos))) 
      # , xlim = c(0,100)) # ,
      # main = paste("UT = ", upth*100,"%"))
      arrows(diag, oti_95ci_inf, diag, oti_95ci_sup, length=0.03, angle=90, code=3, col = "black")
      # arrows(diag, oti_avg-oti_95ci_neg, diag, oti_avg+oti_95ci_pos, length=0.03, angle=90, code=3, col = "black")
    }
  }
}

#### Compile results of the comparison between the FLI and ATI strategies ####

OTI_diagnostic <- function(df, upth, variance = c("sd", "ci"), nb_replicates, omit.extinction = FALSE) {
  # divide into four boxes
  layout(matrix(c(1,2,3,4), nrow = 2), widths = c(3,3))
  
  # set space for a title
  par(oma = c(0, 0, 3, 0))
  #layout.show(n = 4)
  
  # upper left: extinction frequency (goal 0)
  OTI_vs_FLI_plot(df, upth, goal = 0, variance, nb_replicates)
  # lower left: time steps without intervention
  OTI_vs_FLI_plot(df, upth, goal = 4, variance, nb_replicates)
  # upper right: Resource population deviation from MT (goal 1)
  OTI_vs_FLI_plot(df, upth, goal = 1, variance, nb_replicates)
  # lower right: sum of Users final yield
  OTI_vs_FLI_plot(df, upth, goal = 2, variance, nb_replicates)
  
  # Global title
  # mtext(paste("UT =", upth*100, "%"), outer = TRUE, cex = 1, line = 1)
  mtext(paste("UT =", upth*100, "% -", ifelse(omit.extinction == FALSE,"including", "excluding"), "extinctions"), outer = TRUE, cex = 1, line = 1.5)
  
}

# Examples
OTI_diagnostic(df = stat, upth = 0.1, variance = "ci", nb_replicates = 100, omit.extinction = F)
OTI_diagnostic(df = woe_stat, upth = 0.3, variance = "ci", nb_replicates = 100, omit.extinction = T)

#### Plot the evolution of population, costs and actions in FLI and OTI strategy ####

multi <- function(df, upth, bubo, tmax, yaxis) {
  # subsetting
  dd <- subset(df, UT == upth & BB == bubo) 
  
  # plot
  xrange <- seq(1, tmax, 1)
  plot(1, type = "n", xlab = "time", ylab = yaxis, ylim = c(0,max(dd[9:dim(dd)[2]])), xlim = c(0,20)) # , main = paste("UT = ", upth, " BB = ",bubo)
  
  # max with FLI strategy
  if (str_detect(yaxis, "Cost")){
    abline(h = (dd[1,2]/10)+10, lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "budget")){
    abline(h = dd[1,2], lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "action")){
    abline(h = dd[1,2]/10, lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "population")){
    xtendrange <- seq(-1,tmax+1,1)
    trans <- adjustcolor("lightgreen",alpha.f=0.5)
    upperUT <- rep((1+upth)*dd[1,7], length(xtendrange))
    lowerUT <- rep((1-upth)*dd[1,7], length(xtendrange))
    
    polygon(c(xtendrange,rev(xtendrange)),c(upperUT,rev(lowerUT)),col=trans, border = "green")
    abline(h = dd[1,7], col = "darkgreen", lty = 2)
  }
  
  # all the trajectories
  for (i in 1:(dim(dd)[1])) {
    points(x = xrange, y = dd[i,9:dim(dd)[2]], type = "b", pch = i, col = "black")
  }
}

  plot_ATI_diag <- function(upd_thr, bud_bon) {
  
  layout(matrix(c(1,2,3,4,5,6), nrow = 3), widths = c(4,4))
  # layout.show(n = 3)
  # set space for a title
  # par(oma = c(0, 0, 3, 0))
  
  multi(we_popul, upth = 0, bubo = 0, tmax = 20, yaxis = "population")
  multi(we_costs, upth = 0, bubo = 0, tmax = 20, yaxis = "Costs")
  # multi(we_actio, upth = 0, bubo = 0, tmax = 20, yaxis = "actions")
  multi(we_budge, upth = 0, bubo = 0, tmax = 20, yaxis = "budget")
  multi(we_popul, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "population")
  multi(we_costs, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "Costs")
  # multi(we_actio, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "actions")
  multi(we_budge, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "budget")
  
  mtext(paste("FLI VS UT =", upd_thr*100, "% - BB =", bud_bon*100, "%"), outer = TRUE, cex = 1, line = 1.5)
}

# example
plot_ATI_diag(upd_thr = 0.3, bud_bon = 0.1)
 