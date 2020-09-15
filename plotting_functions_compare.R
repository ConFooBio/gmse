# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy updating timing with a fictional case
# 4 strategies: default, memory of previous observation of population size, bonus reset only after raisong the costs, a combination of the latter two

#### libraries ####
library("colorspace")
library("ggplot2")
library("stringr")

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

#### import data ####

path <- "Desktop/PhD/GitKraken/gmse_fork_RQ1/"
setwd(dir = path)

# instant observation

ins_stat <- read.csv("stats-NORESET-SAD.csv")[,-1]
ins_woestat <- read.csv("woe-stats-NORESET-SAD.csv")[,-1]

# evolution of some replicates along simulation time
ins_costs <- read.csv("noreset-results-save/noreset-merged-results/cos-ATI-noreset-merged.csv")[,-1]
ins_popul <- read.csv("noreset-results-save/noreset-merged-results/pop-ATI-noreset-merged.csv")[,-1]
ins_actio <- read.csv("noreset-results-save/noreset-merged-results/act-ATI-noreset-merged.csv")[,-1]
ins_budge <- read.csv("noreset-results-save/noreset-merged-results/bgt-ATI-noreset-merged.csv")[,-1]

# only without extinction
ins_wecosts <- subset(ins_costs, Extinct == 0)
ins_wepopul <- subset(ins_popul, Extinct == 0)
ins_weactio <- subset(ins_actio, Extinct == 0)
ins_webudge <- subset(ins_budge, Extinct == 0)

# memory

mem_stat <- read.csv("stats-MEMORY-SAD.csv")[,-1]
mem_woestat <- read.csv("woe-stats-MEMORY-SAD.csv")[,-1]

# evolution of some replicates along simulation time
mem_costs <- read.csv("mem-noreset-results/mem-noreset-merged-results/cos-ATI-mem-noreset-merged.csv")[,-1]
mem_popul <- read.csv("mem-noreset-results/mem-noreset-merged-results/pop-ATI-mem-noreset-merged.csv")[,-1]
mem_actio <- read.csv("mem-noreset-results/mem-noreset-merged-results/act-ATI-mem-noreset-merged.csv")[,-1]
mem_budge <- read.csv("mem-noreset-results/mem-noreset-merged-results/bgt-ATI-mem-noreset-merged.csv")[,-1]

# only without extinction
mem_wecosts <- subset(mem_costs, Extinct == 0)
mem_wepopul <- subset(mem_popul, Extinct == 0)
mem_weactio <- subset(mem_actio, Extinct == 0)
mem_webudge <- subset(mem_budge, Extinct == 0)

#### effect of UT:BB on extinction frequency ####

{d <- ins_stat
  
  d$at <- d$at*100
  
  # get max, min and average of each UT
  upth <- levels(as.factor(d$at))
  bubo <- levels(as.factor(d$bb))
  
  sub <- as.data.frame(subset(d, at == upth[1]))
  ext <- c(sub$ext_prob, rep(NA,length(upth)-1))
  sd <- c(sub$ext_prob_sd, rep(NA,length(upth)-1))
  ci_inf <- c(sub$ext_prob_95ci_inf, rep(NA,length(upth)-1))
  ci_sup <- c(sub$ext_prob_95ci_sup, rep(NA,length(upth)-1))
  
  for (i in 2:length(upth)) {
    sub <- as.data.frame(subset(d, at == upth[i]))
    ext <- rbind(ext, sub$ext_prob)
    sd <- rbind(sd, sub$ext_prob_sd)
    ci_inf <- rbind(ci_inf, sub$ext_prob_95ci_inf)
    ci_sup <- rbind(ci_sup, sub$ext_prob_95ci_sup)
  }
  
  xadj1 <- as.numeric(upth[-1]) - 1;
  xadj2 <- as.numeric(upth[-1]) + 1;
  xtendrange <- seq(-10,110,1)
  
  par(mar = c(5, 5, 1, 1))
  plot(1, xlab = "Update threshold", type = "n", pch = 20,
       ylab = "Extinction frequency", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
       ylim = c(0, 1), xlim = c(0, 100), lwd = 2, col = "blue")
  polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
  
  # Put the other budget bonuses in a faint grey to show but de-emphasise
  for (i in 3:(dim(ext)[2]-1)) {
    points(x = as.numeric(upth[-1]), y = ext[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
           col = "grey", pch = 20);
  }
  
  arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.05, angle=90, code=3, col = "black")
  arrows(xadj1, ci_inf[-1,2], xadj1, ci_sup[-1,2], length=0.05, angle=90, code=3, col = "darkblue")
  arrows(xadj2, ci_inf[-1,11], xadj2, ci_sup[-1,11], length=0.05, angle=90, code=3, col = "darkblue")
  points(x = xadj1, y = ext[-1,2], xlab = "Update threshold", type = "b", pch = 20,
         ylab = "Extinction frequency (+/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
         ylim = c(0, 1), xlim = c(0, 100), lwd = 2, col = "blue");
  points(x = xadj2, y = ext[-1,11], type = "b", cex = 1, lwd = 2, lty = "dashed", col = "blue")
  points(x = 0, y = ext[1,1], pch = 15)
  abline(h=ext[1,1], lty = 1, col = "black")
  # Without management?
  no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  
  # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
  no.mgmt.extfreq <- sum(no.mgmt[,5])/dim(no.mgmt)[1]
  points(y = no.mgmt.extfreq, x = 0, pch = 17, col = "black")
  abline(h = no.mgmt.extfreq, lty = 2, lwd = 1, col = "black")
  
  # Without humans?
  no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  
  no.hum.extfreq <- sum(no.hum[,5])/dim(no.hum)[1]
  points(y = no.hum.extfreq, x = 0, pch = 23, col = "black")
  abline(h = no.hum.extfreq, lty = 3, lwd = 1, col = "black")
  
  # now for the other strategy
  
  d <- mem_stat
  
  d$at <- d$at*100
  
  sub <- NULL
  ext <- NULL
  sd <- NULL
  ci_inf <- NULL
  ci_sup <- NULL
  
  for (i in 2:length(upth)) {
    sub <- as.data.frame(subset(d, at == upth[i]))
    ext <- rbind(ext, sub$ext_prob)
    sd <- rbind(sd, sub$ext_prob_sd)
    ci_inf <- rbind(ci_inf, sub$ext_prob_95ci_inf)
    ci_sup <- rbind(ci_sup, sub$ext_prob_95ci_sup)
  }
  
  # Put the other budget bonuses in a faint grey to show but de-emphasise
  for (i in 2:(dim(ext)[2]-1)) {
    points(x = as.numeric(upth[-1]), y = ext[,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
           col = "grey", pch = 20);
  }
  
  arrows(xadj1, ci_inf[,2], xadj1, ci_sup[,2], length=0.05, angle=90, code=3, col = "darkred")
  arrows(xadj2, ci_inf[,10], xadj2, ci_sup[,10], length=0.05, angle=90, code=3, col = "darkred")
  points(x = xadj1, y = ext[,2], xlab = "Update threshold", type = "b", pch = 4,
         ylab = "Extinction frequency (+/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1,
         ylim = c(0, 1), xlim = c(0, 100), lwd = 2, col = "darkred");
  points(x = xadj2, y = ext[,10], type = "b", pch = 4, cex = 1, lwd = 2, lty = "dashed", col = "darkred")
}

#### effect of UT:BB on poplation deviation from target ####

{d <- ins_stat

d$at <- d$at*100
d$bb <- d$bb*100
d$act_dev <- d$act_dev*100
d$act_dev_95ci_inf <- d$act_dev_95ci_inf*100
d$act_dev_95ci_sup <- d$act_dev_95ci_sup*100

# get max, min and average of each UT
upth <- levels(as.factor(d$at))
bubo <- levels(as.factor(d$bb))

sub <- as.data.frame(subset(d, at == upth[1]))
dev <- c(sub$act_dev, rep(NA,length(upth)-1))
sd <- c(sub$act_dev_sd, rep(NA,length(upth)-1))
ci_inf <- c(sub$act_dev_95ci_inf, rep(NA,length(upth)-1))
ci_sup <- c(sub$act_dev_95ci_sup, rep(NA,length(upth)-1))

for (i in 2:length(upth)) {
  sub <- as.data.frame(subset(d, at == upth[i]))
  dev <- rbind(dev, sub$act_dev)
  sd <- rbind(sd, sub$act_dev_sd)
  ci_inf <- rbind(ci_inf, sub$act_dev_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$act_dev_95ci_sup)
  
  # # Without management?
  # no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
  # no.mgmt.extfreq <- sum(no.mgmt[,5])/dim(no.mgmt)[1]
  # points(y = no.mgmt.extfreq, x = 0, pch = 17, col = "black")
  # abline(h = no.mgmt.extfreq, lty = 2, lwd = 1, col = "black")
  # 
  # # Without humans?
  # no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # no.hum.extfreq <- sum(no.hum[,5])/dim(no.hum)[1]
  # points(y = no.hum.extfreq, x = 5, pch = 23, col = "black")
  # abline(h = no.hum.extfreq, lty = 3, lwd = 1, col = "black")
}

xadj1 <- as.numeric(upth[-1]) - 3;
xadj2 <- as.numeric(upth[-1]) - 1;
xtendrange <- seq(-10,110,1)

par(mar = c(5, 5, 1, 1))
plot(1, xlab = "Update threshold", type = "n", pch = 20,
     ylab = "Population deviation from target", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
     ylim = c(-100, 100), xlim = c(0, 100), lwd = 2, col = "blue")
polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 

# # Put the other budget bonuses in a faint grey to show but de-emphasise
# for (i in 3:(dim(dev)[2]-1)) {
#   points(x = as.numeric(upth[-1])-2, y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
#          col = "grey", pch = 20);
# }
abline(h = 0, col = "darkgreen", lty = 2)

arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.05, angle=90, code=3, col = "black")
arrows(xadj1, ci_inf[-1,2], xadj1, ci_sup[-1,2], length=0.05, angle=90, code=3, col = "darkblue")
arrows(xadj2, ci_inf[-1,11], xadj2, ci_sup[-1,11], length=0.05, angle=90, code=3, col = "darkblue")
points(x = xadj1, y = dev[-1,2], xlab = "Update threshold", type = "b", pch = 20,
       ylab = "Population deviation from target (+/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
       ylim = c(0, 1), xlim = c(0, 100), lwd = 2, col = "blue");
points(x = xadj2, y = dev[-1,11], type = "b", cex = 1, lwd = 2, lty = "dashed", col = "blue")
points(x = 0, y = dev[1,1], pch = 15)
abline(h=dev[1,1], lty = 1, col = "black")

# Without management?
no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)

# no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
no.mgmt.extfreq <- 100*sum(no.mgmt[,6])/dim(no.mgmt)[1]
boot <- 100*boot_sd_ci(no.mgmt[,6])
arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
points(y = no.mgmt.extfreq, x = 0, pch = 17, col = "black")
abline(h = no.mgmt.extfreq, lty = 2, lwd = 1, col = "black")

# Without humans?
no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)

no.hum.extfreq <- 100*sum(no.hum[,6])/dim(no.hum)[1]
boot <- 100*boot_sd_ci(no.hum[,6])
arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
points(y = no.hum.extfreq, x = 0, pch = 23, col = "black")
abline(h = no.hum.extfreq, lty = 3, lwd = 1, col = "black")

# now for the other strategy

d <- mem_stat

d$at <- d$at*100
d$bb <- d$bb*100
d$act_dev <- d$act_dev*100
d$act_dev_95ci_inf <- d$act_dev_95ci_inf*100
d$act_dev_95ci_sup <- d$act_dev_95ci_sup*100

sub <- NULL
dev <- NULL
sd <- NULL
ci_inf <- NULL
ci_sup <- NULL

for (i in 2:length(upth)) {
  sub <- as.data.frame(subset(d, at == upth[i]))
  dev <- rbind(dev, sub$act_dev)
  sd <- rbind(sd, sub$act_dev_sd)
  ci_inf <- rbind(ci_inf, sub$act_dev_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$act_dev_95ci_sup)
}

xadj1 <- as.numeric(upth[-1]) + 1;
xadj2 <- as.numeric(upth[-1]) + 3;
xtendrange <- seq(-10,110,1)

# # Put the other budget bonuses in a faint grey to show but de-emphasise
# for (i in 2:(dim(dev)[2]-1)) {
#   points(x = as.numeric(upth[-1])+2, y = dev[,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
#          col = "grey", pch = 20);
# }

arrows(xadj1, ci_inf[,2], xadj1, ci_sup[,2], length=0.05, angle=90, code=3, col = "darkred")
arrows(xadj2, ci_inf[,10], xadj2, ci_sup[,10], length=0.05, angle=90, code=3, col = "darkred")
points(x = xadj1, y = dev[,2], xlab = "Update threshold", type = "b", pch = 4,
       ylab = "Population deviation from target in % (+/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1,
       ylim = c(0, 1), xlim = c(0, 100), lwd = 2, col = "darkred");
points(x = xadj2, y = dev[,10], type = "b", pch = 4, cex = 1, lwd = 2, lty = "dashed", col = "darkred")
}

#### effect of UT:BB on the users final yield ####

{d <- ins_stat

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
  
  # # Without management?
  # no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
  # no.mgmt.finyie <- sum(no.mgmt[,8])/dim(no.mgmt)[1]
  # points(y = no.mgmt.extfreq, x = 0, pch = 17, col = "black")
  # abline(h = no.mgmt.extfreq, lty = 2, lwd = 1, col = "black")
  # 
  # # Without humans?
  # no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # no.hum.finyie <- sum(no.hum[,8])/dim(no.hum)[1]
  # points(y = no.hum.extfreq, x = 5, pch = 23, col = "black")
  # abline(h = no.hum.extfreq, lty = 3, lwd = 1, col = "black")
}

xadj1 <- as.numeric(upth[-1]) - 3;
xadj2 <- as.numeric(upth[-1]) - 1;
xtendrange <- seq(-10,110,1)

par(mar = c(5, 5, 1, 1))
plot(1, xlab = "Update threshold", type = "n", pch = 20,
     ylab = "Sum of users' final yield (10^3 a.b.u +/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
     ylim = c(20, 40), xlim = c(0, 100), lwd = 2, col = "blue")
polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 

# # Put the other budget bonuses in a faint grey to show but de-emphasise
# for (i in 3:(dim(dev)[2]-1)) {
#   points(x = as.numeric(upth[-1])-2, y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
#          col = "grey", pch = 20);
# }
# abline(h = 0, col = "darkgreen", lty = 2)

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
abline(h = no.mgmt.finyie, lty = 2, lwd = 1, col = "black")

# Without humans?
no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)

no.hum.finyie <- sum(no.hum[,8])/dim(no.hum)[1] * 1/1000
boot <- boot_sd_ci(no.hum[,8]) * 1/1000
arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
points(y = no.hum.finyie, x = 0, pch = 23, col = "black")
abline(h = no.hum.finyie, lty = 3, lwd = 1, col = "black")

# now for the other strategy

d <- mem_stat

d$at <- d$at*100
d$bb <- d$bb*100
d$fin_yield <- d$fin_yield/1000
d$fin_yield_95ci_inf <- d$fin_yield_95ci_inf/1000
d$fin_yield_95ci_sup <- d$fin_yield_95ci_sup/1000

sub <- NULL
dev <- NULL
sd <- NULL
ci_inf <- NULL
ci_sup <- NULL

for (i in 2:length(upth)) {
  sub <- as.data.frame(subset(d, at == upth[i]))
  dev <- rbind(dev, sub$fin_yield)
  sd <- rbind(sd, sub$fin_yield_sd)
  ci_inf <- rbind(ci_inf, sub$fin_yield_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$fin_yield_95ci_sup)
}

xadj1 <- as.numeric(upth[-1]) + 1;
xadj2 <- as.numeric(upth[-1]) + 3;
xtendrange <- seq(-10,110,1)

# # Put the other budget bonuses in a faint grey to show but de-emphasise
# for (i in 2:(dim(dev)[2]-1)) {
#   points(x = as.numeric(upth[-1])+2, y = dev[,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
#          col = "grey", pch = 20);
# }

arrows(xadj1, ci_inf[,2], xadj1, ci_sup[,2], length=0.05, angle=90, code=3, col = "darkred")
arrows(xadj2, ci_inf[,10], xadj2, ci_sup[,10], length=0.05, angle=90, code=3, col = "darkred")
points(x = xadj1, y = dev[,2], xlab = "Update threshold", type = "b", pch = 4,
      cex.lab = 1.5, cex.axis = 1.5, cex = 1,
      lwd = 2, col = "darkred");
points(x = xadj2, y = dev[,10], type = "b", pch = 4, cex = 1, lwd = 2, lty = "dashed", col = "darkred")
}

#### effect of UT:BB on yield inequity ####

{d <- ins_stat

d$at <- d$at*100
d$bb <- d$bb*100
d$max_diff_yield <- d$max_diff_yield*100
d$max_diff_yield_95ci_inf <- d$max_diff_yield_95ci_inf*100
d$max_diff_yield_95ci_sup <- d$max_diff_yield_95ci_sup*100

# get max, min and average of each UT
upth <- levels(as.factor(d$at))
bubo <- levels(as.factor(d$bb))

sub <- as.data.frame(subset(d, at == upth[1]))
dev <- c(sub$max_diff_yield, rep(NA,length(upth)-1))
sd <- c(sub$max_diff_yield_sd, rep(NA,length(upth)-1))
ci_inf <- c(sub$max_diff_yield_95ci_inf, rep(NA,length(upth)-1))
ci_sup <- c(sub$max_diff_yield_95ci_sup, rep(NA,length(upth)-1))

for (i in 2:length(upth)) {
  sub <- as.data.frame(subset(d, at == upth[i]))
  dev <- rbind(dev, sub$max_diff_yield)
  sd <- rbind(sd, sub$max_diff_yield_sd)
  ci_inf <- rbind(ci_inf, sub$max_diff_yield_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$max_diff_yield_95ci_sup)
  
  # # Without management?
  # no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
  # no.mgmt.finyie <- sum(no.mgmt[,8])/dim(no.mgmt)[1]
  # points(y = no.mgmt.extfreq, x = 0, pch = 17, col = "black")
  # abline(h = no.mgmt.extfreq, lty = 2, lwd = 1, col = "black")
  # 
  # # Without humans?
  # no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # no.hum.finyie <- sum(no.hum[,8])/dim(no.hum)[1]
  # points(y = no.hum.extfreq, x = 5, pch = 23, col = "black")
  # abline(h = no.hum.extfreq, lty = 3, lwd = 1, col = "black")
}

xadj1 <- as.numeric(upth[-1]) - 3;
xadj2 <- as.numeric(upth[-1]) - 1;
xtendrange <- seq(-10,110,1)

par(mar = c(5, 5, 1, 1))
plot(1, xlab = "Update threshold", type = "n", pch = 20,
     ylab = "Max diff in users final yield in % (+/- 95%CI)", cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
     ylim = c(0, 20), xlim = c(0, 100), lwd = 2, col = "blue")
polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 

# # Put the other budget bonuses in a faint grey to show but de-emphasise
# for (i in 3:(dim(dev)[2]-1)) {
#   points(x = as.numeric(upth[-1])-2, y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
#          col = "grey", pch = 20);
# }
abline(h = 0, col = "darkgreen", lty = 2)

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
no.mgmt.finyie <- (sum(no.mgmt[,9])/dim(no.mgmt)[1])*100
boot <- boot_sd_ci(no.mgmt[,9])*100
arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
points(y = no.mgmt.finyie, x = 0, pch = 17, col = "black")
abline(h = no.mgmt.finyie, lty = 2, lwd = 1, col = "black")

# Without humans?
no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)

no.hum.finyie <- sum(no.hum[,9])/dim(no.hum)[1] * 100
boot <- boot_sd_ci(no.hum[,9]) * 100
arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
points(y = no.hum.finyie, x = 0, pch = 23, col = "black")
abline(h = no.hum.finyie, lty = 3, lwd = 1, col = "black")

# now for the other strategy

d <- mem_stat

d$at <- d$at*100
d$bb <- d$bb*100
d$max_diff_yield <- d$max_diff_yield*100
d$max_diff_yield_95ci_inf <- d$max_diff_yield_95ci_inf*100
d$max_diff_yield_95ci_sup <- d$max_diff_yield_95ci_sup*100

sub <- NULL
dev <- NULL
sd <- NULL
ci_inf <- NULL
ci_sup <- NULL

for (i in 2:length(upth)) {
  sub <- as.data.frame(subset(d, at == upth[i]))
  dev <- rbind(dev, sub$max_diff_yield)
  sd <- rbind(sd, sub$max_diff_yield_sd)
  ci_inf <- rbind(ci_inf, sub$max_diff_yield_95ci_inf)
  ci_sup <- rbind(ci_sup, sub$max_diff_yield_95ci_sup)
}

xadj1 <- as.numeric(upth[-1]) + 1;
xadj2 <- as.numeric(upth[-1]) + 3;
xtendrange <- seq(-10,110,1)

# # Put the other budget bonuses in a faint grey to show but de-emphasise
# for (i in 2:(dim(dev)[2]-1)) {
#   points(x = as.numeric(upth[-1])+2, y = dev[,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
#          col = "grey", pch = 20);
# }

arrows(xadj1, ci_inf[,2], xadj1, ci_sup[,2], length=0.05, angle=90, code=3, col = "darkred")
arrows(xadj2, ci_inf[,10], xadj2, ci_sup[,10], length=0.05, angle=90, code=3, col = "darkred")
points(x = xadj1, y = dev[,2], xlab = "Update threshold", type = "b", pch = 4,
       cex.lab = 1.5, cex.axis = 1.5, cex = 1,
       lwd = 2, col = "darkred");
points(x = xadj2, y = dev[,10], type = "b", pch = 4, cex = 1, lwd = 2, lty = "dashed", col = "darkred")
}

#### Large parameter exploration figure ####

stats_resplot <- function(df, proxy = c("dev", "abs.dev", "fin.yie", "yie.equ", "non.int", "sq.dev"), variance = c("sd", "ci"), omit.extinction = FALSE) {
  
  attach(df)
  
  # change at into % to ease plotting
  df$at <- df$at*100
  
  # get the different Update Threshold values
  upth <- levels(as.factor(at*100))
  
  # get the colnames
  colnm <- colnames(df)
  
  # number of rows to make it as squared as possible
  rows <- floor(sqrt(length(upth)))
  
  # which proxy to plot?
  if (proxy == "dev") {
    yvar <- act_dev*100
    ysd <- act_dev_sd*100
    yci_inf <- act_dev_95ci_inf*100
    yci_sup <- act_dev_95ci_sup*100
  } else if (proxy == "abs.dev") {
    yvar <- abs_act_dev*100
    ysd <- abs_act_dev_sd*100
    yci_inf <- abs_act_dev_95ci_inf*100
    yci_sup <- abs_act_dev_95ci_sup*100
  } else if (proxy == "fin.yie") {
    yvar <- fin_yield/100
    ysd <- fin_yield_sd/100
    yci_inf <- fin_yield_95ci_inf/100
    yci_sup <- fin_yield_95ci_sup/100
  } else if (proxy == "yie.equ") {
    yvar <- max_diff_yield*100
    ysd <- max_diff_yield_sd*100
    yci_inf <- max_diff_yield_95ci_inf*100
    yci_sup <- max_diff_yield_95ci_sup*100
  } else if (proxy == "non.int") {
    yvar <- inac_ts*100
    ysd <- inac_ts_sd*100
    yci_inf <- inac_ts_95ci_inf*100
    yci_sup <- inac_ts_95ci_sup*100
  } else if (proxy == "sq.dev") {
    yvar <- SumSqDev/10^6
    ysd <- SumSqDev_sd/10^6
    yci_inf <- SumSqDev_95ci_inf/10^6
    yci_sup <- SumSqDev_95ci_sup/10^6 }
  
  # plot base
  p <- ggplot(df, aes(x=as.factor(bb*100), y=yvar)) +
    # coord_cartesian(ylim = c(-5000, 5000))
    facet_wrap(~at, ncol = ceiling(length(upth)/rows)) +
    labs(title=paste("(", ifelse(omit.extinction == F, "Including", "Excluding"), "extinctions )")) +
    theme_gray() +
    theme(strip.background=element_rect(fill="grey"),
          strip.text=element_text(color="white", face="bold"),
          axis.title=element_text(size=18),
          legend.text=element_text(size=15),
          legend.title = element_text(size = 18))
  
  ## Add the FLI strategy results
  # get the line where at = 0 is
  at0 <- which(at == 0)
  fli <- rep(yvar[at0], dim(df)[1])
  
  if (variance == "sd") {
    p <- p + geom_ribbon(aes(ymin = fli - ysd[at0], ymax = fli + ysd[at0]), fill = "grey") +
      geom_hline(yintercept = yvar[at0], color = "black")
    
    if (proxy == "dev") {
      p <- p + geom_errorbar(aes(ymin=yvar-ysd, ymax=yvar+ysd), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = -100, linetype = "dashed", color = "red") + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Resource population deviation from target \n (in % of Manager's target)")
    } else if (proxy == "abs.dev") {
      p <- p + geom_errorbar(aes(ymin=yvar-ysd, ymax=yvar+ysd), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Resource population absolute deviation from target \n (in % of Manager's target)") + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen")
    } else if (proxy == "fin.yie") {
      p <- p + geom_errorbar(aes(ymin=yvar-ysd, ymax=yvar+ysd), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = 100, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Sum of Users final yields \n (in thousands of a.b.u)")
    } else if (proxy == "yie.equ") {
      p <- p + geom_errorbar(aes(ymin=yvar-ysd, ymax=yvar+ysd), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Maximum difference between Users final yields \n (in % of the highest yield)")
    } else if (proxy == "non.int") {
      p <-  p + geom_errorbar(aes(ymin=yvar-ysd, ymax=yvar+ysd), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Time steps of non-intervention \n (in % of conservation scheme period)")
    } else if (proxy == "sq.dev") {
      p <-  p + geom_errorbar(aes(ymin=yvar-ysd, ymax=yvar+ysd), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Sum of population squared deviation from target (ind.10^5)")
    }
    
  } else if (variance == "ci") {
    p <- p + geom_ribbon(aes(ymin = yci_inf[at0], ymax = yci_sup[at0]), fill = "grey") +
      geom_hline(yintercept = yvar[at0], color = "black")
    
    if (proxy == "dev") {
      p <- p + geom_errorbar(aes(ymin=yci_inf, ymax=yci_sup), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = -100, linetype = "dashed", color = "red") + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Resource population deviation from target \n (in % of Manager's target)")
    } else if (proxy == "abs.dev") {
      p <- p + geom_errorbar(aes(ymin=yci_inf, ymax=yci_sup), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Resource population absolute deviation from target \n (in % of Manager's target)") + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen")
    } else if (proxy == "fin.yie") {
      p <- p + geom_errorbar(aes(ymin=yci_inf, ymax=yci_sup), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = 100, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Sum of Users final yields \n (in thousands of a.b.u)")
    } else if (proxy == "yie.equ") {
      p <- p + geom_errorbar(aes(ymin=yci_inf, ymax=yci_sup), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Maximum difference between Users final yields \n (in % of the highest yield)")
    } else if (proxy == "non.int") {
      p <-  p + geom_errorbar(aes(ymin=yci_inf, ymax=yci_sup), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Time steps of non-intervention \n (in % of conservation scheme period)")
    } else if (proxy == "sq.dev") {
      p <-  p + geom_errorbar(aes(ymin=yci_inf, ymax=yci_sup), position=position_dodge(1), colour = "darkblue", width=0.5) +
        geom_point(colour = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen") +
        labs(x="Budget Bonus (in % of Initial Budget)", y= "Sum of population squared deviation from target (ind.10^5)")
    }
  }  
  
  detach(df)
  
  return(p)
}

## Examples
stats_resplot(df = stat, proxy = "sq.dev", variance = "ci", omit.extinction = F)
stats_resplot(df = woe_stat, proxy = "yie.equ", variance = "ci", omit.extinction = T)

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
    oti_avg <- oti$SumSqDev/10^6
    
    if (variance == "sd") {
      
      # OTI strat
      oti_sd <- oti$SumSqDev_sd/10^6
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
      oti_95ci_inf <- oti$SumSqDev_95ci_inf/10^6
      oti_95ci_sup <- oti$SumSqDev_95ci_sup/10^6
      
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
  d <- subset(df, UT == upth)
  dd <- subset(df, UT == upth & BB == bubo) 
  
  # plot
  xrange <- seq(1, tmax, 1)
  plot(1, type = "n", xlab = "time", ylab = yaxis, ylim = c(0,max(d[8:dim(d)[2]])), xlim = c(0,20)) # , main = paste("UT = ", upth, " BB = ",bubo)
  
  # max with FLI strategy
  if (str_detect(yaxis, "Cost")){
    abline(h = (dd[1,1]/10)+10, lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "budget")){
    abline(h = dd[1,1], lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "action")){
    abline(h = dd[1,1]/10, lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "population")){
    xtendrange <- seq(-1,tmax+1,1)
    trans <- adjustcolor("lightgreen",alpha.f=0.5)
    upperUT <- rep((1+upth)*dd[1,6], length(xtendrange))
    lowerUT <- rep((1-upth)*dd[1,6], length(xtendrange))
    
    polygon(c(xtendrange,rev(xtendrange)),c(upperUT,rev(lowerUT)),col=trans, border = "green")
    abline(h = dd[1,6], col = "darkgreen", lty = 2)
  }
  
  # all the trajectories
  for (i in 1:(dim(dd)[1])) {
    points(x = xrange, y = dd[i,8:dim(dd)[2]], type = "b", pch = i, col = "black")
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

uniq <- function(df, upth, bubo, tmax, yaxis, replicate) {
  # subsetting
  d <- subset(df, UT == upth)
  dd <- subset(df, UT == upth & BB == bubo & rep == replicate) 
  
  # plot
  xrange <- seq(1, tmax, 1)
  plot(1, type = "n", xlab = "time", ylab = yaxis, ylim = c(0,max(d[8:dim(d)[2]])), xlim = c(0,20)) # , main = paste("UT = ", upth, " BB = ",bubo)
  
  # max with FLI strategy
  if (str_detect(yaxis, "Cost")){
    abline(h = (dd[1,1]/10)+10, lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "budget")){
    abline(h = dd[1,1], lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "action")){
    abline(h = dd[1,1]/10, lty = 2, pch = 2, col = "black")
  } else if (str_detect(yaxis, "population")){
    xtendrange <- seq(-1,tmax+1,1)
    trans <- adjustcolor("lightgreen",alpha.f=0.5)
    upperUT <- rep((1+upth)*dd[1,6], length(xtendrange))
    lowerUT <- rep((1-upth)*dd[1,6], length(xtendrange))
    
    polygon(c(xtendrange,rev(xtendrange)),c(upperUT,rev(lowerUT)),col=trans, border = "green")
    abline(h = dd[1,6], col = "darkgreen", lty = 2)
  }
  
  points(x = xrange, y = dd[,8:dim(dd)[2]], type = "b", pch = 1, col = "black")
}

uniq_ins_compare <- function(upd_thr, bud_bon, rep) {
  
  plot.new()
  layout(matrix(c(1,2,3,4,5,6), nrow = 3), widths = c(4,4))
  # layout.show(n = 6)
  # set space for a title
  # par(oma = c(0, 0, 3, 0))
  
  uniq(ins_popul, upth = upd_thr, bubo = bud_bon[1], tmax = 20, yaxis = "population", replicate = rep[1])
  uniq(ins_costs, upth = upd_thr, bubo = bud_bon[1], tmax = 20, yaxis = "Costs", replicate = rep[1])
  # uniq(ins_actio, upth = 0, bubo = bud_bon[1], tmax = 20, yaxis = "actions")
  uniq(ins_budge, upth = upd_thr, bubo = bud_bon[1], tmax = 20, yaxis = "budget", replicate = rep[1])
  uniq(ins_popul, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "population", replicate = rep[2])
  uniq(ins_costs, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "Costs", replicate = rep[2])
  # uniq(ins_actio, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "actions", replicate = rep[2])
  uniq(ins_budge, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "budget", replicate = rep[2])
  
  mtext(paste("DO-ATI - UT =", upd_thr*100, "% - BB", bud_bon[1]*100, "% VS", bud_bon[2]*100, "%"), outer = TRUE, cex = 1, line = 1.5)
}

# example
uniq_ins_compare(upd_thr = 0.1, bud_bon = c(0.1,1), rep = c(80,60))

ins_compare <- function(upd_thr, bud_bon, rep) {
  
  plot.new()
  layout(matrix(c(1,2,3,4,5,6), nrow = 3), widths = c(4,4))
  # layout.show(n = 6)
  # set space for a title
  # par(oma = c(0, 0, 3, 0))
  
  multi(ins_popul, upth = upd_thr, bubo = bud_bon[1], tmax = 20, yaxis = "population")
  multi(ins_costs, upth = upd_thr, bubo = bud_bon[1], tmax = 20, yaxis = "Costs")
  # multi(ins_actio, upth = 0, bubo = bud_bon[1], tmax = 20, yaxis = "actions")
  multi(ins_budge, upth = upd_thr, bubo = bud_bon[1], tmax = 20, yaxis = "budget")
  multi(ins_popul, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "population")
  multi(ins_costs, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "Costs")
  # multi(ins_actio, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "actions")
  multi(ins_budge, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "budget")
  
  mtext(paste("DO-ATI - UT =", upd_thr*100, "% - BB", bud_bon[1]*100, "% VS", bud_bon[2]*100, "%"), outer = TRUE, cex = 1, line = 1.5)
}

# example
ins_compare(upd_thr = 0.2, bud_bon = c(0.1,1))

{d <- ins_stat
  
  d$at <- d$at*100
  d$bb <- d$bb*100
  d$SumAbsDev <- d$SumAbsDev
  d$SumAbsDev_95ci_inf <- d$SumAbsDev_95ci_inf
  d$SumAbsDev_95ci_sup <- d$SumAbsDev_95ci_sup
  
  # get max, min and average of each UT
  upth <- c(10,20)
  bubo <- levels(as.factor(d$bb))
  
  sub <- as.data.frame(subset(d, at == upth[1]))
  dev <- sub$SumAbsDev
  sd <- sub$SumAbsDev_sd
  ci_inf <- sub$SumAbsDev_95ci_inf
  ci_sup <- sub$SumAbsDev_95ci_sup
  
  for (i in 2:length(upth)) {
    sub <- as.data.frame(subset(d, at == upth[i]))
    dev <- rbind(dev, sub$SumAbsDev)
    sd <- rbind(sd, sub$SumAbsDev_sd)
    ci_inf <- rbind(ci_inf, sub$SumAbsDev_95ci_inf)
    ci_sup <- rbind(ci_sup, sub$SumAbsDev_95ci_sup)
  }
  
  xadj1 <- as.numeric(bubo) -1.5;
  xadj2 <- as.numeric(bubo) +1.5;
  # xtendrange <- seq(-10,110,1)
  
  # par(mar = c(5, 5, 1, 1))
  plot(1, xlab = "Budget Bonus (in %)", type = "n", pch = 20,
       ylab = "Average absolute deviation from target per ts", cex.lab = 1.5, cex.axis = 1.5, cex = 2,
       ylim = c(min(ci_inf)-10, max(ci_sup)+10), xlim = c(0, 100), lwd = 2)
  # polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
  
  # # Put the other budget bonuses in a faint grey to show but de-emphasise
  # for (i in 3:(dim(dev)[2]-1)) {
  #   points(x = as.numeric(upth[-1])-2, y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
  #          col = "grey", pch = 20);
  # }
  # abline(h = 0, col = "darkgreen", lty = 2)
  
  # arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.05, angle=90, code=3, col = "black")
  arrows(xadj1, ci_inf[1,], xadj1, ci_sup[1,], length=0.05, angle=90, code=3, col = "darkblue")
  arrows(xadj2, ci_inf[2,], xadj2, ci_sup[2,], length=0.05, angle=90, code=3, col = "darkred")
  points(x = xadj1, y = dev[1,], type = "b", pch = 20, col = "blue");
  points(x = xadj2, y = dev[2,], type = "b", pch = 4, lty = "dashed", col = "red")
  # points(x = 0, y = dev[1,1], pch = 15)
  # abline(h=dev[1,1], lty = 1, col = "black")
  
  # # Without management?
  # no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
  # no.mgmt.finyie <- (sum(no.mgmt[,9])/dim(no.mgmt)[1])*100
  # boot <- boot_sd_ci(no.mgmt[,9])*100
  # arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
  # points(y = no.mgmt.finyie, x = 0, pch = 17, col = "black")
  # abline(h = no.mgmt.finyie, lty = 2, lwd = 1, col = "black")
  # 
  # # Without humans?
  # no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # no.hum.finyie <- sum(no.hum[,9])/dim(no.hum)[1] * 100
  # boot <- boot_sd_ci(no.hum[,9]) * 100
  # arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
  # points(y = no.hum.finyie, x = 0, pch = 23, col = "black")
  # abline(h = no.hum.finyie, lty = 3, lwd = 1, col = "black")
  # 
  # # now for the other strategy
  # 
  # d <- mem_stat
  # 
  # d$at <- d$at*100
  # d$bb <- d$bb*100
  # d$SumAbsDev <- d$SumAbsDev*100
  # d$SumAbsDev_95ci_inf <- d$SumAbsDev_95ci_inf*100
  # d$SumAbsDev_95ci_sup <- d$SumAbsDev_95ci_sup*100
  # 
  # sub <- NULL
  # dev <- NULL
  # sd <- NULL
  # ci_inf <- NULL
  # ci_sup <- NULL
  # 
  # for (i in 2:length(upth)) {
  #   sub <- as.data.frame(subset(d, at == upth[i]))
  #   dev <- rbind(dev, sub$SumAbsDev)
  #   sd <- rbind(sd, sub$SumAbsDev_sd)
  #   ci_inf <- rbind(ci_inf, sub$SumAbsDev_95ci_inf)
  #   ci_sup <- rbind(ci_sup, sub$SumAbsDev_95ci_sup)
  # }
  # 
  # xadj1 <- as.numeric(upth[-1]) + 1;
  # xadj2 <- as.numeric(upth[-1]) + 3;
  # xtendrange <- seq(-10,110,1)
  # 
  # # # Put the other budget bonuses in a faint grey to show but de-emphasise
  # # for (i in 2:(dim(dev)[2]-1)) {
  # #   points(x = as.numeric(upth[-1])+2, y = dev[,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
  # #          col = "grey", pch = 20);
  # # }
  # 
  # arrows(xadj1, ci_inf[,2], xadj1, ci_sup[,2], length=0.05, angle=90, code=3, col = "darkred")
  # arrows(xadj2, ci_inf[,10], xadj2, ci_sup[,10], length=0.05, angle=90, code=3, col = "darkred")
  # points(x = xadj1, y = dev[,2], xlab = "Update threshold", type = "b", pch = 4,
  #        cex.lab = 1.5, cex.axis = 1.5, cex = 1,
  #        lwd = 2, col = "darkred");
  # points(x = xadj2, y = dev[,10], type = "b", pch = 4, cex = 1, lwd = 2, lty = "dashed", col = "darkred")
}

ins_mem_compare <- function(upd_thr, bud_bon, rep) {
  
  plot.new()
  layout(matrix(c(1,2,3,4,5,6), nrow = 3), widths = c(4,4))
  # layout.show(n = 6)
  # set space for a title
  # par(oma = c(0, 0, 3, 0))
  
  uniq(ins_popul, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "population", replicate = rep[1])
  uniq(ins_costs, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "Costs", replicate = rep[1])
  # uniq(ins_actio, upth = 0, bubo = bud_bon[1], tmax = 20, yaxis = "actions")
  uniq(ins_budge, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "budget", replicate = rep[1])
  uniq(mem_popul, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "population", replicate = rep[2])
  uniq(mem_costs, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "Costs", replicate = rep[2])
  # uniq(ins_actio, upth = upd_thr, bubo = bud_bon[2], tmax = 20, yaxis = "actions", replicate = rep[2])
  uniq(mem_budge, upth = upd_thr, bubo = bud_bon, tmax = 20, yaxis = "budget", replicate = rep[2])
  
  mtext(paste("DO-ATI VS V-ATI - UT =", upd_thr*100, "% & BB =", bud_bon*100, "%"), outer = TRUE, cex = 1, line = 1.5)
}

ins_mem_compare(upd_thr = 0.3, bud_bon = 0.5, rep = c(20,40))

{d <- mem_stat
  
  d$at <- d$at*100
  d$bb <- d$bb*100
  d$SumAbsDev <- d$SumAbsDev
  d$SumAbsDev_95ci_inf <- d$SumAbsDev_95ci_inf
  d$SumAbsDev_95ci_sup <- d$SumAbsDev_95ci_sup
  
  # get max, min and average of each UT
  upth <- c(0,30)
  bubo <- levels(as.factor(d$bb))
  
  sub <- as.data.frame(subset(d, at == upth[1]))
  dev <- sub$SumAbsDev
  sd <- sub$SumAbsDev_sd
  ci_inf <- sub$SumAbsDev_95ci_inf
  ci_sup <- sub$SumAbsDev_95ci_sup
  
  for (i in 2:length(upth)) {
    sub <- as.data.frame(subset(d, at == upth[i]))
    dev <- rbind(dev, sub$SumAbsDev)
    sd <- rbind(sd, sub$SumAbsDev_sd)
    ci_inf <- rbind(ci_inf, sub$SumAbsDev_95ci_inf)
    ci_sup <- rbind(ci_sup, sub$SumAbsDev_95ci_sup)
  }
  
  xadj1 <- as.numeric(bubo) -1.5;
  xadj2 <- as.numeric(bubo) +1.5;
  xtendrange <- seq(-10,110,1)
  
  # par(mar = c(5, 5, 1, 1))
  plot(1, xlab = "Budget Bonus (in %)", type = "n", pch = 20,
       ylab = "Average absolute deviation from target per ts", cex.lab = 1.5, cex.axis = 1.5, cex = 2,
       ylim = c(min(ci_inf)-10, max(ci_sup)+10), xlim = c(0, 100), lwd = 2)
  polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
  abline(h=dev[1], lwd=2)
  
  # # Put the other budget bonuses in a faint grey to show but de-emphasise
  # for (i in 3:(dim(dev)[2]-1)) {
  #   points(x = as.numeric(upth[-1])-2, y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
  #          col = "grey", pch = 20);
  # }
  # abline(h = 0, col = "darkgreen", lty = 2)
  
  # arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.05, angle=90, code=3, col = "black")
  # arrows(xadj1, ci_inf[1,], xadj1, ci_sup[1,], length=0.05, angle=90, code=3, col = "darkblue")
  arrows(xadj2, ci_inf[2,], xadj2, ci_sup[2,], length=0.05, angle=90, code=3, col = "darkred")
  # points(x = xadj1, y = dev[1,], type = "b", pch = 20, col = "blue");
  points(x = xadj2, y = dev[2,], type = "b", pch = 4, lty = "dashed", col = "red")
  # points(x = 0, y = dev[1,1], pch = 15)
  # abline(h=dev[1,1], lty = 1, col = "black")
  
  # # Without management?
  # no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
  # no.mgmt.finyie <- (sum(no.mgmt[,9])/dim(no.mgmt)[1])*100
  # boot <- boot_sd_ci(no.mgmt[,9])*100
  # arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
  # points(y = no.mgmt.finyie, x = 0, pch = 17, col = "black")
  # abline(h = no.mgmt.finyie, lty = 2, lwd = 1, col = "black")
  # 
  # # Without humans?
  # no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # no.hum.finyie <- sum(no.hum[,9])/dim(no.hum)[1] * 100
  # boot <- boot_sd_ci(no.hum[,9]) * 100
  # arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
  # points(y = no.hum.finyie, x = 0, pch = 23, col = "black")
  # abline(h = no.hum.finyie, lty = 3, lwd = 1, col = "black")
  # 
  # # now for the other strategy
  # 
  # d <- mem_stat
  # 
  # d$at <- d$at*100
  # d$bb <- d$bb*100
  # d$SumAbsDev <- d$SumAbsDev*100
  # d$SumAbsDev_95ci_inf <- d$SumAbsDev_95ci_inf*100
  # d$SumAbsDev_95ci_sup <- d$SumAbsDev_95ci_sup*100
  # 
  # sub <- NULL
  # dev <- NULL
  # sd <- NULL
  # ci_inf <- NULL
  # ci_sup <- NULL
  # 
  # for (i in 2:length(upth)) {
  #   sub <- as.data.frame(subset(d, at == upth[i]))
  #   dev <- rbind(dev, sub$SumAbsDev)
  #   sd <- rbind(sd, sub$SumAbsDev_sd)
  #   ci_inf <- rbind(ci_inf, sub$SumAbsDev_95ci_inf)
  #   ci_sup <- rbind(ci_sup, sub$SumAbsDev_95ci_sup)
  # }
  # 
  # xadj1 <- as.numeric(upth[-1]) + 1;
  # xadj2 <- as.numeric(upth[-1]) + 3;
  # xtendrange <- seq(-10,110,1)
  # 
  # # # Put the other budget bonuses in a faint grey to show but de-emphasise
  # # for (i in 2:(dim(dev)[2]-1)) {
  # #   points(x = as.numeric(upth[-1])+2, y = dev[,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
  # #          col = "grey", pch = 20);
  # # }
  # 
  # arrows(xadj1, ci_inf[,2], xadj1, ci_sup[,2], length=0.05, angle=90, code=3, col = "darkred")
  # arrows(xadj2, ci_inf[,10], xadj2, ci_sup[,10], length=0.05, angle=90, code=3, col = "darkred")
  # points(x = xadj1, y = dev[,2], xlab = "Update threshold", type = "b", pch = 4,
  #        cex.lab = 1.5, cex.axis = 1.5, cex = 1,
  #        lwd = 2, col = "darkred");
  # points(x = xadj2, y = dev[,10], type = "b", pch = 4, cex = 1, lwd = 2, lty = "dashed", col = "darkred")
}

{d <- ins_stat
  
  d$at <- d$at*100
  d$bb <- d$bb*100
  d$SumAbsDev <- d$SumAbsDev
  d$SumAbsDev_95ci_inf <- d$SumAbsDev_95ci_inf
  d$SumAbsDev_95ci_sup <- d$SumAbsDev_95ci_sup
  
  # get max, min and average of each UT
  upth <- 10
  bubo <- levels(as.factor(d$bb))
  
  sub <- as.data.frame(subset(d, at == upth))
  dev <- sub$SumAbsDev
  sd <- sub$SumAbsDev_sd
  ci_inf <- sub$SumAbsDev_95ci_inf
  ci_sup <- sub$SumAbsDev_95ci_sup
  
  # for (i in 2:length(upth)) {
  #   sub <- as.data.frame(subset(d, at == upth[i]))
  #   dev <- rbind(dev, sub$SumAbsDev)
  #   sd <- rbind(sd, sub$SumAbsDev_sd)
  #   ci_inf <- rbind(ci_inf, sub$SumAbsDev_95ci_inf)
  #   ci_sup <- rbind(ci_sup, sub$SumAbsDev_95ci_sup)
  # }
  
  xadj1 <- as.numeric(bubo) -1.5;
  xadj2 <- as.numeric(bubo) +1.5;
  # xtendrange <- seq(-10,110,1)
  
  # par(mar = c(5, 5, 1, 1))
  plot(1, xlab = "Budget Bonus (in %)", type = "n", pch = 20,
       ylab = "Average absolute deviation from target per ts", cex.lab = 1.5, cex.axis = 1.5, cex = 2,
       ylim = c(0, 800), xlim = c(0, 100), lwd = 2)
  # polygon(c(xtendrange,rev(xtendrange)),c(rep(ci_sup[1,1], length(xtendrange)),rev(rep(ci_inf[1,1], length(xtendrange)))),col="lightgrey", border = "grey") 
  # abline(h=dev[1], lwd=2)
  
  # # Put the other budget bonuses in a faint grey to show but de-emphasise
  # for (i in 3:(dim(dev)[2]-1)) {
  #   points(x = as.numeric(upth[-1])-2, y = dev[-1,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
  #          col = "grey", pch = 20);
  # }
  # abline(h = 0, col = "darkgreen", lty = 2)
  
  # arrows(0, ci_inf[1,1], 0, ci_sup[1,1], length=0.05, angle=90, code=3, col = "black")
  # arrows(xadj1, ci_inf[1,], xadj1, ci_sup[1,], length=0.05, angle=90, code=3, col = "darkblue")
  arrows(xadj2, ci_inf, xadj2, ci_sup, length=0.05, angle=90, code=3, col = "darkred")
  # points(x = xadj1, y = dev[1,], type = "b", pch = 20, col = "blue");
  points(x = xadj2, y = dev, type = "b", pch = 4, lty = "dashed", col = "red")
  # points(x = 0, y = dev[1,1], pch = 15)
  # abline(h=dev[1,1], lty = 1, col = "black")
  
  # # Without management?
  # no.mgmt <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # # no.mgmt.extfreq <- length(which(no.mgmt$time_step < 20))/100
  # no.mgmt.finyie <- (sum(no.mgmt[,9])/dim(no.mgmt)[1])*100
  # boot <- boot_sd_ci(no.mgmt[,9])*100
  # arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
  # points(y = no.mgmt.finyie, x = 0, pch = 17, col = "black")
  # abline(h = no.mgmt.finyie, lty = 2, lwd = 1, col = "black")
  # 
  # # Without humans?
  # no.hum <- read.csv("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/batch6-perfObs/user_and_manager_budget_is_1.csv", sep = "\t", header = FALSE)
  # 
  # no.hum.finyie <- sum(no.hum[,9])/dim(no.hum)[1] * 100
  # boot <- boot_sd_ci(no.hum[,9]) * 100
  # arrows(0, boot[2], 0, boot[3], length=0.05, angle=90, code=3, col = "black")
  # points(y = no.hum.finyie, x = 0, pch = 23, col = "black")
  # abline(h = no.hum.finyie, lty = 3, lwd = 1, col = "black")
  # 
  # now for the other strategy

  d <- mem_stat

  d$at <- d$at*100
  d$bb <- d$bb*100
  d$SumAbsDev <- d$SumAbsDev
  d$SumAbsDev_95ci_inf <- d$SumAbsDev_95ci_inf
  d$SumAbsDev_95ci_sup <- d$SumAbsDev_95ci_sup

  upth <- 30
  bubo <- levels(as.factor(d$bb))
  
  sub <- as.data.frame(subset(d, at == upth))
  dev <- sub$SumAbsDev
  sd <- sub$SumAbsDev_sd
  ci_inf <- sub$SumAbsDev_95ci_inf
  ci_sup <- sub$SumAbsDev_95ci_sup

  # for (i in 2:length(upth)) {
  #   sub <- as.data.frame(subset(d, at == upth[i]))
  #   dev <- rbind(dev, sub$SumAbsDev)
  #   sd <- rbind(sd, sub$SumAbsDev_sd)
  #   ci_inf <- rbind(ci_inf, sub$SumAbsDev_95ci_inf)
  #   ci_sup <- rbind(ci_sup, sub$SumAbsDev_95ci_sup)
  # }

  # xadj1 <- as.numeric(upth[-1]) + 1;
  # xadj2 <- as.numeric(upth[-1]) + 3;
  # xtendrange <- seq(-10,110,1)

  # # Put the other budget bonuses in a faint grey to show but de-emphasise
  # for (i in 2:(dim(dev)[2]-1)) {
  #   points(x = as.numeric(upth[-1])+2, y = dev[,i], type = "b", cex = 0.6, lwd = 0.8, lty = "solid",
  #          col = "grey", pch = 20);
  # }

  arrows(xadj1, ci_inf, xadj1, ci_sup, length=0.05, angle=90, code=3, col = "darkblue")
  # arrows(xadj2, ci_inf[,10], xadj2, ci_sup[,10], length=0.05, angle=90, code=3, col = "darkred")
  points(x = xadj1, y = dev, xlab = "Update threshold", type = "b", pch = 4,
         cex.lab = 1.5, cex.axis = 1.5, cex = 1,
         lwd = 2, col = "darkblue");
  # points(x = xadj2, y = dev, type = "b", pch = 4, cex = 1, lwd = 2, lty = "dashed", col = "darkblue")
}
