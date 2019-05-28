# Adrian Bach
# PhD : Using AI to improve decision-making in conservation conflicts
# University of Stirling

# Reserch Question 1
# Optimizing managers policy update timing
# Stats

######## Initialization ########

#### Packages ####

#### Libraries ####

######## Stats ########

## import data sets 
tab_res <- read.csv("~/Desktop/Thèse/GitKraken/gmse_fork_RQ1/data/tab_OYA_batch1_results_abs.csv", header = T)

mean_res <- read.csv("~/Desktop/Thèse/GitKraken/gmse_fork_RQ1/data/stats_batch1_extprob.csv", header = T)

#### LM ANOVA ####

# effect of at, bb, scar on act_dev
m1 <- with(tab_res, lm(act_dev ~ at + bb + scar + at:bb + at:scar + bb:scar))
m1

anova(m1)

# effect of at, bb, scar on abs_act_dev
m2 <- with(tab_res, lm(abs_act_dev ~ at + bb + scar + at:bb + at:scar + bb:scar))
m2

anova(m2)

# effect of at, bb, scar on fin_yield
m3 <- with(tab_res, lm(fin_yield ~ at + bb + scar + at:bb + at:scar + bb:scar))
m3

anova(m3)

# effect of at, bb, scar on act_dev
m4 <- with(mean_res, lm(ext_prob ~ at + bb + scar + at:bb + at:scar + bb:scar))
m4

anova(m4)
