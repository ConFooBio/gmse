path <- "~/Desktop/PhD/GitKraken/gmse_fork_RQ1/NORESET_results/"
setwd(dir = path)

# get the directory content
content <- dir()

# order alphabetically
content <- content[order(content)]

#### sim results ####

# initialize a table with the UT0 
at0 <- grep(pattern = c("UT0pt0_"), x = content, fixed = T, value = T)
at0.tab <- grep(pattern = "flw", x = at0, fixed = T, value = T, invert = T)

tab <- read.csv(file = paste(path,at0.tab, sep = ""), sep = "\t", header = F)

tab.names <- c("rep", "budget", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "SumSqDev")
colnames(tab) <- tab.names

# remove them from content
from0pt1 <- content[-grep(pattern = c("UT0pt0_"), x = content, fixed = T, value = F)]

# select the sim results only
from0pt1.sim <- grep(pattern = c("flw_"), x = from0pt1, fixed = T, value = T, invert = T)

# loop over the from0pt1.sim and rbind to tab
for (i in 1:length(from0pt1.sim)) {
  zz <- read.csv(file = paste(path,from0pt1.sim[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- tab.names
  tab <- rbind(tab, zz)
}

# export table
write.csv(tab, file = paste(path, "ATI-sim-merged-results.csv", sep = ""))

#### follow up over population ####

# initialize a table with the AT0 and AT0.1 
at0.pop <- grep(pattern = "pop", x = at0, fixed = T, value = T)

pop <- read.csv(file = paste(path,at0.pop, sep = ""), sep = "\t", header = F)#[,-1]

t <- rep(NA, 20)
for (i in 1:20) {t[i] <- paste("t",i, sep = "")}
pop.names <- c("budget", "UT", "BB", "Extinct", "rep", "target", "popInit", t)
colnames(pop) <- pop.names

# select the sim results only
from0pt1.pop <- grep(pattern = c("flw_pop"), x = from0pt1, fixed = T, value = T, invert = F)

# loop over the from0pt1.sim and rbind to pop
for (i in 1:length(from0pt1.pop)) {
  zz <- read.csv(file = paste(path,from0pt1.pop[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  pop <- rbind(pop, zz)
}

# export table
write.csv(pop, file = paste(path, "pop-ATI-res-merged.csv", sep = ""))

#### follow up over costs ####

# initialize a table with the AT0 and AT0.1 
at0.cos <- grep(pattern = "cos", x = at0, fixed = T, value = T)

cos <- read.csv(file = paste(path,at0.cos, sep = ""), sep = "\t", header = F)#[,-1]

colnames(cos) <- pop.names

# select the sim results only
from0pt1.cos <- grep(pattern = c("flw_cos"), x = from0pt1, fixed = T, value = T, invert = F)

# loop over the from0pt1.sim and rbind to cos
for (i in 1:length(from0pt1.cos)) {
  zz <- read.csv(file = paste(path,from0pt1.cos[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  cos <- rbind(cos, zz)
}

# export table
write.csv(cos, file = paste(path, "cos-ATI-res-merged.csv", sep = ""))

#### follow up over actions ####

# initialize a table with the AT0 and AT0.1 
at0.act <- grep(pattern = "act", x = at0, fixed = T, value = T)

act <- read.csv(file = paste(path,at0.act, sep = ""), sep = "\t", header = F) #[,-1]

colnames(act) <- pop.names

# select the sim results only
from0pt1.act <- grep(pattern = c("flw_act"), x = from0pt1, fixed = T, value = T, invert = F)

# loop over the from0pt1.sim and rbind to act
for (i in 1:length(from0pt1.act)) {
  zz <- read.csv(file = paste(path,from0pt1.act[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  act <- rbind(act, zz)
}

# export table
write.csv(act, file = paste(path, "act-ATI-res-merged.csv", sep = ""))

#### follow up over budget ####

# initialize a table with the AT0 and AT0.1 
at0.bgt <- grep(pattern = "bgt", x = at0, fixed = T, value = T)

bgt <- read.csv(file = paste(path,at0.bgt, sep = ""), sep = "\t", header = F)#[,-1]

colnames(bgt) <- pop.names

# select the sim results only
from0pt1.bgt <- grep(pattern = c("flw_bgt"), x = from0pt1, fixed = T, value = T, invert = F)

# loop over the from0pt1.sim and rbind to bgt
for (i in 1:length(from0pt1.bgt)) {
  zz <- read.csv(file = paste(path,from0pt1.bgt[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  bgt <- rbind(bgt, zz)
}

# export table
write.csv(bgt, file = paste("~/Desktop/PhD/GitKraken/gmse_fork_RQ1/", "bgt-ATI-res-merged.csv", sep = ""))
