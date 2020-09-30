path <- "~/Desktop/PhD/GitKraken/gmse_fork_RQ1/Budget-ratio-large-batch/noMem-res/"
setwd(dir = path)

# get the directory content
content <- dir()
# 
# # remove weird "Squishy's conflicted copy"
# content <- content[-which(content == grep(pattern = "conflicted", x = content, value = T))]
# content <- content[-which(content == grep(pattern = "conflicted", x = content, value = T))]

# order alphabetically
content <- content[order(content)]

# only the csv files
content <- grep(pattern = c(".csv"), x = content, fixed = T, value = T, invert = F)

#### sim results ####

# initialize a table with the UT0 BR0 results
# at0 <- grep(pattern = c("UT0pt0_"), x = content, fixed = T, value = T)
# at0.tab <- grep(pattern = "flw", x = at0, fixed = T, value = T, invert = T)

# tab <- read.csv(file = paste(path,at0.tab[1], sep = ""), sep = "\t", header = F)
tab.sim <- grep(pattern = "flw", x = content, fixed = T, value = T, invert = T)
tab <- read.csv(file = paste(path,tab.sim[1], sep = ""), sep = "\t", header = F)

tab.names <- c("rep", "memory", "budget", "ratio", "at", "bb", "extinct", "act_dev", "abs_act_dev", "fin_yield", "max_diff_yield", "inac_ts", "SumAbsDev", "final_ts")
colnames(tab) <- tab.names

# remove it from content
# rest <- content[-which(content == at0.tab[1])]
rest <- content[-which(content == tab.sim[1])]

# # only the csv files
# rest <- grep(pattern = c(".csv"), x = rest, fixed = T, value = T, invert = F)

# select the sim results only
rest.sim <- grep(pattern = c("flw_"), x = rest, fixed = T, value = T, invert = T)

# loop over the rest.sim and rbind to tab
for (i in 1:length(rest.sim)) {
  zz <- read.csv(file = paste(path,rest.sim[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- tab.names
  tab <- rbind(tab, zz)
}

# export table
write.csv(tab, file = paste(path, "/merged-res/noMem-budget-ratio-merged.csv", sep = ""), row.names = FALSE)

#### follow up over population ####

# initialize a table with the AT0 and AT0.1 
# at0.pop <- grep(pattern = "pop", x = at0, fixed = T, value = T)

# pop <- read.csv(file = paste(path,at0.pop[1], sep = ""), sep = "\t", header = F)#[,-1]
pop.all <- grep(pattern = "pop", x = content, fixed = T, value = T, invert = F)
pop <- read.csv(file = paste(path,pop.all[1], sep = ""), sep = "\t", header = F)

# remove it from content
# rest <- rest[-which(content == at0.pop[1])]
rest.pop <- pop.all[-1]

t <- rep(NA, 20)
for (i in 1:20) {t[i] <- paste("t",i, sep = "")}
pop.names <- c("memory", "budget", "ratio", "UT", "BB", "Extinct", "rep", "target", "popInit", t)
colnames(pop) <- pop.names

# # select the sim results only
# rest.pop <- grep(pattern = c("flw_pop"), x = rest, fixed = T, value = T, invert = F)

# loop over the rest.sim and rbind to pop
for (i in 1:length(rest.pop)) {
  zz <- read.csv(file = paste(path,rest.pop[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  pop <- rbind(pop, zz)
}

# export table
write.csv(pop, file = paste(path, "/merged-res/pop-noMem-budget-ratio-merged.csv", sep = ""), row.names = FALSE)

#### follow up over costs ####

# initialize a table with the AT0 and AT0.1 
# at0.cos <- grep(pattern = "cos", x = at0, fixed = T, value = T)
# 
# cos <- read.csv(file = paste(path,at0.cos[1], sep = ""), sep = "\t", header = F)#[,-1]
cos.all <- grep(pattern = "cos", x = content, fixed = T, value = T, invert = F)
cos <- read.csv(file = paste(path,cos.all[1], sep = ""), sep = "\t", header = F)

# remove it from content
rest.cos <- cos.all[-1]

colnames(cos) <- pop.names

# # select the sim results only
# rest.cos <- grep(pattern = c("flw_cos"), x = rest, fixed = T, value = T, invert = F)

# loop over the rest.sim and rbind to cos
for (i in 1:length(rest.cos)) {
  zz <- read.csv(file = paste(path,rest.cos[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  cos <- rbind(cos, zz)
}

# export table
write.csv(cos, file = paste(path, "/merged-res/cos-noMem-budget-ratio-merged.csv", sep = ""), row.names = FALSE)

#### follow up over actions ####

# initialize a table with the AT0 and AT0.1 
# at0.act <- grep(pattern = "act", x = at0, fixed = T, value = T)
# 
# act <- read.csv(file = paste(path,at0.act[1], sep = ""), sep = "\t", header = F) #[,-1]
act.all <- grep(pattern = "act", x = content, fixed = T, value = T, invert = F)
act <- read.csv(file = paste(path,act.all[1], sep = ""), sep = "\t", header = F)

# # remove it from content
# rest <- rest[-which(content == at0.act[1])]
rest.act <- act.all[-1]

colnames(act) <- pop.names

# # select the sim results only
# rest.act <- grep(pattern = c("flw_act"), x = rest, fixed = T, value = T, invert = F)

# loop over the rest.sim and rbind to act
for (i in 1:length(rest.act)) {
  zz <- read.csv(file = paste(path,rest.act[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  act <- rbind(act, zz)
}

# export table
write.csv(act, file = paste(path, "/merged-res/act-noMem-budget-ratio-merged.csv", sep = ""), row.names = FALSE)

#### follow up over budget ####

# initialize a table with the AT0 and AT0.1 
# at0.bgt <- grep(pattern = "bgt", x = at0, fixed = T, value = T)
# 
# bgt <- read.csv(file = paste(path,at0.bgt[1], sep = ""), sep = "\t", header = F)#[,-1]
bgt.all <- grep(pattern = "bgt", x = content, fixed = T, value = T, invert = F)
bgt <- read.csv(file = paste(path,bgt.all[1], sep = ""), sep = "\t", header = F)

# # remove it from content
# rest <- rest[-which(content == at0.bgt[1])]
rest.bgt <- bgt.all[-1]

colnames(bgt) <- pop.names

# # select the sim results only
# rest.bgt <- grep(pattern = c("flw_bgt"), x = rest, fixed = T, value = T, invert = F)

# loop over the rest.sim and rbind to bgt
for (i in 1:length(rest.bgt)) {
  zz <- read.csv(file = paste(path,rest.bgt[i], sep = ""), sep = "\t", header = F)
  colnames(zz) <- pop.names
  bgt <- rbind(bgt, zz)
}

# export table
write.csv(bgt, file = paste(path, "/merged-res/bgt-noMem-budget-ratio-merged.csv", sep = ""), row.names = FALSE)
