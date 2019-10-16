## Adrian Bach
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

#### Randomizing case parameters ####

# draw an initial population in a uniform law (every value has the same probability)
popini <- floor(runif(n = 1, min = 10, max = 10000))

# draw a K reasonably close to popini
# use a normal law?
K <- floor(rnorm(n = 1, mean = popini, sd = 5000))

# make sure it is not negative
K <- ifelse(K<0, -K, K)

# Set a manager target: which will probably be close to but under K 
MT <- signif(rnorm(n = 1, mean = 0.9, sd = 0.05)*K, digits = 2)

# draw a number of stakeholders between 2 and 50
nbstk <- floor(runif(n = 1, min = 2, max = 50))

# I should make sure AT values are does not include 0 (extinction) and K (overshooting carrying capacity)

