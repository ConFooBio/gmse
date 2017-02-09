################################################################################
################################################################################
# Proto-type genetic algorithm
################################################################################
################################################################################

# Note: I'm coding this inefficiently in R, deliberately, because it will make
#       switching it over to c easier in the long run.
# Optimisation tips below:
# https://www.codeproject.com/Articles/6154/Writing-Efficient-C-and-C-Code-Optimization

min_cost <- function(budget_total, util, row){
    the_min <- budget_total;
    for(check in 1:5){
         index <- (2*check) + 5;
         if(util[row, index] < the_min){
             the_min <- util[row, index];    
         }
    }
    return( as.numeric(the_min) );
}

# Add row 10X to 90 random (first brown box)
initialise_pop <- function(UTILITY, focal_agent, population){
    for(agent in 1:dim(population)[1]){
        if(agent < clone_seed){
            for(u_trait in 1:dim(population)[2]){
                population[agent, u_trait] <- UTILITY[focal_agent, u_trait];
            }
        }else{ # No need to bother with a loop here -- unroll to save some time
            population[agent, 1]  <- UTILITY[focal_agent, 1];
            population[agent, 2]  <- UTILITY[focal_agent, 2];
            population[agent, 3]  <- UTILITY[focal_agent, 3];
            population[agent, 4]  <- UTILITY[focal_agent, 4];
            population[agent, 5]  <- UTILITY[focal_agent, 5];
            population[agent, 6]  <- UTILITY[focal_agent, 6];
            population[agent, 7]  <- UTILITY[focal_agent, 7];
            population[agent, 9]  <- UTILITY[focal_agent, 9];
            population[agent, 11] <- UTILITY[focal_agent, 11];
            population[agent, 13] <- UTILITY[focal_agent, 13];
            population[agent, 15] <- UTILITY[focal_agent, 15];
            population[agent, 8]  <- 0;
            population[agent, 10] <- 0;
            population[agent, 12] <- 0;
            population[agent, 14] <- 0;
            population[agent, 14] <- 0;
            population[agent, 16] <- 0;
            lowest_cost <- min_cost(budget_total = budget_total, util = UTILITY,
                                    row = focal_agent);
            budget_count <- budget_total;
            while(budget_count > lowest_cost){
                affect_it <- 2 * floor( runif(n=1) * 5); # In c, do{ }while(!=6)
                cost_col  <- affect_it + 7;
                act_col   <- affect_it + 8;
                the_cost  <- population[agent, cost_col];
                if(budget_count - the_cost > 0){
                    population[agent, act_col] <- population[agent, act_col]+1;
                    budget_count <- budget_count - the_cost;
                } # Inf possible if keeps looping and can't remove 1
            }
        }
    }
    return(population);
}

################################################################################
# Below will be looped
################################################################################
# Crossover (second brown box)
# Would really help to define the SWAP function in c here -- use int trick
crossover <- function(population){
    agents     <- dim(population)[1];
    cross_prob <- 0.1;
    for(agent in 1:dim(population)[1]){
        c1 <- runif(n=1);
        if(c1 < cross_prob){
            cross_with                <- agents * floor(runif(n=1)) + 1;
            temp                      <- population[cross_with, 8];
            population[cross_with, 8] <- population[agent, 8];
            population[agent, 8]      <- temp;
        }
        c2 <- runif(n=1);
        if(c2 < cross_prob){
            cross_with                <- agents * floor(runif(n=1)) + 1;
            temp                      <- population[cross_with, 10];
            population[cross_with,10] <- population[agent, 10];
            population[agent, 10]     <- temp;
        }
        c1 <- runif(n=1);
        if(c1 < cross_prob){
            cross_with                <- agents * floor(runif(n=1)) + 1;
            temp                      <- population[cross_with, 12];
            population[cross_with,12] <- population[agent, 12];
            population[agent, 12]     <- temp;
        }
        c1 <- runif(n=1);
        if(c1 < cross_prob){
            cross_with                <- agents * floor(runif(n=1)) + 1;
            temp                      <- population[cross_with, 14];
            population[cross_with,14] <- population[agent, 14];
            population[agent, 14]     <- temp;
        }
        c1 <- runif(n=1);
        if(c1 < cross_prob){
            cross_with                <- agents * floor(runif(n=1)) + 1;
            temp                      <- population[cross_with, 16];
            population[cross_with,16] <- population[agent, 16];
            population[agent, 16]     <- temp;
        }
    }
    return(population);
}

# Mutation (third brown box)
# Note that negative values equate to zero -- there can be a sort of threshold
# evolution, therefore, a la Duthie et al. 2016 Evolution
mutation <- function(population, mutation_prob){
    mutation_prob <- mutation_prob * 0.5;
    for(agent in 1:dim(population)[1]){
        c1 <- runif(n=1);
        if(c1 < mutation_prob){
            population[agent,8] <- population[agent, 8] - 1;    
        }
        if(c1 > (1 - mutation_prob) ){
            population[agent,8] <- population[agent, 8] + 1;    
        }
        c2 <- runif(n=1);
        if(c2 < mutation_prob){
            population[agent,10] <- population[agent, 10] - 1;    
        }
        if(c2 > (1 - mutation_prob) ){
            population[agent,10] <- population[agent, 10] + 1;    
        }     
        c3 <- runif(n=1);
        if(c3 < mutation_prob){
            population[agent,12] <- population[agent, 12] - 1;    
        }
        if(c3 > (1 - mutation_prob) ){
            population[agent,12] <- population[agent, 12] + 1;    
        }         
        c4 <- runif(n=1);
        if(c4 < mutation_prob){
            population[agent,14] <- population[agent, 14] - 1;    
        }
        if(c4 > (1 - mutation_prob) ){
            population[agent,14] <- population[agent, 14] + 1;    
        }  
        c5 <- runif(n=1);
        if(c5 < mutation_prob){
            population[agent,16] <- population[agent, 16] - 1;    
        }
        if(c5 > (1 - mutation_prob) ){
            population[agent,16] <- population[agent, 16] + 1;    
        }                 
    }
    return(population);
}

# Need to incorporate selection on *going over budget* and *negative values*
constrain_cost <- function(population){
    for(agent in 1:dim(population)[1]){
        over <- 0;
        if(population[agent, 8] < 0){
            population[agent, 8] <- 0;   
        }
        over <- over + (population[agent, 8] * population[agent, 7]);
        if(population[agent, 10] < 0){
            population[agent, 10] <- 0;   
        }
        over <- over + (population[agent, 10] * population[agent, 9]);
        if(population[agent, 12] < 0){
            population[agent, 12] <- 0;   
        }
        over <- over + (population[agent, 12] * population[agent, 11]);
        if(population[agent, 14] < 0){
            population[agent, 14] <- 0;   
        }
        over <- over + (population[agent, 14] * population[agent, 13]);
        if(population[agent, 16] < 0){
            population[agent, 16] <- 0;   
        }
        over <- over + (population[agent, 16] * population[agent, 15]);
        while(over > budget_total){
            affect_it <- 2 * floor( runif(n=1) * 5); # Must be a better way
            cost_col  <- affect_it + 7;
            act_col   <- affect_it + 8;
            if(population[agent,act_col] > 0){
                the_cost  <- population[agent, cost_col];
                population[agent, act_col] <- population[agent, act_col] - 1;
                over <- over - the_cost;
            }
        }
    }
    return(population);
}

# Fitness -- this is the most challenging function
# Just as proof of concept, let's just say fitness is maximised by helpem (16)
strat_fitness <- function(population){
    fitness <- rep(0, dim(population)[1]);
    for(agent in 1:length(fitness)){
        fitness[agent] <- population[agent,16];    
    }
    return(fitness);
}

# Tournament selection on population
tournament <- function(population, fitness){
    agents <- dim(population)[1];
    traits <- dim(population)[2];
    winners <- matrix(data = 0, nrow = agents, ncol=traits);
    for(agent in 1:dim(winners)[1]){
        r1   <- floor( runif(n=1) * dim(winners)[1] ) + 1   
        r2   <- floor( runif(n=1) * dim(winners)[1] ) + 1   
        r3   <- floor( runif(n=1) * dim(winners)[1] ) + 1   
        r4   <- floor( runif(n=1) * dim(winners)[1] ) + 1   
        wins <- r1;
        if(fitness[wins] < fitness[r2]){
            wins <- r2;
        }
        if(fitness[wins] < fitness[r3]){
            wins <- r3;
        }
        if(fitness[wins] < fitness[r4]){
            wins <- r4;   
        }
        for(trait in 1:dim(winners)[2]){
            winners[agent, trait] <- population[wins, trait];
        }
    }
    return(winners);
}
# Replacement of tournament winners;
# Note, to swap the address of arrays, look here: http://stackoverflow.com/questions/13246615/swap-two-pointers-to-exchange-arrays
# I think this is just a matter of swapping all pointer addresses.





################################################################################
################################################################################
# Combined genetic algorithm:
################################################################################
################################################################################
#proc_start <- proc.time();


#mean_fitness <- NULL;
#clone_seed   <- 11;
#budget_total <- 100;
#focal_agent  <- 2;

# Add three agents, representing three stake-holders, to the utility array
#a0 <- c(0, 0, 0,  0, 0, 0, 0, 0,  0, 0,  0, 0,  0, 0,  0, 0);
#a1 <- c(1, 0, 0,  2, 0, 0, 8, 5, 30, 0, 20, 0, 10, 0, 10, 0);
#a2 <- c(2, 0, 0, -1, 1, 1, 0, 0, 50, 0,  0, 1,  1, 2,  2, 1);

#UTILITY <- rbind(a0, a1, a2);

#population <- matrix(data = 0, ncol = 16, nrow = 100);

#population <- initialise_pop(UTILITY = UTILITY, focal_agent = 2, 
 #                            population = population);

#mean_fit   <- NULL;
#iterations <- 30;
#while(iterations > 0){
#    population <- crossover(population = population);
#    population <- mutation(population = population, mutation_prob = 0.2);
#    population <- constrain_cost(population = population);
#    fitness    <- strat_fitness(population);
#    population <- tournament(population = population, fitness = fitness);
#    mean_fit   <- c(mean_fit, mean(fitness));
#    iterations <- iterations - 1;
#}

#proc_end   <- proc.time();
#time_taken <- proc_end - proc_start;


#plot(x=1:length(mean_fit), y=mean_fit, pch=20, cex=1.5, type="b",
#     xlab="Iterations of genetic algorithm", ylab="Mean strategy fitness");







################################################################################
################################################################################
################################################################################
################################################################################
# GENETIC ALGORITHM FOR A 2 BY 2 GAME, sequential
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


agents <- NULL;

history_vec <- c(0,0,0,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1);
history     <- matrix(data = history_vec, ncol = 3, byrow = TRUE);

CC <- c(2, 2);
CD <- c(0, 3);
DC <- c(3, 0);
DD <- c(1, 1);

for(i in 1:100){
    agents[[i]] <- rbinom(n=8, size=1, prob=0.5);
}


PD <- function(a1_play, a2_play){
    points <- 0;
    if(a1_play == 0 & a2_play == 0){
        points <- 2;
    }
    if(a1_play == 0 & a2_play == 1){
        points <- 0;
    }
    if(a1_play == 1 & a2_play == 0){
        points <- 3;
    }
    if(a1_play == 1 & a2_play == 1){
        points <- 1;
    }
    return(points);
}


#for(foc in 1:100){

    opps    <- sample(x=1:100, size=10, replace=TRUE);

    agent_1 <- rep(0, 100);
    agent_2 <- rep(0, 100);
    payoff1 <- rep(0, 100);
    payoff2 <- rep(0, 100);

    foc <- 1;
    opp <- 2;
    
    # Special round 1 (not enough history);
    use        <- 1:8;
    agent_1[1] <- sample(x=agents[[foc]][use], size=1);
    agent_2[1] <- sample(x=agents[[opp]][use], size=1);
    payoff1[1] <- PD(agent_1[1], agent_2[1]);
    payoff2[1] <- PD(agent_2[1], agent_1[1]);










































    # Special round 2 (not enough history);
    use1       <- c(1, 3, 4, 7);
    use2       <- c(2, 5, 6, 8);
    if(payoffs[1] == 2){
        agent_1[2] <- sample(x=agents[[foc]][use1], size=1);
        agent_2[2] <- sample(x=agents[[opp]][use1], size=1);
        payoffs[2] <- PD(agent_1[2], agent_2[2]);
    }
    if(payoffs[1] == 0){
        agent_1[2] <- sample(x=agents[[foc]][use2], size=1);
        agent_2[2] <- sample(x=agents[[opp]][use1], size=1);
        payoffs[2] <- PD(agent_1[2], agent_2[2]);
    }
    if(payoffs[1] == 3){
        agent_1[2] <- sample(x=agents[[foc]][use1], size=1);
        agent_2[2] <- sample(x=agents[[opp]][use2], size=1);
        payoffs[2] <- PD(agent_1[2], agent_2[2]);
    }
    if(payoffs[1] == 1){
        agent_1[2] <- sample(x=agents[[foc]][use2], size=1);
        agent_2[2] <- sample(x=agents[[opp]][use2], size=1);
        payoffs[2] <- PD(agent_1[2], agent_2[2]);
    }
    
    # Special round 3 (still not enough history);
    use1       <- c(1, 4);
    use2       <- c(2, 6);
    use3       <- c(3, 7);
    use4       <- c(5, 8);
    # ---------------------------------  Last was CC
    if(payoffs[1] == 2 & payoffs[2] == 2){ # CC then CC
        agent_1[3] <- sample(x=agents[[foc]][use1], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use1], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 0 & payoffs[2] == 2){ # CD then CC
        agent_1[3] <- sample(x=agents[[foc]][use3], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use1], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 3 & payoffs[2] == 2){ # DC then CC
        agent_1[3] <- sample(x=agents[[foc]][use1], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use3], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 1 & payoffs[2] == 2){ # DD then CC
        agent_1[3] <- sample(x=agents[[foc]][use3], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use3], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    # ---------------------------------  Last was CD
    if(payoffs[1] == 2 & payoffs[2] == 0){ # CC then CD
        agent_1[3] <- sample(x=agents[[foc]][use2], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use1], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 0 & payoffs[2] == 0){ # CD then CD
        agent_1[3] <- sample(x=agents[[foc]][use4], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use1], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 3 & payoffs[2] == 0){ # DC then CD
        agent_1[3] <- sample(x=agents[[foc]][use2], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use3], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 1 & payoffs[2] == 0){ # DD then CD
        agent_1[3] <- sample(x=agents[[foc]][use3], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use2], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    # ---------------------------------  Last was DC
    if(payoffs[1] == 2 & payoffs[2] == 3){ # CC then DC
        agent_1[3] <- sample(x=agents[[foc]][use1], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use2], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 0 & payoffs[2] == 3){ # CD then DC
        agent_1[3] <- sample(x=agents[[foc]][use3], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use2], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 3 & payoffs[2] == 3){ # DC then DC
        agent_1[3] <- sample(x=agents[[foc]][use1], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use4], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 1 & payoffs[2] == 3){ # DD then DC
        agent_1[3] <- sample(x=agents[[foc]][use3], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use4], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    # ---------------------------------  Last was DD
    if(payoffs[1] == 2 & payoffs[2] == 3){ # CC then DD
        agent_1[3] <- sample(x=agents[[foc]][use2], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use2], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 0 & payoffs[2] == 3){ # CD then DD
        agent_1[3] <- sample(x=agents[[foc]][use4], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use2], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 3 & payoffs[2] == 3){ # DC then DD
        agent_1[3] <- sample(x=agents[[foc]][use2], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use4], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }
    if(payoffs[1] == 1 & payoffs[2] == 3){ # DD then DD
        agent_1[3] <- sample(x=agents[[foc]][use4], size=1);
        agent_2[3] <- sample(x=agents[[opp]][use4], size=1);
        payoffs[3] <- PD(agent_1[3], agent_2[3]);
    }





#}



































