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

rm(list=ls(all=TRUE));


agents <- NULL;

history_vec <- c(0,0,0,0,0,1,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,1,1);
history     <- matrix(data = history_vec, ncol = 3, byrow = TRUE);

for(i in 1:100){
    agents[[i]] <- rbinom(n=9, size=1, prob=0.5);
}


crossover <- function(agents, prob = 0.1){
    for(i in 1:length(agents)){
        cross <- runif(n=9);
        if(min(cross) < prob){
            partner <- sample(x=1:length(agents), size = 1);
            switch  <- which(cross < prob);
            temp    <- agents[[i]][switch];
            agents[[i]][switch] <- agents[[partner]][switch];
            agents[[partner]][switch] <- temp;
        }
    }
    return(agents);
}

mutation <- function(agents, prob = 0.01){
    for(i in 1:length(agents)){
        mutate <- runif(n=9);
        if(min(mutate) < prob){
            change <- which(mutate < prob); 
            newmut <- rbinom(n=length(change), size=1, prob=0.5);
            agents[[i]][change] <- newmut;
        }
    }
    return(agents);
}

PD <- function(a1_play, a2_play){
    points <- 0;
    if(a1_play == 0 & a2_play == 0){
        points <- 0;
    }
    if(a1_play == 0 & a2_play == 1){
        points <- 0;
    }
    if(a1_play == 1 & a2_play == 0){
        points <- 20;
    }
    if(a1_play == 1 & a2_play == 1){
        points <- 0;
    }
    return(points);
}



check_fitness <- function(history, agents){

    fitness <- NULL;

    for(foc in 1:100){

        opponents <- sample(x=1:100, size=10, replace=TRUE);
        foc_score <- rep(0, length(opponents));
    
        for(opps in 1:length(opponents)){
            opp     <- opponents[opps];
            agent_1 <- rep(0, 100);
            agent_2 <- rep(0, 100);
            payoff1 <- rep(0, 100);
            payoff2 <- rep(0, 100);
    
            # Special round 1 (not enough history);
            agent_1[1] <- agents[[foc]][9]; # First turn
            agent_2[1] <- agents[[opp]][9];
            payoff1[1] <- PD(agent_1[1], agent_2[1]);
            payoff2[1] <- PD(agent_2[1], agent_1[1]);

            # Special round 2 (not enough history);
            resp_1     <- which(history[1:2,3] == agent_2[1]);
            resp_2     <- which(history[1:2,3] == agent_1[1]);
            agent_1[2] <- agents[[foc]][resp_1];
            agent_2[2] <- agents[[opp]][resp_2];
            payoff1[2] <- PD(agent_1[2], agent_2[2]);
            payoff2[2] <- PD(agent_1[2], agent_2[2]);

            # Special round 3 (not enough history);
            resp_1     <- which( history[1:4,2] == agent_2[1] & 
                                 history[1:4,3] == agent_2[2]
                               );
            resp_2     <- which( history[1:4,2] == agent_1[1] & 
                                 history[1:4,3] == agent_1[2]
                               );
            agent_1[3] <- agents[[foc]][resp_1];
            agent_2[3] <- agents[[opp]][resp_2];
            payoff1[3] <- PD(agent_1[3], agent_2[3]);
            payoff2[3] <- PD(agent_1[3], agent_2[3]);

            # Remaining rounds (enough history for complete memory);
            for(round in 4:100){
                mem1 <- round - 3;
                mem2 <- round - 2;
                mem3 <- round - 1;
                resp_1     <- which( history[,1] == agent_2[mem1] & 
                                     history[,2] == agent_2[mem2] &
                                     history[,3] == agent_2[mem3]
                                   );
                resp_2     <- which( history[,1] == agent_1[mem1] & 
                                     history[,2] == agent_1[mem2] &
                                     history[,3] == agent_1[mem3]
                                   );        
                agent_1[round] <- agents[[foc]][resp_1];
                agent_2[round] <- agents[[opp]][resp_2];
                payoff1[round] <- PD(agent_1[round], agent_2[round]);
                payoff2[round] <- PD(agent_2[round], agent_1[round]);
            }

            foc_score[opps] <- sum(payoff1);
        }
    
        fitness[foc] <- sum(foc_score);
    }
    return(fitness);
}



tournament <- function(agents, fitness){
    sum_fit    <- lapply(X = fitness, FUN = sum);
    tot_fit    <- unlist(sum_fit);
    new_agents <- NULL;
    for(i in 1:length(fitness)){
        rand_draw <- sample(x = 1:length(fitness), size = 10, replace = TRUE);
        maxfit    <- max(tot_fit[rand_draw]);
        take_draw <- which(tot_fit[rand_draw] == maxfit)[1];
        winner    <- rand_draw[take_draw];
        new_agents[[i]] <- agents[[winner]];
    } 
    return(new_agents);
}





mean_fitness <- NULL;
generations  <- 20;

gen <- 0;

while(gen < generations){

    agents  <- crossover(agents = agents, prob = 0.01);

    agents  <- mutation(agents = agents, prob = 0.01);

    fitness <- check_fitness(history = history, agents = agents);

    agents  <- tournament(agents = agents, fitness = fitness);

    mean_fitness[[gen+1]] <- mean(fitness);
    
    gen <- gen + 1;
    
    print(gen);
}


final_agent_vec <- unlist(agents);
final_agents    <- matrix(data=final_agent_vec, ncol = 9 , byrow = TRUE);



see_rounds <- function(agents, foc_agent, opp_agent){
    agent_1 <- rep(0, 100);
    agent_2 <- rep(0, 100);
    payoff1 <- rep(0, 100);
    payoff2 <- rep(0, 100);

    # Special round 1 (not enough history);
    agent_1[1] <- agents[[foc_agent]][9]; # First turn
    agent_2[1] <- agents[[opp_agent]][9];
    payoff1[1] <- PD(agent_1[1], agent_2[1]);
    payoff2[1] <- PD(agent_2[1], agent_1[1]);
    
    # Special round 2 (not enough history);
    resp_1     <- which(history[1:2,3] == agent_2[1]);
    resp_2     <- which(history[1:2,3] == agent_1[1]);
    agent_1[2] <- agents[[foc_agent]][resp_1];
    agent_2[2] <- agents[[opp_agent]][resp_2];
    payoff1[2] <- PD(agent_1[2], agent_2[2]);
    payoff2[2] <- PD(agent_1[2], agent_2[2]);
    
    # Special round 3 (not enough history);
    resp_1     <- which( history[1:4,2] == agent_2[1] & 
                         history[1:4,3] == agent_2[2]
                       );
    resp_2     <- which( history[1:4,2] == agent_1[1] & 
                         history[1:4,3] == agent_1[2]
                       );
    agent_1[3] <- agents[[foc_agent]][resp_1];
    agent_2[3] <- agents[[opp_agent]][resp_2];
    payoff1[3] <- PD(agent_1[3], agent_2[3]);
    payoff2[3] <- PD(agent_1[3], agent_2[3]);
    
    # Remaining rounds (enough history for complete memory);
    for(round in 4:100){
        mem1 <- round - 3;
        mem2 <- round - 2;
        mem3 <- round - 1;
        resp_1     <- which( history[,1] == agent_2[mem1] & 
                                 history[,2] == agent_2[mem2] &
                                 history[,3] == agent_2[mem3]
        );
        resp_2     <- which( history[,1] == agent_1[mem1] & 
                                 history[,2] == agent_1[mem2] &
                                 history[,3] == agent_1[mem3]
        );        
        agent_1[round] <- agents[[foc_agent]][resp_1];
        agent_2[round] <- agents[[opp_agent]][resp_2];
        payoff1[round] <- PD(agent_1[round], agent_2[round]);
        payoff2[round] <- PD(agent_2[round], agent_1[round]);
    }

    round_hist <- data.frame(agent_1, agent_2, payoff1, payoff2);
    return(round_hist);
}




