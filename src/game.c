#include "utilities.h"

/* =============================================================================
 * This file is a work in progress, which will have all of the necessary
 * functions for running the genetic algorithm. Separate functions will 
 * call the genetic algorithm from R and C (C is default in G-MSE), and this
 * file will link with the user.c file to run a genetic algorithm for each
 * unique individual agent.
 * ========================================================================== */

/* =============================================================================
 * This function will find the minimum cost of an action in the COST array
 * for a particular agent (layer). Inputs include:
 *     COST: A full 3D COST array
 *     layer: The layer on which the minimum is going to be found
 *     budget: The total budget that the agent has to work with (initliases)
 *     rows: The total number of rows in the COST array
 *     cols: The total number of cols in the COST array
 * ========================================================================== */
int min_cost(double ***COST, int layer, double budget, int rows, int cols){
    int i, j;
    double the_min;
    
    the_min = budget;
    for(i = 0; i < rows; i++){
        for(j = 0; j < cols; j++){
            if(COST[i][j][layer] < the_min){
                the_min = COST[i][j][layer];
            }
        }
    }
    return the_min; 
}

/* =============================================================================
 * This function will initialise a population from the ACTION and COST arrays, a
 * particular focal agent, and specification of how many times an agent should
 * be exactly replicated versus how many times random values shoudl be used.
 * Necessary variable inputs include:
 *     ACTION: A 3D array of action values
 *     COST: A 3D array of costs of performing actions
 *     layer: The 'z' layer of the COST and ACTION arrays to be initialised
 *     pop_size: The size of the total population (layers to population)
 *     carbon_copies: The number of identical agents used as seeds
 *     budget: The budget that random agents have to work with
 *     ROWS: Number of rows in the COST and ACTION arrays
 *     COLS: Number of columns in the COST and ACTION arrays
 *     population: array of the population that is made (malloc needed earlier)
 * ========================================================================== */
void initialise_pop(double ***ACTION, double ***COST, int layer, int pop_size,
                    int budget, int carbon_copies, int ROWS, int COLS,
                    double ***population){
    
    int xpos, ypos;
    int agent;
    int row, col;
    double lowest_cost;
    double budget_count;
    double check_cost;

    /* First read in pop_size copies of the ACTION layer of interest */
    for(agent = 0; agent < pop_size; agent++){
        for(row = 0; row < ROWS; row++){
            population[row][0][agent] = ACTION[row][0][layer];
            population[row][1][agent] = ACTION[row][1][layer];
            population[row][2][agent] = ACTION[row][2][layer];
            population[row][3][agent] = ACTION[row][3][layer];
            if(agent < carbon_copies){
                for(col = 4; col < COLS; col++){
                    population[row][col][agent] = ACTION[row][col][layer];
                }
            }else{
                for(col = 4; col < COLS; col++){
                    population[row][col][agent] = 0;
                }
            }
        }
        lowest_cost  =  min_cost(COST, layer, budget, ROWS, COLS);
        budget_count =  budget;
        if(lowest_cost <= 0){
            printf("Lowest cost is too low (must be positive) \n");
            break;
        }
        while(budget_count > lowest_cost){
            do{
                do{ /* This do assures xpos never equals ROWS (unlikely) */
                    xpos = (int) floor( runif(0,ROWS) );
                }while(xpos == ROWS);
                do{
                    ypos = (int) floor( runif(4,COLS) );
                }while(ypos == COLS);
            }while(COST[xpos][ypos][layer] > budget_count);
            population[xpos][ypos][agent]++;
            budget_count -= COST[xpos][ypos][layer];
        } /* Should now make random actions allowed by budget */
    }
}

/* =============================================================================
 * This function will use the initialised population from intialise_pop to make
 * the population array undergo crossing over and random locations for 
 * individuals in the population. Note that we'll later keep things in budget
 * Necessary variable inputs include:
 *     population: array of the population that is made (malloc needed earlier)
 *     pop_size: The size of the total population (layers to population)
 *     ROWS: Number of rows in the COST and ACTION arrays
 *     COLS: Number of columns in the COST and ACTION arrays
 *     pr: Probability of a crossover site occurring at an element.
 * ========================================================================== */
void crossover(double ***population, int pop_size, int ROWS, int COLS, 
               double pr){
    
    int agent, row, col;
    int cross_partner;
    double do_cross;
    double agent_val, partner_val;
    
    /* First do the crossovers */
    for(agent = 0; agent < pop_size; agent++){
        do{
            cross_partner = (int) floor( runif(0, pop_size) );
        }while(cross_partner == agent || cross_partner == pop_size);
        for(row = 0; row < ROWS; row++){
            for(col = 4; col < COLS; col++){
                do_cross = runif(0,1);
                if(do_cross < pr){
                    agent_val   = population[row][col][agent];
                    partner_val = population[row][col][cross_partner];
                    population[row][col][agent]         = partner_val;
                    population[row][col][cross_partner] = agent_val;
                }
            }
        }
    }
}

/* =============================================================================
 * This function will use the initialised population from intialise_pop to make
 * the population array undergo mutations at random elements in their array
 * Necessary variable inputs include:
 *     population: array of the population that is made (malloc needed earlier)
 *     pop_size: The size of the total population (layers to population)
 *     ROWS: Number of rows in the COST and ACTION arrays
 *     COLS: Number of columns in the COST and ACTION arrays
 *     pr: Probability of a mutation occurring at an element.
 * ========================================================================== */
void mutation(double ***population, int pop_size, int ROWS, int COLS, 
               double pr){
    
    int agent, row, col;
    double do_mutation;
    double agent_val;
    double half_pr;
    
    half_pr = 0.5 * pr;
    
    /* First do the crossovers */
    for(agent = 0; agent < pop_size; agent++){
        for(row = 0; row < ROWS; row++){
            for(col = 4; col < COLS; col++){
                do_mutation = runif(0,1);
                if( do_mutation < half_pr ){
                    population[row][col][agent]--;
                }
                if( do_mutation > (1 - half_pr) ){
                    population[row][col][agent]++;
                }
                if( population[row][col][agent] < 0 ){
                    population[row][col][agent] *= -1;    
                } /* Change sign if mutates to a negative value */
            }
        }
    }
}


/* =============================================================================
 * This function will ensure that the actions of individuals in the population
 * are within the cost budget after crossover and mutation has taken place
 * Necessary variable inputs include:
 *     population: array of the population that is made (malloc needed earlier)
 *     COST: A 3D array of costs of performing actions
 *     layer: The 'z' layer of the COST and ACTION arrays to be initialised
 *     pop_size: The size of the total population (layers to population)
 *     ROWS: Number of rows in the COST and ACTION arrays
 *     COLS: Number of columns in the COST and ACTION arrays
 *     budget: The budget that random agents have to work with
 * ========================================================================== */
void constrain_costs(double ***population, double ***COST, int layer, 
                     int pop_size, int ROWS, int COLS, double budget){
    
    int xpos, ypos;
    int agent, row, col;
    double tot_cost, action_val, action_cost;

    for(agent = 0; agent < pop_size; agent++){
        tot_cost = 0;
        for(row = 0; row < ROWS; row++){
            for(col = 4; col < COLS; col++){
                action_val  = population[row][col][agent];
                action_cost = COST[row][col][layer];
                tot_cost   += (action_val * action_cost);
            }
        }
        while(tot_cost > budget){
            do{ /* This do assures xpos never equals ROWS (unlikely) */
                xpos = (int) floor( runif(0,ROWS) );
            }while(xpos == ROWS);
            do{
                ypos = (int) floor( runif(4,COLS) );
            }while(ypos == COLS);
            if(population[xpos][ypos][agent] > 0){
                population[xpos][ypos][agent]--;
                tot_cost -= COST[xpos][ypos][layer];
            }
        }
    }
}

/* =============================================================================
 * This is a preliminary function that checks the fitness of each agent -- as of
 * now, fitness is just defined by how much action is placed into savem (last
 * column). Things will get much more complex in a bit, but there needs to be
 * some sort of framework in place to first check to see that everything else is
 * working so that I can isolate the fitness function's effect later.
 *     fitnesses: Array to order fitnesses of the agents in the population
 *     population: array of the population that is made (malloc needed earlier)
 *     pop_size: The size of the total population (layers to population)
 *     ROWS: Number of rows in the COST and ACTION arrays
 *     COLS: Number of columns in the COST and ACTION arrays
 *     landscape: The landscape array
 *     resources: The resource array
 *     agent_array: The agent array
 * ========================================================================== */
void strategy_fitness(double *fitnesses, double ***population, int pop_size, 
                      int ROWS, int COLS, double ***landscape,  
                      double **resources, double **agent_array){
    int agent;
    
    for(agent = 0; agent < pop_size; agent++){
        fitnesses[agent] = population[0][12][agent];
    }
}

/* =============================================================================
 * This function takes an array of fitnesses and returns an equal size array of
 * indices, the values of which will define which new individuals will make it
 * into the next population array, and in what proportions.
 *     fitnesses: Array to order fitnesses of the agents in the population
 *     winners: Array of the winners of the tournament
 *     pop_size: The size of the total population (layers to population)
 *     sampleK: The size of the subset of fitnesses sampled to compete
 *     chooseK: The number of individuals selected from the sample
 * ========================================================================== */
void tournament(double *fitnesses, int *winners, int pop_size, int sampleK, 
                int chooseK){
    int samp, i;
    int *samples;
    int left_to_place, placed;
    int rand_samp;
    double *samp_fit;
    
    samples  = malloc(sampleK * sizeof(double));
    samp_fit = malloc(sampleK * sizeof(double));
    placed   = 0;
    
    if(chooseK > sampleK){
        printf("ERROR: Can't choose more winners than sampled in tournament\n");
        printf("Defaulting to sampleK = chooseK \n");
        chooseK = sampleK;
    }
    while(placed < pop_size){ /* Note sampling is done with replacement */
        for(samp = 0; samp < sampleK; samp++){
            do{
                rand_samp      = (int) floor( runif(0, pop_size) );
                samples[samp]  = rand_samp;
                samp_fit[samp] = fitnesses[rand_samp];
            }while(rand_samp == pop_size);
        }
      
        find_descending_order(samples, samp_fit, sampleK);

        if( (chooseK + placed) >= pop_size){
            chooseK = pop_size - placed;    
        }
        samp = 0;
        while(samp < chooseK && placed < pop_size){
            winners[placed] = samples[samp];
            placed++;
            samp++;
        }
    }
    free(samp_fit);
    free(samples);
}


/* =============================================================================
 * This function takes winners from the tournament function and replicates them
 * in a new population array. 
 *     population: array of the population
 *     winners: Array of the winners of the tournament
 *     pop_size: The size of the total population (layers to population)
 *     new_pop: Array of the new population to be made
 *     ROWS: Number of rows in the COST and ACTION arrays
 *     COLS: Number of columns in the COST and ACTION arrays
 * ========================================================================== */
void place_winners(double ****population, int *winners, int pop_size, int ROWS, 
                   int COLS){

    int i, row, col, layer, winner;
    double a_value;
    double ***NEW_POP;
    
    NEW_POP    = malloc(ROWS * sizeof(double *));
    for(row = 0; row < ROWS; row++){
        NEW_POP[row]    = malloc(COLS * sizeof(double *));
        for(col = 0; col < COLS; col++){
            NEW_POP[row][col]    = malloc(pop_size * sizeof(double));
        }
    }
    
    for(i = 0; i < pop_size; i++){
        winner = winners[i];
        for(row = 0; row < ROWS; row++){
            for(col = 0; col < COLS; col++){
                a_value              = (*population)[row][col][winner];
                NEW_POP[row][col][i] = a_value;
            }
        }
    }
    
    swap_arrays((void*)&(*population), (void*)&NEW_POP);
    
    for(row = 0; row < ROWS; row++){
        for(col = 0; col < COLS; col++){
            free(NEW_POP[row][col]);
        }
        free(NEW_POP[row]); 
    }
    free(NEW_POP);
}




/* 
 * This function will eventually call all of the other functions used in the
 * genetic algorithm. For now, it is being used just to call the other functions
 * and therefore test out whether or not they work.
 */
void ga(double ***ACTION, double ***COST, double **AGENT, double **RESOURCES,
        double ***LANDSCAPE, int agent){
    
    int row, col, gen, layer;
    int xdim, ydim;
    int sampleK, chooseK;
    int popsize;
    int generations;
    int *winners;
    double ***POPULATION;
    double ***NEW_POP;
    double *fitnesses;

    generations = 20;
    popsize = 100;
    sampleK = 5;
    chooseK = 2;
    
    ydim = 13;
    xdim = 4;
    
    POPULATION = malloc(xdim * sizeof(double *));
    for(row = 0; row < xdim; row++){
        POPULATION[row] = malloc(ydim * sizeof(double *));
        for(col = 0; col < ydim; col++){
            POPULATION[row][col] = malloc(popsize * sizeof(double));
        }
    }
    for(layer = 0; layer < popsize; layer++){
        for(col = 0; col < ydim; col++){
            for(row = 0; row < xdim; row++){
                POPULATION[row][col][layer] = 0;
            }
        }
    }  
    
    fitnesses = malloc(popsize * sizeof(double));
    winners   = malloc(popsize * sizeof(int));
    
    for(row = 0; row < popsize; row++){ /* Need to initialise to some values */
        fitnesses[row] = 0;
        winners[row]   = 0;
    }
    
    
    initialise_pop(ACTION, COST, agent, popsize, 100, 10, xdim, ydim, 
                   POPULATION);
    
    gen = 0;
    while(gen < generations){

        crossover(POPULATION, popsize, xdim, ydim, 0.1);
        
        mutation(POPULATION, popsize, xdim, ydim, 0.1);
    
        constrain_costs(POPULATION, COST, agent, popsize, xdim, ydim, 100);
  
        strategy_fitness(fitnesses, POPULATION, popsize, xdim, ydim, LANDSCAPE, 
                         RESOURCES, AGENT);
  
        tournament(fitnesses, winners, popsize, sampleK, chooseK);
   
        place_winners(&POPULATION, winners, popsize, xdim, ydim);
   
        gen++;
    
    }
    
    for(row = 0; row < xdim; row++){
        for(col = 0; col < ydim; col++){
            ACTION[row][col][agent] = POPULATION[row][col][agent];
        }
    }

    free(winners);
    free(fitnesses);
    for(row = 0; row < xdim; row++){
        for(col = 0; col < ydim; col++){
            free(POPULATION[row][col]);   
        }
        free(POPULATION[row]); 
    }
    free(POPULATION);

}
    
    

    
