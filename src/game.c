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
                    double budget, int carbon_copies, int ROWS, int COLS,
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
 * This function returns how many resources are on a stake-holder's land
 * ========================================================================== */
int res_on_my_land(double **resources, double ***land, int total, int owner,
                   int type1, int type2, int type3){
    int xloc, yloc;
    int resource;
    int resources_on_land;
    
    resources_on_land = 0;
    for(resource = 0; resource < total; resource++){
        if(resources[resource][1] == type1 &&
           resources[resource][2] == type2 &&
           resources[resource][3] == type3
          ){
            xloc = (int) resources[resource][4];
            yloc = (int) resources[resource][5];
            if(land[xloc][yloc][2] == owner){
                resources_on_land++;
            }
        }
    }
    
    return resources_on_land;
}


/* =============================================================================
 * This function calculated each payoff for rows in the action matrix
 * ========================================================================== */
void calc_payoffs(double ***population, int ROWS, double ***landscape, 
                  double **resources, int res_number, int landowner,
                  int land_x, int land_y, double *payoff_vector, int agent){
    
    int xloc, yloc, yield_layer;
    int resource, row;
    int landscape_specific;
    int res_count;
    double cell_yield;
    
    for(row = 0; row < ROWS; row++){
        payoff_vector[row] = 0;
        if(population[row][0][agent] == -2){
            for(resource = 0; resource < res_number; resource++){
                if(population[row][1][agent] == resources[resource][1]  &&
                   population[row][2][agent] == resources[resource][2]  &&
                   population[row][3][agent] == resources[resource][3]
                ){    
                    landscape_specific = population[row][6][agent];
                    if(landscape_specific == 0){
                        res_count++;
                    }else{
                        xloc = resources[resource][4];
                        yloc = resources[resource][5];
                        if(landscape[xloc][yloc][2] == landowner){
                            res_count++;    
                        }
                    }
                }
            }
            payoff_vector[row] += res_count;
        }
        if(population[row][0][agent] == -1){
            yield_layer = population[row][1][agent];
            for(xloc = 0; xloc < land_x; xloc++){
                for(yloc = 0; yloc < land_y; yloc++){
                    if(landscape[xloc][yloc][2] == landowner){
                        cell_yield = landscape[xloc][yloc][yield_layer];
                        payoff_vector[row] += cell_yield;
                    }
                }
            }
        }
        if(population[row][0][agent] > -1){
            payoff_vector[row] = 0;
        }
    }
}


/* =============================================================================
 * This function calculates an individual agent's fitness
 * ========================================================================== */
void calc_agent_fitness(double ***population, int ROWS, int COLS, int landowner,
                        double ***landscape, double **resources, int res_number,
                        int land_x, int land_y, double *fitnesses){
    
    int agent;
    int res_on_land;
    double *payoff_vector;
    
    payoff_vector = malloc(ROWS * sizeof(double));
    
    res_on_land = res_on_my_land(resources, landscape, res_number, landowner,
                                 1, 0, 0);
    
    calc_payoffs(population, ROWS, landscape, resources, res_number, landowner,
                 land_x, land_y, payoff_vector, agent);
    
    
    free(payoff_vector);
    
}


/* =============================================================================
 * This is a preliminary function that checks the fitness of each agent by 
 * passing through a loop to calc_agent_fitness
 *     fitnesses: Array to order fitnesses of the agents in the population
 *     population: array of the population that is made (malloc needed earlier)
 *     pop_size: The size of the total population (layers to population)
 *     ROWS: Number of rows in the COST and ACTION arrays
 *     COLS: Number of columns in the COST and ACTION arrays
 *     landscape: The landscape array
 *     resources: The resource array
 *     agent_array: The agent array
 *     res_number: The number of rows in the resource array
 *     landowner: The agent ID of interest -- also the landowner
 *     land_x: The x dimension of the landscape
 *     land_y: The y dimension of the landscape
 * ========================================================================== */
void strategy_fitness(double *fitnesses, double ***population, int pop_size, 
                      int ROWS, int COLS, double ***landscape,  
                      double **resources, double **agent_array,
                      int res_number, int landowner, int land_x, int land_y){
    int xloc, yloc, yield_layer;
    int agent, resource, row;
    int res_on_land, landscape_specific;
    double cell_yield, res_count;
    
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
        double ***LANDSCAPE, double *paras, int xdim, int ydim, 
        int res_number, int land_x, int land_y, int agent){
    
    int row, col, gen, layer;
    int sampleK, chooseK;
    int popsize, agent_seed;
    int budget;
    int landowner;
    int generations;
    int *winners;
    double mutation_rate, crossover_rate;
    double ***POPULATION;
    double ***NEW_POP;
    double *fitnesses;

    popsize        = (int) paras[21];
    generations    = (int) paras[22];
    agent_seed     = (int) paras[23];
    sampleK        = (int) paras[24];
    chooseK        = (int) paras[25];
    mutation_rate  = paras[26];
    crossover_rate = paras[27];
    budget         = AGENT[agent][16];
    landowner      = AGENT[agent][0];

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
    
    initialise_pop(ACTION, COST, agent, popsize, budget, agent_seed, xdim, ydim, 
                   POPULATION);
    
    gen = 0;
    while(gen < generations){

        crossover(POPULATION, popsize, xdim, ydim, crossover_rate);
        
        mutation(POPULATION, popsize, xdim, ydim, mutation_rate);
    
        constrain_costs(POPULATION, COST, agent, popsize, xdim, ydim, budget);
  
        strategy_fitness(fitnesses, POPULATION, popsize, xdim, ydim, LANDSCAPE, 
                         RESOURCES, AGENT, res_number, landowner, land_x, land_y
                        );
  
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
    
    

    
