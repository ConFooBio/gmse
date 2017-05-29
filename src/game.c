#include "resource.h"

/* =============================================================================
 * This function will find the minimum cost of an action in the COST array
 * for a particular agent (layer). Inputs include:
 *     COST: A full 3D COST array
 *     paras: Vector of global parameters
 *     layer: The layer on which the minimum is going to be found
 *     budget: The total budget that the agent has to work with (initliases)
 * ========================================================================== */
double min_cost(double ***COST, double *paras, int layer, double budget){
    
    int i, j, ROWS, COLS;
    double the_min;
    
    ROWS = (int) paras[68];
    COLS = (int) paras[69];
    
    the_min = budget;
    for(i = 0; i < ROWS; i++){
        for(j = 0; j < COLS; j++){
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
 * be exactly replicated versus how many times random values should be used.
 * Necessary variable inputs include:
 *     ACTION: A 3D array of action values
 *     COST: A 3D array of costs of performing actions
 *     paras: Vector of global parameters
 *     layer: The 'z' layer of the COST and ACTION arrays to be initialised
 *     budget: The budget that random agents have to work with
 *     population: array of the population that is made (malloc needed earlier)
 *     agentID: The ID of the focal agent
 * ========================================================================== */
void initialise_pop(double ***ACTION, double ***COST, double *paras, int layer,
                    double budget, double ***population, int agentID){
    
    int xpos, ypos, pop_size, carbon_copies, ROWS, COLS, agent;
    int row, col, start_col, col_check, col_start_other, col_start_self;
    double lowest_cost, budget_count, check_cost;
    
    pop_size        = (int) paras[21];
    carbon_copies   = (int) paras[23]; 
    ROWS            = (int) paras[68];
    COLS            = (int) paras[69];
    col_start_other = (int) paras[70];
    col_start_self  = (int) paras[71];

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
                population[row][4][agent] = ACTION[row][4][layer];
                population[row][5][agent] = ACTION[row][5][layer];
                population[row][6][agent] = ACTION[row][6][layer];
                start_col = col_start_self;
                col_check = population[row][0][agent];
                if(col_check > 0 && col_check != agentID){
                    start_col = col_start_other;
                }             
                for(col = start_col; col < COLS; col++){
                    population[row][col][agent] = 0;
                }
            }
        }
        lowest_cost  =  min_cost(COST, paras, layer, budget);
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
 * This function uses the initialised population from intialise_pop to make
 * the population array undergo crossing over and random locations for 
 * individuals in the population. Note that we later keep things in budget
 * Necessary variable inputs include:
 *     population: array of the population that is made (malloc needed earlier)
 *     paras: Vector of global parameters
 *     agentID: The ID of the agent
 * ========================================================================== */
void crossover(double ***population, double *paras, int agentID){
    
    int agent, row, col, start_col, col_check, cross_partner;
    int pop_size, ROWS, COLS, col_start_other, col_start_self;
    double do_cross, pr, agent_val, partner_val;
    
    pop_size        = (int) paras[21];
    pr              = paras[27];
    ROWS            = (int) paras[68];
    COLS            = (int) paras[69];
    col_start_other = (int) paras[70];
    col_start_self  = (int) paras[71];
    
    /* First do the crossovers */
    for(agent = 0; agent < pop_size; agent++){
        do{
            cross_partner = (int) floor( runif(0, pop_size) );
        }while(cross_partner == agent || cross_partner == pop_size);
        for(row = 0; row < ROWS; row++){
            start_col = col_start_self;
            col_check = population[row][0][agent];
            if(col_check > 0 && col_check != agentID){
                start_col = col_start_other;
            }
            for(col = start_col; col < COLS; col++){
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
 *     paras: Vector of global parameters
 *     agentID: The ID of the agent
 * ========================================================================== */
void mutation(double ***population, double *paras, int agentID){
    
    int agent, row, col, start_col, col_check, pop_size, ROWS, COLS;
    int col_start_other, col_start_self;
    double do_mutation, agent_val, half_pr, pr;

    pop_size        = (int) paras[21];
    pr              = paras[26];
    ROWS            = (int) paras[68];
    COLS            = (int) paras[69];
    col_start_other = (int) paras[70];
    col_start_self  = (int) paras[71];
    
    half_pr = 0.5 * pr;
    
    for(agent = 0; agent < pop_size; agent++){
        for(row = 0; row < ROWS; row++){
            start_col = col_start_self;
            col_check = population[row][0][agent];
            if(col_check > 0 && col_check != agentID){
                start_col = col_start_other;
            }
            for(col = start_col; col < COLS; col++){
                do_mutation = runif(0,1);
                if( do_mutation < half_pr){
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
 * This function ensures that the actions of individuals in the population
 * are within the cost budget after crossover and mutation has taken place
 * Necessary variable inputs include:
 *     population: array of the population that is made (malloc needed earlier)
 *     COST: A 3D array of costs of performing actions
 *     paras: Vector of global parameters
 *     layer: The 'z' layer of the COST and ACTION arrays to be initialised
 *     budget: The budget that random agents have to work with
 *     agentID: The ID of the agent
 * ========================================================================== */
void constrain_costs(double ***population, double ***COST, double *paras, 
                     int layer, double budget, int agentID){
    
    int xpos, ypos, agent, row, col, start_col, col_check, pop_size, ROWS, COLS;
    double tot_cost, action_val, action_cost;

    pop_size        = (int) paras[21];
    ROWS            = (int) paras[68];
    COLS            = (int) paras[69];
    
    for(agent = 0; agent < pop_size; agent++){
        tot_cost = 0;
        for(row = 0; row < ROWS; row++){
            start_col = 4;
            col_check = population[row][0][agent];
            if(col_check < 0 || col_check == agentID){
                start_col = 7;
            }
            for(col = start_col; col < COLS; col++){
                action_val  = population[row][col][agent];
                action_cost = COST[row][col][layer];
                /* Don't allow prohibited actions */
                if(action_cost > budget){ 
                    population[row][col][agent] = 0;
                    action_val                  = 0;
                }
                tot_cost   += (action_val * action_cost);
            }
        }
        while(tot_cost > budget){
            do{ /* This do assures xpos never equals ROWS (unlikely) */
                xpos = (int) floor( runif(0,ROWS) );
            }while(xpos == ROWS);
            col_check = population[xpos][0][agent];
            if(col_check > 0 && col_check != agentID){
                do{
                    ypos = (int) floor( runif(4,COLS) );
                }while(ypos == COLS);
            }else{
                do{
                    ypos = (int) floor( runif(7,COLS) );
                }while(ypos == COLS);               
            }
            if(population[xpos][ypos][agent] > 0){
                population[xpos][ypos][agent]--;
                tot_cost -= COST[xpos][ypos][layer];
            }
        }
    }
}

/* =============================================================================
 * This function updates count change and utility arrays for direct actions on 
 * resources
 *     population: The population array of agents in the genetic algorithm
 *     interact_table: The lookup table for figuring out how resources interact
 *     paras: Vector of global parameters
 *     count_change: A vector of how counts have changed as a result of actions
 *     utilities: A vector of the utilities of each resource/landscape level
 *     jaco: The interaction table itself (i.e., Jacobian matrix)
 *     row: The row of the interaction and lookup table being examined
 *     agent: The agent in the population whose fitness is being assessed
 * ========================================================================== */
void res_to_counts(double ***population, int **interact_table, double *paras,
                   double *count_change, double *utilities, double **jaco,
                   int row, int agent){
    
    int i, interest_row, int_num;
    double foc_effect;
    
    int_num = (int) paras[60];
    
    foc_effect  = 0.0;
    foc_effect -= population[row][7][agent];  /* Times birth account for repr?*/
    foc_effect -= population[row][8][agent];  /* But only remove E offspring? */
    foc_effect -= population[row][9][agent];  /* But also remove E offspring? */
    foc_effect += population[row][10][agent]; /* But should less mortality    */
    foc_effect += population[row][11][agent]; /* But should affect offspring? */
    interest_row = 0;
    while(interest_row < int_num){
        if(interact_table[interest_row][0] == 0                         &&
           interact_table[interest_row][1] == population[row][1][agent] &&
           interact_table[interest_row][2] == population[row][2][agent] &&
           interact_table[interest_row][3] == population[row][3][agent]
          ){
               break;
           }else{
               interest_row++;
        }
    }
    for(i = 0; i < int_num; i++){
        count_change[i] += foc_effect * jaco[interest_row][i];
    }
    utilities[interest_row] = population[row][4][agent];
}

/* =============================================================================
 * This function updates count change and utility arrays for direct actions on 
 * a landscape
 *     population: The population array of agents in the genetic algorithm
 *     interact_table: The lookup table for figuring out how resources interact
 *     paras: Vector of global parameters
 *     utilities: A vector of the utilities of each resource/landscape level
 *     row: The row of the interaction and lookup table being examined
 *     agent: The agent in the population whose fitness is being assessed
 *     jaco: The interaction table itself (i.e., Jacobian matrix)
 *     count_change: A vector of how counts have changed as a result of actions
 * ========================================================================== */
void land_to_counts(double ***population, int **interact_table, double *paras,
                    double *utilities, int row, int agent, double **jaco,
                    double *count_change){
    
    int i, act_type, interest_row, int_num;
    double foc_effect, feedin;
    
    int_num = (int) paras[60];
    feedin  = paras[79];
    
    foc_effect   = 0.0;
    foc_effect  -= population[row][9][agent];             /* Kill the crop */
    foc_effect  += (population[row][10][agent] * feedin); /* Feed the crop */
    interest_row = 0;
    while(interest_row < int_num){ /* THIS LOOP IS THE PROBLEM */
        if(interact_table[interest_row][0] == 1){
               break;
           }else{
               interest_row++;
           }
    }
    for(i = 0; i < int_num; i++){
        count_change[i] += foc_effect * jaco[interest_row][i];
    }
    utilities[interest_row] = population[row][4][agent];
}

/* =============================================================================
 * This function checks the fitness of each agent
 *     agent_array: The array of agents
 *     population: array of the population that is made (malloc needed earlier)
 *     paras: Vector of global parameters
 *     fitnesses: Array to order fitnesses of the agents in the population
 *     jaco: The jacobian matrix of resource and landscape interactions
 *     interact_table: Lookup table for figuring out rows of jaco and types
 * ========================================================================== */
void strategy_fitness(double **agent_array, double ***population, double *paras,
                      double *fitnesses, double **jaco, int **interact_table){
    
    int agent, i, row, act_type, int_num, pop_size, ROWS;
    double *count_change, *utilities;
    
    pop_size = (int) paras[21];
    int_num  = (int) paras[60];
    ROWS     = (int) paras[68];
    
    count_change = malloc(int_num * sizeof(double));
    utilities    = malloc(int_num * sizeof(double));
    
    for(agent = 0; agent < pop_size; agent++){
        for(i = 0; i < int_num; i++){
            count_change[i] = 0; /* Initialise all count changes at zero */
            utilities[i]    = 0; /* Same for utilities */
        }
        for(row = 0; row < ROWS; row++){
            act_type   = (int) population[row][0][agent];
            switch(act_type){
                case -2:
                    res_to_counts(population, interact_table, paras,
                                  count_change, utilities, jaco, row, agent);
                    break;
                case -1:
                    land_to_counts(population, interact_table, paras, utilities, 
                                   row, agent, jaco, count_change);
                    break;
                default:
                    break;
            }
        }
        fitnesses[agent] = 0;
        for(i = 0; i < int_num; i++){
            fitnesses[agent] += count_change[i] * utilities[i];
        }
    }
    free(utilities);
    free(count_change);
}

/* =============================================================================
 * This function sums (or averages) a row of COST or ACTION across all layers
 *    array: The 3D array that is meant to be summed or averaged
 *    out: The 2D array where the summed/average values are to be stored
 *    get_mean: TRUE (1) or FALSE (0) indiciating whether to get mean vs sum
 *    paras: Vector of global parameters
 *    agent_array: The agent array
 * ========================================================================== */
void sum_array_layers(double ***array, double **out, int get_mean, 
                      double *paras, double **agent_array){
    
    int row, col, layer, layer_count, ROWS, COLS, layers;

    layers = (int) paras[65];
    ROWS   = (int) paras[68];
    COLS   = (int) paras[69];
    
    if(get_mean == 1){
        layer_count = 0;
        for(layer = 0; layer < layers; layer++){
            if(agent_array[layer][1] > 0){
                layer_count++;
            }
        }
    }
    for(row = 0; row < ROWS; row++){
        for(col = 0; col < COLS; col++){
            out[row][col] = 0;
            for(layer = 0; layer < layers; layer++){
                if(agent_array[layer][1] > 0){
                    if(get_mean == 1){
                        out[row][col] += array[row][col][layer] / layer_count;
                    }else{
                        out[row][col] += array[row][col][layer];
                    }
                }
            }                
        }
    }
}

/* =============================================================================
 * This function updates a temporary action array for changes in policy
 *     population: The population array of agents in the genetic algorithm
 *     merged_acts: The action 2D array of summed elements across 3D ACTION
 *     agent: The agent (layer) in the population being simulated
 *     merged_costs: The mean cost paid for each element in the ACTION array
 *     act_change: The array of predicted new actions given new costs
 *     action_row: The row where the action and old costs are located
 *     manager_row: The row where the new costs from the manager are located
 *     paras: Vector of global parameters
 * ========================================================================== */
void policy_to_counts(double ***population, double **merged_acts, int agent,
                      double **merged_costs, double **act_change, 
                      int action_row, int manager_row, double *paras){
    
    int col, COLS;
    double old_cost, new_cost, cost_change, new_action;
    
    COLS   = (int) paras[69];
    
    for(col = 7; col < COLS; col++){
        old_cost    = merged_costs[action_row][col];
        new_cost    = population[manager_row][col][agent];
        if(new_cost <= 0){
            new_cost = 1;
            population[manager_row][col][agent] = new_cost;
        }
        cost_change = old_cost / new_cost;
        new_action  = merged_acts[action_row][col] * cost_change;
        act_change[action_row][col] = floor(new_action);
    }
}

/* =============================================================================
 * This is a preliminary function that checks the fitness of a manager
 *     fitnesses: Array to order fitnesses of the agents in the population
 *     population: array of the population that is made (malloc needed earlier)
 *     jaco: The jacobian matrix of resource and landscape interactions
 *     agent_array: The agent array
 *     interact_table: Lookup table for figuring out rows of jaco and types
 *     agentID: The ID of the agent
 *     COST: A 3D array of costs of performing actions
 *     ACTION: A 3D array of action values
 *     paras: Vector of global parameters
 * ========================================================================== */
void manager_fitness(double *fitnesses, double ***population, double **jaco,
                     double **agent_array, int **interact_table, int agentID, 
                     double ***COST, double ***ACTION, double *paras){
    
    int agent, i, j, m_lyr, action_row, manager_row, type1, type2, type3;
    int pop_size, int_num, ROWS, COLS, layers;
    double agent_fitness, *count_change, foc_effect, change_dev, max_dev;
    double movem, castem, killem, feedem, helpem, *dev_from_util;
    double utility, *utils, **merged_acts, **merged_costs, **act_change;
    
    pop_size = (int) paras[21];
    int_num  = (int) paras[60];
    layers   = (int) paras[65];
    ROWS     = (int) paras[68];
    COLS     = (int) paras[69];
    
    count_change  = malloc(int_num * sizeof(double));
    utils         = malloc(int_num * sizeof(double));
    dev_from_util = malloc(pop_size * sizeof(double));
    merged_acts   = malloc(ROWS * sizeof(double *));
    for(i = 0; i < ROWS; i++){
        merged_acts[i] = malloc(COLS * sizeof(double));
    }
    merged_costs = malloc(ROWS * sizeof(double *));
    for(i = 0; i < ROWS; i++){
        merged_costs[i] = malloc(COLS * sizeof(double));
    }
    act_change = malloc(ROWS * sizeof(double *));
    for(i = 0; i < ROWS; i++){
        act_change[i] = malloc(COLS * sizeof(double));
    }
    
    m_lyr = 0;
    while(agent_array[m_lyr][0] != agentID && m_lyr < ROWS){
        m_lyr++;
    }
    
    sum_array_layers(ACTION, merged_acts, 0, paras, agent_array);
    sum_array_layers(COST,  merged_costs, 1, paras, agent_array);
    
    for(i = 0; i < ROWS; i++){ /* Actions > 0 to respond to possible change */
        for(j = 0; j < COLS; j++){
            merged_acts[i][j] += 1; 
        }
    } 

    max_dev = 0;
    for(agent = 0; agent < pop_size; agent++){
        for(action_row = 0; action_row < int_num; action_row++){
            count_change[action_row] = 0; 
            utils[action_row]        = 0; 
            manager_row              = 0;
            type1                    = population[action_row][1][agent];
            type2                    = population[action_row][2][agent];
            type3                    = population[action_row][3][agent];
            while(population[manager_row][0][agent] != agentID ||
                  population[manager_row][1][agent] != type1   ||
                  population[manager_row][2][agent] != type2   ||
                  population[manager_row][3][agent] != type3
            ){
                manager_row++;
            }
            policy_to_counts(population, merged_acts, agent, merged_costs, 
                             act_change, action_row, manager_row, paras);
            foc_effect  = 0.0;
            foc_effect += (paras[74] * act_change[action_row][7]);
            foc_effect += (paras[75] * act_change[action_row][8]); 
            foc_effect += (paras[76] * act_change[action_row][9]);  
            foc_effect += (paras[77] * act_change[action_row][10]); 
            foc_effect += (paras[78] * act_change[action_row][11]);
            for(i = 0; i < int_num; i++){
                count_change[i] += foc_effect * jaco[action_row][i];
            }
            utils[action_row] = ACTION[manager_row][4][m_lyr];
        }
        change_dev = 0;
        for(i = 0; i < int_num; i++){
            change_dev += (count_change[i]-utils[i])*(count_change[i]-utils[i]);
        }
        if(change_dev > max_dev){
            max_dev = change_dev;
        }
        dev_from_util[agent] = change_dev;
    }
    for(agent = 0; agent < pop_size; agent++){
        fitnesses[agent] = max_dev - dev_from_util[agent];
    }
    
    for(i = 0; i < ROWS; i++){
        free(act_change[i]);
    }
    free(act_change);
    for(i = 0; i < ROWS; i++){
        free(merged_costs[i]);
    }
    free(merged_costs);
    for(i = 0; i < ROWS; i++){
        free(merged_acts[i]);
    }
    free(merged_acts);
    free(dev_from_util);
    free(utils);
    free(count_change);
}

/* =============================================================================
 * This function takes an array of fitnesses and returns an equal size array of
 * indices, the values of which will define which new individuals will make it
 * into the next population array, and in what proportions.
 *     fitnesses: Array to order fitnesses of the agents in the population
 *     winners: Array of the winners of the tournament
 *     paras: Vector of global parameters
 * ========================================================================== */
void tournament(double *fitnesses, int *winners, double *paras){

    int samp, i, left_to_place, placed, pop_size, rand_samp, sampleK, chooseK;
    int *samples;
    double *samp_fit;
    
    pop_size = (int) paras[21];
    sampleK  = (int) paras[24];
    chooseK  = (int) paras[25];
    
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
 *     paras: Vector of global parameters
 * ========================================================================== */
void place_winners(double ****population, int *winners, double *paras){

    int i, row, col, layer, winner, pop_size, ROWS, COLS;
    double a_value, ***NEW_POP;
    
    pop_size = (int) paras[21];
    ROWS     = (int) paras[68];
    COLS     = (int) paras[69];
    
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


/* =============================================================================
 * This function calls all of the sub-functions used in the genetic algorithm;
 * it returns a new agent with updated actions based on its goals (utility)
 *  Inputs include:
 *      ACTION:    An array of the action of agents
 *      COST:      An array of the cost of actions for each agent
 *      AGENT:     An array of *row agents and *col traits for each agent
 *      RESOURCES: An array of *row resources & *col traits for each resource
 *      LANDSCAPE: An array of *row by *col size that makes up the landscape
 *      JACOBIAN:  A Jacobian matrix of resource type and landscape effects
 *      lookup:    A table indexing types with rows of interaction array
 *      paras:     Parameters read into the function for population processes
 *      agent:     The row of the agent undergoing a genetic algorithm
 *      managing:  Whether or not the agent is managing a population
 * ========================================================================== */
void ga(double ***ACTION, double ***COST, double **AGENT, double **RESOURCES,
        double ***LANDSCAPE, double **JACOBIAN, int **lookup, double *paras, 
        int agent, int managing){
    
    int row, col, gen, layer, most_fit, land_x, land_y, land_z, popsize;
    int generations, res_number, trait_number, jaco_dim, xdim, ydim, agentID;
    int sampleK, chooseK, agent_seed, *winners;
    double budget, mutation_rate, crossover_rate;
    double ***POPULATION, ***NEW_POP, *fitnesses;

    land_x         = (int) paras[12];
    land_y         = (int) paras[13];
    popsize        = (int) paras[21];
    generations    = (int) paras[22];
    res_number     = (int) paras[32];
    land_z         = (int) paras[36];
    trait_number   = (int) paras[41];
    jaco_dim       = (int) paras[60];
    xdim           = (int) paras[68];
    ydim           = (int) paras[69];
    budget         = (double) AGENT[agent][16];
    agentID        = AGENT[agent][0];

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
    
    initialise_pop(ACTION, COST, paras, agent, budget, POPULATION, agentID);
    
    gen = 0;
    while(gen < generations){
        
        crossover(POPULATION, paras, agentID);
        
        mutation(POPULATION, paras, agentID);
 
        constrain_costs(POPULATION, COST, paras, agent, budget, agentID);
 
        if(managing == 1){
            manager_fitness(fitnesses, POPULATION, JACOBIAN, AGENT, lookup, 
                            agentID, COST, ACTION, paras);
        }else{
            strategy_fitness(AGENT, POPULATION, paras, fitnesses, JACOBIAN, 
                             lookup);
        }
  
        tournament(fitnesses, winners, paras);
   
        place_winners(&POPULATION, winners, paras);

        gen++;
    }
    
    most_fit = 0;
    for(layer = 0; layer < popsize; layer++){
        if(fitnesses[layer] > fitnesses[most_fit]){
            most_fit = layer;
        }
    }
    
    for(row = 0; row < xdim; row++){
        for(col = 0; col < ydim; col++){
            ACTION[row][col][agent] = POPULATION[row][col][most_fit];  
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
