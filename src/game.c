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
                    xpos = floor( runif(0,ROWS) );
                }while(xpos == ROWS);
                do{
                    ypos = floor( runif(4,COLS) );
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
            cross_partner = floor( runif(0, pop_size) );
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


/* 
 * This function will eventually call all of the other functions used in the
 * genetic algorithm. For now, it is being used just to call the other functions
 * and therefore test out whether or not they work.
 */
void ga(double ***ACTION, double ***COST, double **AGENT, double **RESOURCES){
    
    int row, col, layer;
    int xdim, ydim;
    double ***POPULATION;
    
    ydim = 13;
    xdim = 4;
    
    POPULATION = malloc(xdim * sizeof(double *));
    for(row = 0; row < xdim; row++){
        POPULATION[row] = malloc(ydim * sizeof(double *));
        for(col = 0; col < ydim; col++){
            POPULATION[row][col] = malloc(100 * sizeof(double));
        }
    }
    for(layer = 0; layer < 100; layer++){
        for(col = 0; col < ydim; col++){
            for(row = 0; row < xdim; row++){
                POPULATION[row][col][layer] = 0;
            }
        }
    }  
    
    initialise_pop(ACTION, COST, 0, 100, 100, 10, xdim, ydim, POPULATION);
    
    /*
    printf("1 ------------------------------- \n");
    for(row = 0; row < xdim; row++){
        for(col = 0; col < ydim; col++){
            printf("%f ", POPULATION[row][col][20]);
        }
        printf("\n");
    } 
    */
    
    crossover(POPULATION, 100, xdim, ydim, 0.1);
    
    /*
    printf("2 ------------------------------- \n");
    for(row = 0; row < xdim; row++){
        for(col = 0; col < ydim; col++){
            printf("%f ", POPULATION[row][col][20]);
        }
        printf("\n");
    }
     */
    

    for(row = 0; row < xdim; row++){
        for(col = 0; col < ydim; col++){
            free(POPULATION[row][col]);   
        }
        free(POPULATION[row]); 
    }
    free(POPULATION);
    
}
    
    
    
    
