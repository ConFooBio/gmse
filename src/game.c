#include "utilities.h"

/* =============================================================================
 * This file is a work in progress, which will have all of the necessary
 * functions for running the genetic algorithm. Separate functions will 
 * call the genetic algorithm from R and C (C is default in G-MSE), and this
 * file will link with the user.c file to run a genetic algorithm for each
 * unique individual agent.
 * ========================================================================== */

/* =============================================================================
 * This function will find the minimum cost of an action in the UTILITY array
 * for a particular agent (layer). Inputs include:
 *     UTILITY: A full 3D utility array
 *     layer: The layer on which the minimum is going to be found
 *     budget: The total budget that the agent has to work with (initliases)
 *     x0: Row to start randomising values
 *     x1: Row to stop randomising values
 *     y0: Column to start randomising values
 *     y1: Column to stop randomising values
 * ========================================================================== */
int min_cost(double ***UTILITY, int layer, double budget, int x0, int x1,
             int y0, int y1){
    int i, j;
    double the_min;
    
    the_min = budget;
    for(i = x0; i < x1; i++){
        for(j = y0; j < y1; j++){
            if(UTILITY[i][j][layer] < the_min){
                the_min = UTILITY[i][j][layer];
            }
        }
    }

    return the_min; 
}
    
/* =============================================================================
 * This function will initialise a population from the UTILITY array , a
 * particular focal agent, and specification of how many times an agent should
 * be exactly replicated versus how many times random values shoudl be used.
 * Necessary variable inputs include:
 *     UTILITY: A 3D array of utility values
 *     layer: The 'z' layer of the UTILITY array to be initialised
 *     pop_size: The size of the total population (layers to population)
 *     carbon_copies: The number of identical agents used as seeds
 *     budget: The budget that random agents have to work with
 *     UTILITY_rows: Number of rows in the UTILITY array
 *     UTILITY_cols: Number of columns in the UTILITY array 
 *     population: array of the population that is made (malloc needed earlier)
 *     x0: Row to start randomising values
 *     x1: Row to stop randomising values
 *     y0: Column to start randomising values
 *     y1: Column to stop randomising values
 * ========================================================================== */
void initialise_pop(double ***UTILITY, int layer, int pop_size, int budget,
                    int carbon_copies, int UTILITY_rows, int UTILITY_cols,
                    double ***population, int x0, int x1, int y0, int y1){
    
    int agent;
    int row, col;
    double lowest_cost;

    /* First read in pop_size copies of the UTILITY layer of interest */
    for(agent = 0; agent < pop_size; agent++){
        for(row = 0; row < UTILITY_rows; row++){
            for(col = 0; col < UTILITY_cols; col++){
                population[row][col][agent] = UTILITY[row][col][layer];
            }
        }
        /* Re-assign values where it is necessary */
        if(agent >= carbon_copies){
            for(row = x0; row < x1; row++){
                for(col = y0; col < y1; col++){
                    population[row][col][agent] = 0;
                }
            }
            lowest_cost =  min_cost(UTILITY, layer
        }
    }
    
    
}