#include "utilities.h"

void min_cost(double ***COST, double *paras, int layer, double budget);

void initialise_pop(double ***ACTION, double ***COST, double *paras, int layer,
                    double budget, double ***population, int agentID);

void crossover(double ***population, double *paras, int agentID);

void mutation(double ***population, int pop_size, int ROWS, int COLS, 
         double pr, int agentID);

void constrain_costs(double ***population, double ***COST, int layer, 
                int pop_size, int ROWS, int COLS, double budget);

void strategy_fitness(double *fitnesses, double ***population, int pop_size, 
                 int ROWS, int COLS, double ***landscape,  
                 double **resources, double **agent_array, int land_x,
                 int land_y);

void manager_fitness(double *fitnesses, double ***population, int pop_size, 
                int ROWS, double **agent_array, double **jaco,
                int **interact_table, int interest_num, int agentID,
                double ***COST, double ***ACTION, int COLS, int layers);

void tournament(double *fitnesses, int *winners, int pop_size, 
                int sampleK, int chooseK);

void place_winners(double ****population, int *winners, int pop_size, int ROWS, 
                   int COLS);

void ga(double ***ACTION, double ***COST, double **AGENT, double **RESOURCES,
        double ***LANDSCAPE, double **JACOBIAN, int **interact_table, 
        double *paras, int xdim, int ydim, int res_number, int land_x, 
        int land_y, int land_z, int trait_number, int jaco_dim, int agent,
        int managing, int ACT_rows, int ACT_cols, int ACT_depth);