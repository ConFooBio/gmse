#include "utilities.h"

int min_cost(double ***COST, int layer, double budget, int rows, int cols);

void initialise_pop(double ***ACTION, double ***COST, int layer, int pop_size,
                    int budget, int carbon_copies, int ROWS, int COLS,
                    double ***population);

void crossover(double ***population, int pop_size, int ROWS, int COLS, 
          double pr);

void mutation(double ***population, int pop_size, int ROWS, int COLS, 
         double pr);

void constrain_costs(double ***population, double ***COST, int layer, 
                int pop_size, int ROWS, int COLS, double budget);

void strategy_fitness(double *fitnesses, double ***population, int pop_size, 
                 int ROWS, int COLS, double ***landscape,  
                 double **resources, double **agent_array);

void tournament(double *fitnesses, int *winners, int pop_size, 
                int sampleK, int chooseK);

void place_winners(double ****population, int *winners, int pop_size, int ROWS, 
                   int COLS);

void ga(double ***ACTION, double ***COST, double **AGENT, double **RESOURCES,
        double ***LANDSCAPE);