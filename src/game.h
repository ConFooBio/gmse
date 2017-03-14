#include "utilities.h"

int min_cost(double ***COST, int layer, double budget, int rows, int cols);

void initialise_pop(double ***ACTION, double ***COST, int layer, int pop_size,
                    int budget, int carbon_copies, int ROWS, int COLS,
                    double ***population);