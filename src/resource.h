#include "utilities.h"

void add_time(double **res_adding, double *paras);

void res_add(double **res_adding, double *paras);

void res_place(double **make, double **old, double *paras, int res_added);

void res_remove(double **res_removing, double *paras);

void res_landscape_interaction(double **resource_array, double ***landscape,
                               double *paras, int resource_number);