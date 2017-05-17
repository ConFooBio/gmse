#include "utilities.h"

void add_time(double **res_adding, double *paras);

void res_add(double **res_adding, double *paras);

void res_place(double **make, double **old, double *paras, int res_added);

void res_remove(double **res_removing, double *paras);

void res_landscape_interaction(double **resource_array, int resource_type_col,
                               int resource_type, int resource_col, int rows,
                               int resource_effect, double ***landscape, 
                               int landscape_layer);