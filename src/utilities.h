#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void swap_arrays(void **ARRAY_A, void **ARRAY_B);

void swap_int(int *a, int *b);

void swap_double(double *a, double *b);

void sort_vector(int *vector, int vector_length);

void sort_vector_by(int *vector, double *by, int vector_length);

int edge_effect(int pos, int edge_1, int edge_2, int edge_type);

void res_mover(double **res_moving, int xloc, int yloc, int move_para, int rows,
            int edge_eff, double ***landscape, int land_x, int land_y, 
            int type);