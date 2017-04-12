#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void is_on_owner_land(int res_number, double **resources, int owner,
                      double ***land, int *ident_vector);

void is_correct_type(int res_number, double **resources, int type1, int type2, 
                     int type3, int *ident_vector);

void find_descending_order(int *order_array, double *by_array, int length);

void swap_arrays(void **ARRAY_A, void **ARRAY_B);

void swap_int(int *a, int *b);

void swap_double(double *a, double *b);

void sort_vector(int *vector, int vector_length);

int edge_effect(int pos, int edge_1, int edge_2, int edge_type);

void res_mover(double **res_moving, int xloc, int yloc, int move_para, int rows,
            int edge_eff, double ***landscape, int land_x, int land_y, 
            int type);