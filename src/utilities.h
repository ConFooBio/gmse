#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

int get_rand_int(int from, int to);

void is_on_owner_land(int res_number, double **resources, int owner,
                      double ***land, int *ident_vector);

void is_correct_type(int res_number, double **resources, int type1, int type2, 
                     int type3, int *ident_vector);

void find_descending_order(int *order_array, double *by_array, int length);

void swap_arrays(void **ARRAY_A, void **ARRAY_B);

int edge_effect(int pos, int edge_1, int edge_2, int edge_type);

void res_mover(double **res_moving, double ***landscape, double *paras);

int rand_dir(void);

int unif_move(int max_move);

int pois_move(double move_para);

int unif_pois_move(double move_para);

void res_mover(double **res_moving, double ***landscape, double *paras);

void move_a_resource(double **res_moving, double ***landscape, double *paras,
                     int resource);

void count_owned_cells(double ***landscape, double *paras, double **agent_array, 
                       int land_x, int land_y, int agent_number);

void count_cell_yield(double **agent_array, double ***land, double *paras);