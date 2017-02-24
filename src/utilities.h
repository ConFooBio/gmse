#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

int edge_effect(int pos, int edge_1, int edge_2, int edge_type);

void res_mover(double **res_moving, int xloc, int yloc, int move_para, int rows,
            int edge_eff, double ***landscape, int land_x, int land_y, 
            int type);