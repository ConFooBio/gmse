#include "utilities.h"

/* =============================================================================
 *  This function uses recursion to break the landscape into ever smaller 
 *  rectangular chunks. The end result is a mostly even distribution of owned
 *  cells on the landscape.
 * ===========================================================================*/
void break_land(double **land, int x0, int x1, int y0, int y1, int N, 
                double land_var, int *count, int *bin, int ow){
    
    int xx, yy, halfway, N1, N2, x0_new;
    double Nd1, Nd2, ratio, ratio_adj;
    
    if(N > 1){
        Nd1    = (double) floor(0.5 * N);
        Nd2    = (double) Nd1 + (N % 2);
        ratio  = (double) Nd2 / N;
        N1     = (int) Nd1;
        N2     = (int) Nd2;
        
        ratio_adj = 0.0;
        if(ow < (*count)){
            ratio_adj = land_var * ratio;
        }
        ratio *= (1 - ratio_adj);
        
        if( (x1 - x0) > (y1 - y0) ){
            halfway  = floor( ratio * (x1 - x0) + x0 );
            break_land(land, x0, halfway, y0, y1, N2, land_var, count, bin, ow);
            break_land(land, halfway, x1, y0, y1, N1, land_var, count, bin, ow);
        }
        if( (x1 - x0) < (y1 - y0) ){
            halfway  = floor( ratio * (y1 - y0) + y0);
            break_land(land, x0, x1, y0, halfway, N2, land_var, count, bin, ow);
            break_land(land, x0, x1, halfway, y1, N1, land_var, count, bin, ow);
        }
        if( (x1 - x0) == (y1 - y0) && (*bin) == 0 ){
            halfway  = (int) floor( ratio * (y1 - y0) + y0 );
            break_land(land, x0, x1, y0, halfway, N2, land_var, count, bin, ow);
            break_land(land, x0, x1, halfway, y1, N1, land_var, count, bin, ow);
        }
        if( (x1 - x0) == (y1 - y0) && (*bin) == 1){
            halfway  = (int) floor( ratio * (x1 - x0) + x0 );
            break_land(land, x0, x1, y0, halfway, N2, land_var, count, bin, ow);
            break_land(land, x0, x1, halfway, y1, N1, land_var, count, bin, ow);
        }
    }else{
        for(xx = x0; xx < x1; xx++){
            for(yy = y0; yy < y1; yy++){
                land[xx][yy] = *count;
            }
        }
        (*count)++; /* Recall '++' takes precedence over '*' */
    }
}

/* =============================================================================
 *  This function is called when the amount of public land is less than that
 *  allocated to each user (i.e., very small amounts). In such cases, the 
 *  function places the relevant amount of public land right in the centre.
 * ===========================================================================*/
void small_public_land(double **land, int dim_x, int dim_y, double public_land){
    
    int xx, yy, x0, x1, y0, y1, Lx, Ly, sq_len_x, sq_len_y, leftover;
    double cells, rand_x, rand_y;
    
    Lx = (int) floor( sqrt(public_land) * (dim_x) );
    Ly = (int) floor( sqrt(public_land) * (dim_y) );
    
    x0 = (0.5 * dim_x) - (0.5 * Lx);
    x1 = (0.5 * dim_x) + (0.5 * Lx);
    y0 = (0.5 * dim_y) - (0.5 * Ly);
    y1 = (0.5 * dim_y) + (0.5 * Ly);
    
    if(x0 < 0){ /* This should never happen, but just to avoid a crash. */
        x0 = 1;
    }
    if(x1 >= dim_x){
        x1 = dim_x - 1;
    }
    if(y0 < 0){
        y0 = 1;
    }
    if(y1 >= dim_y){
        y1 = dim_y - 1;
    }
    
    for(xx = x0; xx < x1; xx++){
        for(yy = y0; yy < y1; yy++){
            land[xx][yy] = 0;
        }
    }
    
    /* Calculate, then add the leftover cells */
    leftover = (int) ((dim_x * dim_y) * public_land ) - (Lx * Ly);
    
    while(leftover > 0){ /* Inelegant, but it works */
        do{
            xx = (int) get_rand_int( (x0-1) , (x1+1) );
            yy = (int) get_rand_int( (y0-1) , (y1+1) );
        } while (land[xx][yy] == 0);
        land[xx][yy] = 0;
        leftover--;
    }
}

/* =============================================================================
 *  This function takes landscape dimensions, number of land owners, and
 *  proportion of public land as a 4 element vector. It then uses a 
 *  shortest-splitline algorithm to divide the land evenly among owners and also
 *  reserve a portion designated as public land. This is returned as a two 
 *  dimensional array to be added to a landscape array in GMSE.
 * ===========================================================================*/
SEXP build_ownership(SEXP PARAMETERS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc, dim_x, dim_y, vec_pos, owners, unique_cells;
    int *count, *bin;
    int protected_n, len_P;
    double p_land, land_var, *paras_ptr, *land_ptr_new, *build_paras, **land;
    
    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;

    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras_ptr = REAL(PARAMETERS);
    
    len_P = GET_LENGTH(PARAMETERS);
    
    /* Code below copies the paras vector into C */
    build_paras = malloc(len_P * sizeof(double *));
    vec_pos     = 0;
    for(xloc = 0; xloc < len_P; xloc++){
        build_paras[xloc] = paras_ptr[vec_pos];
        vec_pos++;
    } /* The parameters vector is now copied into C */
    
    dim_y    = (int) build_paras[0]; /* Note the flip to avoid jagged owns */
    dim_x    = (int) build_paras[1];
    owners   = (int) build_paras[2];
    p_land   = (double) build_paras[3];
    land_var = (double) build_paras[4];
    
    /* Do the biology here now */
    /* ====================================================================== */
    bin    = malloc(1 * sizeof(int));
    count  = malloc(1 * sizeof(int));
    land   = malloc(dim_x * sizeof(double *));
    for(xloc = 0; xloc < dim_x; xloc++){
        land[xloc] = malloc(dim_y * sizeof(double));   
    } 
    
    (*bin)   = 0;
    (*count) = 1;
    
    if(p_land == 1){
        unique_cells = 1;
        owners       = 0;
    }else{
        unique_cells = floor( owners / (1 - p_land) );
    }
    
    break_land(land, 0, dim_x, 0, dim_y, unique_cells, land_var, count, bin,
               (unique_cells - owners) );

    for(xloc = 0; xloc < dim_x; xloc++){ /* Add the public land back in */
        for(yloc = 0; yloc < dim_y; yloc++){
            land[xloc][yloc] -= (unique_cells - owners);
            if(land[xloc][yloc] < 0){
                land[xloc][yloc] = 0;
            }
        }
    }
    
    if(p_land > 0 && unique_cells == owners){ /* Very little public land */
        small_public_land(land, dim_x, dim_y, p_land);
    }
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP LAND_NEW;
    PROTECT( LAND_NEW = allocMatrix(REALSXP, dim_x, dim_y) );
    protected_n++;
    
    land_ptr_new = REAL(LAND_NEW);
    
    vec_pos = 0;
    for(xloc = 0; xloc < dim_x; xloc++){
        for(yloc = 0; yloc < dim_y; yloc++){
            land_ptr_new[vec_pos] = land[xloc][yloc] + 1;
            vec_pos++;
        }
    } 

    UNPROTECT(protected_n);
    
    for(xloc = 0; xloc < dim_x; xloc++){
        free(land[xloc]);
    }
    free(land);
    free(count);
    
    return(LAND_NEW); 
}
/* ===========================================================================*/

