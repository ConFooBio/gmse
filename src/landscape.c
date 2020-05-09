#include "utilities.h"

void break_land(int **land, int x0, int x1, int y0, int y1, int N, int *count){
    
    int xx, yy;
    
    if(N > 1){
        
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
 *  This function takes landscape dimensions, number of land owners, and
 *  proportion of public land as a 4 element vector. It then uses a 
 *  shortest-splitline algorithm to divide the land evenly among owners and also
 *  reserve a portion designated as public land. This is returned as a two 
 *  dimensional array to be added to a landscape array in GMSE.
 * ===========================================================================*/
SEXP build_ownership(SEXP PARAMETERS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc, dim_x, dim_y, vec_pos, owners, p_land;
    int **land, *count;
    int protected_n, len_PARAMETERS;
    double *paras_ptr, *land_ptr_new, *build_paras;
    
    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;

    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras_ptr = REAL(PARAMETERS);
    
    len_PARAMETERS = GET_LENGTH(PARAMETERS);
    
    /* Code below copies the paras vector into C */
    build_paras = malloc(len_PARAMETERS * sizeof(double *));
    vec_pos     = 0;
    for(xloc = 0; xloc < len_PARAMETERS; xloc++){
        build_paras[xloc] = paras_ptr[vec_pos];
        vec_pos++;
    } /* The parameters vector is now copied into C */
    
    dim_x  = (int) build_paras[0];
    dim_y  = (int) build_paras[1];
    owners = (int) build_paras[2];
    p_land = (int) build_paras[3];
    
    /* Do the biology here now */
    /* ====================================================================== */
    
    count  = malloc(1 * sizeof(int));
    land   = malloc(dim_x * sizeof(int *));
    for(xloc = 0; xloc < dim_x; xloc++){
        land[xloc] = malloc(dim_y * sizeof(int));   
    } 
    
    for(xloc = 0; xloc < dim_x; xloc++){
        for(yloc = 0; yloc < dim_y; yloc++){
            land[xloc][yloc] = xloc + yloc;
        }
    }
    
    *count = 3;
    
    break_land(land, 0, 1, 0, 2, 1, count);
    break_land(land, 1, 2, 0, 2, 1, count);

    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP LAND_NEW;
    PROTECT( LAND_NEW = allocMatrix(REALSXP, xloc, yloc) );
    protected_n++;
    
    land_ptr_new = REAL(LAND_NEW);
    
    vec_pos = 0;
    for(xloc = 0; xloc < dim_x; xloc++){
        for(yloc = 0; yloc < dim_y; yloc++){
            land_ptr_new[vec_pos] = land[xloc][yloc];
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

