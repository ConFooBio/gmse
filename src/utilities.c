#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* =============================================================================
 * Swap pointers to rewrite ARRAY_B into ARRAY_A for a an array of any dimension
 * ========================================================================== */
void find_descending_order(int *order_array, double *by_array, int length){
    int i, k, max_index;
    double max_val, min_val;
    
    k = 0;
    min_val = 0;
    for(i = 0; i < length; i++){
        if(by_array[i] < min_val){
            min_val = by_array[i];
        }
    }
    while(k < length){
        max_val   = min_val - 1;
        max_index = 0;
        for(i = 0; i < length; i++){
            if(by_array[i] > max_val){
                max_index = i;
                max_val   = by_array[i];
            }
        }
        by_array[max_index] = min_val - 1;
        order_array[k]      = order_array[max_index]; 
        k++;   
    }
}

/* =============================================================================
 * Swap pointers to rewrite ARRAY_B into ARRAY_A for a an array of any dimension
 * ========================================================================== */
void swap_arrays(void **ARRAY_A, void **ARRAY_B){

    void *TEMP_ARRAY;

    TEMP_ARRAY = *ARRAY_A;
    *ARRAY_A   = *ARRAY_B;
    *ARRAY_B   = TEMP_ARRAY;
}

/* =============================================================================
 * Swap two values (avoiding the XOR trick deliberately here)
 * ========================================================================== */
void swap_int(int *a, int *b){
    int temp; 
    
    temp   = *a;
    *a     = *b;
    *b     = temp;
}

/* =============================================================================
 * Swap two values (avoiding the XOR trick deliberately here)
 * ========================================================================== */
void swap_double(double *a, double *b){
    double temp; 
    
    temp   = *a;
    *a     = *b;
    *b     = temp;
}

/* =============================================================================
 * Sort a vector of integers
 * ========================================================================== */
void sort_vector(int *vector, int vector_length){
    int i, j, temp;
    
    for(i = 0; i < vector_length; i++){
        for(j = i + 1; j < vector_length; j++){
            if(vector[i] < vector[j]){
                swap_int(&vector[i], &vector[j]);
            }
        }
    }
}

/* =============================================================================
 * This function applies the edge effect during movement
 * ========================================================================== */
int edge_effect(int pos, int edge_1, int edge_2, int edge_type){
    if(pos >= edge_2 || pos < edge_1){ /* If off the edge */
        switch(edge_type){
            case 0: /* Nothing happens (effectively, no edge) */
                break;
            case 1: /* Corresponds to a torus landscape */
                while(pos >= edge_2){
                    pos = pos - edge_2;   
                }
                while(pos < edge_1){
                    pos = pos + edge_2;   
                }
                break;
            default:
                printf("ERROR: Edge effects set incorrectly \n");
                break;
        }
    }
    return pos;
}

/* =============================================================================
 * This function moves individuals on the landscape according to some rules
 * The 'edge_eff' argument defines what happens at the landscape edge:
 *     0: Nothing happens (individual is just off the map)
 *     1: Torus landscape (individual wraps around to the other side)
 * The 'type' argument defines the type of movement allowed:
 *     0: No movement is allowed
 *     1: Movement is random uniform from zero to move_para in any direction
 *     2: Movement length is poisson(move_para) in x then y direction
 *     3: Movement length is poisson(move_para) in any direction
 * ========================================================================== */
void res_mover(double **res_moving, int xloc, int yloc, int move_para, int rows,
           int edge_eff, double ***landscape, int land_x, int land_y, 
           int type){
    
    int res_num;      /* Resource number index                        */
    int move_len;     /* Length of a move                             */
    int move_dir;     /* Move direction (-1 or 1)                     */
    int new_pos;      /* New position: check if over landscape edge   */
    double rand_num;  /* Random number used for sampling              */
    double rand_uni;  /* Random uniform number                        */
    double rand_pois; /* Random poisson number                        */
    double raw_move;  /* Movement length before floor() truncation    */
    
    for(res_num=0; res_num < rows; res_num++){
        /* Move first in the xloc direction --------------------------------- */
        new_pos  = (int) res_moving[res_num][xloc];
        rand_num = 0.5;
        do{ /* Note that rand_num can never be exactly 0.5 */
            rand_num = runif(0, 1);
        } while(rand_num == 0.5);
        if(rand_num > 0.5){
            move_dir = 1;   
        }else{
            move_dir = -1;   
        } /* Now we have the direction the resource is moving */
        switch(type){
            case 0: /* No change in position */
                move_len = 0;
                break;
            case 1: /* Uniform selection of position change */
                do{ /* Again, so that res_num never moves too far */
                    rand_uni = runif(0, 1);
                } while(rand_uni == 1.0);
                raw_move = rand_uni * (res_moving[res_num][move_para] + 1);
                move_len = (int) floor(raw_move);
                break;
            case 2: /* Poisson selection of position change */
                rand_pois = rpois(res_moving[res_num][move_para]);    
                raw_move  = rand_pois * (res_moving[res_num][move_para] + 1);
                move_len  = (int) floor(raw_move);
                break;
            case 3: /* Uniform position movement a Poisson number of times */
                rand_pois = rpois(res_moving[res_num][move_para]);
                raw_move  = 0;
                while(rand_pois > 0){
                    do{
                        rand_uni = runif(0, 1);
                    } while(rand_uni == 1.0);
                    raw_move += rand_uni * (res_moving[res_num][move_para] + 1);
                    rand_pois--;
                }
                move_len = (int) floor(raw_move);
                break;
            default:
                if(res_num == 0){
                    printf("Unclear specification of movement type \n");
                }
                break;
        }
        new_pos  = (int) res_moving[res_num][xloc] + (move_dir * move_len);
        if(new_pos >= land_x || new_pos < 0){ /* If off the edge */
            new_pos = edge_effect(new_pos, 0, land_x, edge_eff);
        }
        res_moving[res_num][xloc] = new_pos;
        /* Move next in the yloc direction ---------------------------------- */
        new_pos  = (int) res_moving[res_num][yloc];
        rand_num = 0.5;
        do{ /* Note that rand_num can never be exactly 0.5 */
            rand_num = runif(0, 1);
        } while(rand_num == 0.5);
        if(rand_num > 0.5){
            move_dir = 1;   
        }else{
            move_dir = -1;   
        } /* Now we have the direction the resource is moving */
        switch(type){
            case 0: /* No change in position */
                move_len = 0;
                break;
            case 1: /* Uniform selection of position change */
                do{ /* Again, so that res_num never moves too far */
                    rand_uni = runif(0, 1);
                } while(rand_uni == 1.0);
                raw_move = rand_uni * (res_moving[res_num][move_para] + 1);
                move_len = (int) floor(raw_move);
                break;
            case 2: /* Poisson selection of position change */
                rand_pois = rpois(res_moving[res_num][move_para]);    
                raw_move  = rand_pois * (res_moving[res_num][move_para] + 1);
                move_len  = (int) floor(raw_move);
                break;
            case 3: /* Uniform position movement a Poisson number of times */
                rand_pois = rpois(res_moving[res_num][move_para]);
                raw_move  = 0;
                while(rand_pois > 0){
                    do{
                        rand_uni = runif(0, 1);
                    } while(rand_uni == 1.0);
                    raw_move += rand_uni * (res_moving[res_num][move_para] + 1);
                    rand_pois--;
                }
                move_len = (int) floor(raw_move);
                break;
            default:
                break;
        }
        new_pos  = (int) res_moving[res_num][yloc] + (move_dir * move_len); 
        if(new_pos >= land_y || new_pos < 0){ /* If off the edge */
            new_pos = edge_effect(new_pos, 0, land_y, edge_eff);
        }
        res_moving[res_num][yloc] = new_pos;
    }
}
/* ===========================================================================*/
