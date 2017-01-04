#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* =============================================================================
 * This function just adds a time step to the relevant individual column 
 * ========================================================================== */
void add_time(double **res_adding, int time_trait, int rows, int time_para,
              int age_trait){
    int resource;
    
    for(resource = 0; resource < rows; resource++){
        res_adding[resource][time_trait] = time_para;
        res_adding[resource][age_trait]++;
    }
}
/* ===========================================================================*/

/* =============================================================================
 * This function moves individuals on the landscape according to some rules
 * The 'edge' argument defines what happens at the landscape edge:
 *     0: Nothing happens (individual is just off the map)
 *     1: Torus landscape (individual wraps around to the other side)
 * The 'type' argument defines the type of movement allowed:
 *     0: No movement is allowed
 *     1: Movement is random uniform from zero to move_para in any direction
 *     2: Movement is poisson(move_para) in any direction
 * ========================================================================== */
void mover(double **res_moving, int xloc, int yloc, int move_para, int rows,
           int edge, double **landscape, int land_x, int land_y, int type){
    
    int res_num;      /* Resource number index                       */
    int move_len;     /* Length of a move                            */
    int move_dir;     /* Move direction (-1 or 1)                    */
    int new_pos;      /* New position: check if over landscape edge  */
    double rand_num;  /* Random number used for sampling             */
    double rand_uni;  /* Random uniform number                       */
    double rand_pois; /* Random poisson number                       */
    double raw_move;  /* Movement length before floor() truncation   */
    
    for(res_num=0; res_num < rows; res_num++){
        /* Move first in the xloc direction --------------------------------- */
        new_pos  = res_moving[res_num][xloc];
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
                move_len = floor(raw_move);
                break;
            case 2: /* Poisson selection of position change */
                rand_pois = rpois(res_moving[res_num][move_para]);    
                raw_move  = rand_pois * (res_moving[res_num][move_para] + 1);
                move_len  = floor(raw_move);
                break;
            default:
                if(res_num == 0){
                    printf("Unclear specification of movement type \n");
                }
                break;
        }
        new_pos  = res_moving[res_num][xloc] + (move_dir * move_len); 
        if(new_pos > land_x || new_pos < 0){ /* If off the edge */
            switch(edge){
                case 0: /* Nothing happens (effectively, no edge) */
                    break;
                case 1: /* Corresponds to a torus landscape */
                    if(new_pos > land_x){
                        new_pos = new_pos - land_x;   
                    }
                    if(new_pos < 0){
                        new_pos = new_pos + land_x;   
                    }
                    break;
                default:
                    if(res_num == 0){
                        printf("ERROR: Edge effects set incorrectly \n");
                    }
                    break;
            }
        }
        res_moving[res_num][xloc] = new_pos;
        /* Move next in the yloc direction ---------------------------------- */
        new_pos  = res_moving[res_num][yloc];
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
                move_len = floor(raw_move);
                break;
            case 2: /* Poisson selection of position change */
                rand_pois = rpois(res_moving[res_num][move_para]);    
                raw_move  = rand_pois * (res_moving[res_num][move_para] + 1);
                move_len  = floor(raw_move);
                break;
            default:
                break;
        }
        new_pos  = res_moving[res_num][yloc] + (move_dir * move_len); 
        if(new_pos > land_y || new_pos < 0){ /* If off the edge */
            switch(edge){
                case 0: /* Nothing happens (effectively, no edge) */
                    break;
                case 1: /* Corresponds to a torus landscape */
                    if(new_pos > land_y){
                        new_pos = new_pos - land_y;  
                    }
                    if(new_pos < 0){
                        new_pos = new_pos + land_y;   
                    }
                    break;
                default:
                    break;
            }
        }
        res_moving[res_num][yloc] = new_pos;
    }
}
/* ===========================================================================*/

/* =============================================================================
 * This function determines the number of new resources to be added, as
 * contributed by each individual resource
 * Inputs include:
 *     res_adding: data frame of individuals doing the adding (e.g., births)
 *     rows: Total number of rows in the res_adding data frame
 *     add: which row in res_adding is the growth parameter
 *     type: Type of growth (0: , 1: , 2: poisson)
 *     K_add: Carrying capacity applied to the addition of a new resource
 * ========================================================================== */
void res_add(double **res_adding, int rows, int add, int type, int K_add){
    int resource;
    int realised; /* Where the count of added goes (using add + 1) */
    int sampled;
    int added;
    int loops;
    double rand_pois;
    double rand_unif;

    realised = add + 1;
    
    switch(type){
        case 0:
            break;
        case 1:
            break; /* Add in a different type of birth here */
        case 2:
            for(resource = 0; resource < rows; resource++){
                res_adding[resource][realised] = 0;
                rand_pois = rpois(res_adding[resource][add]);
                res_adding[resource][realised] = rand_pois;
                added += rand_pois;
            }
            break;
        default:
            printf("ERROR: Resource growth/birth type set incorrectly \n");
            break;
    }
    if(K_add > 0){ /* If there is a carrying capacity applied to adding */
        loops = 1000000000;
        while(added > K_add){ 
            rand_unif = runif(0, 1);
            sampled   = floor(rand_unif * rows);
            if(res_adding[sampled][realised] > 0){
                res_adding[sampled][realised]--; /* Less memory used now */
                added--;
            }
            loops--;
            if(loops < 0){
                printf("ERROR: Possible infinite loop in res_add");
                break;
            }
        }
    }
}
/* ===========================================================================*/

/* =============================================================================
 * This function adds in the new resource to their own array
 * ========================================================================== */
void res_place(double **make, double **old, int res_added, int old_number, 
               int traits, int realised, int age_trait){
    int resource;
    int newbie;
    int trait;
    int to_make;
    int to_add;
    int make_res;
    int last_old;
    double res_index;
    
    make_res  = 0;
    to_make   = 0;
    to_add    = 0; /* Maybe try to cut down the loops here later? */
    last_old  = old_number - 1;
    res_index = old[last_old][0] + 1;
    for(resource = 0; resource < old_number; resource++){
        to_add += old[resource][realised];
        for(newbie = to_make; newbie < to_add; newbie++){
            make[newbie][0] = res_index;
            for(trait = 1; trait < traits; trait++){
                make[newbie][trait] = old[resource][trait];
            }
            make[newbie][age_trait] = 0;
            res_index++;
        }
        to_make = to_add;
    }
    if(to_make > res_added){
        printf("WARNING: Non-conformable arrays placing new resources");   
    }
}

/* =============================================================================
 * This function selects a random uniform and tests whether or not the resource
 * should be removed (e.g., an individual should die at a fixed rate
 * Inputs include:
 *     res_adding: data frame of individuals to potentially be removed
 *     rows: total number of rows in the res_adding data frame
 *     rm_row: which col in res_adding is the removal (i.e., death) parameter
 *     type: Type of removal (0: None, 1: uniform, 2: K based)
 *     K: Carrying capacity on removal/death -- only used some cases 
 * ========================================================================== */
void res_remove(double **res_removing, int rows, int rm_row, int type, int K){
    int resource;
    int over_K;
    double rand_unif;
    double rm_odds;

    switch(type){
        case 0: /* No removal */
            break;
        case 1:
            for(resource = 0; resource < rows; resource++){
                rand_unif = runif(0, 1);
                if(rand_unif < res_removing[resource][rm_row]){
                    res_removing[resource][rm_row] = -1;   
                }
            }
            break;
        case 2: 
            over_K  = rows - K;
            if(over_K > 0){
                rm_odds = (double) over_K / rows;
                for(resource = 0; resource < rows; resource++){
                    rand_unif = runif(0, 1);
                    if(rand_unif < rm_odds){
                        res_removing[resource][rm_row] = -1;   
                    }
                }
            }
            break;
        default:
            printf("ERROR: Resource removal/death type set incorrectly \n");
        break;
    }
}
/* ===========================================================================*/


/* =============================================================================
 * MAIN RESOURCE FUNCTION:
 * ===========================================================================*/

/* =============================================================================
 *  ****     This is the main function for the resource model     ****
 *  This function reads resource and landscape arrays, and a parameter vector
 *  from R, runs other functions in the resources.c file, then returns the
 *  new resource array.
 *  Inputs include:
 *      RESOURCE:   An array of *row resources and *col traits for each resource
 *      LANDSCAPE:  An array of *row by *col size that makes up the landscape
 *      PARAMETERS: Parameters read into the function for population processes
 * ===========================================================================*/
/* TODO: Argument: VECTOR_OF_PARAMETER_VALUES */
SEXP resource(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* x and y locations in the RESOURCE array */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int resource;            /* Index for resource (rows of RESOURCE) */
    int resource_new;        /* Index for resource in new array */
    int trait;               /* Index for resource traits (cols of RESOURCE) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included */
    int para_number;         /* Number of parameters included */
    int res_nums_added;      /* Number of resources to be added */
    int res_nums_subtracted; /* Number of resoruces to be removed */
    int res_num_total;       /* Total number of resources in returned array */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int *add_resource;       /* Vector of added resources */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *len_PARAMETERS;     /* Length of the PARAMETERS vector incoming */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *R_ptr_new;       /* Pointer to RESOURCE_NEW (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double **res_old;        /* Array to store the old RESOURCE in C */
    double **res_make;       /* Array of newly made resources */
    double **res_new;        /* Array to store the new RESOURCE in C */
    double **land;           /* Array to store the landscape in C*/

    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;

    PROTECT( RESOURCE = AS_NUMERIC(RESOURCE) );
    protected_n++;
    R_ptr = REAL(RESOURCE);
    
    PROTECT( LANDSCAPE = AS_NUMERIC(LANDSCAPE) );
    protected_n++;
    land_ptr = REAL(LANDSCAPE);
    
    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras = REAL(PARAMETERS);
    
    dim_RESOURCE   = INTEGER( GET_DIM(RESOURCE)  );
    dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );

    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    /* Code below remakes the RESOURCE matrix for easier use */
    res_number        = dim_RESOURCE[0];
    trait_number      = dim_RESOURCE[1];
    res_old   = malloc(res_number * sizeof(double *));
    for(resource = 0; resource < res_number; resource++){
        res_old[resource] = malloc(trait_number * sizeof(double));   
    } 
    vec_pos = 0;
    for(trait = 0; trait < trait_number; trait++){
        for(resource = 0; resource < res_number; resource++){
            res_old[resource][trait] = R_ptr[vec_pos];
            vec_pos++;
        }
    }
    /* RESOURCE is now stored as res_old (discrete resources) */

    /* Code below reads in the LANDSCAPE for easy of use */
    land_y = dim_LANDSCAPE[1];
    land_x = dim_LANDSCAPE[0];
    land   = malloc(land_x * sizeof(double *));
    for(xloc = 0; xloc < land_x; xloc++){
        land[xloc] = malloc(land_y * sizeof(double));   
    } /* LANDSCAPE is now stored as land */

    /* Do the biology here now */
    /* ====================================================================== */
    
    /* Resource time step and age needs to be increased by one */
    add_time(res_old, 6, res_number, paras[0], 10);
    
    /* Resources move according to move function and parameter) */
    mover(res_old, 3, 4, 5, res_number, paras[1], land, land_x, land_y, 
          paras[2]); 

    /* Identify, and calculate the number of, added individuals */
    res_add(res_old, res_number, 8, paras[3], paras[5]);
    res_nums_added      = 0; 
    for(resource = 0; resource < res_number; resource++){
        res_nums_added += res_old[resource][9];
    }
    res_make = malloc(res_nums_added * sizeof(double *));
    for(resource = 0; resource < res_nums_added; resource++){
        res_make[resource] = malloc(trait_number * sizeof(double));   
    }
    res_place(res_make, res_old, res_nums_added, res_number, trait_number, 
              9, 10);
    
    /* Identify, and calculate the number, of removed individuals */    
    res_remove(res_old, res_number, 7, paras[4], paras[6]);
    res_nums_subtracted = 0; 
    for(resource = 0; resource < res_number; resource++){
        if(res_old[resource][7] < 0){
            res_nums_subtracted += 1;
        }
    }
    
    res_num_total  = res_number + res_nums_added - res_nums_subtracted;

    /* Below makes a new array for new RESOURCE, then adds it */
    res_new = malloc(res_num_total * sizeof(double *));
    for(resource = 0; resource < res_num_total; resource++){
        res_new[resource] = malloc(trait_number * sizeof(double));   
    }    
    
    /* TODO: Loop first through all retained res_nums, then new ones */
    /* Will need some more complex rules for the below -- these loops will */
    /* eventually paste every res_num together that makes it out */
    resource_new = 0;
    for(resource = 0; resource < res_number; resource++){
        if(res_old[resource][7] >= 0){
            for(trait=0; trait < trait_number; trait++){
                res_new[resource_new][trait] = res_old[resource][trait];
            }
            resource_new++; /* Move on to the next new resource */
        }
    }
    for(resource = 0; resource < res_nums_added; resource++){
        for(trait = 0; trait < trait_number; trait++){
            res_new[resource_new][trait] = res_make[resource][trait];
        }
        resource_new++;
    }
    
    /* add_time(res_new, 6, res_num_total, paras[0]); */
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP RESOURCE_NEW;
    PROTECT( RESOURCE_NEW = allocMatrix(REALSXP, res_num_total, trait_number) );
    protected_n++;
    
    R_ptr_new = REAL(RESOURCE_NEW);

    vec_pos = 0;
    for(trait=0; trait<trait_number; trait++){
        for(resource=0; resource<res_num_total; resource++){
            R_ptr_new[vec_pos] = res_new[resource][trait];
            vec_pos++;
        }
    }            
        
        
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(resource = 0; resource < res_nums_added; resource++){
        free(res_make[resource]);
    }
    for(resource = 0; resource < res_num_total; resource++){
        free(res_new[resource]);
    }
    for(resource = 0; resource < res_number; resource++){
        free(res_old[resource]);
    } 
    for(xloc = 0; xloc < land_x; xloc++){
        free(land[xloc]);        
    }
    
    return(RESOURCE_NEW); 
}
/* ===========================================================================*/
          
          
          
          
