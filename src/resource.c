#include "utilities.h"

/* =============================================================================
 * This function just adds a time step to the relevant individual column 
 * ========================================================================== */
void add_time(double **res_adding, double *paras){
    
    int resource, time_para, time_trait, age_trait, resource_number;
    
    time_para       = (int) paras[0];
    time_trait      = (int) paras[30];
    age_trait       = (int) paras[31];
    resource_number = (int) paras[32];
    
    for(resource = 0; resource < resource_number; resource++){
        res_adding[resource][time_trait] = time_para;
        res_adding[resource][age_trait]++;
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
 *     ladj: Adjustment to the growth rate column ('add' above)
 *     dadj: Adjustment to the offspring number column ('realised' above)
 * ========================================================================== */
void res_add(double **res_adding, int rows, int add, int type, int K_add, 
             int ladj, int dadj){
    
    int resource, realised, sampled, added, loops;
    double rand_pois, rand_unif, lambda;

    realised = add + 1;
    
    switch(type){
        case 0:
            break;
        case 1:
            break; /* Add in a different type of birth here */
        case 2:
            added = 0; 
            for(resource = 0; resource < rows; resource++){
                res_adding[resource][realised] = 0;
                lambda = res_adding[resource][add] + res_adding[resource][ladj];
                if(lambda < 0){
                    lambda = 0;
                }
                rand_pois  = rpois(lambda);
                rand_pois += res_adding[resource][dadj];
                res_adding[resource][realised] = rand_pois;
                added += (int) rand_pois;
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
 * Inputs include:
 *     make: The data frame being used to place old and new resources
 *     old: The old data frame that stores the old resources to be retained
 *     res_added: The number of new resources to be added
 *     old_number: The number of old resources to be retained in the add
 *     traits: The number of traits to be added
 *     realised: The column in the old array that defines number added to new
 *     age: The column in which age is located (always starts at zero)
 * ========================================================================== */
void res_place(double **make, double **old, int res_added, int old_number, 
               int traits, int realised, int age){
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
    res_index = old[last_old][0] + 1; /* Note: arrays should not be shuffled */
    for(resource = 0; resource < old_number; resource++){
        to_add += old[resource][realised];
        for(newbie = to_make; newbie < to_add; newbie++){
            make[newbie][0] = res_index;
            for(trait = 1; trait < traits; trait++){
                make[newbie][trait] = old[resource][trait];
            }
            make[newbie][age] = 0; /* A bit inefficient given loop above */
            make[newbie][10]  = 0; /* Get rid of hard code (offspring = 0) */
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
 *     rm_adj: Which col in res_adding is adjusting removal (i.e. death)
 *     max_age: What is the maximum age of the resource?
 * ========================================================================== */
void res_remove(double **res_removing, int rows, int rm_row, int type, int K,
                int rm_adj, int max_age){

    int resource, over_K;
    double rand_unif, rm_from_K, rm_from_Ind, rm_odds;

    switch(type){
        case 0: /* No removal */
            break;
        case 1:
            for(resource = 0; resource < rows; resource++){
                rand_unif = runif(0, 1);
                rm_odds   = res_removing[resource][rm_row] + 
                            res_removing[resource][rm_adj];
                if(rand_unif < rm_odds){
                    res_removing[resource][rm_row] = -1;   
                }
            }
            break;
        case 2: 
            over_K  = rows - K;
            if(over_K > 0){
                rm_from_K  = (double) over_K / rows;
                for(resource = 0; resource < rows; resource++){
                    rand_unif   = runif(0, 1);
                    if(rand_unif < rm_from_K){
                        res_removing[resource][rm_row] = -1;   
                    }
                }
            }
            for(resource = 0; resource < rows; resource++){
                rm_from_Ind = res_removing[resource][rm_adj];
                rand_unif   = runif(0, 1);
                if(rand_unif < rm_from_Ind){
                    res_removing[resource][rm_row] = -1;   
                }
            }
            break;
        default:
            printf("ERROR: Resource removal/death type set incorrectly \n");
        break;
    }

    for(resource = 0; resource < rows; resource++){
        if(res_removing[resource][11] > max_age){
            res_removing[resource][rm_row] = -1;
        }
    }
}
/* ===========================================================================*/

/* =============================================================================
 * This function reads in resources and landscape values, then determines how
 * each should affect the other based on resource position and trait values
 * Inputs include:
 *     resource_array: resource array of individuals to interact
 *     resource_type_col: which type column defines the type of resource
 *     resource_type: type of resources to do the interacting
 *     resource_col: the column of the resources that affects or is affected
 *     rows: the number of resources (represented by rows) in the array
 *     resource_effect: the column of the resources of landscape effect size
 *     landscape: landscape array of cell values that affect individuals
 *     landscape_layer: layer of the landscape that is affected
 * ========================================================================== */
void res_landscape_interaction(double **resource_array, int resource_type_col,
                               int resource_type, int resource_col, int rows,
                               int resource_effect, double ***landscape, 
                               int landscape_layer){
    
    int resource;
    int x_pos, y_pos;
    double c_rate;
    double current_val;
    /* double esize; */
    
    for(resource = 0; resource < rows; resource++){
        if(resource_array[resource][resource_type_col] == resource_type){
            x_pos  = resource_array[resource][4];
            y_pos  = resource_array[resource][5];
            c_rate = resource_array[resource][14];
        
            landscape[x_pos][y_pos][landscape_layer] *= (1 - c_rate);
            /* TODO: FIX THIS
            current_val = resource_array[resource][resource_col];
            esize       = resource_array[resource][resource_effect];
            resource_array[resource][resource_col] += (1 - current_val) * esize;
            */
        }                
    }
}

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
SEXP resource(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* x and y locations in the RESOURCE array */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int zloc, land_z;        /* z locations */
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
    int time_para;           /* Time in the simulation the function called */
    int edge_type;           /* The type of edge on the landscape */
    int move_type;           /* How resources move on the landscape */
    int birthtype;           /* The type of birth of resources */
    int birth_K;             /* Carrying capacity affecting birth rate */
    int deathtype;           /* The type of death of resources */
    int death_K;             /* Carrying capacity affecting death rate */
    int move_res;            /* Should resources be allowed to move */
    int max_age;             /* What is the maximum age allowed for resources */
    int *add_resource;       /* Vector of added resources */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *len_PARAMETERS;     /* Length of the PARAMETERS vector incoming */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *R_ptr_new;       /* Pointer to RESOURCE_NEW (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *land_ptr_new;    /* Pointer to LAND_NEW (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double **res_old;        /* Array to store the old RESOURCE in C */
    double **res_make;       /* Array of newly made resources */
    double **res_new;        /* Array to store the new RESOURCE in C */
    double ***land;          /* Array to store the landscape in C*/

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
    land_z = dim_LANDSCAPE[2];
    land_y = dim_LANDSCAPE[1];
    land_x = dim_LANDSCAPE[0];
    land   = malloc(land_x * sizeof(double *));
    for(xloc = 0; xloc < land_x; xloc++){
        land[xloc] = malloc(land_y * sizeof(double *));
        for(yloc = 0; yloc < land_y; yloc++){
            land[xloc][yloc] = malloc(land_z * sizeof(double));   
        }
    } 
    vec_pos = 0;
    for(zloc = 0; zloc < land_z; zloc++){
        for(yloc = 0; yloc < land_y; yloc++){
            for(xloc = 0; xloc < land_x; xloc++){
                land[xloc][yloc][zloc] = land_ptr[vec_pos];
                vec_pos++;
            }
        }
    }  /* LANDSCAPE is now stored as land */    
    
    /* Do the biology here now */
    /* ====================================================================== */
    
    edge_type = (int) paras[1];
    move_type = (int) paras[2];
    birthtype = (int) paras[3];
    deathtype = (int) paras[4];
    birth_K   = (int) paras[5];
    death_K   = (int) paras[6];
    move_res  = (int) paras[19]; /* Should the resources be moved?        */
    max_age   = (int) paras[29];
    
    /* Resource time step and age needs to be increased by one */
    add_time(res_old, paras);
    
    /* Resources move according to move function and parameter) */
    if(move_res == 1){
        res_mover(res_old, 4, 5, 6, res_number, edge_type, land, land_x, land_y, 
              move_type);
    }

    /* Identify, and calculate the number of, added individuals */
    res_add(res_old, res_number, 9, birthtype, birth_K, 16, 17);
    res_nums_added      = 0; 
    for(resource = 0; resource < res_number; resource++){
        res_nums_added += res_old[resource][10];
    }
    res_make = malloc(res_nums_added * sizeof(double *));
    for(resource = 0; resource < res_nums_added; resource++){
        res_make[resource] = malloc(trait_number * sizeof(double));   
    }
    res_place(res_make, res_old, res_nums_added, res_number, trait_number, 
              10, 11);
    
    /* Identify, and calculate the number, of removed individuals */    
    res_remove(res_old, res_number, 8, deathtype, death_K, 15, 5);
    res_nums_subtracted = 0; 
    for(resource = 0; resource < res_number; resource++){
        if(res_old[resource][8] < 0){
            res_nums_subtracted += 1;
        }
    }
    
    res_num_total = res_number + res_nums_added - res_nums_subtracted;

    /* Below makes a new array for new RESOURCE, then adds it */
    res_new = malloc(res_num_total * sizeof(double *));
    for(resource = 0; resource < res_num_total; resource++){
        res_new[resource] = malloc(trait_number * sizeof(double));   
    }    
    
    /* Below pastes surviving old and new resources into the new array */
    resource_new = 0;
    for(resource = 0; resource < res_number; resource++){
        if(res_old[resource][8] >= 0){
            for(trait = 0; trait < trait_number; trait++){
                res_new[resource_new][trait] = res_old[resource][trait];
            }
            res_new[resource_new][15] = 0;
            res_new[resource_new][16] = 0;
            res_new[resource_new][17] = 0;
            resource_new++; /* Move on to the next new resource */
        }
    }
    for(resource = 0; resource < res_nums_added; resource++){
        for(trait = 0; trait < trait_number; trait++){
            res_new[resource_new][trait] = res_make[resource][trait];
        }
        res_new[resource_new][15] = 0;
        res_new[resource_new][16] = 0;
        res_new[resource_new][17] = 0;
        resource_new++;
    }
    
    /* Resources affect the landscape (note the **ORDER** of this -- change? */
    res_landscape_interaction(res_new, 1, 1, 15, res_num_total, 14, land, 1);
        
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP RESOURCE_NEW;
    PROTECT( RESOURCE_NEW = allocMatrix(REALSXP, res_num_total, trait_number) );
    protected_n++;
    
    R_ptr_new = REAL(RESOURCE_NEW);

    vec_pos = 0;
    for(trait = 0; trait < trait_number; trait++){
        for(resource = 0; resource < res_num_total; resource++){
            R_ptr_new[vec_pos] = res_new[resource][trait];
            vec_pos++;
        }
    } 
    paras[32] = (double) res_num_total;
    
    SEXP LAND_NEW;
    PROTECT( LAND_NEW = alloc3DArray(REALSXP, land_x, land_y, land_z) );
    protected_n++;
    
    land_ptr_new = REAL(LAND_NEW);
    
    vec_pos = 0;
    for(zloc=0; zloc<land_z; zloc++){
        for(yloc=0; yloc<land_y; yloc++){
            for(xloc=0; xloc<land_x; xloc++){
                land_ptr_new[vec_pos] = land[xloc][yloc][zloc];
                vec_pos++;
            }
        }
    }
    
    SEXP EVERYTHING;
    EVERYTHING = PROTECT( allocVector(VECSXP, 3) );
    protected_n++;
    SET_VECTOR_ELT(EVERYTHING, 0, RESOURCE_NEW);
    SET_VECTOR_ELT(EVERYTHING, 1, LAND_NEW);
    SET_VECTOR_ELT(EVERYTHING, 2, PARAMETERS);   
    
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(resource = 0; resource < res_num_total; resource++){
        free(res_new[resource]);
    }
    free(res_new);
    for(resource = 0; resource < res_nums_added; resource++){
        free(res_make[resource]);
    }
    free(res_make);
    for(xloc = 0; xloc < land_x; xloc++){
        for(yloc = 0; yloc < land_y; yloc++){
            free(land[xloc][yloc]);   
        }
        free(land[xloc]);        
    }
    free(land); 
    for(resource = 0; resource < res_number; resource++){
        free(res_old[resource]);
    }
    free(res_old);

    return(EVERYTHING); 
}
/* ===========================================================================*/

