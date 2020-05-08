#include "utilities.h"

/* =============================================================================
 * This function just adds a time step to the relevant individual column
 * Inputs include:
 *     res_adding: data frame of individuals doing the adding (e.g., births)
 *     paras: Global parameters needed
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

/* =============================================================================
 * This function determines the number of new resources to be added, as
 * contributed by each individual resource
 * Inputs include:
 *     res_adding: data frame of individuals doing the adding (e.g., births)
 *     paras: Global parameters needed
 * ========================================================================== */
void res_add(double **res_adding, double *paras){
    
    int resource_number, add, realised, type, K_add, gadj, oadj, cadj, ind_age;
    int land_add, ccl;
    int resource, sampled, added, loops, castrated, killed, klld, arp, age;
    double rand_pois, base_lambda, add_lambda, lambda, crp;
    
    type            = (int) paras[3];  /* Type of growth (e.g., poisson) */
    K_add           = (int) paras[5];  /* Carrying capacity applied  */
    age             = (int) paras[31]; /* Age column for the individual */
    resource_number = (int) paras[32];
    add             = (int) paras[37];
    realised        = (int) paras[38];
    gadj            = (int) paras[39]; /* Adjustment to the growth rate col  */
    oadj            = (int) paras[40]; /* Adjustment to offspring number col */
    klld            = (int) paras[42]; /* Adjustment to kill */
    cadj            = (int) paras[73]; /* Adjust to castrate */
    arp             = (int) paras[111]; /* Minimum age of reproduction */
    ccl             = (int) paras[115]; /* Column for holding consumption */
    crp             = paras[117]; /* Consumption required for one offspring */

    added = 0; 
    switch(type){
        case 0:
            break;
        case 1:
            break; /* Add in a different type of birth here */
        case 2:
            for(resource = 0; resource < resource_number; resource++){
                res_adding[resource][realised] = 0;
                castrated = res_adding[resource][cadj];
                killed    = res_adding[resource][klld];
                ind_age   = res_adding[resource][age];
                if(castrated >= 1 || killed >= 1 || ind_age < arp){
                    rand_pois = 0;
                }else{
                    base_lambda = res_adding[resource][add];
                    add_lambda  = base_lambda * res_adding[resource][gadj];
                    lambda      = base_lambda + add_lambda;
                    if(lambda < 0){
                        lambda = 0;
                    }
                    rand_pois  = rpois(lambda);
                    rand_pois += res_adding[resource][oadj];
                    res_adding[resource][realised] = rand_pois;
                }
                added += (int) rand_pois;
            }
            break;
        default:
            for(resource = 0; resource < resource_number; resource++){
                res_adding[resource][realised] = 0;
                castrated = res_adding[resource][cadj];
                killed    = res_adding[resource][klld];
                ind_age   = res_adding[resource][age];
                if(castrated >= 1 || killed >= 1 || ind_age < arp){
                    rand_pois = 0;
                }else{
                    base_lambda = res_adding[resource][add];
                    add_lambda  = base_lambda * res_adding[resource][gadj];
                    lambda      = base_lambda + add_lambda;
                    if(lambda < 0){
                        lambda = 0;
                    }
                    rand_pois  = rpois(lambda);
                    rand_pois += res_adding[resource][oadj];
                    res_adding[resource][realised] = rand_pois;
               }
               added += (int) rand_pois;
            }
            break;
    }
    for(resource = 0; resource < resource_number; resource++){
        ind_age   = res_adding[resource][age];
        if(crp > 0 && ind_age >= arp){
            land_add = floor(res_adding[resource][ccl] / crp);
            res_adding[resource][realised] += (int) land_add;
            added                          += (int) land_add;
        }
    }
    if(K_add > 0){ /* If there is a carrying capacity applied to adding */
        loops = 1000000000;
        while(added > K_add){ 
            sampled = get_rand_int(0, resource_number);
            if(res_adding[sampled][realised] > 0){
                res_adding[sampled][realised]--; /* Less memory used now */
                added--;
            }
            loops--;
            if(loops < 0){
                break;
            }
        }
    }
}

/* =============================================================================
 * This function adds in the new resource to their own array
 * Inputs include:
 *     make: The data frame being used to place old and new resources
 *     old: The old data frame that stores the old resources to be retained
 *     paras: Global parameters needed
 *     res_added: The number of new resources to be added
 * ========================================================================== */
void res_place(double **make, double **old, double *paras, int res_added){
               
    int old_number, traits, realised, age, off_col;
    int resource, newbie, trait, to_make, to_add, last_old;
    double res_index;
    
    age        = (int) paras[31];
    old_number = (int) paras[32];
    realised   = (int) paras[38];
    traits     = (int) paras[41];
    off_col    = (int) paras[114];
    
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
            make[newbie][age]     = 0; /* A bit inefficient given loop above */
            make[newbie][off_col] = 0;
            res_index++;
        }
        to_make = to_add;
    }
}

/* =============================================================================
 * This function selects a random uniform and tests whether or not the resource
 * should be removed (e.g., an individual should die at a fixed rate
 * Inputs include:
 *     res_removing: data frame of individuals to potentially be removed
 *     paras: Global parameters needed
 * ========================================================================== */
void res_remove(double **res_removing, double *paras){

    int resource_number, rm_col, type, K, rm_adj, max_age, age_col, cons_col;
    int resource, over_K;
    double rand_unif, rm_from_K, rm_from_Ind, base_rm, adj_rm, rm_odds, csr;

    type            = (int) paras[4];
    K               = (int) paras[6]; /* Carrying capacity on removal/death */
    max_age         = (int) paras[29];
    age_col         = (int) paras[31];
    resource_number = (int) paras[32];
    rm_adj          = (int) paras[42]; /* col in res_removing adjust removal */
    rm_col          = (int) paras[43];
    cons_col        = (int) paras[115]; /* col in res_removing of consumption */
    csr             = paras[116]; /* Consumption required for survival */
    
    switch(type){
        case 0: /* No removal */
            break;
        case 1:
            for(resource = 0; resource < resource_number; resource++){
                base_rm   = res_removing[resource][rm_col];
                adj_rm    = res_removing[resource][rm_adj];
                rm_odds   = base_rm + adj_rm;
                rand_unif = runif(0, 1);
                if(rand_unif < rm_odds){
                    res_removing[resource][rm_col] = -1;   
                }
            }
            break;
        case 2: 
            over_K  = resource_number - K;
            if(over_K > 0){
                rm_from_K  = (double) over_K / resource_number;
                for(resource = 0; resource < resource_number; resource++){
                    rand_unif   = runif(0, 1);
                    if(rand_unif < rm_from_K){
                        res_removing[resource][rm_col] = -1;   
                    }
                }
            }
            for(resource = 0; resource < resource_number; resource++){
                rm_from_Ind = res_removing[resource][rm_adj];
                rand_unif   = runif(0, 1);
                if(rand_unif < rm_from_Ind){
                    res_removing[resource][rm_col] = -1;   
                }
            }
            break;
        case 3:
            for(resource = 0; resource < resource_number; resource++){
                rm_odds   = res_removing[resource][rm_col];
                rand_unif = runif(0, 1);
                if(rand_unif < rm_odds){
                    res_removing[resource][rm_col] = -1;   
                }
            }
            over_K  = resource_number - K;
            if(over_K > 0){
                rm_from_K  = (double) over_K / resource_number;
                for(resource = 0; resource < resource_number; resource++){
                    rand_unif   = runif(0, 1);
                    if(rand_unif < rm_from_K){
                        res_removing[resource][rm_col] = -1;   
                    }
                }
            }
            for(resource = 0; resource < resource_number; resource++){
                rm_from_Ind = res_removing[resource][rm_adj];
                rand_unif   = runif(0, 1);
                if(rand_unif < rm_from_Ind){
                    res_removing[resource][rm_col] = -1;   
                }
            }
            break;
        default:
            over_K  = resource_number - K;
            if(over_K > 0){
                rm_from_K  = (double) over_K / resource_number;
                for(resource = 0; resource < resource_number; resource++){
                    rand_unif   = runif(0, 1);
                    if(rand_unif < rm_from_K){
                        res_removing[resource][rm_col] = -1;   
                    }
                }
            }
            for(resource = 0; resource < resource_number; resource++){
                rm_from_Ind = res_removing[resource][rm_adj];
                rand_unif   = runif(0, 1);
                if(rand_unif < rm_from_Ind){
                   res_removing[resource][rm_col] = -1;   
                }
            }
            break;
    }
    for(resource = 0; resource < resource_number; resource++){
        if(res_removing[resource][age_col] > max_age){
            res_removing[resource][rm_col] = -1;
        }
        if(res_removing[resource][cons_col] < csr){
            res_removing[resource][rm_col] = -1;
        }
    }
}
/* ===========================================================================*/

/* =============================================================================
 * This function reads in resources and landscape values, then determines how
 * each should affect the other based on resource position and trait values
 * Inputs include:
 *     resource_array: resource array of individuals to interact
 *     landscape: landscape array of cell values that affect individuals
 *     paras: Global parameters needed
 *     resource_number: The number of resources in the resource_array
 * ========================================================================== */
void res_landscape_interaction(double **resource_array, double ***landscape,
                               double *paras, int resource_number){
                              
    int resource_type_col, resource_type, resource_effect;
    int landscape_layer, resource, x_col, y_col, x_pos, y_pos, gadj, klld;
    double c_rate, esize_grow, esize_death, land_grow, land_die;
    
    x_col             = (int) paras[33];
    y_col             = (int) paras[34];
    gadj              = (int) paras[39]; /* Adjustment to the growth rate col */
    klld              = (int) paras[42]; /* Adjustment to kill                */
    resource_type_col = (int) paras[44];
    resource_type     = (int) paras[45];
    resource_effect   = (int) paras[47];
    landscape_layer   = (int) paras[48];
    esize_grow        = paras[86];
    esize_death       = paras[87];
    
    for(resource = 0; resource < resource_number; resource++){
        if(resource_array[resource][resource_type_col] == resource_type){
            x_pos  = resource_array[resource][x_col];
            y_pos  = resource_array[resource][y_col];
            c_rate = resource_array[resource][resource_effect];
            landscape[x_pos][y_pos][landscape_layer] *= (1 - c_rate);
            land_grow = (c_rate * esize_grow)  * resource_array[resource][9];
            land_die  = (c_rate * esize_death) * resource_array[resource][10];
            resource_array[resource][gadj] += land_grow;
            resource_array[resource][klld] += land_die;
        } 
    }
}

/* =============================================================================
 * This function checks to see if a resource population size exceeds its 
 * carrying capacity as applied to death
 * Inputs include:
 *     res_num_total: The total number of resources in the population
 *     paras: Global parameters needed
 * ========================================================================== */
void resource_over_death_K(int res_num_total, double *paras){
    
    int death_K;
    
    death_K = (int) paras[6];
    
    paras[108] = 0;
    if(res_num_total > death_K){
        paras[108] = 1;
    }
}


/* =============================================================================
 * This function models a single resource eating on the landscape
 * ========================================================================== */
void resource_feeds(double **resource_array, double ***landscape, double *paras,
                    int resource){

    int resource_effect, landscape_layer, x_col, y_col, x_pos, y_pos, cons_col;
    double c_rate, consumed;
    
    x_col             = (int) paras[33];
    y_col             = (int) paras[34];
    resource_effect   = (int) paras[47];
    landscape_layer   = (int) paras[48];
    cons_col          = (int) paras[115]; /* col in res_removing of consump */
    
    x_pos    = (int) resource_array[resource][x_col];
    y_pos    = (int) resource_array[resource][y_col];
    c_rate   = resource_array[resource][resource_effect];
    consumed = c_rate * landscape[x_pos][y_pos][landscape_layer];
    
    resource_array[resource][cons_col]       += consumed;
    landscape[x_pos][y_pos][landscape_layer] -= consumed;
}

/* =============================================================================
 * This function models the process of resources feeding on the landscape
 * ========================================================================== */
void resource_feeding(double **resource_array, double ***landscape,
                      double *paras, int resource_number){
    
    int resource, fed_col, tot_fed, is_dead, rm_row;
    int *fed;
    
    fed_col = (int) paras[118]; /* Location of the column tracking fed */
    rm_row  = (int) paras[42];  /* Has the resource already died? */
    tot_fed = 0;

    fed = (int *) malloc(resource_number * sizeof(int));
    for(resource = 0; resource < resource_number; resource++){
        is_dead = resource_array[resource][rm_row];
        if(is_dead < 1){
            fed[resource]  = resource_array[resource][fed_col];
            tot_fed       += fed[resource];
        }
    }
    
    while(tot_fed > 0){
        
        do{ /* This will find a resource with feeding actions left */
            resource = get_rand_int(0, resource_number);
        } while(fed[resource] == 0);
                    
        move_a_resource(resource_array, landscape, paras, resource);
        
        resource_feeds(resource_array, landscape, paras, resource);
        
        fed[resource]--;
        tot_fed--;
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
    int res_nums_added;      /* Number of resources to be added */
    int res_nums_subtracted; /* Number of resoruces to be removed */
    int res_num_total;       /* Total number of resources in returned array */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int off_col;             /* The column where the offspring are held */
    int rm_col;              /* Column where removal is indiciated */
    int len_PARAMETERS;      /* Length of the parameters vector */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    double csr, crp;         /* Consumption requirements, survival and repr */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *R_ptr_new;       /* Pointer to RESOURCE_NEW (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *land_ptr_new;    /* Pointer to LAND_NEW (interface R and C) */
    double *paras_ptr;       /* Pointer to PARAMETERS (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *paras_ptr_new;   /* Pointer to new paras (interface R and C) */
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
    paras_ptr = REAL(PARAMETERS);
    
    len_PARAMETERS = GET_LENGTH(PARAMETERS);
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
    
    /* Code below copies the paras vector into C */
    paras   = malloc(len_PARAMETERS * sizeof(double *));
    vec_pos   = 0;
    for(xloc = 0; xloc < len_PARAMETERS; xloc++){
        paras[xloc] = paras_ptr[vec_pos];
        vec_pos++;
    } /* The parameters vector is now copied into C */
    
    /* Do the biology here now */
    /* ====================================================================== */
    
    off_col   = (int) paras[38];
    rm_col    = (int) paras[43];
    csr       = paras[116]; /* Consumption required for survival */
    crp       = paras[117]; /* Consumption needed for one offspring */
    
    /* Resource time step and age needs to be increased by one */
    add_time(res_old, paras);

    /* If we need to get amount eaten because it affects death or birth*/
    if(csr > 0 || crp > 0){ 
        resource_feeding(res_old, land, paras, res_number);
    }else{ /* One-off resources affect the landscape */
        res_landscape_interaction(res_new, land, paras, res_num_total); 
    }
    
    /* Identify, and calculate the number of, added individuals */
    res_add(res_old, paras);
    res_nums_added = 0; 
    for(resource = 0; resource < res_number; resource++){
        res_nums_added += res_old[resource][off_col];
    }
    res_make = malloc(res_nums_added * sizeof(double *));
    for(resource = 0; resource < res_nums_added; resource++){
        res_make[resource] = malloc(trait_number * sizeof(double));   
    }
    
    res_place(res_make, res_old, paras, res_nums_added);
    
    /* Identify, and calculate the number, of removed individuals */    
    res_remove(res_old, paras);
    res_nums_subtracted = 0; 
    for(resource = 0; resource < res_number; resource++){
        if(res_old[resource][rm_col] < 0){
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
        if(res_old[resource][rm_col] >= 0){
            for(trait = 0; trait < trait_number; trait++){
                res_new[resource_new][trait] = res_old[resource][trait];
            }
            res_new[resource_new][15] = 0;
            res_new[resource_new][16] = 0;
            res_new[resource_new][17] = 0;
            res_new[resource_new][18] = 0;
            res_new[resource_new][19] = 0;
            res_new[resource_new][20] = 0;
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
        res_new[resource_new][18] = 0;
        res_new[resource_new][19] = 0;
        res_new[resource_new][20] = 0;
        resource_new++;
    }
    
    /* Resources move according to move function and parameter) */
    res_mover(res_new, land, paras);
    
    /* Check to see if the population is over carrying capacity */
    resource_over_death_K(res_num_total, paras);

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
    
    SEXP PARAMETERS_NEW;
    PROTECT( PARAMETERS_NEW = allocVector(REALSXP, len_PARAMETERS) );
    protected_n++;
    
    paras_ptr_new = REAL(PARAMETERS_NEW);

    vec_pos = 0;
    for(xloc = 0; xloc < len_PARAMETERS; xloc++){
        paras_ptr_new[vec_pos] = paras[xloc];
        vec_pos++;
    }    
    
    SEXP EVERYTHING;
    EVERYTHING = PROTECT( allocVector(VECSXP, 3) );
    protected_n++;
    SET_VECTOR_ELT(EVERYTHING, 0, RESOURCE_NEW);
    SET_VECTOR_ELT(EVERYTHING, 1, LAND_NEW);
    SET_VECTOR_ELT(EVERYTHING, 2, PARAMETERS_NEW);   
    
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    free(paras);
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

