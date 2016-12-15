#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

void mover(double **inds_moving, int xloc, int yloc, int move_para, int rows){
    
    int individual;
    int move_len, move_dir;
    double rand_uni;
    
    for(individual=0; individual < rows; individual++){
        /* Move first in the xloc direction -------------------------------- */
        rand_uni = 0.0;
        do{ /* On the very of chance that runif selects zero exactly */
            rand_uni  = runif(-1, 1); /* Rand uni between -1 and 2 */
        } while(rand_uni == 0.0);
        if(rand_uni > 0){
            move_dir = 1;
        }else{
            move_dir = -1;
        }
        rand_uni = 1.0;
        do{ /* Again, just to make sure the individual never moves too far */
            rand_uni = runif(0, 1);
        } while(rand_uni == 1.0);
        move_len = floor( rand_uni * (inds_moving[individual][move_para] + 1) );
        inds_moving[individual][xloc] += (move_dir * move_len); 
        /* Move next in the yloc direction --------------------------------- */
        rand_uni = 0.0;
        do{ /* On the very of chance that runif selects zero exactly */
            rand_uni  = runif(-1, 1); /* Rand uni between -1 and 1 */
        } while(rand_uni == 0);
        if(rand_uni > 0){
            move_dir = 1;
        }else{
            move_dir = -1;
        }
        rand_uni = 1.0;
        do{ /* Again, just to make sure the individual never moves too far */
            rand_uni = runif(0, 1);
        } while(rand_uni == 1.0);
        move_len = floor( rand_uni * (inds_moving[individual][move_para] + 1) );
        inds_moving[individual][yloc] += (move_dir * move_len);
    }
}

/* Just add a number by two to test */
/* TODO: Arguments: RESOURCE_1, RESOURCE_2, VECTOR_OF_PARAMETER_VALUES */
SEXP resource(SEXP RESOURCE_1, SEXP LANDSCAPE){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;
    int land_x, land_y;
    int individual;
    int trait;
    int individual_number_1, trait_number_1;
    int individuals_added_1, individuals_total_1;
    int protected_n;
    int vec_pos;
    int *dim_RESOURCE_1;
    int *dim_LANDSCAPE;
    double *R_ptr_1;      /* These pointers need to be doubles for R to C */
    double *R_ptr_1_new;
    double *land_ptr;
    double **individuals_old_1;
    double **individuals_new_1;
    double **land;

    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;
    
    PROTECT( RESOURCE_1 = AS_NUMERIC(RESOURCE_1) );
    protected_n++;
    R_ptr_1 = REAL(RESOURCE_1);
    
    PROTECT( LANDSCAPE = AS_NUMERIC(LANDSCAPE) );
    protected_n++;
    land_ptr = REAL(LANDSCAPE);
    
    dim_RESOURCE_1 = INTEGER( GET_DIM(RESOURCE_1) );
    dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );
    
    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    /* Code below remakes the RESOURCE_1 matrix for easier use */
    individual_number_1 = dim_RESOURCE_1[0];
    trait_number_1      = dim_RESOURCE_1[1];
    individuals_old_1   = malloc(individual_number_1 * sizeof(double *));
    for(individual = 0; individual < individual_number_1; individual++){
        individuals_old_1[individual] = malloc(trait_number_1 * sizeof(double));   
    } 
    vec_pos = 0;
    for(trait = 0; trait < trait_number_1; trait++){
        for(individual = 0; individual < individual_number_1; individual++){
            individuals_old_1[individual][trait] = R_ptr_1[vec_pos];
            vec_pos++;
        }
    }
    /* RESOURCE_1 is now stored as individuals_old_1 (discrete resources) */

    /* Code below reads in the LANDSCAPE for easy of use */
    land_y = dim_LANDSCAPE[0];
    land_x = dim_LANDSCAPE[1];
    land   = malloc(land_y * sizeof(double *));
    for(xloc = 0; xloc < land_x; xloc++){
        land[xloc] = malloc(land_y * sizeof(double));   
    } /* LANDSCAPE is now stored as land */
    
    /* Do the biology here now */
    
    /* Individuals move according to move function and parameter) */
    mover(individuals_old_1, 1, 2, 3, individual_number_1); 
    
    
    
    
    
    individuals_added_1 = 0; /* TODO: Add a birth function here */
    individuals_total_1 = individual_number_1 + individuals_added_1;

    /* Below makes a new array for new Resource_1, then adds it */
    individuals_new_1 = malloc(individuals_total_1 * sizeof(double *));
    for(individual = 0; individual < individuals_total_1; individual++){
        individuals_new_1[individual] = malloc(trait_number_1 * sizeof(double));   
    }    
    
    /* TODO: Loop first through all retained individuals, then new ones */
    /* Will need some more complex rules for the below -- these loops will */
    /* eventually paste every individual together that makes it out */
    for(trait=0; trait<trait_number_1; trait++){
        for(individual=0; individual<individuals_total_1; individual++){
            individuals_new_1[individual][trait] = 
                individuals_old_1[individual][trait];
        }
    }
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP RESOURCE_1_NEW;
    PROTECT( RESOURCE_1_NEW = allocMatrix(REALSXP, individuals_total_1, 
                                          trait_number_1) );
    protected_n++;
    
    R_ptr_1_new = REAL(RESOURCE_1_NEW);

    vec_pos = 0;
    for(trait=0; trait<trait_number_1; trait++){
        for(individual=0; individual<individuals_total_1; individual++){
            R_ptr_1_new[vec_pos] = individuals_new_1[individual][trait];
            vec_pos++;
        }
    }            
        
        
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(individual = 0; individual < individual_number_1; individual++){
        free(individuals_old_1[individual]);
    } 
    for(individual = 0; individual < individuals_total_1; individual++){
        free(individuals_new_1[individual]);
    }
    for(xloc = 0; xloc < land_x; xloc++){
        free(land[xloc]);        
    }
    
    return(RESOURCE_1_NEW); 
}

          
          
          
          
