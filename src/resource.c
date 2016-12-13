#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Just add a number by two to test */
/* TODO: Arguments: RESOURCE_1, RESOURCE_2, VECTOR_OF_PARAMETER_VALUES */
SEXP resource(SEXP RESOURCE_1){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int individual, trait;
    int individual_number, trait_number;
    int individuals_added, individuals_total;
    int protected_n;
    int *dimX;
    double *R_ptr_1;
    double *R_ptr_1_new;
    double *ansptr;
    int **individuals_old;
    int **individuals_new;
    int vec_pos;

    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;
    
    PROTECT( RESOURCE_1 = AS_NUMERIC(RESOURCE_1) );
    protected_n++;
    
    R_ptr_1 = REAL(RESOURCE_1);
    
    dimX = INTEGER( GET_DIM(RESOURCE_1) );
    
    /* The C code for the model itself falls under here */
    /* ====================================================================== */
    
    /* Code below remakes the RESOURCE_1 matrix for easier use */
    individual_number = dimX[0];
    trait_number      = dimX[1];
    individuals_old = malloc(individual_number * sizeof(int *));
    for(individual = 0; individual < individual_number; individual++){
        individuals_old[individual] = malloc(trait_number * sizeof(int));   
    }
    
    individuals_added = 2;
    individuals_total = individual_number + individuals_added;

    individuals_new = malloc(individuals_total * sizeof(int *));
    for(individual = 0; individual < individuals_total; individual++){
        individuals_new[individual] = malloc(trait_number * sizeof(int));   
    }    
    
    for(trait=0; trait<trait_number; trait++){
        for(individual=0; individual<individuals_total; individual++){
            individuals_new[individual][trait] = individual + trait;
        }
    }
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP RESOURCE_1_NEW;
    PROTECT( RESOURCE_1_NEW = allocMatrix(REALSXP, individuals_total, 
                                          trait_number) );
    protected_n++;
    
    R_ptr_1_new = REAL(RESOURCE_1_NEW);

    vec_pos = 0;
    for(trait=0; trait<trait_number; trait++){
        for(individual=0; individual<individuals_total; individual++){
            R_ptr_1_new[vec_pos] = individuals_new[individual][trait] + 1;
            vec_pos++;
        }
    }            
        
        
    UNPROTECT(protected_n);
    
    return(RESOURCE_1_NEW); 
}
