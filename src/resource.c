#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Just add a number by two to test */
/* TODO: Arguments: RESOURCE_1, RESOURCE_2, VECTOR_OF_PARAMETER_VALUES */
SEXP resource(SEXP RESOURCE_1){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int protected_n;
    int *dimX;
    double *R_ptr_1;
    double *ansptr;

    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;
    
    PROTECT(RESOURCE_1 = AS_NUMERIC(RESOURCE_1));
    protected_n++;
    
    R_ptr_1 = REAL(RESOURCE_1);
    
    dimX = INTEGER(GET_DIM(RESOURCE_1));
    
    
    SEXP ans;
    PROTECT(ans = allocMatrix(REALSXP, 1, 2));
    protected_n++;
    
    ansptr = REAL(ans);
    ansptr[0] = dimX[0];
    ansptr[1] = dimX[1];
    
    UNPROTECT(protected_n);
    

    return(ans); /* RESOURCE_1_NEW; */
}
