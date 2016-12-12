#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* Just add a number by two to test */
/* TODO: Arguments: RESOURCE_1, RESOURCE_2, VECTOR_OF_PARAMETER_VALUES */
SEXP resource(SEXP RESOURCE_1){

    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */
    
    /* Below tells R not to delete the object if garbage collector activated */
    PROTECT(RESOURCE_1 = AS_INTEGER(RESOURCE_1));

    /* Defines and protects a new array to return */
    SEXP RESOURCE_1_NEW;
    PROTECT(RESOURCE_1_NEW = NEW_INTEGER(1));

    /* PROTECT(ans = allocMatrix(RESOURCE_1_NEW, nx, ny)); */
    
    /* New structure below to check the dimensions of each resource */
    SEXP dims;
    PROTECT(dims = NEW_INTEGER(1));
    
    
    /* Some more familiar c code down here */
    /* ====================================================================== */
        double resource_1, resource_n, stocha;
    
    stocha     = runif(0, 1);
    resource_1 = asReal(RESOURCE_1);
    resource_n = resource_1 + (100 * stocha);
    resource_n = floor(resource_n);

    /*-------------------------------------*/
    
    dims = getAttrib(RESOURCE_1, install("dim"));
    
    /* INTEGER(RESOURCE_1_NEW)[0] = resource_n; */
        
    UNPROTECT(3);

    return dims; /* RESOURCE_1_NEW; */
}


/* R_xlen_t n = xlength(x) */