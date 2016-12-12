#include <R.h>
#include <Rdefines.h>

/* Just add a number by two to test */
SEXP resource(SEXP RESOURCE_1){

    PROTECT(RESOURCE_1 = AS_INTEGER(RESOURCE_1));

    SEXP RESOURCE_1_NEW;
    PROTECT(RESOURCE_1_NEW = NEW_INTEGER(1));

    /* Some more familiar c code down here */
    int resource_1, resource_n;
    
    resource_1 = asReal(RESOURCE_1);
    resource_n = resource_1 * 2;
    
    /*-------------------------------------*/
    
    INTEGER(RESOURCE_1_NEW)[0] = resource_n;
        
    UNPROTECT(2);

    return RESOURCE_1_NEW;
}
