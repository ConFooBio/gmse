#include <R.h>
#include <Rdefines.h>

/* Just add a number by two to test */
SEXP resource(SEXP RESOURCE_1){

    PROTECT(RESOURCE_1 = AS_INTEGER(RESOURCE_1));

    
    SEXP RESOURCE_1_NEW;
    PROTECT(RESOURCE_1_NEW = NEW_INTEGER(1));

    INTEGER(RESOURCE_1_NEW)[0] = 2 + asReal(RESOURCE_1);
    
    UNPROTECT(2);

    return RESOURCE_1_NEW;
}
