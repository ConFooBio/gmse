#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP anecdotal(SEXP, SEXP, SEXP, SEXP);
extern SEXP manager(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP observation(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP resource(SEXP, SEXP, SEXP);
extern SEXP user(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"anecdotal",   (DL_FUNC) &anecdotal,   4},
    {"manager",     (DL_FUNC) &manager,     9},
    {"observation", (DL_FUNC) &observation, 5},
    {"resource",    (DL_FUNC) &resource,    3},
    {"user",        (DL_FUNC) &user,        8},
    {NULL, NULL, 0}
};

void R_init_GMSE(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}