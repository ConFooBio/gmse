#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* This function just adds a time step to the relevant individual column */
void add_time(double **res_adding, int time_trait, int rows, int time_para){
    int resource;
    
    for(resource = 0; resource < rows; resource++){
        res_adding[resource][time_trait] = time_para;   
    }
}

/* This function moves individuals on the landscape according to some rules */
void mover(double **res_moving, int xloc, int yloc, int move_para, int rows){
    
    int res_num;      /* Resource number index    */
    int move_len;     /* Length of a move         */
    int move_dir;     /* Move direction (-1 or 1) */
    double rand_uni;  /* Randum uniform number    */
    
    for(res_num=0; res_num < rows; res_num++){
        /* Move first in the xloc direction --------------------------------- */
        rand_uni = 0.0;
        do{ /* On the very off chance that runif selects zero exactly */
            rand_uni  = runif(-1, 1); /* Rand uni between -1 and 1 */
        } while(rand_uni == 0.0);
        if(rand_uni > 0){  /* Below assigns a sign to move_dir */
            move_dir = 1;
        }else{
            move_dir = -1;
        }
        rand_uni = 1.0;
        do{ /* Again, just to make sure the res_num never moves too far */
            rand_uni = runif(0, 1);
        } while(rand_uni == 1.0);
        move_len = floor( rand_uni * (res_moving[res_num][move_para] + 1) );
        res_moving[res_num][xloc] += (move_dir * move_len); /* x change added */
        /* Move next in the yloc direction ---------------------------------- */
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
        do{ /* Again, just to make sure the res_num never moves too far */
            rand_uni = runif(0, 1);
        } while(rand_uni == 1.0);
        move_len = floor( rand_uni * (res_moving[res_num][move_para] + 1) );
        res_moving[res_num][yloc] += (move_dir * move_len); /* y change added */
    }
}

/* TODO: Argument: VECTOR_OF_PARAMETER_VALUES */
SEXP resource(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* x and y locations in the RESOURCE array */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int resource;            /* Index for resource (rows of RESOURCE) */
    int trait;               /* Index for resource traits (cols of RESOURCE) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included */
    int para_number;         /* Number of parameters included */
    int res_nums_added;      /* Number of resources to be added */
    int res_num_total;       /* Total number of resources in returned array */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *len_PARAMETERS;     /* Length of the PARAMETERS vector incoming */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *R_ptr_new;       /* Pointer to RESOURCE_NEW (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double **res_old;        /* Array to store the old RESOURCE in C */
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
    land_y = dim_LANDSCAPE[0];
    land_x = dim_LANDSCAPE[1];
    land   = malloc(land_y * sizeof(double *));
    for(xloc = 0; xloc < land_x; xloc++){
        land[xloc] = malloc(land_y * sizeof(double));   
    } /* LANDSCAPE is now stored as land */

    /* Do the biology here now */
    /* ====================================================================== */
    
    /* Resources move according to move function and parameter) */
    mover(res_old, 3, 4, 5, res_number); 
    
    
    
    
    
    res_nums_added = 0; /* TODO: Add a birth function here */
    res_num_total  = res_number + res_nums_added;

    /* Below makes a new array for new RESOURCE, then adds it */
    res_new = malloc(res_num_total * sizeof(double *));
    for(resource = 0; resource < res_num_total; resource++){
        res_new[resource] = malloc(trait_number * sizeof(double));   
    }    
    
    /* TODO: Loop first through all retained res_nums, then new ones */
    /* Will need some more complex rules for the below -- these loops will */
    /* eventually paste every res_num together that makes it out */
    for(trait=0; trait<trait_number; trait++){
        for(resource=0; resource<res_num_total; resource++){
            res_new[resource][trait] = res_old[resource][trait];
        }
    }
    
    add_time(res_new, 6, res_num_total, paras[0]);
    
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

          
          
          
          
