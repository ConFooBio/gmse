#include "utilities.h"

/* =============================================================================
 * MAIN OBSERVATION FUNCTION:
 * ===========================================================================*/

/* =============================================================================
 *  ****     This is the main function for the observation model     ****
 *  This function reads resource, landscape, and agent arrays, and a parameter 
 *  vector from R, runs other functions in the observation.c file, then returns 
 *  the an observation array, which replicates the resource array with added
 *  columns for tracking observations.
 *  Inputs include:
 *      RESOURCE:   An array of *row resources and *col traits for each resource
 *      LANDSCAPE:  An array of *row by *col size that makes up the landscape
 *      PARAMETERS: Parameters read into the function for population processes
 *      AGENT:      An array of *row agents and *col traits for each agent
 * ===========================================================================*/
SEXP observation(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* Index of x & y locations on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int zloc, land_z;        /* z locations */
    int resource;            /* Index for resource (rows of RESOURCE) */
    int res_trait;           /* Index for resource traits (cols of RESOURCE) */
    int agent;               /* Index for agent in the array (rows) */
    int agent_trait;         /* Index for agent traits (cols of agent_array) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included in the resource */
    int obs_columns;         /* Columns for the observation array */
    int agent_number;        /* Number of agents that can potentially observe */
    int agent_traits;        /* Number of traits that each agent has */
    int para_number;         /* Number of parameters included */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int new_obs;             /* New observations made */
    int add_obs;             /* Index for adding observations */
    int method;              /* Type of method used to estimate pop size */
    int times_obs;           /* Number of times observation is conducted */
    int obs_iter;            /* To count up -- which observation iteration */
    int res_type;            /* Types of resources that are being observed */
    int a_type;              /* Type of agent doing observing (manager = 0) */
    int by_type;             /* Type category for observing (default = 1)*/
    int time_para;           /* Time in the simulation the function called */
    int edge_type;           /* The type of edge on the landscape */
    int move_type;           /* How resources move on the landscape */
    int move_res;            /* Should the resources move between obs */
    int fixed_sample;        /* The fixed number of resources sampled */
    int tx0, tx1, ty0, ty1;  /* Variables that define a transect sampled */
    int working_agents;      /* How many agents are helping out? */
    int transect_len;        /* The length of a transect sampled */
    int transect_eff;        /* Transect efficiency: How many observers help */
    int *add_resource;       /* Vector of added resources */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *agent_ptr;       /* Pointer to AGENT (interface R and C) */
    double *new_agent_ptr;   /* Pointer to new agents that are returned */
    double *data_ptr;        /* Pointer to DATA (interface R and C) */
    double *land_ptr_new;    /* Pointer to a new landscape */
    double **resource_array; /* Array to store the old RESOURCE in C */
    double ***land;          /* Array to store the landscape in C*/
    double **agent_array;    /* Array to store the agents in C */
    double **obs_array;      /* Array to store the observations made in C */

    /* First take care of all the reading in of code from R to C */
    /* ====================================================================== */

    protected_n = 0;

    PROTECT( RESOURCE = AS_NUMERIC(RESOURCE) );
    protected_n++;
    R_ptr = REAL(RESOURCE);
    
    PROTECT( LANDSCAPE = AS_NUMERIC(LANDSCAPE) );
    protected_n++;
    land_ptr = REAL(LANDSCAPE);
    
    PROTECT( AGENT = AS_NUMERIC(AGENT) );
    protected_n++;
    agent_ptr = REAL(AGENT);    
    
    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras = REAL(PARAMETERS);
    
    dim_RESOURCE   = INTEGER( GET_DIM(RESOURCE)  );
    dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );
    dim_AGENT      = INTEGER( GET_DIM(AGENT) );

    /* The C code for the model itself falls under here */
    /* ====================================================================== */

    /* Code below remakes the RESOURCE matrix, with extra columns for obs */
    res_number        = dim_RESOURCE[0];
    trait_number      = dim_RESOURCE[1]; 
    resource_array    = malloc(res_number * sizeof(double *));
    for(resource = 0; resource < res_number; resource++){
        resource_array[resource] = malloc(trait_number * sizeof(double));   
    } 
    vec_pos = 0;
    for(res_trait = 0; res_trait < trait_number; res_trait++){
        for(resource = 0; resource < res_number; resource++){
            resource_array[resource][res_trait] = R_ptr[vec_pos];
            vec_pos++;
        }
    }
    /* RESOURCE is now stored as resource_array (discrete resources) */

    /* Code below reads in the LANDSCAPE for ease of use */
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
    for(xloc = 0; xloc < land_x; xloc++){
        for(yloc = 0; yloc < land_y; yloc++){
            for(zloc = 0; zloc < land_z; zloc++){
                land[xloc][yloc][zloc] = land_ptr[vec_pos];
                vec_pos++;
            }
        }
    }  /* LANDSCAPE is now stored as land */    
    
    /* Code below remakes the AGENT matrix for easier use */
    agent_number        = dim_AGENT[0];
    agent_traits        = dim_AGENT[1];
    agent_array         = malloc(agent_number * sizeof(double *));
    for(agent = 0; agent < agent_number; agent++){
        agent_array[agent] = malloc(agent_traits * sizeof(double));   
    } 
    vec_pos = 0;
    for(agent_trait = 0; agent_trait < agent_traits; agent_trait++){
        for(agent = 0; agent < agent_number; agent++){
            agent_array[agent][agent_trait] = agent_ptr[vec_pos];
            vec_pos++;
        }
    }
    /* RESOURCE is now stored as resource_array (discrete resources) */    

    /* Do the biology here now */
    /* ====================================================================== */

    printf("Do I work? \n");
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP NEW_RESOURCES;
    PROTECT( NEW_RESOURCES = allocMatrix(REALSXP, res_number, trait_number) );
    protected_n++;
    
    data_ptr = REAL(NEW_RESOURCES);

    vec_pos = 0;
    for(res_trait = 0; res_trait < trait_number; res_trait++){
        for(resource = 0; resource < res_number; resource++){
            data_ptr[vec_pos] = obs_array[resource][res_trait];
            vec_pos++;
        }
    }   

    SEXP NEW_AGENTS;
    PROTECT( NEW_AGENTS = allocMatrix(REALSXP, agent_number, agent_traits) );
    protected_n++;
    
    new_agent_ptr = REAL(NEW_AGENTS);
    
    vec_pos = 0;
    for(agent_trait = 0; agent_trait < agent_traits; agent_trait++){
        for(agent = 0; agent < agent_number; agent++){
            new_agent_ptr[vec_pos] = agent_array[agent][agent_trait];
            vec_pos++;
        }
    }
    
    SEXP NEW_LANDSCAPE;
    PROTECT( NEW_LANDSCAPE = alloc3DArray(REALSXP, land_x, land_y, land_z) );
    protected_n++;
    
    land_ptr_new = REAL(NEW_LANDSCAPE);
    
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
    SET_VECTOR_ELT(EVERYTHING, 0, NEW_RESOURCES);
    SET_VECTOR_ELT(EVERYTHING, 1, NEW_AGENTS);
    SET_VECTOR_ELT(EVERYTHING, 2, NEW_LANDSCAPE);
    
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(agent = 0; agent < agent_number; agent++){
        free(agent_array[agent]);
    }
    free(agent_array);
    for(xloc = 0; xloc < land_x; xloc++){
        for(yloc = 0; yloc < land_y; yloc++){
            free(land[xloc][yloc]);   
        }
        free(land[xloc]);        
    }
    free(land); 
    for(resource = 0; resource < res_number; resource++){
        free(resource_array[resource]);
    }
    free(resource_array);

    return(EVERYTHING); 
}
/* ===========================================================================*/
