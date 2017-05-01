#include "game.h"

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
 *      RESOURCE:    An array of *row resources & *col traits for each resource
 *      LANDSCAPE:   An array of *row by *col size that makes up the landscape
 *      PARAMETERS:  Parameters read into the function for population processes
 *      AGENT:       An array of *row agents and *col traits for each agent
 *      COST:        An array of the cost of actions for each agent
 *      ACTION:      An array of the action of agents
 *      JACOBIAN:    A Jacobian matrix of resource type and landscape effects
 *      INTERACT:    A table indexing types with rows of interaction array
 *      OBSERVATION: An array of the observations from the observation model
 * ===========================================================================*/
SEXP manager(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT, 
             SEXP COST, SEXP ACTION, SEXP JACOBIAN, SEXP INTERACT, 
             SEXP OBSERVATION){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* Index of x & y locations on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int zloc, land_z;        /* z locations */
    int c_x, c_y, c_z;       /* Dimensions of cost array */
    int a_x, a_y, a_z;       /* Dimensions of action array */
    int int_d0, int_d1;      /* Dimensions of the interaction lookup table */
    int obs_d0, obs_d1;      /* Dimensions of the observation array */
    int row, col, layer;     /* Indices for utility (COST & ACTION) arrays */
    int resource;            /* Index for resource (rows of RESOURCE) */
    int res_trait;           /* Index for resource traits (cols of RESOURCE) */
    int agent;               /* Index for agent in the array (rows) */
    int agentID;             /* Index for the ID of an agent */
    int agent_trait;         /* Index for agent traits (cols of agent_array) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included in the resource */
    int agent_number;        /* Number of agents that can potentially observe */
    int agent_traits;        /* Number of traits that each agent has */
    int jacobian_dim;        /* Dimensions of the (square) Jacobian matrix */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    int *dim_COST;           /* Dimensions of the COST array incoming */
    int *dim_ACTION;         /* Dimensions of the ACTION array incoming */
    int *dim_JACOBIAN;       /* Dimensions of the JACOBIAN matrix incoming */
    int *dim_INTERACT;       /* Dimensions of the INTERACT matrix incoming */
    int *dim_OBSERVATION;    /* Dimensions of the OBSERVATION array incoming */
    int **interact_table;    /* Lookup table for resource & land interactions */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *agent_ptr;       /* Pointer to AGENT (interface R and C) */
    double *cost_ptr;        /* Pointer to COST (interface R and C) */
    double *action_ptr;      /* Pointer to ACTION (interface R and C) */
    double *new_agent_ptr;   /* Pointer to new agents that are returned */
    double *new_action_ptr;  /* Pointer to new action array that is returned */
    double *new_cost_ptr;    /* Pointer to new cost array that is returned */
    double *data_ptr;        /* Pointer to DATA (interface R and C) */
    double *land_ptr_new;    /* Pointer to a new landscape */
    double *jaco_ptr;        /* Pointer to JACOBIAN (interface R and C) */
    double *intr_ptr;        /* Pointer to INTERACT (interface R and C) */
    double *obs_ptr;         /* Pointer to OBSERVATION (interface R and C) */
    double **resource_array; /* Array to store the old RESOURCE in C */
    double **Jacobian_mat;   /* Array to store the Jacobian matrix in C */
    double ***land;          /* Array to store the landscape in C*/
    double **agent_array;    /* Array to store the agents in C */
    double ***costs;         /* Array of the costs of user actions */
    double ***actions;       /* Array of user actions */
    double **obs_array;      /* Array of observations from observation model */ 

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
    
    PROTECT( COST = AS_NUMERIC(COST) );
    protected_n++;
    cost_ptr = REAL(COST);
    
    PROTECT( ACTION = AS_NUMERIC(ACTION) );
    protected_n++;
    action_ptr = REAL(ACTION);
    
    PROTECT( JACOBIAN = AS_NUMERIC(JACOBIAN) );
    protected_n++;
    jaco_ptr = REAL(JACOBIAN);
    
    PROTECT( INTERACT = AS_NUMERIC(INTERACT) );
    protected_n++;
    intr_ptr = REAL(INTERACT);
    
    PROTECT( OBSERVATION = AS_NUMERIC(OBSERVATION) );
    protected_n++;
    obs_ptr = REAL(OBSERVATION);
    
    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras = REAL(PARAMETERS);
    
    dim_RESOURCE    = INTEGER( GET_DIM(RESOURCE)  );
    dim_LANDSCAPE   = INTEGER( GET_DIM(LANDSCAPE) );
    dim_AGENT       = INTEGER( GET_DIM(AGENT) );
    dim_COST        = INTEGER( GET_DIM(COST) );
    dim_ACTION      = INTEGER( GET_DIM(ACTION) );
    dim_JACOBIAN    = INTEGER( GET_DIM(JACOBIAN) );
    dim_INTERACT    = INTEGER( GET_DIM(INTERACT) );
    dim_OBSERVATION = INTEGER( GET_DIM(OBSERVATION) );

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
    for(zloc = 0; zloc < land_z; zloc++){
        for(yloc = 0; yloc < land_y; yloc++){
            for(xloc = 0; xloc < land_x; xloc++){
                land[xloc][yloc][zloc] = land_ptr[vec_pos];
                vec_pos++;
            }
        }
    }  /* LANDSCAPE is now stored as land */
    
    /* Code below reads in the COST array for ease of use */
    c_z   = dim_COST[2];
    c_y   = dim_COST[1];
    c_x   = dim_COST[0];
    costs = malloc(c_x * sizeof(double *));
    for(row = 0; row < c_x; row++){
        costs[row] = malloc(c_y * sizeof(double *));
        for(col = 0; col < c_y; col++){
            costs[row][col] = malloc(c_z * sizeof(double));
        }
    }
    vec_pos = 0;
    for(layer = 0; layer < c_z; layer++){
        for(col = 0; col < c_y; col++){
            for(row = 0; row < c_x; row++){
                costs[row][col][layer] = cost_ptr[vec_pos];
                vec_pos++;
            }
        }
    } /* COST is now stored as costs */

    /* Code below reads in the ACTION array for ease of use */
    a_z     = dim_ACTION[2];
    a_y     = dim_ACTION[1];
    a_x     = dim_ACTION[0];
    actions = malloc(a_x * sizeof(double *));
    for(row = 0; row < a_x; row++){
        actions[row] = malloc(a_y * sizeof(double *));
        for(col = 0; col < a_y; col++){
            actions[row][col] = malloc(a_z * sizeof(double));
        }
    }
    vec_pos = 0;
    for(layer = 0; layer < a_z; layer++){
        for(col = 0; col < a_y; col++){
            for(row = 0; row < a_x; row++){
                actions[row][col][layer] = action_ptr[vec_pos];
                vec_pos++;
            }
        }
    } /* ACTION is now stored as costs */
    
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

    /* Code below remakes the JACOBIAN matrix for easier use */
    jacobian_dim  = dim_JACOBIAN[0];
    Jacobian_mat  = malloc(jacobian_dim * sizeof(double *));
    for(row = 0; row < jacobian_dim; row++){
        Jacobian_mat[row] = malloc(jacobian_dim * sizeof(double));
    }
    vec_pos = 0;
    for(col = 0; col < jacobian_dim; col++){
        for(row = 0; row < jacobian_dim; row++){
            Jacobian_mat[row][col] = jaco_ptr[vec_pos];
            vec_pos++;
        }
    }
    
    /* Code below remakes the INTERACT table for easier use */
    int_d0  = dim_INTERACT[0];
    int_d1  = dim_INTERACT[1];
    interact_table  = malloc(int_d0 * sizeof(int *));
    for(row = 0; row < int_d0; row++){
        interact_table[row] = malloc(int_d1 * sizeof(int));
    }
    vec_pos = 0;
    for(col = 0; col < int_d1; col++){
        for(row = 0; row < int_d0; row++){
            interact_table[row][col] = intr_ptr[vec_pos];
            vec_pos++;
        }
    }
    
    /* Code below remakes the OBSERVATION array for easier use */
    obs_d0 = dim_OBSERVATION[0]; 
    obs_d1 = dim_OBSERVATION[1];
    obs_array = malloc(obs_d0 * sizeof(double *));
    for(row = 0; row < obs_d0; row++){
        obs_array[row] = malloc(obs_d1 * sizeof(double));
    }
    vec_pos = 0;
    for(col = 0; col < obs_d1; col++){
        for(row = 0; row < obs_d0; row++){
            obs_array[row][col] = obs_ptr[vec_pos];
            vec_pos++;
        }
    }
    
    /* Do the biology here now */
    /* ====================================================================== */

    /* 1. Get summary statistics for resources from the observation array     */
    /* 2. Place estimated resource abundances in a vector the size of int_d0  */
    /* 3. Initialise new vector of size int_d0 with temp utilities of manager */  
    /* 4. Subtract abundances from temp utilities to get marginal utilities   */
    /* 5. Insert the marginal utilities into the agent = 1 col1 of ACTION     */
    /* 6. Run the genetic algorithm (add extension to interpet cost effects)  */
    /* 7. Put in place the new ACTION array from 6                            */
    /* 8. Adjust the COST array appropriately from the new manager actions    */
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP NEW_RESOURCES;
    PROTECT( NEW_RESOURCES = allocMatrix(REALSXP, res_number, trait_number) );
    protected_n++;
    
    data_ptr = REAL(NEW_RESOURCES);

    vec_pos = 0;
    for(res_trait = 0; res_trait < trait_number; res_trait++){
        for(resource = 0; resource < res_number; resource++){
            data_ptr[vec_pos] = resource_array[resource][res_trait];
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
    
    SEXP NEW_ACTIONS;
    PROTECT( NEW_ACTIONS = alloc3DArray(REALSXP, a_x, a_y, a_z) );
    protected_n++;
    
    new_action_ptr = REAL(NEW_ACTIONS);
    
    vec_pos = 0;
    for(layer=0; layer<a_z; layer++){
        for(col=0; col<a_y; col++){
            for(row=0; row<a_x; row++){
                new_action_ptr[vec_pos] = actions[row][col][layer];
                vec_pos++;
            }
        }
    }
    
    SEXP NEW_COSTS;
    PROTECT( NEW_COSTS = alloc3DArray(REALSXP, c_x, c_y, c_z) );
    protected_n++;
    
    new_cost_ptr = REAL(NEW_COSTS);

    vec_pos = 0;
    for(layer=0; layer<c_z; layer++){
        for(col=0; col<c_y; col++){
            for(row=0; row<c_x; row++){
                new_cost_ptr[vec_pos] = costs[row][col][layer];
                vec_pos++;
            }
        }
    }
    
    SEXP EVERYTHING;
    EVERYTHING = PROTECT( allocVector(VECSXP, 5) );
    protected_n++;
    SET_VECTOR_ELT(EVERYTHING, 0, NEW_RESOURCES);
    SET_VECTOR_ELT(EVERYTHING, 1, NEW_AGENTS);
    SET_VECTOR_ELT(EVERYTHING, 2, NEW_LANDSCAPE);
    SET_VECTOR_ELT(EVERYTHING, 3, NEW_ACTIONS);
    SET_VECTOR_ELT(EVERYTHING, 4, NEW_COSTS);
    
    UNPROTECT(protected_n);

    /* Free all of the allocated memory used in the observation array */
    for(row = 0; row < obs_d0; row++){
        free(obs_array[row]);
    }
    free(obs_array);
    /* Free all of the allocated memory used in the interaction table */
    for(row = 0; row < int_d0; row++){
        free(interact_table[row]);
    }
    free(interact_table);    
    /* Free all of the allocated memory used in the Jacobian matrix */
    for(row = 0; row < jacobian_dim; row++){
        free(Jacobian_mat[row]);
    }
    free(Jacobian_mat);
    /* Free all of the allocated memory used in agent array */
    for(agent = 0; agent < agent_number; agent++){
        free(agent_array[agent]);
    }
    free(agent_array);
    /* Free all of the allocated memory used in action array */
    for(row = 0; row < a_x; row++){
        for(col = 0; col < a_y; col++){
            free(actions[row][col]);   
        }
        free(actions[row]); 
    }
    free(actions);
    /* Free all of the allocated memory used in cost array */
    for(row = 0; row < c_x; row++){
        for(col = 0; col < c_y; col++){
            free(costs[row][col]);   
        }
        free(costs[row]); 
    }
    free(costs);
    /* Free all of the allocated memory used in land array */
    for(xloc = 0; xloc < land_x; xloc++){
        for(yloc = 0; yloc < land_y; yloc++){
            free(land[xloc][yloc]);   
        }
        free(land[xloc]);        
    }
    free(land);
    /* Free all of the allocated memory used in resource array */
    for(resource = 0; resource < res_number; resource++){
        free(resource_array[resource]);
    }
    free(resource_array);

    return(EVERYTHING); 
}
/* ===========================================================================*/
