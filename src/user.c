#include "game.h"

/* =============================================================================
 * This function checks a landscape layer to see if there is a particular number
 * on that layer
 * Inputs include:
 *     LANDSCAPE: The landscape array
 *     layer: The layer own which ownership is defined (should be 2)
 *     number: The number the function is looking for on the landscape (agentID)
 *     xdim: The length of the x dimension of the landscape
 *     ydim: The length of the y dimension of the landscape 
 * ========================================================================== */
int is_number_on_landscape(double ***landscape, int layer, int number, int xdim,
                           int ydim){
    int xval, yval;

    for(xval = 0; xval < xdim; xval++){
        for(yval = 0; yval < ydim; yval++){
            if(landscape[xval][yval][layer] == number){
                return 1;   
            }
        }
    }
    
    return 0;
}

/* =============================================================================
 * This function puts an agent somewhere (a random cell) on its own landscape,
 * but  only if the agent does in fact own some land
 * Inputs include:
 *     AGENTS: The array of agents
 *     LANDSCAPE: The landscape array
 *     xdim: The length of the x dimension of the landscape
 *     ydim: The length of the y dimension of the landscape
 *     agent_number: The total number of agents in the agent array
 *     layer: The layer on which ownership is defined (should be 2)
 * ========================================================================== */
void send_agents_home(double **agent_array, double ***landscape, int xdim,
                      int ydim, int agent_number, int layer){
    int agent;
    int owned;
    int agent_xloc;
    int agent_yloc;
    int agent_ID;
    int landowner;
    
    for(agent = 0; agent < agent_number; agent++){
        agent_ID  = agent_array[agent][0];
        owned     = is_number_on_landscape(landscape, 3, agent_ID, xdim, ydim);
        if(owned > 0){
            agent_xloc = agent_array[agent][4];
            agent_yloc = agent_array[agent][5];
            landowner  = landscape[agent_xloc][agent_yloc][layer];
            while(agent_ID != landowner){
                do{
                    agent_xloc = floor( runif(0, 1) * xdim ); 
                }while(agent_xloc == xdim);
                do{
                    agent_yloc = floor( runif(0, 1) * ydim ); 
                }while(agent_yloc == ydim);
                landowner = landscape[agent_xloc][agent_yloc][layer];
            }
            agent_array[agent][4] = agent_xloc;
            agent_array[agent][5] = agent_yloc;
        }
    }
}


/* =============================================================================
 * This function puts an agent somewhere (a random cell) on its own landscape,
 * but  only if the agent does in fact own some land
 * Inputs include:
 *     AGENTS: The array of agents
 *     LANDSCAPE: The landscape array
 *     xdim: The length of the x dimension of the landscape
 *     ydim: The length of the y dimension of the landscape
 *     agent_number: The total number of agents in the agent array
 *     yield_layer: The layer on which cell yield is defined (should be 1)
 *     own_layer: The layer on which cell ownership is defined (should be 2)
 *     yield_column: The colum of agent_array used for putting the yield
 * ========================================================================== */
void count_cell_yield(double **agent_array, double ***landscape, int xdim,
                      int ydim, int agent_number, int yield_layer,
                      int own_layer, int yield_column){

    int xpos, ypos;
    int agent;
    int agent_ID;
    double agent_yield;

    for(agent = 0; agent < agent_number; agent++){
        agent_ID    = agent_array[agent][0];
        agent_yield = 0.0; 
        for(xpos = 0; xpos < xdim; xpos++){
            for(ypos = 0; ypos < ydim; ypos++){
                if(landscape[xpos][ypos][own_layer] == agent_ID){
                    agent_yield += landscape[xpos][ypos][yield_layer];    
                }
            }
        }
        agent_array[agent][yield_column] = agent_yield; 
    }
}

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
SEXP user(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT, SEXP COST,
          SEXP ACTION){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* Index of x & y locations on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int zloc, land_z;        /* z locations */
    int c_x, c_y, c_z;       /* Dimensions of cost array */
    int a_x, a_y, a_z;       /* Dimensions of action array */
    int row, col, layer;     /* Indices for utility (COST & ACTION) arrays */
    int resource;            /* Index for resource (rows of RESOURCE) */
    int res_trait;           /* Index for resource traits (cols of RESOURCE) */
    int agent;               /* Index for agent in the array (rows) */
    int agent_trait;         /* Index for agent traits (cols of agent_array) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included in the resource */
    int agent_number;        /* Number of agents that can potentially observe */
    int agent_traits;        /* Number of traits that each agent has */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    int *dim_COST;           /* Dimensions of the COST array incoming */
    int *dim_ACTION;         /* Dimensions of the ACTION array incoming */
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
    double **resource_array; /* Array to store the old RESOURCE in C */
    double ***land;          /* Array to store the landscape in C*/
    double **agent_array;    /* Array to store the agents in C */
    double ***costs;         /* Array of the costs of user actions */
    double ***actions;       /* Array of user actions */

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
    
    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras = REAL(PARAMETERS);
    
    dim_RESOURCE   = INTEGER( GET_DIM(RESOURCE)  );
    dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );
    dim_AGENT      = INTEGER( GET_DIM(AGENT) );
    dim_COST       = INTEGER( GET_DIM(COST) );
    dim_ACTION     = INTEGER( GET_DIM(ACTION) );

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

    /* Do the biology here now */
    /* ====================================================================== */

    send_agents_home(agent_array, land, land_x, land_y, agent_number, 2);
        
    count_cell_yield(agent_array, land, land_x, land_y, agent_number, 1, 2, 15);

    /*------------------------------------------------------------------------*/
    /* Temporary call to genetic algorithm below for ONE agent -- testing */
    /*------------------------------------------------------------------------*/
    ga(actions, costs, agent_array, resource_array);
    /*------------------------------------------------------------------------*/
    
    
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
