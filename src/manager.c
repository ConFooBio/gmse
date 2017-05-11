#include "game.h"

/* =============================================================================
 * This function calculates density-based abundance estimates
 *     obs_array:  The observation array
 *     obs_rows:   Number of rows in the observation array obs_array
 *     obs_cols:   Number of cols in the observation array obs_array
 *     type1:      Resources of type 1 being observed
 *     type2:      Resources of type 2 being observed
 *     type3:      Resources of type 3 being observed
 * ========================================================================== */
int res_obs(double **obs_array, int obs_rows, int obs_cols, int type1, 
            int type2, int type3){

    int i, j, obs_count;
    
    obs_count = 0;
 
    for(i = 0; i < obs_rows; i++){
        if( (obs_array[i][1] == type1 || obs_array[i][1] < 0) &&
            (obs_array[i][2] == type2 || obs_array[i][2] < 0) &&
            (obs_array[i][3] == type3 || obs_array[i][3] < 0)
        ){
            for(j = 15; j < obs_cols; j++){
                obs_count += obs_array[i][j];
            }
        }
    }
    return obs_count;
}

/* =============================================================================
 * This function calculates density-based abundance estimates
 *     obs_array:      The observation array
 *     para:           A vector of parameters needed to handle the obs_array
 *     agent_array:    Agent array, including managers (agent type 0)
 *     agents:         Total number of agents (rows) in the agents array
 *     obs_array_rows: Number of rows in the observation array obs_array
 *     obs_array_cols: Number of cols in the observation array obs_array
 *     abun_est:       Vector where abundance estimates for each type are placed
 *     interact_table: Lookup table to get all types of resource values
 *     int_table_rows: The number of rows in the interact_table
 * ========================================================================== */
void dens_est(double **obs_array, double *para, double **agent_array, 
              int agents, int obs_array_rows, int obs_array_cols, 
              double *abun_est, int **interact_table, int int_table_rows){
 
    int i, j, resource;
    int view, a_type, land_x, land_y, type1, type2, type3;
    int vision, area, cells, times_obs, tot_obs;
    double prop_obs, estimate;

    a_type    = (int) para[7];  /* What type of agent does the observing  */
    times_obs = (int) para[11];
    land_x    = (int) para[12];
    land_y    = (int) para[13];
    
    view = 0;
    for(i = 0; i < agents; i++){
        if(agent_array[i][1] == a_type){
            view += agent_array[i][8];
        }
    }

    vision  = (2 * view) + 1;
    area    = vision * vision * times_obs;
    cells   = land_x * land_y; /* Plus one needed for zero index */
    tot_obs = 0;
    
    for(resource = 0; resource < int_table_rows; resource++){
        abun_est[resource] = 0;
        if(interact_table[resource][0] == 0){ /* Change when turn off type? */
            type1   = interact_table[resource][1];
            type2   = interact_table[resource][2];
            type3   = interact_table[resource][3];
            tot_obs = res_obs(obs_array, obs_array_rows, obs_array_cols, type1, 
                              type2, type3);
            prop_obs = (double) tot_obs / area;
            estimate = prop_obs * cells;
            
            abun_est[resource] = estimate;
        }
    }
}


/* =============================================================================
 * This function calculates RMR (chapman) for one resource type
 *     obs_array:      The observation array
 *     para:           A vector of parameters needed to handle the obs_array
 *     obs_array_rows: Number of rows in the observation array obs_array
 *     obs_array_cols: Number of cols in the observation array obs_array
 *     trait_number:   The number of traits in the resource array
 *     type1:          Resource type 1
 *     type2:          Resource type 2
 *     type3:          Resource type 3
 * ========================================================================== */
double chapman_est(double **obs_array, double *para, int obs_array_rows, 
                   int obs_array_cols, int trait_number, int type1, int type2,
                   int type3){
    
    int row, col;
    int total_marks, recaptures, mark_start, recapture_start;
    int *marked, sum_marked, n, K, k;
    double estimate, floored_est;
    
    total_marks     = (int) para[11];
    recaptures      = (int) para[10];
    mark_start      = trait_number + 1;
    recapture_start = mark_start + (total_marks - recaptures);
    
    if(total_marks < 2 || recaptures < 1){
        printf("ERROR: Not enough marks or recaptures for management");
        return 0;
    }
    
    n      = 0;
    marked = malloc(obs_array_rows * sizeof(int));
    for(row = 0; row < obs_array_rows; row++){
        marked[row] = 0;
        if(obs_array[row][1] == type1 && 
           obs_array[row][2] == type2 &&
           obs_array[row][3] == type3
        ){
            for(col = mark_start; col < recapture_start; col++){
                if(obs_array[row][col] > 0){
                    marked[row] = 1;
                    n++;
                    break;
                }
            }
        }
    }
    
    K = 0;
    k = 0;
    for(row = 0; row < obs_array_rows; row++){
        if(obs_array[row][1] == type1 && 
           obs_array[row][2] == type2 &&
           obs_array[row][3] == type3
        ){
            for(col = recapture_start; col < obs_array_cols; col++){
                if(obs_array[row][col] > 0){
                    K++;
                    if(marked[row] > 0){
                        k++;
                    }
                    break;
                }
            }
        }
    }
    
    estimate    = ((n + 1) * (K + 1) / (k + 1)) - 1;
    floored_est = floor(estimate);

    free(marked);  
    
    return floored_est;
}


/* =============================================================================
 * This function calculates mark-recapture-based (Chapman) abundance estimates
 *     obs_array:      The observation array
 *     para:           A vector of parameters needed to handle the obs_array
 *     obs_array_rows: Number of rows in the observation array obs_array
 *     obs_array_cols: Number of cols in the observation array obs_array
 *     abun_est:       Vector where abundance estimates for each type are placed
 *     interact_table: Lookup table to get all types of resource values
 *     int_table_rows: The number of rows in the interact_table
 *     trait_number:   The number of traits in the resouce array
 * ========================================================================== */
void rmr_est(double **obs_array, double *para, int obs_array_rows, 
             int obs_array_cols, double *abun_est, int **interact_table, 
             int int_table_rows, int trait_number){
    
    int resource, type1, type2, type3;
    double estimate;
    
    for(resource = 0; resource < int_table_rows; resource++){
        abun_est[resource] = 0;
        if(interact_table[resource][0] == 0){ /* Change when turn off type? */
            type1    = interact_table[resource][1];
            type2    = interact_table[resource][2];
            type3    = interact_table[resource][3];
            estimate = chapman_est(obs_array, para, obs_array_rows, 
                                   obs_array_cols, trait_number, type1, type2,
                                   type3);
            abun_est[resource] = estimate;
        }
    }
}



/* =============================================================================
 * This function calculates mark-recapture-based (Chapman) abundance estimates
 *     obs_array:      The observation array
 *     para:           A vector of parameters needed to handle the obs_array
 *     obs_array_rows: Number of rows in the observation array obs_array
 *     abun_est:       Vector where abundance estimates for each type are placed
 *     interact_table: Lookup table to get all types of resource values
 *     int_table_rows: The number of rows in the interact_table
 * ========================================================================== */
void transect_est(double **obs_array, double *para, int obs_array_rows, 
                  double *abun_est, int **interact_table, int int_table_rows){
    
    int resource, observation, type1, type2, type3;
    
    for(resource = 0; resource < int_table_rows; resource++){
        abun_est[resource] = 0;
        if(interact_table[resource][0] == 0){ /* Change when turn off type? */
            type1    = interact_table[resource][1];
            type2    = interact_table[resource][2];
            type3    = interact_table[resource][3];
            for(observation = 0; observation < obs_array_rows; observation++){
                if(obs_array[observation][1] == type1 && 
                   obs_array[observation][2] == type2 && 
                   obs_array[observation][3] == type3
                ){
                    abun_est[resource] += obs_array[observation][12];
                }
                    
            }
        }
    }
}

/* =============================================================================
 * This function uses the observation array to estimate resource abundances
 *     obs_array:      The observation array
 *     para:           A vector of parameters needed to handle the obs_array
 *     interact_table: Lookup table to get all types of resource values
 *     agent_array:    Agent array, including managers (agent type 0)
 *     agents:         Total number of agents (rows) in the agents array
 *     obs_x:          Number of rows in the observation array
 *     obs_y:          Number of cols in the observation array
 *     abun_est:       Vector where abundance estimates for each type are placed
 *     int_table_rows: The number of rows in the interact_table
 * ========================================================================== */
void estimate_abundances(double **obs_array, double *para, int **interact_table,
                         double **agent_array, int agents, int obs_x, int obs_y,
                         double *abun_est, int int_table_rows, int trait_num){
    
    int estimate_type;
    double abun;
    
    estimate_type = (int) para[8];

    switch(estimate_type){
        case 0:
            dens_est(obs_array, para, agent_array, agents, obs_x, obs_y, 
                     abun_est, interact_table, int_table_rows);
            break;
        case 1:
            rmr_est(obs_array, para, obs_x, obs_y, abun_est, interact_table, 
                    int_table_rows, trait_num);
            break;
        case 2:
            transect_est(obs_array, para, obs_x, abun_est, interact_table, 
                         int_table_rows);
            break;
        case 3:
            transect_est(obs_array, para, obs_x, abun_est, interact_table, 
                         int_table_rows);
            break;
        default:
            break;
    }
}


/* =============================================================================
 * This function uses the observation array to estimate resource abundances
 *      COST:        An array of the cost of actions for each agent
 *      ACTION:      An array of the action of agents
 *      manID:       The ID of the managing agent (should usually be 1)
 *      mRow:        The layer of the action column of manager (usually 0) 
 * ========================================================================== */
void set_action_costs(double ***ACTION, double ***COST, int manID, int mlayer,
                      int interest_num, int total_layers, double **agent_array){

    int cost_row, manager_row, type1, type2, type3, layer;
    
    for(cost_row = 0; cost_row < interest_num; cost_row++){
        manager_row              = 0;
        type1                    = ACTION[cost_row][1][mlayer];
        type2                    = ACTION[cost_row][2][mlayer];
        type3                    = ACTION[cost_row][3][mlayer];
        while(ACTION[manager_row][0][mlayer] != manID   ||
              ACTION[manager_row][1][mlayer] != type1   ||
              ACTION[manager_row][2][mlayer] != type2   ||
              ACTION[manager_row][3][mlayer] != type3
        ){
            manager_row++;
        }
        for(layer = 0; layer < total_layers; layer++){
            if(agent_array[layer][1] > 0){ /* Managers can't affect self */
                COST[cost_row][7][layer]  = ACTION[manager_row][7][mlayer];
                COST[cost_row][8][layer]  = ACTION[manager_row][8][mlayer];
                COST[cost_row][9][layer]  = ACTION[manager_row][9][mlayer];
                COST[cost_row][10][layer] = ACTION[manager_row][10][mlayer];
                COST[cost_row][11][layer] = ACTION[manager_row][11][mlayer];
                COST[cost_row][12][layer] = ACTION[manager_row][12][mlayer];
            }
        }
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
    int xloc, yloc, i;       /* Index of x & y locations on the landscape */ 
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
    double *abun_est;        /* Vector used to estimate abundances */
    double *temp_util;       /* Vector temporarily holding manager utils */
    double *marg_util;       /* Margin utilities for a manager's actions */

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

    abun_est  = malloc(int_d0 * sizeof(double));
    temp_util = malloc(int_d0 * sizeof(double));
    marg_util = malloc(int_d0 * sizeof(double));
    
    estimate_abundances(obs_array, paras, interact_table, agent_array,
                        agent_number, obs_d0, obs_d1, abun_est, int_d0,
                        trait_number);
    
    for(row = 0; row < int_d0; row++){
        temp_util[row] = 0;
        marg_util[row] = 0;
        if(actions[row][0][0] < 0){
            temp_util[row] = actions[row][4][0];
            marg_util[row] = temp_util[row] - abun_est[row];
        }
    }

    i = 0;
    for(row = 0; row < a_x; row++){
        if(actions[row][0][0] == 1){
            actions[row][4][0] = marg_util[i];
            i++;
        }
    }

    ga(actions, costs, agent_array, resource_array, land, Jacobian_mat, 
       interact_table, paras, c_x, c_y, res_number, land_x, land_y, land_z, 
       trait_number, 1, 0, 1, a_x, a_y, a_z);
    
    set_action_costs(actions, costs, 1, 0, jacobian_dim - 1, a_z, agent_array);
    
    free(marg_util);
    free(temp_util);
    free(abun_est);
    
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
