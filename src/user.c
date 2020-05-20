#include "game.h"

/* =============================================================================
 * This function puts an agent somewhere (a random cell) on its own landscape,
 * but  only if the agent does in fact own some land
 * Inputs include:
 *     agent_array: The array of agents
 *     land: The landscape array
 *     paras: Vector of global parameters used in the model
 * ========================================================================== */
void send_agents_home(double **agent_array, double ***land, double *paras){

    int land_x, land_y, agent_number, ownership, agent, owned, landowner;
    int agent_xloc, agent_yloc, agent_ID, xval, yval, land_num;

    land_x       = (int) paras[12];
    land_y       = (int) paras[13];
    agent_number = (int) paras[54];
    ownership    = (int) paras[81];
    
    for(agent = 0; agent < agent_number; agent++){
        agent_ID  = (int) agent_array[agent][0];
        owned     = 0;
        for(xval = 0; xval < land_x; xval++){
            for(yval = 0; yval < land_y; yval++){
                land_num = (int) land[xval][yval][ownership];
                if(land_num == agent_ID){
                    owned++;   
                }
            }
        }  
        if(owned > 0){
            agent_xloc = agent_array[agent][4];
            agent_yloc = agent_array[agent][5];            
            if(agent_xloc < 0 || agent_xloc >= land_x){
                agent_xloc = 0;
            }
            if(agent_yloc < 0 || agent_yloc >= land_y){
                agent_yloc = 0;
            }
            landowner  = (int) land[agent_xloc][agent_yloc][ownership];
            while(agent_ID != landowner){
                agent_xloc = get_rand_int(0, land_x);
                agent_yloc = get_rand_int(0, land_y);
                landowner = (int) land[agent_xloc][agent_yloc][ownership];
            }
            agent_array[agent][4] = (double) agent_xloc;
            agent_array[agent][5] = (double) agent_yloc;
        }
    }
}

/* =============================================================================
 *  This function clones action arrays so actions can be decremented temporarily
 *  Inputs include:
 *      action_array: The array of actions to be cloned
 *      clone: The array of same dimensions to put action_array values in
 *      paras: Vector of global parameters used in the model
 * ========================================================================== */
void clone_action_array(double ***action_array, double ***clone, double *paras){
    
    int layers, ROWS, COLS, layer, row, col, total_actions, start_count;
    
    layers         = (int) paras[65];
    ROWS           = (int) paras[68];
    COLS           = (int) paras[69];
    start_count    = (int) paras[71];  
    
    total_actions  = 0; 

    for(layer = 0; layer < layers; layer++){
        for(row = 0; row < ROWS; row++){
            for(col = 0; col < COLS; col++){
                clone[row][col][layer] = action_array[row][col][layer];
                if(col >= start_count){
                    total_actions += action_array[row][col][layer];
                }
            }
        }
    }
    paras[72] = (double) total_actions;
}

/* =============================================================================
 *  This function checks to see if an agent owns any land
 *  Inputs include:
 *      land: The landscape a
 *      paras: Vector of global parameters used in the model
 *      agentID: The ID number of the agent being checked
 * ========================================================================== */
int check_owns_land(double ***land, double *paras, int agentID){
    
    int xpos, ypos, land_x, land_y;
    
    land_x = (int) paras[12];
    land_y = (int) paras[13];
    
    for(xpos = 0; xpos < land_x; xpos++){
        for(ypos = 0; ypos < land_y; ypos++){
            if(land[xpos][ypos][2] == agentID){
                return 1;
            }
        }
    }
    return 0;
}

/* =============================================================================
 *  This function causes an agent to perform actions on a landscape
 *  Inputs include:
 *      land: The landscape array
 *      paras: Vector of global parameters used in the model
 *      action_array: An array of the action of agents
 * ========================================================================== */
void act_on_landscape(double ***land, double *paras, double ***action_array){
    
    int land_x, land_y, xpos, ypos, need_to_own, does_own_land, agentID;
    int action_row, action_col, agent;
    double feedin;
    
    land_x     = (int) paras[12];
    land_y     = (int) paras[13];
    feedin     = paras[79];
    action_row = (int) paras[83];
    action_col = (int) paras[84];
    agent      = (int) paras[85];

    agentID = agent + 1;
    
    does_own_land = check_owns_land(land, paras, agentID);
    need_to_own   = (int) action_array[action_row][5][agent];
    
    if(does_own_land == 1  || need_to_own == 0){
        do{
            xpos = get_rand_int(0, land_x);
            ypos = get_rand_int(0, land_y);
        }while(need_to_own == 1 && land[xpos][ypos][2] != agentID);
        switch(action_col){
        case 7:
            break;
        case 8: /* Kill crop yield */ 
            land[xpos][ypos][1] = 0;
            break;
        case 9:  /* Tend the crop to increase yield */ 
            land[xpos][ypos][1] += (land[xpos][ypos][1] * feedin);
            break;
        case 10: 
            break;
        case 11: 
            break;
        default:
            break;
        }
    }
}

/* =============================================================================
 *  This function causes an agent to perform actions on a landscape
 *  Inputs include:
 *      resource_array: The array of resources in the model
 *      land: The landscape array
 *      paras: Vector of global parameters used in the model
 *      action_array: An array of the action of agents
 * ========================================================================== */
int find_a_resource(double **resource_array, double ***land, double *paras,
                    double ***action_array){

    int the_resource, res_t1, res_t2, res_t3, type1, type2, type3, u_loc;
    int resource_number, xloc, yloc, res_x, res_y, resource;
    int agentID, available, action_row, action_col, agent, *can_act;

    resource_number = (int) paras[32];
    res_x           = (int) paras[33];
    res_y           = (int) paras[34];
    res_t1          = (int) paras[56];
    res_t2          = (int) paras[57];
    res_t3          = (int) paras[58];
    action_row      = (int) paras[83];
    action_col      = (int) paras[84];
    agent           = (int) paras[85];
    
    type1 = (int) action_array[action_row][res_t1][agent];
    type2 = (int) action_array[action_row][res_t2][agent];
    type3 = (int) action_array[action_row][res_t3][agent];
    u_loc = (int) action_array[action_row][5][agent];
    
    can_act = malloc(resource_number * sizeof(int));
    
    agentID   = agent + 1;
    available = 0;
    
    for(resource = 0; resource < resource_number; resource++){
        xloc              = resource_array[resource][res_x];
        yloc              = resource_array[resource][res_y];
        can_act[resource] = 1; 
        if(u_loc == 1 && land[xloc][yloc][2] != agentID){
            can_act[resource] = 0;
        }
        if(resource_array[resource][res_t1] != type1){
            can_act[resource] = 0;
        }
        if(resource_array[resource][res_t2] != type2){
            can_act[resource] = 0;
        }
        if(resource_array[resource][res_t3] != type3){
            can_act[resource] = 0;
        }
        if(resource_array[resource][17] >= 1 && action_col != 9){ 
            can_act[resource] = 0;
        }
        if(resource_array[resource][16] >= 1){
            can_act[resource] = 0;
        } 
        available += can_act[resource];
    }

    if(available > 0){
        do{
            the_resource = get_rand_int(0, resource_number);
        }while(can_act[the_resource] == 0);
    }else{
        the_resource = -1;   
    }
    
    free(can_act);

    return the_resource;
}

/* =============================================================================
 *  This function causes an agent to perform actions on resources
 *  Inputs include:
 *      resource_array: The array of resources in the model
 *      paras: Vector of global parameters used in the model
 *      land: The landscape array
 *      action_array: An array of the action of agents
 * ========================================================================== */
void act_on_resource(double **resource_array, double *paras, double ***land,
                     double ***action_array){
    
    int samp, xloc, yloc, land_x, land_y, action_col, action_layer;
    
    samp = find_a_resource(resource_array, land, paras, action_array);
    
    if(samp < 0){
        return;
    }
    
    land_x       = (int) paras[12];
    land_y       = (int) paras[13];
    action_col   = (int) paras[84];
    action_layer = (int) paras[85];

    switch(action_col){
        case 7: /* Move resource */
            xloc                    = get_rand_int(0, land_x);
            yloc                    = get_rand_int(0, land_y);
            resource_array[samp][4] = xloc;
            resource_array[samp][5] = yloc;
            resource_array[samp][15]++;
            break;
        case 8: /* Kill resource */
            resource_array[samp][16] = (double) action_layer + 1;
            if(action_layer < 1){ /* Should not happen -- manager is culling */
                resource_array[samp][16]++;
            }
            break;
        case 9: /* Castrate resource */
            resource_array[samp][17]++;
            break;
        case 10: /* Feed resource (increase birth-rate)*/
            resource_array[samp][18]++;
            break;
        case 11: /* Help resource (increase offspring number directly) */
            resource_array[samp][19]++;
            break;            
        default:
            break;
    }
}

/* =============================================================================
 *  This function causes an agent to perform actions on resources
 *  Inputs include:
 *      action_array: An array of the action of agents
 *      resource_array: The array of resources in the model
 *      paras: Vector of global parameters used in the model
 *      land: The landscape array
 * ========================================================================== */
void do_acts(double ***action_array, double **resource_array, double *paras,
             double ***land){
    
    int layers, ROWS, COLS, start_col, row, col;
    int total_actions, rand_row, rand_col, rand_layer, act_type;
    double ***action_clone;

    layers          = (int) paras[65];
    ROWS            = (int) paras[68];
    COLS            = (int) paras[69];
    start_col       = (int) paras[71];
    
    action_clone = malloc(ROWS * sizeof(double *));
    for(row = 0; row < ROWS; row++){
        action_clone[row] = malloc(COLS * sizeof(double *));
        for(col = 0; col < COLS; col++){
            action_clone[row][col] = malloc(layers * sizeof(double));
        }
    }
    
    clone_action_array(action_array, action_clone, paras);
    
    total_actions = (int) paras[72];

    while(total_actions > 0){
        do{
            rand_layer = get_rand_int(0, layers);
            rand_row   = get_rand_int(0, ROWS);
            rand_col   = get_rand_int(start_col, COLS);
        }while(action_clone[rand_row][rand_col][rand_layer] <= 0);
    
        act_type = (int) action_clone[rand_row][0][rand_layer];
        action_clone[rand_row][rand_col][rand_layer]--;
    
        paras[83] = (double) rand_row;
        paras[84] = (double) rand_col;
        paras[85] = (double) rand_layer;
        
        switch(act_type){
            case -2:
                act_on_resource(resource_array, paras, land, action_clone);
                break;
            case -1:
                act_on_landscape(land, paras, action_clone);
            default:
                break;
        }
        total_actions--;
    }

    for(row = 0; row < ROWS; row++){
        for(col = 0; col < COLS; col++){
            free(action_clone[row][col]);   
        }
        free(action_clone[row]); 
    }
    free(action_clone);
}

/* =============================================================================
 *  This copies actions from one agent to another
 *  Inputs include:
 *      action_array: An array of the action of agents
 *      from: The layer of the action array to be copied
 *      to: The layer of the action array to copy to
 *      paras: Vector of global parameters used in the model
 * ========================================================================== */
void copycat(double ***action_array, int from, int to, double *paras){
    
    int ROWS, COLS, start_col, row, col;
    
    ROWS            = (int) paras[68];
    COLS            = (int) paras[69];
    start_col       = (int) paras[71];
    
    for(col = start_col; col < COLS; col++){
        for(row = 0; row < ROWS; row++){
            action_array[row][col][to] = action_array[row][col][from];
        }
    }
}

/* =============================================================================
 * This adds more budget to a column in agent_array based on agent yield
 * Inputs include:
 *     action_array: An array of the action of agents
 *     paras: Vector of global parameters used in the model
 * ========================================================================== */
void yield_to_budget(double **agent_array, double *paras){   
    
    int agent, N_agents, agent_type, user_budget, y_bonus_col, yield_col;
    double yield_budget, yield_incr;
    
    N_agents     = (int) paras[54];
    yield_col    = (int) paras[82];
    user_budget  = (double) paras[97];
    yield_budget = (double) paras[125]; 
    y_bonus_col  = (int) paras[128];

    for(agent = 0; agent < N_agents; agent++){
        agent_type = agent_array[agent][1];   
        if(agent_type == 1) {
            yield_incr = agent_array[agent][yield_col] * yield_budget;
            agent_array[agent][y_bonus_col] = floor(yield_incr);
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
 *      RESOURCE:   An array of *row resources and *col traits for each resource
 *      LANDSCAPE:  An array of *row by *col size that makes up the landscape
 *      PARAMETERS: Parameters read into the function for population processes
 *      AGENT:      An array of *row agents and *col traits for each agent
 *      COST:       An array of the cost of actions for each agent
 *      ACTION:     An array of the action of agents
 *      JACOBIAN:   A Jacobian matrix of resource type and landscape effects
 *      INTERACT:   A table indexing types with rows of interaction array
 * ===========================================================================*/
SEXP user(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT, SEXP COST,
          SEXP ACTION, SEXP JACOBIAN, SEXP INTERACT){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* Index of x & y locations on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int zloc, land_z;        /* z locations */
    int c_x, c_y, c_z;       /* Dimensions of cost array */
    int a_x, a_y, a_z;       /* Dimensions of action array */
    int int_d0, int_d1;      /* Dimensions of the interaction lookup table */
    int row, col, layer;     /* Indices for utility (COST & ACTION) arrays */
    int resource;            /* Index for resource (rows of RESOURCE) */
    int res_trait;           /* Index for resource traits (cols of RESOURCE) */
    int agent;               /* Index for agent in the array (rows) */
    int agent_trait;         /* Index for agent traits (cols of agent_array) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included in the resource */
    int agent_number;        /* Number of agents that can potentially observe */
    int agent_traits;        /* Number of traits that each agent has */
    int jacobian_dim;        /* Dimensions of the (square) Jacobian matrix */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int group_think;         /* Should one user do the thinking for all */
    double yield_budget;     
    int len_PARAMETERS;      /* Length of the parameters vector */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    int *dim_COST;           /* Dimensions of the COST array incoming */
    int *dim_ACTION;         /* Dimensions of the ACTION array incoming */
    int *dim_JACOBIAN;       /* Dimensions of the JACOBIAN matrix incoming */
    int *dim_INTERACT;       /* Dimensions of the INTERACT matrix incoming */
    int **lookup;            /* Lookup table for resource & land interactions */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras_ptr;       /* Pointer to PARAMETERS (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *paras_ptr_new;   /* Pointer to new paras (interface R and C) */
    double *agent_ptr;       /* Pointer to AGENT (interface R and C) */
    double *cost_ptr;        /* Pointer to COST (interface R and C) */
    double *action_ptr;      /* Pointer to ACTION (interface R and C) */
    double *new_agent_ptr;   /* Pointer to new agents that are returned */
    double *new_action_ptr;  /* Pointer to new action array that is returned */
    double *new_cost_ptr;    /* Pointer to new cost array that is returned */
    double *data_ptr;        /* Pointer to DATA (interface R and C) */
    double *land_ptr_new;    /* Pointer to a new landscape */
    double *jaco_ptr;        /* Pointer to JACOBIAN (interface R and C) */
    double *intr_ptr;           /* Pointer to INTERACT (interface R and C) */
    double **resource_array; /* Array to store the old RESOURCE in C */
    double **Jacobian_mat;   /* Array to store the Jacobian matrix in C */
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
    
    PROTECT( JACOBIAN = AS_NUMERIC(JACOBIAN) );
    protected_n++;
    jaco_ptr = REAL(JACOBIAN);
    
    PROTECT( INTERACT = AS_NUMERIC(INTERACT) );
    protected_n++;
    intr_ptr = REAL(INTERACT);
    
    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras_ptr = REAL(PARAMETERS);
    
    len_PARAMETERS = GET_LENGTH(PARAMETERS);
    dim_RESOURCE   = INTEGER( GET_DIM(RESOURCE)  );
    dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );
    dim_AGENT      = INTEGER( GET_DIM(AGENT) );
    dim_COST       = INTEGER( GET_DIM(COST) );
    dim_ACTION     = INTEGER( GET_DIM(ACTION) );
    dim_JACOBIAN   = INTEGER( GET_DIM(JACOBIAN) );
    dim_INTERACT   = INTEGER( GET_DIM(INTERACT) );

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
    lookup  = malloc(int_d0 * sizeof(int *));
    for(row = 0; row < int_d0; row++){
        lookup[row] = malloc(int_d1 * sizeof(int));
    }
    vec_pos = 0;
    for(col = 0; col < int_d1; col++){
        for(row = 0; row < int_d0; row++){
            lookup[row][col] = intr_ptr[vec_pos];
            vec_pos++;
        }
    }
    
    /* Code below copies the paras vector into C */
    paras   = malloc(len_PARAMETERS * sizeof(double *));
    vec_pos   = 0;
    for(xloc = 0; xloc < len_PARAMETERS; xloc++){
        paras[xloc] = paras_ptr[vec_pos];
        vec_pos++;
    } /* The parameters vector is now copied into C */
    
    /* Do the biology here now */
    /* ====================================================================== */
    
    group_think  = (int) paras[101];
    yield_budget = (double) paras[125];
    
    send_agents_home(agent_array, land, paras);
    
    count_owned_cells(land, paras, agent_array, land_x, land_y, agent_number);
    
    count_cell_yield(agent_array, land, paras);
    
    if(yield_budget > 0) {
        yield_to_budget(agent_array, paras);
    }
    
    if(group_think > 0 && agent_number > 2){
        ga(actions, costs, agent_array, resource_array, land, Jacobian_mat,
           lookup, paras, 1, 0);
        for(agent = 2; agent < agent_number; agent++){
            copycat(actions, 1, agent, paras);
        }
    }else{
        for(agent = 1; agent < agent_number; agent++){ 
            ga(actions, costs, agent_array, resource_array, land, Jacobian_mat,
               lookup, paras, agent, 0);
        }
    }
    
    do_acts(actions, resource_array, paras, land);

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
    
    SEXP PARAMETERS_NEW;
    PROTECT( PARAMETERS_NEW = allocVector(REALSXP, len_PARAMETERS) );
    protected_n++;
    
    paras_ptr_new = REAL(PARAMETERS_NEW);
    
    vec_pos = 0;
    for(xloc = 0; xloc < len_PARAMETERS; xloc++){
        paras_ptr_new[vec_pos] = paras[xloc];
        vec_pos++;
    }    
    
    SEXP EVERYTHING;
    EVERYTHING = PROTECT( allocVector(VECSXP, 6) );
    protected_n++;
    SET_VECTOR_ELT(EVERYTHING, 0, NEW_RESOURCES);
    SET_VECTOR_ELT(EVERYTHING, 1, NEW_AGENTS);
    SET_VECTOR_ELT(EVERYTHING, 2, NEW_LANDSCAPE);
    SET_VECTOR_ELT(EVERYTHING, 3, NEW_ACTIONS);
    SET_VECTOR_ELT(EVERYTHING, 4, NEW_COSTS);
    SET_VECTOR_ELT(EVERYTHING, 5, PARAMETERS_NEW);
    
    UNPROTECT(protected_n);

    free(paras);
    /* Free all of the allocated memory used in the interaction table */
    for(row = 0; row < int_d0; row++){
        free(lookup[row]);
    }
    free(lookup);    
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
