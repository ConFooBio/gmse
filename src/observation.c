#include "utilities.h"

/* =============================================================================
 * This function moves one on the landscape according to some rules
 * For now, it is repeated in both c files, but if there are more functions
 * that serve two purposes, then a general utilily c file might be created
 * agent_moving: Agent array
 * land: The landscape array
 * paras: Vector of parameters needed
 * a_row: The row of the agent of interest
 * ========================================================================== */
void a_mover(double **agent_moving, double ***land, double *paras, int a_row){

    int xloc, yloc, move_para, edge, land_x, land_y, type;
    int move_len;     /* Length of a move                            */
    int move_dir;     /* Move direction (-1 or 1)                    */
    int new_pos;      /* New position: check if over landscape edge  */
    double rand_num;  /* Random number used for sampling             */
    double rand_uni;  /* Random uniform number                       */
    double rand_pois; /* Random poisson number                       */
    double raw_move;  /* Movement length before floor() truncation   */

    edge      = (int) paras[1];  /* Defines what happens at landscape edge */
    land_x    = (int) paras[12]; /* Max x dimension of the landscape */
    land_y    = (int) paras[13]; /* Max y dimension of the landscape */
    type      = (int) paras[14]; /* Defines the type of movement allowed */
    xloc      = (int) paras[49]; /* Column indicating the agents' x positions */
    yloc      = (int) paras[50]; /* Column indicating the agents' y positions */
    move_para = (int) paras[51]; /* Column for distance an agent can move */

    /* Move first in the xloc direction --------------------------------- */
    new_pos  = agent_moving[a_row][xloc];
    rand_num = 0.5;
    do{ /* Note that rand_num can never be exactly 0.5 */
        rand_num = runif(0, 1);
    } while(rand_num == 0.5);
    if(rand_num > 0.5){
        move_dir = 1;   
    }else{
        move_dir = -1;   
    } /* Now we have the direction the resource is moving */
    switch(type){
        case 0: /* No change in position */
            move_len = 0;
            break;
        case 1: /* Uniform selection of position change */
            do{ /* Again, so that agent never moves too far */
                rand_uni = runif(0, 1);
            } while(rand_uni == 1.0);
            raw_move = rand_uni * (agent_moving[a_row][move_para] + 1);
            move_len = (int) floor(raw_move);
            break;
        case 2: /* Poisson selection of position change */
            rand_pois = rpois(agent_moving[a_row][move_para]);    
            raw_move  = rand_pois * (agent_moving[a_row][move_para] + 1);
            move_len  = (int) floor(raw_move);
            break;
        case 3: /* Uniform position movement a Poisson number of times */
            rand_pois = rpois(agent_moving[a_row][move_para]);
            raw_move  = 0;
            while(rand_pois > 0){
                do{
                    rand_uni = runif(0, 1);
                } while(rand_uni == 1.0);
                raw_move += rand_uni * (agent_moving[a_row][move_para] + 1);
                rand_pois--;
            }
            move_len = (int) floor(raw_move);
            break;            
        default:
            break;
    }
    new_pos  = agent_moving[a_row][xloc] + (move_dir * move_len); 
    if(new_pos >= land_x || new_pos < 0){ /* If off the edge */
        switch(edge){
            case 0: /* Nothing happens (effectively, no edge) */
                break;
            case 1: /* Corresponds to a torus landscape */
                if(new_pos >= land_x){
                    new_pos = new_pos - land_x;   
                }
                if(new_pos < 0){
                    new_pos = new_pos + land_x;   
                }
                break;
            default:
                break;
        }
    }
    agent_moving[a_row][xloc] = new_pos;
    /* Move next in the yloc direction ---------------------------------- */
    new_pos  = agent_moving[a_row][yloc];
    rand_num = 0.5;
    do{ /* Note that rand_num can never be exactly 0.5 */
        rand_num = runif(0, 1);
    } while(rand_num == 0.5);
    if(rand_num > 0.5){
        move_dir = 1;   
    }else{
        move_dir = -1;   
    } /* Now we have the direction the resource is moving */
    switch(type){
        case 0: /* No change in position */
            move_len = 0;
            break;
        case 1: /* Uniform selection of position change */
            do{ /* Again, so that a_num never moves too far */
                rand_uni = runif(0, 1);
            } while(rand_uni == 1.0);
            raw_move = rand_uni * (agent_moving[a_row][move_para] + 1);
            move_len = (int) floor(raw_move);
            break;
        case 2: /* Poisson selection of position change */
            rand_pois = rpois(agent_moving[a_row][move_para]);    
            raw_move  = rand_pois * (agent_moving[a_row][move_para] + 1);
            move_len  = (int) floor(raw_move);
            break;
        case 3: /* Uniform position movement a Poisson number of times */
            rand_pois = rpois(agent_moving[a_row][move_para]);
            raw_move  = 0;
            while(rand_pois > 0){
                do{
                    rand_uni = runif(0, 1);
                } while(rand_uni == 1.0);
                raw_move += rand_uni * (agent_moving[a_row][move_para] + 1);
                rand_pois--;
            }
            move_len = (int) floor(raw_move);
            break;                        
        default:
            break;
    }
    new_pos  = agent_moving[a_row][yloc] + (move_dir * move_len); 
    if(new_pos >= land_y || new_pos < 0){ /* If off the edge */
        switch(edge){
            case 0: /* Nothing happens (effectively, no edge) */
                break;
            case 1: /* Corresponds to a torus landscape */
                if(new_pos >= land_y){
                    new_pos = new_pos - land_y;  
                }
                if(new_pos < 0){
                    new_pos = new_pos + land_y;   
                }
                break;
            default:
                break;
        }
    }
    agent_moving[a_row][yloc] = new_pos;
}
/* ===========================================================================*/

/* =============================================================================
 * This simulates one agent looking around: Is a particular resource within
 * their field of vision? If yes, return 1, if no, return 0.
 *     obs_x: x location of the observer
 *     obs_y: y location of the observer
 *     res_x: x location of the resource
 *     res_y: y location of the resource
 *     view:  Distance that an agent can view a resource
 *     paras: Vector holding key parameters
 * ========================================================================== */
int binos(int obs_x, int obs_y, int res_x, int res_y,  int view, double *paras){
    
    int Euc, land_x, land_y, edge, see_it, sq_view;
    double sq_xdist, sq_ydist, min_sq_x, min_sq_y, x_test, y_test, distance;
    
    see_it  = 0;
    sq_view = view * view;

    edge   = (int) paras[1];  /* Type of edge of the landscape */
    land_x = (int) paras[12]; /* Dimension of the landscape on the x-axis */
    land_y = (int) paras[13]; /* Dimension of the landscape on the y-axis */
    Euc    = (int) paras[20]; /* Is the distance calculation Euclidean */

    switch(edge){
        case 0:
            sq_xdist = (obs_x - res_x) * (obs_x - res_x);
            sq_ydist = (obs_y - res_y) * (obs_y - res_y);
            switch(Euc){
                case 1: /* If using Euclidean distance */
                    distance = sqrt(sq_xdist + sq_ydist);
                    if(distance <= view){
                        see_it = 1;    
                    }
                    break;
                default:
                    if(sq_xdist < sq_view && sq_ydist < sq_view){
                        see_it = 1;    
                    }
                    break;
            }
            break;    
        case 1: /* There's a cleverer way, but probably not a clearer one */
            /* Get the squared distance on the x dimension */
            min_sq_x = (obs_x - res_x) * (obs_x - res_x);
            x_test   = ( (obs_x+land_x) - res_x) * ( (obs_x+land_x) - res_x);
            if(x_test < min_sq_x){
                min_sq_x = x_test;    
            }
            x_test   = (obs_x - (res_x+land_x) ) * (obs_x - (res_x+land_x) );
            if(x_test < min_sq_x){
                min_sq_x = x_test;    
            }
            sq_xdist = min_sq_x; /* Now have the squared x distance on torus */
            /* Get the squared distance on the y dimension */
            min_sq_y = (obs_y - res_y) * (obs_y - res_y);
            y_test   = ( (obs_y+land_y) - res_y) * ( (obs_y+land_y) - res_y);
            if(y_test < min_sq_y){
                min_sq_y = y_test;   
            }
            y_test   = (obs_y - (res_y+land_y) ) * (obs_y - (res_y+land_y) );
            if(y_test < min_sq_y){
                min_sq_y = y_test;   
            }    
            sq_ydist = min_sq_y; /* Now have the squared x distance on torus */
            switch(Euc){
                case 1: /* If using Euclidean distance */
                    distance = sqrt(sq_xdist + sq_ydist);
                    if(distance <= view){
                        see_it = 1;
                    }        
                    break;
                default:  /* If using the default cells-away distance */
                    if(sq_xdist <= sq_view && sq_ydist <= sq_view){
                        see_it = 1;    
                    }
                    break;
            }
            break;
        default: /* Default assumes no torus and non-Euclidean */
            sq_xdist = (obs_x - res_x) * (obs_x - res_x);
            sq_ydist = (obs_y - res_y) * (obs_y - res_y);
            if(sq_xdist < sq_view && sq_ydist < sq_view){
                see_it = 1;    
            }
            break;      
    }
    return see_it;
}

/* =============================================================================
 * This simulates an individual agent doing some field work (observing)
 * Inputs include:
 *     resource_array: data frame of resources to be marked and/or recaptured
 *     agent_array: data frame of agents, potentially doing the marking
 *     paras: vector of parameter values
 *     res_rows: Total number of rows in the res_adding data frame
 *     agent: The row of the agent that is doing the working
 *     find_proc: The procedure used for finding and marking resources
 *     res_type: The type of resources being marked
 *     obs_col: The number of columns in the observation array
 * Output:
 *     The resource_array is marked by a particular agent
 * ========================================================================== */
void mark_in_view(double **resource_array, double **agent_array, double *paras,
                  int agent, int obs_col){

    int resource_number, xloc, yloc, view, resource, r_x, r_y, seeme, min_age; 
    int res_x, res_y, res_age_col, a_marks, r_marks;
    
    xloc            = (int) agent_array[agent][4];
    yloc            = (int) agent_array[agent][5];
    view            = (int) agent_array[agent][8];
    min_age         = (int) paras[16];
    res_age_col     = (int) paras[31];
    resource_number = (int) paras[32];
    res_x           = (int) paras[33];
    res_y           = (int) paras[34];
    a_marks         = (int) paras[52];
    r_marks         = (int) paras[53];
    
    for(resource = 0; resource < resource_number; resource++){
        if(resource_array[resource][res_age_col] >= min_age){
            r_x   = resource_array[resource][res_x];
            r_y   = resource_array[resource][res_y];
            seeme = binos(xloc, yloc, r_x, r_y, view, paras);
            agent_array[agent][a_marks]       += seeme;
            resource_array[resource][obs_col] += seeme;
            resource_array[resource][r_marks] += seeme;
        }
    }
}

/* =============================================================================
 * Density method of estimation
 * ===========================================================================*/
/* =============================================================================
 * This simulates the capture-mark-recapture of a resource type
 * Inputs include:
 *     resource_array: data frame of resources to be marked and/or recaptured
 *     agent_array: data frame of agents, potentially doing the marking
 *     land: The landscape on which interactions occur
 *     paras: vector of parameter values
 *     obs_col: The number of columns in the observational array
 * Output:
 *     Accumlated markings of resources by agents
 * ========================================================================== */
void mark_res(double **resource_array, double **agent_array, double ***land,
              double *paras, int obs_col){

    int agent_number, a_type, by_type, agent, sample_num;

    a_type       = (int) paras[7];  /* Type of agent does the observing      */   
    by_type      = (int) paras[17]; /* Category (col) of agent type location */
    sample_num   = (int) paras[11]; /* Times resources observed in time step */
    agent_number = (int) paras[54];

    for(agent = 0; agent < agent_number; agent++){
        if(agent_array[agent][by_type] == a_type){
            mark_in_view(resource_array, agent_array, paras, agent, obs_col);
        }
        if(sample_num > 1){
            a_mover(agent_array, land, paras, agent);
        }
    }
}

/* =============================================================================
 * This simulates an individual agent marking a fixed number of resources
 * Inputs include:
 *     resource_array: data frame of resources to be marked and/or recaptured
 *     agent_array: data frame of agents, potentially doing the marking
 *     paras: vector of parameter values
 *     agent: The row of the agent that is doing the working
 *     obs_col: The number of columns in the observation array
 *     type1: Resource type 1 being marked
 *     type2: Resource type 2 being marked
 *     type3: Resource type 3 being marked
 * Output:
 *     Specific resources in resource_array are marked by a particular agent
 * ========================================================================== */
void mark_fixed(double **resource_array, double **agent_array, double *paras,
                int agent, int obs_col, int type1, int type2, int type3){
    
    int resource, fixn, count, sampled, type_num;  
    int resource_number, t1_col, t2_col, t3_col, tally_col, age_col;
    int agent_mark_col, res_marks_col, min_age;
    
    fixn            = (int) paras[10]; /* Number of fixed samples */
    min_age         = (int) paras[16];
    age_col         = (int) paras[31];
    resource_number = (int) paras[32];
    agent_mark_col  = (int) paras[52];
    res_marks_col   = (int) paras[53];
    t1_col          = (int) paras[56];
    t2_col          = (int) paras[57];
    t3_col          = (int) paras[58];
    tally_col       = (int) paras[59]; 
    
    type_num = 0;
    for(resource = 0; resource < resource_number; resource++){
        if(resource_array[resource][t1_col]  == type1 &&
           resource_array[resource][t2_col]  == type2 &&
           resource_array[resource][t3_col]  == type3 &&
           resource_array[resource][age_col] >= min_age
        ){
            type_num++;
        }
    }
    if(type_num > fixn){ /* If more resources than the sample number */
        /* Temp tallies are used here to sample without replacement */
        for(resource = 0; resource < resource_number; resource++){
            if(resource_array[resource][t1_col]  == type1 &&
               resource_array[resource][t2_col]  == type2 &&
               resource_array[resource][t3_col]  == type3
            ){
                resource_array[resource][tally_col] = 0; /* Start untallied */
            }
        }
        count = fixn;
        while(count > 0){
            do{ /* Find an un-tallied resource in the array */
                 sampled = get_rand_int(0, resource_number);
              } while(resource_array[sampled][tally_col] == 1       || 
                      resource_array[sampled][t1_col]  != type1     ||
                      resource_array[sampled][t2_col]  != type2     ||
                      resource_array[sampled][t3_col]  != type3     ||
                      resource_array[sampled][age_col] <  min_age   ||
                      sampled == resource_number /* In case sample returns 1 */
                );
            resource_array[sampled][obs_col]++; /* Marks accumulate  */
            resource_array[sampled][res_marks_col]++;
            resource_array[sampled][tally_col] = 1;    /* Tally is noted    */
            count--;
        }
        agent_array[agent][agent_mark_col] += fixn;
    }else{ /* Else all of the resources should be marked */
        for(resource = 0; resource < resource_number; resource++){
            if(resource_array[resource][t1_col]  == type1   &&
               resource_array[resource][t2_col]  == type2   &&
               resource_array[resource][t3_col]  == type3   &&
               resource_array[resource][age_col] >= min_age
            ){
               resource_array[resource][obs_col]++;  /* Mark all */
               resource_array[resource][res_marks_col]++;
             }
        }
        agent_array[agent][agent_mark_col] += type_num;
   }
}


/* =============================================================================
 * Mark re-capture method of estimation
 * ===========================================================================*/
/* =============================================================================
 * This simulates the capture-mark-recapture of a resource type
 * Inputs include:
 *     resource_array: data frame of resources to be marked and/or recaptured
 *     agent_array: data frame of agents, potentially doing the marking
 *     land: The landscape on which interactions occur
 *     paras: vector of parameter values
 *     lookup: The table listing resources and landscape layers to lookup
 *     agent_number: Total number of agents in the agent array
 * Output:
 *     Accumlated markings of resources by agents
 * ========================================================================== */
void sample_fixed_res(double **resource_array, double **agent_array, 
                      double ***land, double *paras, int **lookup){

    int edge_type, move_type, fixed_sample, times_obs, move_res, by_type;
    int land_x, land_y, res_rows, agent_number, trait_number, lookup_rows;
    int obs_iter, agent, a_type, row, type1, type2, type3;
    
    edge_type    = (int) paras[1];
    move_type    = (int) paras[2];
    a_type       = (int) paras[7];  /* Type of agent doing observing */
    fixed_sample = (int) paras[10];
    land_x       = (int) paras[12];
    land_y       = (int) paras[13];
    by_type      = (int) paras[17];
    move_res     = (int) paras[19];
    res_rows     = (int) paras[32]; /* Number of resources can be sampled */
    agent_number = (int) paras[54]; /* Number of agents in the agent array */
    trait_number = (int) paras[55]; /* Traits (columns) in the resource array */
    lookup_rows  = (int) paras[60]; /* Number of rows in the lookup table */
    
    if(fixed_sample < 1){
        paras[10]    = 1;
        fixed_sample = 1;
    }
    

    for(row = 0; row < lookup_rows; row++){
        if(lookup[row][0] == 0){
            obs_iter     = trait_number + 1; 
            times_obs    = (int) paras[11];
            
            type1 = lookup[row][1];    
            type2 = lookup[row][2];
            type3 = lookup[row][3];
            while(times_obs > 0){
                for(agent = 0; agent < agent_number; agent++){
                    if(agent_array[agent][by_type] == a_type){ 
                        mark_fixed(resource_array, agent_array, paras, agent,
                                   obs_iter, type1, type2, type3);
                    }
                }
                obs_iter++;
                times_obs--;
                if(move_res == 1){ /* Move resources if need for new sample */
                    res_mover(resource_array, land, paras);
                }
            }
        }
    }
}

/* =============================================================================
 * RECORD ALL RESOURCES ALONG A TRANSECT
 * ===========================================================================*/
/* =============================================================================
 * This simulates the capture-mark-recapture of a resource type
 * Inputs include:
 *     resource_array: data frame of resources to be marked and/or recaptured
 *     agent_array: data frame of agents, potentially doing the marking
 *     land: landscape array
 *     paras: vector of parameter values
 *     start_x: The starting x location included in the transect
 *     start_y: The starting y location included in the transect
 *     end_x: The ending x location included in the transect
 *     end_y: The ending y location included in the transect
 *     res_rows: Total number of resources that can be sampled
 *     res_type: The type of resource being sampled
 *     obs_col: The number of columns in the observational array
 * Output:
 *     Accumlated markings of resources within a given area of landscape
 * ========================================================================== */
void transect(double **resource_array, double *paras, int start_x, int start_y, 
              int end_x, int end_y, int obs_iter){
    
    int resource, agent, min_age, res_rows;
    
    min_age   = paras[16];
    res_rows  = (int) paras[32]; /* Number of resources can be sampled */
    
    for(resource = 0; resource < res_rows; resource++){
        if(resource_array[resource][11] >= min_age   &&
           resource_array[resource][4]  >= start_x   &&
           resource_array[resource][4]  <  end_x     &&
           resource_array[resource][5]  >= start_y   &&
           resource_array[resource][5]  <  end_y 
        ){
            resource_array[resource][12]++;
            resource_array[resource][14] += obs_iter;
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
 *      INTERACT:   A table indexing types with rows of interaction array
 * ===========================================================================*/
SEXP observation(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT,
                 SEXP INTERACT){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* Index of x & y locations on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int zloc, land_z;        /* z locations */
    int int_d0, int_d1;      /* Dimensions of the interaction lookup table */
    int row, col;             /* Indices for interaction lookup table */
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
    int *dim_INTERACT;       /* Dimensions of the INTERACT array incoming */
    int **lookup;            /* Lookup table for resource & land interactions */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *intr_ptr;        /* Pointer to INTERACT (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *agent_ptr;       /* Pointer to AGENT (interface R and C) */
    double *new_agent_ptr;   /* Pointer to new agents that are returned */
    double *data_ptr;        /* Pointer to DATA (interface R and C) */
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
    
    PROTECT( INTERACT = AS_NUMERIC(INTERACT) );
    protected_n++;
    intr_ptr = REAL(INTERACT);
    
    PROTECT( PARAMETERS = AS_NUMERIC(PARAMETERS) );
    protected_n++;
    paras = REAL(PARAMETERS);
    
    dim_RESOURCE   = INTEGER( GET_DIM(RESOURCE)  );
    dim_LANDSCAPE  = INTEGER( GET_DIM(LANDSCAPE) );
    dim_AGENT      = INTEGER( GET_DIM(AGENT) );
    dim_INTERACT   = INTEGER( GET_DIM(INTERACT) );

    /* The C code for the model itself falls under here */
    /* ====================================================================== */

    method    = (int) paras[8];  /* Specifies method of estimation used      */
    times_obs = (int) paras[11]; /* Number of times observation conducted    */
    
    /* Code below remakes the RESOURCE matrix, with extra columns for obs */
    res_number        = dim_RESOURCE[0];
    trait_number      = dim_RESOURCE[1]; 
    obs_columns       = trait_number + 1;
    if(method < 2){ /* If method 0, make column for each time observing */
        obs_columns += times_obs;
    }
    resource_array    = malloc(res_number * sizeof(double *));
    for(resource = 0; resource < res_number; resource++){
        resource_array[resource] = malloc(obs_columns * sizeof(double));   
    } 
    vec_pos = 0;
    for(res_trait = 0; res_trait < trait_number; res_trait++){
        for(resource = 0; resource < res_number; resource++){
            resource_array[resource][res_trait] = R_ptr[vec_pos];
            vec_pos++;
        }
    }
    for(res_trait = trait_number; res_trait < obs_columns; res_trait++){
        for(resource = 0; resource < res_number; resource++){
            resource_array[resource][res_trait] = 0;
        }
    }
    /* RESOURCE is now stored as resource_array (discrete resources) */

    /* Code below reads in the LANDSCAPE for easy of use */
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

    /* Do the biology here now */
    /* ====================================================================== */

    time_para = (int) paras[0];
    edge_type = (int) paras[1];
    move_type = (int) paras[2];
    a_type    = (int) paras[7];  /* What type of agent does the observing    */   
    by_type   = (int) paras[17]; /* Category (column) of agent type location */
    move_res  = (int) paras[19]; /* Should the resources be moved?           */

    for(resource = 0; resource < res_number; resource++){
        resource_array[resource][12] = 0;   /* Set marks to zero   */
        resource_array[resource][13] = 0;   /* Set tallies to zero */
    }

    /* This switch function calls a method of population size estimation */
    switch(method){
        case 0: /* Simple view-based method of sampling */
            obs_iter = trait_number + 1; /* Skips a_type col added later */
            while(times_obs > 0){
                mark_res(resource_array, agent_array, land, paras, obs_iter);
                obs_iter++;
                times_obs--;
                if(move_res == 1){ /* Move resources if need for new sample */
                    res_mover(resource_array, land, paras);
                }
            }
            break;
        case 1: /* Sample fixed_sample resources randomly on landscape */
            sample_fixed_res(resource_array, agent_array, land, paras, lookup);
            break;            
        case 2: /* Sample along a linear (y-axis) transect of all rows */
            transect_len   = 0;
            working_agents = 0; 
            for(agent = 0; agent < agent_number; agent++){
                if(agent_array[agent][by_type] == a_type){
                    transect_len += (int) agent_array[agent][8];
                    working_agents++;
                }
            }
            transect_len *= working_agents;
            if(transect_len < 1){
                transect_len = 1;
            }
            obs_iter     = 0;
            tx0          = 0;
            tx1          = land_x + 1;
            ty0          = 0;
            ty1          = transect_len;
            while(ty0 < land_y){
                transect(resource_array, paras, tx0, ty0, tx1, ty1, obs_iter);
                obs_iter++;
                ty0 =  ty1;
                ty1 += transect_len + 1;
                if(move_res == 1){
                    res_mover(resource_array, land, paras);
                }
            }
            break;
        case 3: /* Sample in blocks of landscape (view by view chunks) */
            transect_len   = 0;
            working_agents = 0; 
            for(agent = 0; agent < agent_number; agent++){
                if(agent_array[agent][by_type] == a_type){
                    transect_len += (int) agent_array[agent][8];
                    working_agents++;
                }
            }
            transect_eff = working_agents;
            if(transect_len < 1){
                transect_len = 1;
            }
            obs_iter     = 0;
            tx0          = 0;
            tx1          = transect_len;
            ty0          = 0;
            ty1          = transect_len;
            while(tx0 < land_x && ty0 < land_y){
                transect(resource_array, paras, tx0, ty0, tx1, ty1, obs_iter);
                obs_iter++;
                tx0 =  tx1;
                tx1 += transect_len;
                if(tx0 >= land_x){
                    tx0 =  0;
                    tx1 =  transect_len;
                    ty0 =  ty1;
                    ty1 += transect_len;
                } 
                transect_eff--;
                if(move_res == 1 && transect_eff == 0){
                    res_mover(resource_array, land, paras); 
                    transect_eff = working_agents;
                }
                if( obs_iter > (land_x * land_y) ){
                    break;   
                }
            }
            break;            
        default:
            obs_iter = trait_number + 1; /* The 1 skips over the agent type */
            while(times_obs > 0){
                mark_res(resource_array, agent_array, land, paras, obs_iter);
                obs_iter++;
                times_obs--; /* Then move agents if need be for new sample */ 
                if(move_res == 1){
                    res_mover(resource_array, land, paras);
                }
            }
        break;
    }

    /* Check to see how many resources were observed */        
    new_obs   = 0;
    for(resource = 0; resource < res_number; resource++){
        if(resource_array[resource][12] > 0){
            new_obs++; 
        }
    }

    /* Build an array with the appropriate number of resources observed */
    obs_array = malloc(new_obs * sizeof(double *));
    for(resource = 0; resource < new_obs; resource++){
        obs_array[resource] = malloc(obs_columns * sizeof(double));
    }

    /* Fill the new array with the resources observed */
    add_obs = 0;
    for(resource = 0; resource < res_number; resource++){
        if(resource_array[resource][12] > 0){
            for(res_trait = 0; res_trait < obs_columns; res_trait++){
                obs_array[add_obs][res_trait] = 
                    resource_array[resource][res_trait];
            }
            obs_array[add_obs][13] = (double) a_type; /* Re-purpose tally col */
            add_obs++;
            if(add_obs > new_obs){ /* Below should not ever happen */
                break;
            }
        }
    }
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP NEW_OBSERVATIONS;
    PROTECT( NEW_OBSERVATIONS = allocMatrix(REALSXP, new_obs, obs_columns) );
    protected_n++;
    
    data_ptr = REAL(NEW_OBSERVATIONS);

    vec_pos = 0;
    for(res_trait = 0; res_trait < obs_columns; res_trait++){
        for(resource = 0; resource < new_obs; resource++){
            data_ptr[vec_pos] = obs_array[resource][res_trait];
            vec_pos++;
        }
    }
    paras[61] = new_obs;
    paras[62] = obs_columns;

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
    
    SEXP EVERYTHING;
    EVERYTHING = PROTECT( allocVector(VECSXP, 3) );
    protected_n++;
    SET_VECTOR_ELT(EVERYTHING, 0, NEW_OBSERVATIONS);
    SET_VECTOR_ELT(EVERYTHING, 1, NEW_AGENTS);
    SET_VECTOR_ELT(EVERYTHING, 2, PARAMETERS);  
    
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(row = 0; row < int_d0; row++){
        free(lookup[row]);
    }
    free(lookup);   
    for(resource = 0; resource < new_obs; resource++){
        free(obs_array[resource]);
    }
    free(obs_array);
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


/* =============================================================================
* MAIN ANECDOTAL FUNCTION:
* ===========================================================================*/

/* =============================================================================
*  ****     This is the main function for the observation model     ****
*  This function reads resource, landscape, and agent arrays, and a parameter 
*  vector from R, runs other functions in the observation.c file, then returns 
*  the the observer array with a column identifying the number of resources
*  within each agents view. Much of the code is replicated from the
*  observation function, but this function is separate to allow for future
*  development that might further specialise it.
*  Inputs include:
*      RESOURCE:   An array of *row resources and *col traits for each resource
*      LANDSCAPE:  An array of *row by *col size that makes up the landscape
*      PARAMETERS: Parameters read into the function for population processes
*      AGENT:      An array of *row agents and *col traits for each agent
* ===========================================================================*/
SEXP anecdotal(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT){
    
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* Index of x & y location on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int land_z;              /* Number of layers in the landscape */
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
    int res_type;            /* Types of resources that are being observed */
    int a_type;              /* Type of agent doing observing (manager = 0) */
    int by_type_r;           /* Res type category for observing (default = 1) */
    int by_type_a;           /* Agent type category for observing */
    int min_age;             /* Minimum age of resources viewed (default = 1) */
    int view;                /* How far an agent views resources (dynamic) */
    int edge;                /* Type of edge modelled on the landscape */
    int EucD;                /* Is an agent viewing by Euclidean distance */
    int a_x;                 /* Agent's x location */
    int a_y;                 /* Agent's y location */
    int r_x;                 /* Resource x location */
    int r_y;                 /* Resource y location */
    int seeit;               /* Index summing number of seen resources */
    int rec_col;             /* Agent col where seen resources are recorded */
    int a_check;             /* Check agent to see if they view (dynamic) */
    int move_agents;         /* Do the agents move once */
    int move_t;              /* Movement type of the agents */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *agent_ptr;       /* Pointer to AGENT (interface R and C) */
    double *data_ptr;        /* Pointer to DATA (interface R and C) */
    double **resource_array; /* Array to store the old RESOURCE in C */
    double ***land;          /* Array to store the landscape in C*/
    double **agent_array;    /* Array to store the agents in C */

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
    
    /* Code below remakes the RESOURCE matrix */
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
    
    /* Code below reads in the LANDSCAPE for easy of use */
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

    edge         = (int) paras[1];  /* What kind of edge is on the landscape  */ 
    a_type       = (int) paras[7];  /* What type of agent is observing        */
    res_type     = (int) paras[9];  /* What type of resources are observed    */
    move_t       = (int) paras[14]; /* Type of movement being used            */
    by_type_r    = (int) paras[15]; /* Category (column) of res type loc      */
    min_age      = (int) paras[16]; /* Minimum age of resources viewed        */
    by_type_a    = (int) paras[17]; /* Category (column) of agent type loc    */
    rec_col      = (int) paras[18]; /* Column where viewed resources recorded */
    EucD         = (int) paras[20]; /* Do individuals view by Euclidean dist  */
    move_agents  = (int) paras[28]; /* Do the agents move once?               */
    
    for(agent = 0; agent < agent_number; agent++){
        seeit    = 0;     /* Start with an agent not seeing anything */
        a_check  = 0;     /* Start assuming the agent won't be checking */
        if( a_type < 0 || agent_array[agent][by_type_a] == a_type){
            a_check = 1;    
        }
        for(resource = 0; resource < res_number; resource++){
            if( a_check                             == 1        &&
                resource_array[resource][by_type_r] == res_type &&
                resource_array[resource][11]        >= min_age
               ){
                a_x   = (int) agent_array[agent][4];
                a_y   = (int) agent_array[agent][5];
                r_x   = (int) resource_array[resource][4];
                r_y   = (int) resource_array[resource][5];
                view  = (int) agent_array[agent][8];
                seeit += binos(a_x, a_y, r_x, r_y, view, paras);
                agent_array[agent][rec_col] = seeit;
            }
        }
        if(move_agents == 1){
            a_mover(agent_array, land, paras, agent);
        }
    }
    
    /* This code switches from C back to R */
    /* ====================================================================== */        
    
    SEXP NEW_AGENT;
    PROTECT( NEW_AGENT = allocMatrix(REALSXP, agent_number, agent_traits) );
    protected_n++;
    
    data_ptr = REAL(NEW_AGENT);
    
    vec_pos = 0;
    for(agent_trait = 0; agent_trait < agent_traits; agent_trait++){
        for(agent = 0; agent < agent_number; agent++){
            data_ptr[vec_pos] = agent_array[agent][agent_trait];
            vec_pos++;
        }
    }   
    
    SEXP EVERYTHING;
    EVERYTHING = PROTECT( allocVector(VECSXP, 2) );
    protected_n++;
    SET_VECTOR_ELT(EVERYTHING, 0, NEW_AGENT);
    SET_VECTOR_ELT(EVERYTHING, 1, PARAMETERS);
    
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
