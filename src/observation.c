#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* =============================================================================
 * This function moves one on the landscape according to some rules
 * For now, it is repeated in both c files, but if there are more functions
 * that serve two purposes, then a general utilily c file might be created
 * agent_moving: Agent array
 * xloc: Column indicating the agents' x positions
 * yloc: Column indicating the agents' y positions
 * move_para: Column affecting the distance that an agent can move
 * edge: Defines what happens at the landscape edge:
 *     0: Nothing happens (individual is just off the map)
 *     1: Torus landscape (individual wraps around to the other side)
 * a_row: Row of the agent of interest
 * landscape: The landscape array
 * land_x: Max x dimension of the landscape
 * land_y: Max y dimension of the landscape
 * type: Defines the type of movement allowed:
 *     0: No movement is allowed
 *     1: Movement is random uniform from zero to move_para in any direction
 *     2: Movement is poisson(move_para) in any direction
 * ========================================================================== */
void a_mover(double **agent_moving, int xloc, int yloc, int move_para, int edge,
             int a_row, double **landscape, int land_x, int land_y, int type){
    
    int move_len;     /* Length of a move                            */
    int move_dir;     /* Move direction (-1 or 1)                    */
    int new_pos;      /* New position: check if over landscape edge  */
    double rand_num;  /* Random number used for sampling             */
    double rand_uni;  /* Random uniform number                       */
    double rand_pois; /* Random poisson number                       */
    double raw_move;  /* Movement length before floor() truncation   */

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
            if(a_row == 0){
                printf("Unclear specification of movement type \n");
            }
            break;
    }
    new_pos  = agent_moving[a_row][xloc] + (move_dir * move_len); 
    if(new_pos > land_x || new_pos < 0){ /* If off the edge */
        switch(edge){
            case 0: /* Nothing happens (effectively, no edge) */
                break;
            case 1: /* Corresponds to a torus landscape */
                if(new_pos > land_x){
                    new_pos = new_pos - land_x;   
                }
                if(new_pos < 0){
                    new_pos = new_pos + land_x;   
                }
                break;
            default:
                if(a_row == 0){
                    printf("ERROR: Edge effects set incorrectly \n");
                }
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
    if(new_pos > land_y || new_pos < 0){ /* If off the edge */
        switch(edge){
            case 0: /* Nothing happens (effectively, no edge) */
                break;
            case 1: /* Corresponds to a torus landscape */
                if(new_pos > land_y){
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
 *     edge:  The type of edge of the landscape (e.g., no edge, torus)
 *     view:  Distance that an agent can view a resource
 *     xdim:  Dimension of the landscape on the x-axis
 *     ydim:  Dimension of the landscape on the y-axis
 * ========================================================================== */
int binos(int obs_x, int obs_y, int res_x, int res_y, int edge, int view,
          int xdim, int ydim){
    
    int see_it;
    double sq_xdist;
    double sq_ydist;
    double min_sq_x;
    double min_sq_y;
    double x_test;
    double y_test;
    double distance;
    
    see_it  = 0;
    
    switch(edge){
        case 0:
            sq_xdist = (obs_x - res_x) * (obs_x - res_x);
            sq_ydist = (obs_y - res_y) * (obs_y - res_y);
            distance = sqrt(sq_xdist + sq_ydist);
            if(distance <= view){
                see_it = 1;   
            }
            break;    
        case 1: /* There's a cleverer way, but probably not a clearer one */
            /* Get the squared distance on the x dimension */
            min_sq_x = (obs_x - res_x) * (obs_x - res_x);
            x_test   = ( (obs_x + xdim) - res_x) * ( (obs_x + xdim) - res_x);
            if(x_test < min_sq_x){
                min_sq_x = x_test;    
            }
            x_test   = (obs_x - (res_x + xdim) ) * (obs_x - (res_x + xdim) );
            if(x_test < min_sq_x){
                min_sq_x = x_test;    
            }
            sq_xdist = min_sq_x; /* Now have the squared x distance on torus */
            /* Get the squared distance on the y dimension */
            min_sq_y = (obs_y - res_y) * (obs_y - res_y);
            y_test   = ( (obs_y + ydim) - res_y) * ( (obs_y + ydim) - res_y);
            if(y_test < min_sq_y){
                min_sq_y = y_test;   
            }
            y_test   = (obs_y - (res_y + ydim) ) * (obs_y - (res_y + ydim) );
            if(y_test < min_sq_y){
                min_sq_y = y_test;   
            }    
            sq_ydist = min_sq_y; /* Now have the squared x distance on torus */
            distance = sqrt(sq_xdist + sq_ydist);
            if(distance <= view){
                see_it = 1;   
            }            
            break;
        default: /* Default assumes no torus */
            sq_xdist = (obs_x - res_x) * (obs_x - res_x);
            sq_ydist = (obs_y - res_y) * (obs_y - res_y);
            distance = sqrt(sq_xdist + sq_ydist);
            if(distance <= view){
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
 *     worker: The row of the agent that is doing the working
 *     find_proc: The procedure used for finding and marking resources
 *     res_type: The type of resources being marked
 *     obs_col: The number of columns in the observation array
 * Output:
 *     The resource_array is marked by a particular agent
 * ========================================================================== */
void field_work(double **resource_array, double **agent_array, double *paras,
                int res_rows, int worker, int find_proc, int res_type, 
                int obs_col){

    int xloc;         /* x location of the agent doing work */
    int yloc;         /* y location of the agent doing work */
    int view;         /* The 'view' (sampling range) around agent's location */
    int edge;         /* What type of edge is being used in the simulation */
    int resource;     /* Index for resource array */
    int r_x;          /* x location of a resource */
    int r_y;          /* y location of a resource */
    int seeme;        /* Test if observer sees/captures the resource */
    int ldx;          /* Landscape dimension on the x-axis */
    int ldy;          /* Landscape dimension on the y-axis */
    int fixn;         /* If procedure is to sample a fixed number; how many? */
    int count;        /* Index for sampling a fixed number of resource */
    int sampled;      /* The resource randomly sampled */
    int type_num;     /* Number of the type of resource to be fixed sampled */
    double sampl;     /* Random uniform sampling of a resource */
    double min_age;   /* Minimum at which sampling can occur */
    
    xloc  = (int) agent_array[worker][4];
    yloc  = (int) agent_array[worker][5];
    view  = (int) agent_array[worker][8];
    edge  = (int) paras[1];
    ldx   = (int) paras[12];
    ldy   = (int) paras[13];
    fixn  = (int) paras[10];
    
    min_age = paras[16];
    
    switch(find_proc){
        case 0: /* Mark all individuals within view */
            for(resource = 0; resource < res_rows; resource++){
                if(resource_array[resource][1] == res_type && 
                   resource_array[resource][11] >= min_age){
                    r_x   = resource_array[resource][4];
                    r_y   = resource_array[resource][5];
                    seeme = binos(xloc, yloc, r_x, r_y, edge, view, ldx, ldy);
                    agent_array[worker][10]           += seeme;
                    resource_array[resource][obs_col] += seeme;
                    resource_array[resource][12]      += seeme;
                }
            }
            break;
        case 1: /* Alternative (only one now) is to sample fixed number */
            type_num = 0;
            for(resource = 0; resource < res_rows; resource++){
                if(resource_array[resource][1]  == res_type &&
                   resource_array[resource][11] >= min_age){
                    type_num++;
                }
            }
            if(type_num > fixn){ /* If more resources than the sample number */
               /* Temp tallies are used here to sample without replacement */
                for(resource = 0; resource < res_rows; resource++){
                    if(resource_array[resource][1] == res_type){
                        resource_array[resource][13] = 0; /* Start untallied */
                    }
                }
                count = fixn;
                sampl = 0;
                while(count > 0){
                    do{ /* Find an un-tallied resource in the array */
                        sampl   = runif(0, 1) * res_rows;
                        sampled = (int) sampl;
                    } while(resource_array[sampled][13] == 1         || 
                            resource_array[sampled][1]  != res_type  ||
                            resource_array[sampled][11] <  min_age   ||
                            sampled == res_rows /* In case sample returns 1 */
                            );
                    resource_array[sampled][obs_col]++; /* Marks accumulate  */
                    resource_array[sampled][12]++;
                    resource_array[sampled][13] = 1;    /* Tally is noted    */
                    count--;
                }
                agent_array[worker][10] += fixn;
            }else{ /* Else all of the resources should be marked */
                for(resource = 0; resource < res_rows; resource++){
                    if(resource_array[resource][1] == res_type && 
                       resource_array[resource][11] >= min_age){
                        resource_array[resource][obs_col]++;  /* Mark all */
                        resource_array[resource][12]++;
                    }
                }
                agent_array[worker][10] += type_num; /* All resources marked */ 
            }
            break;
        default:
            printf("Error setting observation type: using vision-based CMR");
            for(resource = 0; resource < res_rows; resource++){
                if(resource_array[resource][1] == res_type && 
                   resource_array[resource][11] > min_age){
                    r_x = resource_array[resource][4];
                    r_y = resource_array[resource][5];
                    seeme = binos(xloc, yloc, r_x, r_y, edge, view, ldx, ldy);
                    agent_array[worker][10]           += seeme;
                    resource_array[resource][12]      += seeme;
                    resource_array[resource][obs_col] += seeme;
                }
            }
            break;
    }
}

/* =============================================================================
 * CAPTURE MARK-RECAPTURE (CMR) METHOD:
 * ===========================================================================*/
/* =============================================================================
 * This simulates the capture-mark-recapture of a resource type
 * Inputs include:
 *     resource_array: data frame of resources to be marked and/or recaptured
 *     agent_array: data frame of agents, potentially doing the marking
 *     paras: vector of parameter values
 *     res_rows: Total number of resources that can be sampled
 *     a_row: Total number agents that could possibly sample
 *     res_type: The type of resource being sampled
 *     obs_col: The number of columns in the observational array
 *     a_type: The type of agent that is doing the marking
 *     by_type: The type column that is being used
 * Output:
 *     Accumlated markings of resources by agents
 * ========================================================================== */
void mark_res(double **resource_array, double **agent_array, double **land,
              double *paras, int res_rows, int a_row, int res_type, 
              int obs_col, int a_type, int by_type){
    
    int resource;
    int agent;
    int count;
    int is_fixed;
    int find_type;   /* Find type -- fixed number of observations? */
    int edge;        /* How does edge work? (Effects agent vision & movement) */
    int samp_res;    /* A randomly sampled resource */
    int ldx, ldy;
    int move_t;
    int sample_num;   /* Times resources observed during one time step */

    edge       = (int) paras[1];  /* What type of edge is on the landscape */
    sample_num = (int) paras[11];
    ldx        = (int) paras[12]; /* dimensions of landscape -- x and y */
    ldy        = (int) paras[13];
    move_t     = (int) paras[14]; /* Type of movement being used  */

    find_type  = (int) paras[10]; /* Conversion to int type with type caster */
    if(find_type > 0){
        find_type = 1;
    }
    
    for(agent = 0; agent < a_row; agent++){
        if(agent_array[agent][by_type] == a_type){ 
            field_work(resource_array, agent_array, paras, res_rows, agent, 
                       find_type, res_type, obs_col);
        }
        if(sample_num > 1){
            a_mover(agent_array, 4, 5, 6, edge, agent, land, ldx, ldy, move_t);
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
 * ===========================================================================*/
SEXP observation(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc;                /* Index of x location on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
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
    int *add_resource;       /* Vector of added resources */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *agent_ptr;       /* Pointer to AGENT (interface R and C) */
    double *data_ptr;        /* Pointer to DATA (interface R and C) */
    double **resource_array; /* Array to store the old RESOURCE in C */
    double **land;           /* Array to store the landscape in C*/
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

    times_obs    = (int) paras[11]; /* Number of times observation conducted */
    
    /* Code below remakes the RESOURCE matrix, with extra columns for obs */
    res_number        = dim_RESOURCE[0];
    trait_number      = dim_RESOURCE[1]; 
    obs_columns       = trait_number + times_obs;
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
    land_y = dim_LANDSCAPE[1];
    land_x = dim_LANDSCAPE[0];
    land   = malloc(land_x * sizeof(double *));
    for(xloc = 0; xloc < land_x; xloc++){
        land[xloc] = malloc(land_y * sizeof(double));   
    } /* LANDSCAPE is now stored as land */
    
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

    a_type       = (int) paras[7];  /* What type of agent does the observing */   
    method       = (int) paras[8];  /* Specifies method of estimation used   */
    res_type     = (int) paras[9];  /* What type of resources are observed   */
    by_type      = (int) paras[17]; /* Category (column) of type location    */

    for(resource = 0; resource < res_number; resource++){
        resource_array[resource][12] = 0;   /* Set marks to zero   */
        resource_array[resource][13] = 0;   /* Set tallies to zero */
    }

    /* This switch function calls a method of population size estimation */
    switch(method){
       case 0: /* Start of observation column, the +1 skips agent type col */
           obs_iter = trait_number + 1; 
           while(times_obs > 0){
               mark_res(resource_array, agent_array, land, paras, res_number, 
                        agent_number, res_type, obs_iter, a_type, by_type);
               times_obs--; /* Then move agents if need be for new sample */
               obs_iter++;
           }
           break;
       default:
           printf("ERROR: No observation method set: marking all in view \n");
           obs_iter = trait_number + 1; /* The 1 skips over the agent type */
           while(times_obs > 0){
               mark_res(resource_array, agent_array, land, paras, res_number, 
                        agent_number, res_type, obs_iter, a_type, by_type);
               times_obs--; /* Then move agents if need be for new sample */
               obs_iter++;
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
            if(add_obs > new_obs){ /* Below should not every happen */
                printf("\n\nFATAL ERROR: Added observes exceed expected\n\n");  
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

    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(resource = 0; resource < new_obs; resource++){
        free(obs_array[resource]);
    }    
    for(agent = 0; agent < agent_number; agent++){
        free(agent_array[agent]);
    }    
    for(xloc = 0; xloc < land_x; xloc++){
        free(land[xloc]);        
    }
    for(resource = 0; resource < res_number; resource++){
        free(resource_array[resource]);
    } 

    return(NEW_OBSERVATIONS); 
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
    int xloc;                /* Index of x location on the landscape */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
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
    int a_x;                 /* Agent's x location */
    int a_y;                 /* Agent's y location */
    int r_x;                 /* Resource x location */
    int r_y;                 /* Resource y location */
    int seeit;               /* Index summing number of seen resources */
    int rec_col;             /* Agent col where seen resources are recorded */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    double *R_ptr;           /* Pointer to RESOURCE (interface R and C) */
    double *land_ptr;        /* Pointer to LANDSCAPE (interface R and C) */
    double *paras;           /* Pointer to PARAMETER (interface R and C) */
    double *agent_ptr;       /* Pointer to AGENT (interface R and C) */
    double *data_ptr;        /* Pointer to DATA (interface R and C) */
    double **resource_array; /* Array to store the old RESOURCE in C */
    double **land;           /* Array to store the landscape in C*/
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
    land_y = dim_LANDSCAPE[1];
    land_x = dim_LANDSCAPE[0];
    land   = malloc(land_x * sizeof(double *));
    for(xloc = 0; xloc < land_x; xloc++){
        land[xloc] = malloc(land_y * sizeof(double));   
    } /* LANDSCAPE is now stored as land */
    
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
    by_type_r    = (int) paras[15]; /* Category (column) of res type loc      */
    min_age      = (int) paras[16]; /* Minimum age of resources viewed        */
    by_type_a    = (int) paras[17]; /* Category (column) of agent type loc    */
    rec_col      = (int) paras[18]; /* Column where viewed resources recorded */
    
    for(agent = 0; agent < agent_number; agent++){
        seeit = 0;     /* Start with an agent not seeing anything */
        for(resource = 0; resource < res_number; resource++){
            if( agent_array[agent][by_type_a]       == a_type   &&
                resource_array[resource][by_type_r] == res_type &&
                resource_array[resource][11]        >= min_age
               ){
                a_x   = (int) agent_array[agent][4];
                a_y   = (int) agent_array[agent][5];
                r_x   = (int) resource_array[resource][4];
                r_y   = (int) resource_array[resource][5];
                view  = (int) agent_array[agent][8];
                seeit += binos(a_x, a_y, r_x, r_y, edge, view, land_x, land_y);
                agent_array[agent][rec_col] = seeit;
            }
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
    
    UNPROTECT(protected_n);
    
    /* Free all of the allocated memory used in arrays */
    for(agent = 0; agent < agent_number; agent++){
        free(agent_array[agent]);
    }    
    for(xloc = 0; xloc < land_x; xloc++){
        free(land[xloc]);        
    }
    for(resource = 0; resource < res_number; resource++){
        free(resource_array[resource]);
    } 
    
    return(NEW_AGENT); 
}
/* ===========================================================================*/


