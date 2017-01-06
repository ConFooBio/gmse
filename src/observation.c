#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>


/* =============================================================================
 * This function moves agents on the landscape according to some rules
 * For now, it is repeated in both c files, but if there are more functions
 * that serve two purposes, then a general utilily c file might be created
 * The 'edge' argument defines what happens at the landscape edge:
 *     0: Nothing happens (individual is just off the map)
 *     1: Torus landscape (individual wraps around to the other side)
 * The 'type' argument defines the type of movement allowed:
 *     0: No movement is allowed
 *     1: Movement is random uniform from zero to move_para in any direction
 *     2: Movement is poisson(move_para) in any direction
 * ========================================================================== */
void a_mover(double **agent_moving, int xloc, int yloc, int move_para, int rows,
           int edge, double **landscape, int land_x, int land_y, int type){
    
    int a_num;        /* Agent number index                          */
    int move_len;     /* Length of a move                            */
    int move_dir;     /* Move direction (-1 or 1)                    */
    int new_pos;      /* New position: check if over landscape edge  */
    double rand_num;  /* Random number used for sampling             */
    double rand_uni;  /* Random uniform number                       */
    double rand_pois; /* Random poisson number                       */
    double raw_move;  /* Movement length before floor() truncation   */

    for(a_num=0; a_num < rows; a_num++){
        /* Move first in the xloc direction --------------------------------- */
        new_pos  = agent_moving[a_num][xloc];
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
                raw_move = rand_uni * (agent_moving[a_num][move_para] + 1);
                move_len = floor(raw_move);
                break;
            case 2: /* Poisson selection of position change */
                rand_pois = rpois(agent_moving[a_num][move_para]);    
                raw_move  = rand_pois * (agent_moving[a_num][move_para] + 1);
                move_len  = floor(raw_move);
                break;
            default:
                if(a_num == 0){
                    printf("Unclear specification of movement type \n");
                }
                break;
        }
        new_pos  = agent_moving[a_num][xloc] + (move_dir * move_len); 
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
                    if(a_num == 0){
                        printf("ERROR: Edge effects set incorrectly \n");
                    }
                    break;
            }
        }
        agent_moving[a_num][xloc] = new_pos;
        /* Move next in the yloc direction ---------------------------------- */
        new_pos  = agent_moving[a_num][yloc];
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
                raw_move = rand_uni * (agent_moving[a_num][move_para] + 1);
                move_len = floor(raw_move);
                break;
            case 2: /* Poisson selection of position change */
                rand_pois = rpois(agent_moving[a_num][move_para]);    
                raw_move  = rand_pois * (agent_moving[a_num][move_para] + 1);
                move_len  = floor(raw_move);
                break;
            default:
                break;
        }
        new_pos  = agent_moving[a_num][yloc] + (move_dir * move_len); 
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
        agent_moving[a_num][yloc] = new_pos;
    }
}
/* ===========================================================================*/




/* =============================================================================
 * This simulates one agent looking around: Is a particular resource within
 * their field of vision? If yes, return 1, if no, return 0.
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
 * Output:
 *     The resource_array is marked by a particular agent
 * ========================================================================== */
void field_work(double **resource_array, double **agent_array, double *paras,
                int res_rows, int worker, int find_proc){

    int xloc;     /* x location of the agent doing work */
    int yloc;     /* y location of the agent doing work */
    int view;     /* The 'view' (or sampling range) around agent's location */
    int edge;     /* What type of edge is being used in the simulation */
    int resource; /* Index for resource array */
    int res_x;    /* x location of a resource */
    int res_y;    /* y location of a resource */
    int seeme;    /* Test whether or not observer sees/captures the resource */
    int ldx;      /* Landscape dimension on the x-axis */
    int ldy;      /* Landscape dimension on the y-axis */
    int fixn;     /* If procedure is to sample a fixed number; how many? */
    int count;    /* Index for sampling a fixed number of resource */
    int sampled;  /* The resource randomly sampled */
    double sampl; /* Random uniform sampling of a resource */
    
    xloc  = (int) agent_array[worker][4];
    yloc  = (int) agent_array[worker][5];
    view  = (int) agent_array[worker][8];
    edge  = (int) paras[1];
    ldx   = (int) paras[12];
    ldy   = (int) paras[13];
    fixn  = (int) paras[10];
    
    switch(find_proc){
        case 0: /* Mark all individuals within view */
            for(resource = 0; resource < res_rows; resource++){
               res_x = resource_array[resource][4];
               res_y = resource_array[resource][5];
               seeme = binos(xloc, yloc, res_x, res_y, edge, view, ldx, ldy);
               agent_array[worker][10]      += seeme;
               resource_array[resource][12] += seeme;
            }
            break;
        case 1: /* Alternative (only one now) is to sample fixed number */
            if(res_rows > fixn){ /* If more resources than the sample number */
               /* Temp tallies are used here to sample without replacement */
               for(resource = 0; resource < res_rows; resource++){
                   resource_array[resource][13] = 0; /* Start off untallied */   
               }
               count = fixn;
               sampl = 0;
               while(count > 0){
                   do{ /* Find an un-tallied resource in the array */
                       sampl   = runif(0, 1) * res_rows;
                       sampled = (int) sampl;
                   }while(resource_array[sampled][13] == 1 && sampl < res_rows);
                   resource_array[sampled][12]++;   /* Marks accumulate  */
                   resource_array[sampled][13] = 1; /* Tally is noted    */
                   count--;
               }
            }else{ /* Else all of the resources should be marked */
                for(resource = 0; resource < res_rows; resource++){
                    resource_array[resource][12]++; /* Mark all resources */  
                }
                agent_array[worker][10] += res_rows; /* All resources marked */ 
            }
            break;
        default:
            printf("Error setting observation type: using vision-based CMR");
            for(resource = 0; resource < res_rows; resource++){
                res_x = resource_array[resource][4];
                res_y = resource_array[resource][5];
                seeme = binos(xloc, yloc, res_x, res_y, edge, view, ldx, ldy);
                agent_array[worker][10]      += seeme;
                resource_array[resource][12] += seeme;
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
 * Output:
 *     Accumlated markings of resources by agents
 * ========================================================================== */
void mark_res(double **resource_array, double **agent_array, double *paras, 
              int res_rows, int a_row){
    
    int resource;
    int agent;
    int count;
    int is_fixed;
    int ft;        /* Find type -- fixed number of observations? */
    int edge;      /* How does edge work? (Effects agent vision & movement) */
    int samp_res;  /* A randomly sampled resource */
    
    ft = (int) paras[10]; /* Conversion to int type with type caster */
    if(ft > 0){
        ft = 1;
    }
    
    for(agent = 0; agent < a_row; agent++){
        if(agent_array[agent][1] == 0){ /* Zeros are manager agents */
            field_work(resource_array, agent_array, paras, res_rows, agent, ft);
        }
    }
}


/* =============================================================================
 * MAIN OBSERVATION FUNCTION:
 * ===========================================================================*/

/* =============================================================================
 *  ****     This is the main function for the resource model     ****
 *  This function reads resource and landscape arrays, and a parameter vector
 *  from R, runs other functions in the resources.c file, then returns the
 *  new resource array.
 *  Inputs include:
 *      RESOURCE:   An array of *row resources and *col traits for each resource
 *      LANDSCAPE:  An array of *row by *col size that makes up the landscape
 *      PARAMETERS: Parameters read into the function for population processes
 *      AGENT:      An array of *row agents and *col traits for each agent
 * ===========================================================================*/
/* TODO: Argument: VECTOR_OF_PARAMETER_VALUES */
SEXP observation(SEXP RESOURCE, SEXP LANDSCAPE, SEXP PARAMETERS, SEXP AGENT){
 
    /* SOME STANDARD DECLARATIONS OF KEY VARIABLES AND POINTERS               */
    /* ====================================================================== */
    int xloc, yloc;          /* x and y locations in the RESOURCE array */ 
    int land_x, land_y;      /* x and y maximum location given LANDSCAPE */
    int resource;            /* Index for resource (rows of RESOURCE) */
    int res_trait;           /* Index for resource traits (cols of RESOURCE) */
    int agent;               /* Index for agent in the array (rows) */
    int agent_trait;         /* Index for agent traits (cols of agent_array) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included in the resource */
    int agent_number;        /* Number of agents that can potentially observe */
    int agent_traits;        /* Number of traits that each agent has */
    int para_number;         /* Number of parameters included */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int new_obs;             /* New observations made */
    int add_obs;             /* Index for adding observations */
    int method;              /* Type of method used to estimate pop size */
    int who_observes;        /* Type of agent that does the observing */
    int times_obs;           /* Number of times observation is conducted */
    int *add_resource;       /* Vector of added resources */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    int *len_PARAMETERS;     /* Length of the PARAMETERS vector incoming */
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
    
    /* Code below remakes the RESOURCE matrix for easier use */
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

    who_observes = (int) paras[7];  /* What type of agent does the observing */   
    method       = (int) paras[8];  /* Specifies method of estimation used   */
    times_obs    = (int) paras[11]; /* Number of times observation conducted */
    
    for(resource = 0; resource < res_number; resource++){
        resource_array[resource][12] = 0;   /* Set marks to zero   */
        resource_array[resource][13] = 0;   /* Set tallies to zero */
    }
 
    /* This switch function calls a method of population size estimation */
    switch(method){
       case 0:
           while(times_obs > 0){
               mark_res(resource_array, agent_array, paras, res_number, 
                        agent_number);
               times_obs--;
           }
           break;
       default:
           printf("ERROR: No observation method set: marking all in view \n");
           while(times_obs > 0){
               mark_res(resource_array, agent_array, paras, res_number, 
                        agent_number);
               times_obs--;
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
        obs_array[resource] = malloc(trait_number * sizeof(double));
    }

    /* Fill the new array with the resources observed */
    add_obs = 0;
    for(resource = 0; resource < res_number; resource++){
        if(resource_array[resource][12] > 0){
            for(res_trait = 0; res_trait < trait_number; res_trait++){
                obs_array[add_obs][res_trait] = 
                    resource_array[resource][res_trait];
            }
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
    PROTECT( NEW_OBSERVATIONS = allocMatrix(REALSXP, new_obs, trait_number) );
    protected_n++;
    
    data_ptr = REAL(NEW_OBSERVATIONS);

    vec_pos = 0;
    for(res_trait = 0; res_trait < trait_number; res_trait++){
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
          
          
          
          
