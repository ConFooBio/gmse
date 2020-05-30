#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

/* =============================================================================
 * This function checks to see if a resource is of the correct type combination
 * ========================================================================== */
int get_rand_int(int from, int to){
    
    int rand_value;
    
    do{
        rand_value = (int) floor( runif(from, to) );
    }while(rand_value == to  );
    
    return rand_value;
}

/* =============================================================================
 * This function checks to see if a resource is of the correct type combination
 * ========================================================================== */
void is_on_owner_land(int res_number, double **resources, int owner,
                      double ***land, int *ident_vector){

    int resource, xloc, yloc, cell;
    
    for(resource = 0; resource < res_number; resource++){
        xloc = (int) resources[resource][4];
        yloc = (int) resources[resource][5];
        cell = (int) land[xloc][yloc][2];
        if(cell == owner){
            ident_vector[resource] = 1;
        }else{
            ident_vector[resource] = 0;   
        }
    }
}

/* =============================================================================
 * This function checks to see if a resource is of the correct type combination
 * ========================================================================== */
void is_correct_type(int res_number, double **resources, int type1, int type2, 
                     int type3, int *ident_vector){
    
    int resource;
    
    for(resource = 0; resource < res_number; resource++){
        if(resources[resource][1] == type1 &&
           resources[resource][2] == type2 &&
           resources[resource][3] == type3
          ){
            ident_vector[resource] = 1;
        }else{
            ident_vector[resource] = 0;   
        }
    }
}

/* =============================================================================
 * Find the descending order of positions in an array of length 'length'
 * ========================================================================== */
void find_descending_order(int *order_array, double *by_array, int length){
    int i, k, max_index;
    double max_val, min_val;
    
    k = 0;
    min_val = 0;
    for(i = 0; i < length; i++){
        if(by_array[i] < min_val){
            min_val = by_array[i];
        }
    }
    while(k < length){
        max_val   = min_val - 1;
        max_index = 0;
        for(i = 0; i < length; i++){
            if(by_array[i] > max_val){
                max_index = i;
                max_val   = by_array[i];
            }
        }
        by_array[max_index] = min_val - 1;
        order_array[k]      = order_array[max_index]; 
        k++;   
    }
}

/* =============================================================================
 * Swap pointers to rewrite ARRAY_B into ARRAY_A for a an array of any dimension
 * ========================================================================== */
void swap_arrays(void **ARRAY_A, void **ARRAY_B){

    void *TEMP_ARRAY;

    TEMP_ARRAY = *ARRAY_A;
    *ARRAY_A   = *ARRAY_B;
    *ARRAY_B   = TEMP_ARRAY;
}

/* =============================================================================
 * This function applies the edge effect during movement
 * ========================================================================== */
int edge_effect(int pos, int edge_1, int edge_2, int edge_type){
    if(pos >= edge_2 || pos < edge_1){ /* If off the edge */
        switch(edge_type){
            case 0: /* Nothing happens (effectively, no edge) */
                break;
            case 1: /* Corresponds to a torus landscape */
                while(pos >= edge_2){
                    pos = pos - edge_2;   
                }
                while(pos < edge_1){
                    pos = pos + edge_2;   
                }
                break;
            default:
                while(pos >= edge_2){
                    pos = pos - edge_2;   
                }
                while(pos < edge_1){
                    pos = pos + edge_2;   
                }
                break;
        }
    }
    return pos;
}

/* =============================================================================
 * This function returns a random value of -1 or 1 with a 50:50 probability
 * ========================================================================== */
int rand_dir(void){
    int move_dir;
    double rand_num;
    
    move_dir = 1;
    rand_num = 0.5;
    do{ /* Note that rand_num can never be exactly 0.5 */
        rand_num = runif(0, 1);
    } while(rand_num == 0.5);
    if(rand_num > 0.5){
        move_dir = -1;   
    }
    
    return move_dir;
}

/* =============================================================================
 * This function moves a value from 0 to max_move left or right with uniform pr
 * ========================================================================== */
int unif_move(int max_move){
    int raw_move, move_len, move_dir, the_move;
    double rand_uni;
    
    do{ /* Again, so that res_num never moves too far */
        rand_uni = runif(0, 1);
    } while(rand_uni == 1.0);
    
    raw_move = rand_uni * max_move;
    move_len = (int) floor(raw_move);
    move_dir = (int) rand_dir();
    the_move = move_dir * move_len;
    
    return the_move;
}

/* =============================================================================
 * This function moves a value sampled from a poisson distribution
 * ========================================================================== */
int pois_move(double move_para){
    int raw_move, move_len, the_move, move_dir;
    
    raw_move = (int) rpois(move_para);
    move_len = (int) floor(raw_move);
    move_dir = (int) rand_dir();
    the_move = move_dir * move_len;
    
    return the_move;
}

/* =============================================================================
 * This function moves a uniform a poisson number of times
 * ========================================================================== */
int unif_pois_move(double move_para){
    int raw_move, move_len, move_dir;
    double rand_uni;
    
    raw_move  = 0;
    while(move_para > 0){
        do{
            rand_uni = runif(0, 1);
        } while(rand_uni == 1.0);
        move_dir  = (int) rand_dir();
        move_len  = (int) rand_uni * (move_para + 1);
        raw_move += move_dir * move_len;
        move_para--;
    }

    return raw_move;
}

/* =============================================================================
 * This function moves individuals on the landscape according to some rules
 * The 'edge_eff' argument defines what happens at the landscape edge:
 *     0: Nothing happens (individual is just off the map)
 *     1: Torus landscape (individual wraps around to the other side)
 * The 'type' argument defines the type of movement allowed:
 *     0: No movement is allowed
 *     1: Movement is random uniform from zero to move_para in any direction
 *     2: Movement length is poisson(move_para) in x then y direction
 *     3: Movement length is poisson(move_para) in any direction
 * ========================================================================== */
void res_mover(double **res_moving, double ***landscape, double *paras){
    
    int edge_eff, type, land_x, land_y, resource_number, xloc, yloc, move_para;
    int max_move, move_x, move_y, new_pos_x, new_pos_y;
    int resource;     /* Resource number index                        */
    int move_len;     /* Length of a move                             */

    edge_eff        = (int) paras[1];
    type            = (int) paras[2];
    land_x          = (int) paras[12];
    land_y          = (int) paras[13];
    resource_number = (int) paras[32];
    xloc            = (int) paras[33];
    yloc            = (int) paras[34];
    move_para       = (int) paras[35];
    move_len        = 0;

    for(resource=0; resource < resource_number; resource++){
        new_pos_x  = (int) res_moving[resource][xloc];
        new_pos_y  = (int) res_moving[resource][yloc];
        move_x     = 0;
        move_y     = 0;
        switch(type){
            case 0: /* No change in position */
                break;
            case 1: /* Uniform selection of position change */
                max_move = res_moving[resource][move_para] + 1;
                move_x   = unif_move(max_move);
                move_y   = unif_move(max_move);
                break;
            case 2: /* Poisson selection of position change */
                move_x = pois_move(res_moving[resource][move_para]);    
                move_y = pois_move(res_moving[resource][move_para]);
                break;
            case 3: /* Uniform position movement a Poisson number of times */
                move_len = rpois(res_moving[resource][move_para]);
                move_x   = unif_pois_move(move_len);
                move_y   = unif_pois_move(move_len);
                break;
            default:
                break;
        }
        new_pos_x = (int) new_pos_x + move_x;
        new_pos_y = (int) new_pos_y + move_y;
        if(new_pos_x >= land_x || new_pos_x < 0){ /* If off the edge */
            new_pos_x = edge_effect(new_pos_x, 0, land_x, edge_eff);
        }
        if(new_pos_y >= land_y || new_pos_y < 0){ /* If off the edge */
            new_pos_y = edge_effect(new_pos_y, 0, land_y, edge_eff);
        }
        res_moving[resource][xloc] = new_pos_x;
        res_moving[resource][yloc] = new_pos_y;
    }
}

/* =============================================================================
 * This function moves a single resource on the landscape
 * The 'edge_eff' argument defines what happens at the landscape edge:
 *     0: Nothing happens (individual is just off the map)
 *     1: Torus landscape (individual wraps around to the other side)
 * The 'type' argument defines the type of movement allowed:
 *     0: No movement is allowed
 *     1: Movement is random uniform from zero to move_para in any direction
 *     2: Movement length is poisson(move_para) in x then y direction
 *     3: Movement length is poisson(move_para) in any direction
 * ========================================================================== */
void move_a_resource(double **res_moving, double ***landscape, double *paras,
                     int resource){
    
    int edge_eff, type, land_x, land_y, xloc, yloc, move_para;
    int max_move, move_x, move_y, new_pos_x, new_pos_y;
    int move_len;     /* Length of a move                             */
    int move_dir;     /* Move direction (-1 or 1)                     */
    int new_pos;      /* New position: check if over landscape edge   */
    double rand_num;  /* Random number used for sampling              */
    double rand_uni;  /* Random uniform number                        */
    double rand_pois; /* Random poisson number                        */
    double raw_move;  /* Movement length before floor() truncation    */

    edge_eff        = (int) paras[1];
    type            = (int) paras[2];
    land_x          = (int) paras[12];
    land_y          = (int) paras[13];
    xloc            = (int) paras[33];
    yloc            = (int) paras[34];
    move_para       = (int) paras[35];
    move_len        = 0;

    new_pos_x  = (int) res_moving[resource][xloc];
    new_pos_y  = (int) res_moving[resource][yloc];
    move_x     = 0;
    move_y     = 0;
    switch(type){
        case 0: /* No change in position */
            break;
        case 1: /* Uniform selection of position change */
            max_move = res_moving[resource][move_para] + 1;
            move_x   = unif_move(max_move);
            move_y   = unif_move(max_move);
            break;
        case 2: /* Poisson selection of position change */
            move_x = pois_move(res_moving[resource][move_para]);    
            move_y = pois_move(res_moving[resource][move_para]);
            break;
        case 3: /* Uniform position movement a Poisson number of times */
            move_len = rpois(res_moving[resource][move_para]);
            move_x   = unif_pois_move(move_len);
            move_y   = unif_pois_move(move_len);
            break;
        default:
            break;
    }
    new_pos_x = (int) new_pos_x + move_x;
    new_pos_y = (int) new_pos_y + move_y;
    if(new_pos_x >= land_x || new_pos_x < 0){ /* If off the edge */
    new_pos_x = edge_effect(new_pos_x, 0, land_x, edge_eff);
    }
    if(new_pos_y >= land_y || new_pos_y < 0){ /* If off the edge */
    new_pos_y = edge_effect(new_pos_y, 0, land_y, edge_eff);
    }
    res_moving[resource][xloc] = new_pos_x;
    res_moving[resource][yloc] = new_pos_y;
}

/* =============================================================================
 * This function counts the number of cells owned by each agent and adds the
 * number to a column of the agent array.
 * Inputs include:
 *     landscape: the landscape on which cells are located
 *     paras: Vector of global parameters used in the model
 *     agent_array: The array of agents in the model (manager and users)
 *     land_x: The x dimension of the landscape
 *     land_y: The y dimension of the landscape
 *     agent_number: How many agents there are in the agent array
 * ========================================================================== */
void count_owned_cells(double ***landscape, double *paras, double **agent_array, 
                       int land_x, int land_y, int agent_number){
    
    int xloc, yloc, own, tcol, agent, own_val, age_row;
    
    land_x  = (int) paras[12];
    land_y  = (int) paras[13];
    own     = (int) paras[81];
    tcol    = (int) paras[120];

    for(agent = 0; agent < agent_number; agent++){
        agent_array[agent][tcol] = 0; /* Clear out the owned cell column */
    }
    for(xloc = 0; xloc < land_x; xloc++){
        for(yloc = 0; yloc < land_y; yloc++){
            own_val = (int) landscape[xloc][yloc][own];
            if(own_val >= 0){
                age_row = own_val - 1; /* Assumes ID is 1 + agent row */
                agent_array[age_row][tcol]++;
            }
        }
    }
}

/* =============================================================================
 * This function counts the cell yield on a landscape layer
 * Inputs include:
 *     agent_array: The array of agents
 *     land: The landscape array
 *     paras: Vector of global parameters used in the model
 * ========================================================================== */
void count_cell_yield(double **agent_array, double ***land, double *paras){
    
    int land_x, land_y, agent_number, yield_layer, own_layer, yield_column;
    int xpos, ypos, agent, agent_ID;
    double agent_yield;
    
    land_x       = (int) paras[12];
    land_y       = (int) paras[13];
    agent_number = (int) paras[54];
    yield_layer  = (int) paras[80];
    own_layer    = (int) paras[81];
    yield_column = (int) paras[82];
    
    for(agent = 0; agent < agent_number; agent++){
        agent_ID    = agent_array[agent][0];
        agent_yield = 0.0; 
        for(xpos = 0; xpos < land_x; xpos++){
            for(ypos = 0; ypos < land_y; ypos++){
                if(land[xpos][ypos][own_layer] == agent_ID){
                    agent_yield += land[xpos][ypos][yield_layer];    
                }
            }
        }
        agent_array[agent][yield_column] = agent_yield; 
    }
}
/* ===========================================================================*/
