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
int res_obs(double **obs_array, double *paras, int type1, int type2, int type3){

    int i, j, res_cols, obs_count, obs_rows, obs_cols, t1_col, t2_col, t3_col;
    
    res_cols  = (int) paras[41];
    t1_col    = (int) paras[56];
    t2_col    = (int) paras[57];
    t3_col    = (int) paras[58];
    obs_rows  = (int) paras[61];
    obs_cols  = (int) paras[62];
    
    obs_count = 0;
 
    for(i = 0; i < obs_rows; i++){
        if( (obs_array[i][t1_col] == type1 || obs_array[i][t1_col] < 0) &&
            (obs_array[i][t2_col] == type2 || obs_array[i][t2_col] < 0) &&
            (obs_array[i][t3_col] == type3 || obs_array[i][t3_col] < 0)
        ){
            for(j = res_cols; j < obs_cols; j++){
                obs_count += obs_array[i][j];
            }
        }
    }
    return obs_count;
}

/* =============================================================================
 * This function calculates density-based abundance estimates
 *     obs_array:      The observation array
 *     paras:          A vector of parameters needed to handle the obs_array
 *     agent_array:    Agent array, including managers (agent type 0)
 *     agents:         Total number of agents (rows) in the agents array
 *     abun_est:       Vector where abundance estimates for each type are placed
 *     interact_table: Lookup table to get all types of resource values
 * ========================================================================== */
void dens_est(double **obs_array, double *paras, double **agent_array,
              double *abun_est, int **interact_table){
 
    int i, resource, agents, int_table_rows;
    int view, a_type, land_x, land_y, type1, type2, type3, view_col;
    int area, cells, times_obs, tot_obs, t1_col, t2_col, t3_col;
    double prop_obs, estimate, vision;

    a_type         = (int) paras[7];  /* Type of agent does the observing */
    times_obs      = (int) paras[11];
    land_x         = (int) paras[12];
    land_y         = (int) paras[13];
    agents         = (int) paras[54];
    t1_col         = (int) paras[56];
    t2_col         = (int) paras[57];
    t3_col         = (int) paras[58];
    int_table_rows = (int) paras[60];
    view_col       = (int) paras[67];
    
    view = 0;
    for(i = 0; i < agents; i++){
        if(agent_array[i][1] == a_type){
            view += agent_array[i][view_col];
        }
    }

    vision  = (double) (2 * view) + 1;
    area    = vision * vision * times_obs;
    cells   = land_x * land_y; /* Plus one needed for zero index */
    tot_obs = 0;
    
    if(area > cells){
        area = cells;
    }
    
    for(resource = 0; resource < int_table_rows; resource++){
        abun_est[resource] = 0;
        if(interact_table[resource][0] == 0){ /* Change when turn off type? */
            type1    = interact_table[resource][t1_col];
            type2    = interact_table[resource][t2_col];
            type3    = interact_table[resource][t3_col];
            tot_obs  = res_obs(obs_array, paras, type1, type2, type3);
            prop_obs = (double) tot_obs / area;
            estimate = prop_obs * cells;

            abun_est[resource] = estimate;
            if(resource == 0){
                paras[99]  = abun_est[resource];
            }
        }
    }
}

/* =============================================================================
 * This function calculates RMR (chapman) for one resource type
 *     obs_array:      The observation array
 *     para:           A vector of parameters needed to handle the obs_array
 *     type1:          Resource type 1
 *     type2:          Resource type 2
 *     type3:          Resource type 3
 * ========================================================================== */
double chapman_est(double **obs_array, double *paras, int type1, int type2,
                   int type3){
    
    int row, col, trait_number, obs_array_rows, obs_array_cols;
    int total_marks, recaptures, mark_start, recapture_start;
    int n, K, k, t1_col, t2_col, t3_col;
    double estimate, floored_est;
    
    total_marks     = (int) paras[11];
    trait_number    = (int) paras[41];
    t1_col          = (int) paras[56];
    t2_col          = (int) paras[57];
    t3_col          = (int) paras[58];
    obs_array_rows  = (int) paras[61];
    obs_array_cols  = (int) paras[62];
    recaptures      = (int) paras[102];
    
    mark_start      = trait_number + 1;
    recapture_start = mark_start + 1;
    
    if(total_marks < 1 || recaptures < 1){
        return 0;
    }
    
    n = 0;
    K = 0;
    k = 0;
    for(row = 0; row < obs_array_rows; row++){
        if(obs_array[row][t1_col] == type1 && 
           obs_array[row][t2_col] == type2 &&
           obs_array[row][t3_col] == type3
        ){
            if(obs_array[row][mark_start] > 0){
                n++;
            }
            for(col = recapture_start; col < obs_array_cols; col++){
                if(obs_array[row][col] > 0){
                    K++;
                    if(obs_array[row][mark_start] > 0){
                        k++;
                    }
                    break;
                }
            }
        }
    }

    estimate    = ((n + 1) * (K + 1) / (k + 1)) - 1;
    floored_est = floor(estimate);

    if(type1 == 1 && type2 == 0 && type3 == 0){
        paras[99]  = floored_est;
    }
    
    return floored_est;
}

/* =============================================================================
 * This function calculates mark-recapture-based (Chapman) abundance estimates
 *     obs_array:      The observation array
 *     paras:          A vector of parameters needed to handle the obs_array
 *     abun_est:       Vector where abundance estimates for each type are placed
 *     lookup:         Lookup table to get all types of resource values
 * ========================================================================== */
void rmr_est(double **obs_array, double *paras, double *abun_est, int **lookup){
    
    int resource, type1, type2, type3, t1_col, t2_col, t3_col, int_table_rows;
    double estimate;
    
    t1_col          = (int) paras[56];
    t2_col          = (int) paras[57];
    t3_col          = (int) paras[58];
    int_table_rows  = (int) paras[60];
    
    for(resource = 0; resource < int_table_rows; resource++){
        abun_est[resource] = 0;
        if(lookup[resource][0] == 0){ /* Change when turn off type? */
            type1    = lookup[resource][t1_col];
            type2    = lookup[resource][t2_col];
            type3    = lookup[resource][t3_col];
            estimate = chapman_est(obs_array, paras, type1, type2, type3);
            abun_est[resource] = estimate;
        }
    }
    paras[99] = abun_est[0];
}

/* =============================================================================
 * This function calculates mark-recapture-based (Chapman) abundance estimates
 *     obs_array:      The observation array
 *     paras:          A vector of parameters needed to handle the obs_array
 *     abun_est:       Vector where abundance estimates for each type are placed
 *     lookup:         Lookup table to get all types of resource values
 * ========================================================================== */
void transect_est(double **obs_array, double *paras, double *abun_est, 
                  int **lookup){
    
    int resource, observation, type1, type2, type3, t1_col, t2_col, t3_col;
    int int_table_rows, obs_array_rows, mark_col;
    
    mark_col        = (int) paras[53];
    t1_col          = (int) paras[56];
    t2_col          = (int) paras[57];
    t3_col          = (int) paras[58];
    int_table_rows  = (int) paras[60];
    obs_array_rows  = (int) paras[61];
    
    for(resource = 0; resource < int_table_rows; resource++){
        abun_est[resource] = 0;
        if(lookup[resource][0] == 0){ /* Change when turn off type? */
            type1    = lookup[resource][t1_col];
            type2    = lookup[resource][t2_col];
            type3    = lookup[resource][t3_col];
            for(observation = 0; observation < obs_array_rows; observation++){
                if(obs_array[observation][t1_col] == type1 && 
                   obs_array[observation][t2_col] == type2 && 
                   obs_array[observation][t3_col] == type3
                ){
                    abun_est[resource] += obs_array[observation][mark_col];
                }
                    
            }
        }
    }
    paras[99]   = abun_est[0];
}

/* =============================================================================
 * This function uses the observation array to estimate resource abundances
 *     obs_array:      The observation array
 *     para:           A vector of parameters needed to handle the obs_array
 *     lookup:         Lookup table to get all types of resource values
 *     agent_array:    Agent array, including managers (agent type 0)
 *     abun_est:       Vector where abundance estimates for each type are placed
 * ========================================================================== */
void estimate_abundances(double **obs_array, double *paras, int **lookup,
                         double **agent_array, double *abun_est){
    
    int estimate_type;
    
    estimate_type = (int) paras[8];

    switch(estimate_type){
        case 0:
            dens_est(obs_array, paras, agent_array, abun_est, lookup);
            break;
        case 1:
            rmr_est(obs_array, paras, abun_est, lookup);
            break;
        case 2:
            transect_est(obs_array, paras, abun_est, lookup);
            break;
        case 3:
            transect_est(obs_array, paras, abun_est, lookup);
            break;
        default:
            break;
    }
}

/* =============================================================================
 * This computes a prediction of population trajectory as a linear extrapolation
 *      paras:       A vector of parameters needed 
 * ========================================================================== */
void traj_pred_lin_extrap(double *paras){
  
  int t_s;
  double res_abund, prv_est, var, pred;
  
  res_abund = paras[99];  /* Est. of res type 1 from the observation model */
  prv_est   = paras[129]; /* Previous time step population estimation */
  t_s       = paras[0];   /* What is the current time step? */
  
  if (t_s > 1) {
    var = res_abund - prv_est; /* variation from previous time step */
  
    pred = res_abund + var; /* manager's prediction for next time step population size based on variation */
    /* Could be interesting to make the manager_sense inteviene here */
    
    if (pred < 0) {pred = 0;} /* the prediction cannot be negative */
    
    paras[135] = pred; /* store prediction in paras */
    
    /* paras[129] = res_abund; update memory previous of previous time step estimation */
    
  } else {
    paras[129] = res_abund; /* update memory previous of previous time step estimation */
    
    paras[135] = res_abund; /* prediction is current estimation to avoid pbs at the first time step */
  }
}

/* =============================================================================
 * This function updates the marginal abundances
 *     actions:        The array of the action of agents
 *     abun_est:       Vector where abundance estimates for each type are held
 *     temp_util:      Temporary utilities pulled from actions
 *     marg_util:      The marginal utility of each resource
 *     paras:          A vector of parameter needed
 *     int_d0:         The length of the abun_est, temp_util, & marg_util arrays
 *     a_x:            Number of rows in the actions array
 * ========================================================================== */
void update_marg_util(double ***actions, double *abun_est, double *temp_util, 
                      double *marg_util, double *paras, int int_d0, int a_x){
    
    int row, i, trj_prd;
    
    trj_prd = (int) paras[134]; /* make policy based on prediction rather than on latest observation */
    
    for(row = 0; row < int_d0; row++){
        temp_util[row] = 0;
        marg_util[row] = 0;
        if(actions[row][0][0] < 0 ){
            temp_util[row] = actions[row][4][0];
            if (trj_prd == 0){
              marg_util[row] = temp_util[row] - abun_est[row];
            } else {
              /* printf("ts= %f\t marg_util=%f\t abun_est=%f\t prv_est=%f\t prediction = %f\n", paras[0], marg_util[row], paras[99], paras[129], paras[135]); */
              traj_pred_lin_extrap(paras);
              marg_util[row] = temp_util[row] - paras[135]; /* WILL ONLY WORK WITH TYPE1 RESOURCE */
              /* printf("ts= %f\t marg_util=%f\t abun_est=%f\t prv_est=%f\t prediction = %f\n", paras[0], marg_util[row], paras[99], paras[129], paras[135]); */
            }
        }
    }
    
    if (trj_prd == 1) {paras[129] = abun_est[0];} /* update memory previous of previous time step estimation */
    
    i = 0;
    for(row = 0; row < a_x; row++){
        if(actions[row][0][0] == 1){
            actions[row][4][0] = marg_util[i];
            i++;
        }
    }
}

/* =============================================================================
 * This function uses the observation array to estimate resource abundances
 *      COST:        An array of the cost of actions for each agent
 *      ACTION:      An array of the action of agents
 *      paras:       A vector of parameters needed 
 *      agent_array: Agent array, including managers (agent type 0)
 * ========================================================================== */
void set_action_costs(double ***ACTION, double ***COST, double *paras, 
                      double **agent_array){

    int cost_row, manager_row, type1, type2, type3, layer;
    int interest_num, manID, mlayer, total_layers, t1_col, t2_col, t3_col;
    
    t1_col       = (int) paras[56];
    t2_col       = (int) paras[57];
    t3_col       = (int) paras[58];
    manID        = (int) paras[63];
    mlayer       = (int) paras[64];
    total_layers = (int) paras[65];
    interest_num = (int) paras[66]; /* Note -- only for resources */
    
    for(cost_row = 0; cost_row < interest_num; cost_row++){
        manager_row              = 0;
        type1                    = ACTION[cost_row][t1_col][mlayer];
        type2                    = ACTION[cost_row][t2_col][mlayer];
        type3                    = ACTION[cost_row][t3_col][mlayer];
        while(ACTION[manager_row][0][mlayer] != manID        ||
              ACTION[manager_row][t1_col][mlayer] != type1   ||
              ACTION[manager_row][t2_col][mlayer] != type2   ||
              ACTION[manager_row][t3_col][mlayer] != type3
        ){
            manager_row++;
        }
        for(layer = 0; layer < total_layers; layer++){
            if(agent_array[layer][1] > 0){ /* Managers can't affect self */
                if(paras[88] > 0){
                    COST[cost_row][7][layer]  = ACTION[manager_row][7][mlayer];
                }
                if(paras[89] > 0){
                    COST[cost_row][8][layer]  = ACTION[manager_row][8][mlayer];
                }
                if(paras[90] > 0){
                    COST[cost_row][9][layer]  = ACTION[manager_row][9][mlayer];
                }
                if(paras[91] > 0){
                    COST[cost_row][10][layer] = ACTION[manager_row][10][mlayer];
                }
                if(paras[92] > 0){
                    COST[cost_row][11][layer] = ACTION[manager_row][11][mlayer];
                }
                if(paras[93] > 0){
                    COST[cost_row][12][layer] = ACTION[manager_row][12][mlayer];
                }
            }
        }
    }
}

/* =============================================================================
 * This determines whether action threshold conditions are met (Adrian Bach)
 *      ACTION:      An array of the action of agents
 *      paras:       A vector of parameters needed 
 * ========================================================================== */
void check_action_threshold(double ***ACTION, double *paras){
    
    int m_lyr, act_row, targ_row, t_s, mem;
    double res_abund, target, dev, a_t, prv_est, var, pred, up_bound, lo_bound;
    
    m_lyr     = 0; /* Layer of the manager */ 
    act_row   = 0; /* Row where the actions are */
    targ_row  = 4; /* column where the target is located */
    res_abund = paras[99]; /* Est. of res type 1 from the observation model */
    a_t       = paras[105]; /* Dev est pop from manager target trigger */
    t_s       = (int) paras[0]; /* What is the current time step? */
    prv_est   = paras[129]; /* Previous time step population estimation */
    mem       = (int) paras[130]; /* Do manager memorize the previous pop size estimation? */
    target    = ACTION[act_row][targ_row][m_lyr]; /* Manager's target */

    dev = (res_abund / target) - 1; /* Deviation from manager's target */
    if(dev < 0){ /* Get the absolute value */
        dev = -1 * dev;
    }
    
    if (mem == FALSE) {
        
        /* If the population deviation has hit the threshold, and time step, and that prediction is outside the non-updating band */
        if(dev >= a_t || t_s < 3){ 
            paras[106]  = 1; /* Policy is going to be updated now */
            paras[107]  = 0; /* Zero time steps since last policy update */
        }else{
            paras[106]  = 0; /* Policy is not going to be updated now */
            paras[107] += 1; /* One more time step since the last policy update */
        }
        
    } else {
        
        var = res_abund - prv_est; /* variation from previous time step */

        pred = res_abund + var;    /* manager's prediction for next time step population size based on variation */
        /* Could be interesting to make the manager_sense inteviene here */
        /* And use the absolute value to make the following if statement lighter */
        
        if (pred < 0) {pred = 0;} /* the prediction should not be negative, even if it does not matter here */

        up_bound = (1 + a_t) * target; /* upper bound of the non-updating band */

        lo_bound = (1 - a_t) * target; /* lower bound of the non-updating band */
    
        /* If the population deviation has hit the threshold, and time step, and that prediction is outside the non-updating band */
        if(dev >= a_t || t_s < 3 || pred > up_bound || pred < lo_bound){ 
            paras[106]  = 1; /* Policy is going to be updated now */
            paras[107]  = 0; /* Zero time steps since last policy update */
        }else{
            paras[106]  = 0; /* Policy is not going to be updated now */
            paras[107] += 1; /* One more time step since the last policy update */
        }

        paras[129] = res_abund; /* update memory of previous time step estimation */
    }
}
    
/* =============================================================================
 * This function calculates what the budget bonus should be (Adrian Bach)
 *      agent_array: Agent array, including managers (agent type 0)
 *      paras:       A vector of parameters needed 
 *      agent:       The row in agent_array where the bonus is applied
 * ========================================================================== */
void calc_budget_bonus(double **agent_array, double *paras, int agent){
    
    int budget_col, bonus_col;
    double baseline, budget_bonus, new_bonus;
  
    budget_col   = (int) paras[112];    /* Column where budget is recorded */
    budget_bonus = (double) paras[110]; /* The budget bonus */
    bonus_col    = (int) paras[127];    /* Column where budget bonus is */

    /* The recalculation of the baseline allows the bonus to be cumulative */
    baseline  = agent_array[agent][budget_col];  /* + agent_array[agent][bonus_col] */
    new_bonus = baseline * budget_bonus;

    if( (baseline + new_bonus) < 100000.00 ){
        agent_array[agent][bonus_col] += new_bonus;
    }
}
                         
/* =============================================================================
 * This applies the budget bonus for managers as appropriate (Adrian Bach)
 *      agent_array: Agent array, including managers (agent type 0)
 *      paras:       A vector of parameters needed 
 * ========================================================================== */
void apply_budget_bonus(double **agent_array, double *paras){
    
    int recent_update, N_agents, agent, bonus_col, budget_col, bonus_reset, cost_decrease;
    double a_t, b_b;
    
    N_agents       = (int) paras[54];     /* Total number of agents */
    a_t            = (double) paras[105]; /* Dev est pop target trigger */
    b_b            = (double) paras[110]; /* budget bonus */
    recent_update  = (int) paras[106];    /* Policy recently updated */
    budget_col     = (int) paras[112];    /* Column where budget is recorded */
    bonus_col      = (int) paras[127];    /* Column where budget bonus is */
    bonus_reset    = (int) paras[132];    /* Reset budget bonus when cost decreased? */
    cost_decrease  = (int) paras[133];    /* Have the cost decreased last time step */
       
    if (bonus_reset == 1) {
      if(a_t > 0 && recent_update == 0){ /* If action threshold is being used and policy was not updated last time step*/
          for(agent = 0; agent < N_agents; agent++){
              if(agent_array[agent][1] == 0){ /* if the agent is a manager */
                  calc_budget_bonus(agent_array, paras, agent);
              } 
          }
      }else{
          for(agent = 0; agent < N_agents; agent++){
              if(agent_array[agent][1] == 0){
                  agent_array[agent][bonus_col] = 0.0;
              } 
          }
      }
    } else {
      if(a_t > 0 && recent_update == 0){ /* If action threshold is being used and polcy was not updated last ts */
        for(agent = 0; agent < N_agents; agent++){
          if(agent_array[agent][1] == 0){
            calc_budget_bonus(agent_array, paras, agent);
          } 
        }
      }else{ 
        if (cost_decrease == 0) { /* bonus is reset only if the cost were decreased */
          for(agent = 0; agent < N_agents; agent++){
            if(agent_array[agent][1] == 0){
              agent_array[agent][bonus_col] = 0.0;
            } 
          }
        }
      }
    }
      
    /* store managers budget in paras */
    paras[131] = agent_array[0][budget_col] + agent_array[0][bonus_col];
    
}

/* =============================================================================
 * This increases the managers budget based on the mean yield of users
 *      agent_array: Agent array, including managers (agent type 0)
 *      paras:       A vector of parameters needed    
 * ========================================================================== */
void man_budget_from_yield(double **agent_array, double *paras){   
  
    int agent, N_agents, agent_type, yield_col, y_bonus_col;
    double yield_budget, total_yield, mean_yield, user_count;
  
    N_agents     = (int) paras[54];
    yield_col    = (int) paras[82];
    yield_budget = (double) paras[126]; /* yield_to_budget parameter in paras */
    y_bonus_col  = (int) paras[128];

    /* Calculate mean yield over all users */
    user_count = 0.0;
    for(agent = 0; agent < N_agents; agent++){
        agent_type = agent_array[agent][1];
        if(agent_type == 1) {
            total_yield += agent_array[agent][yield_col]; 
            user_count++;
        }
    }
    
    mean_yield = 0.0;
    if(user_count > 0){ /* Should always be, but to avoid any DIV 0 issues */
        mean_yield = total_yield / user_count;
    }
  
    for(agent = 0; agent < N_agents; agent++){
        agent_type = agent_array[agent][1];                  
        if(agent_type == 0) { 
            agent_array[agent][y_bonus_col] = floor(mean_yield * yield_budget);
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
    int agent_trait;         /* Index for agent traits (cols of agent_array) */
    int res_number;          /* Number of resources included (default = 1) */
    int trait_number;        /* Number of traits included in the resource */
    int agent_number;        /* Number of agents that can potentially observe */
    int agent_traits;        /* Number of traits that each agent has */
    int jacobian_dim;        /* Dimensions of the (square) Jacobian matrix */
    int protected_n;         /* Number of protected R objects */
    int vec_pos;             /* Vector position for making arrays */
    int len_PARAMETERS;      /* Length of the parameters vector */
    int update_policy;       /* If managers act */
    int observe_type;        /* Type of observation being performed */
    int bonus_reset;         /* reset budget bonus to zero when cost decreased ? */
    int trj_prd;             /* Make decision based on prediction? */
    int *dim_RESOURCE;       /* Dimensions of the RESOURCE array incoming */
    int *dim_LANDSCAPE;      /* Dimensions of the LANDSCAPE array incoming */
    int *dim_AGENT;          /* Dimensions of the AGENT array incoming */
    int *dim_COST;           /* Dimensions of the COST array incoming */
    int *dim_ACTION;         /* Dimensions of the ACTION array incoming */
    int *dim_JACOBIAN;       /* Dimensions of the JACOBIAN matrix incoming */
    int *dim_INTERACT;       /* Dimensions of the INTERACT matrix incoming */
    int *dim_OBSERVATION;    /* Dimensions of the OBSERVATION array incoming */
    int **lookup;            /* Lookup table for resource & land interactions */
    double man_yld_budget;   /* Link from mean user yield to manager budget */
    double bb;               /* Budget bonus */
    double prv_cost;         /* Culling cost before calling GA */
    double new_cost;         /* Culling cost after calling GA */
    double save;             /* Variable to save a value */
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
    paras_ptr = REAL(PARAMETERS);
    
    len_PARAMETERS  = GET_LENGTH(PARAMETERS);
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
    } /* ACTION is now stored as actions */
    
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
    
    /* Code below copies the paras vector into C */
    paras   = malloc(len_PARAMETERS * sizeof(double *));
    vec_pos   = 0;
    for(xloc = 0; xloc < len_PARAMETERS; xloc++){
        paras[xloc] = paras_ptr[vec_pos];
        vec_pos++;
    } /* The parameters vector is now copied into C */
    
    /* Do the biology here now */
    /* ====================================================================== */

    abun_est  = malloc(int_d0 * sizeof(double));
    temp_util = malloc(int_d0 * sizeof(double));
    marg_util = malloc(int_d0 * sizeof(double));
    
    observe_type   = (int) paras[8];
    man_yld_budget = (double) paras[126];
    bb             = (double) paras[110];
    bonus_reset    = (int) paras[132];    /* Reset budget bonus when cost decreased? */
    
    /* get the costs from last time step */
    if (bb > 0 && bonus_reset == 0) {
      prv_cost = costs[0][8][1];
    }
    
    apply_budget_bonus(agent_array, paras);
    
    if(man_yld_budget > 0.0){
        count_cell_yield(agent_array, land, paras);
        man_budget_from_yield(agent_array, paras);
    }
        
    if(observe_type >= 0){ /* If less than zero, the above already in actions */
        estimate_abundances(obs_array, paras, lookup, agent_array, abun_est);
        update_marg_util(actions, abun_est, temp_util, marg_util, paras, int_d0, a_x);
    }
    
    check_action_threshold(actions, paras); /* Check whether to act */
    update_policy = paras[106];             /* Will managers act? */
    
    if(update_policy > 0){
        ga(actions, costs, agent_array, resource_array, land, Jacobian_mat, 
           lookup, paras, 0, 1);
    }
    
    set_action_costs(actions, costs, paras, agent_array);
    
    /* get the costs after the update */
    if (bb > 0 && bonus_reset == 0) {
     new_cost = costs[0][8][1]; 
     if (prv_cost - new_cost > 0) {
       paras[133] = 1;
     } else {
       paras[133] = 0;
     }
    }

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
    /* Free all of the allocated memory used in the observation array */
    for(row = 0; row < obs_d0; row++){
        free(obs_array[row]);
    }
    free(obs_array);
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
