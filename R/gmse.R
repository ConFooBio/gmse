#' General function for now
#'
#' Function to run G-MSE model
#'@param
#'@return
#'@useDynLib GMSE
#'@export
gmse <- function( time_max       = 100,   # Max number of time steps in sim
                  land_dim_1     = 100,   # x dimension of the landscape
                  land_dim_2     = 100,   # y dimension of the landscape
                  res_movement   = 4,     # How far do resources move
                  remove_pr      = 0.0,   # Density independent resource death
                  lambda         = 0.25,  # Resource growth rate
                  agent_view     = 20,    # Number cells agent view around them
                  agent_move     = 50,    # Number cells agent can move
                  res_birth_K    = 10000, # Carrying capacity applied to birth
                  res_death_K    = 400,   # Carrying capacity applied to death
                  edge_effect    = 1,     # What type of edge on the landscape
                  res_move_type  = 2,     # What type of movement for resources
                  res_birth_type = 2,     # What type of birth for resources
                  res_death_type = 2,     # What type of death for resources
                  observe_type   = 0,     # Type of observation used
                  fixed_observe  = 20,    # How many obs (if type = 1)
                  times_observe  = 40,    # How many times obs (if type = 0)
                  obs_move_type  = 1,     # Type of movement for agents
                  res_min_age    = 1,     # Minimum age recorded and observed
                  res_move_obs   = TRUE,  # Move resources while observing
                  Euclidean_dist = FALSE, # Use Euclidean distance in view
                  plotting       = TRUE,  # Plot the results
                  hunt           = FALSE, # Does the user hunt resources?
                  start_hunting  = 0,     # What generation hunting starts
                  res_consume    = 0.5,   # Pr. landscape cell consumed by res
                  ga_popsize     = 100,   # Pop size in genetic algorithm
                  ga_mingen      = 20,    # Minimum generations in a ga run
                  ga_seedrep     = 20,    # How many copies to seed a ga with
                  ga_sampleK     = 20,    # Random sample size in ga tournament
                  ga_chooseK     = 2,     # Select from sample in ga tournament
                  ga_mutation    = 0.1,   # Mutation rate in genetic algorithm
                  ga_crossover   = 0.1,   # Crossover rate in genetic algorithm
                  move_agents    = TRUE,  # Move agents once per time step
                  max_ages       = 5,     # Maximum age of any resource(s)
                  minimum_cost   = 10,    # Minimum cost value
                  user_budget    = 1000,  # What is the budget of a user
                  manager_budget = 1000,  # The budget of a manager
                  manage_target  = 200,   # The target resource abundance
                  RESOURCE_ini   = 100,   # Number of initial resources
                  scaring        = FALSE, # Scaring allowed in simulations
                  culling        = TRUE,  # Culling/hunting allowed
                  castration     = FALSE, # Castration allowed
                  feeding        = FALSE, # Feeding resources allowed
                  help_offspring = FALSE, # Helping offspring allowed
                  tend_crops     = FALSE, # Tending crops allowed
                  kill_crops     = FALSE, # Killing crops allowed
                  stakeholders   = 4,     # Number of stake-holders
                  manage_caution = 1,     # Caution rate of the manager
                  land_ownership = FALSE, # Do stake-holders act on their land?
                  manage_freq    = 1,     # Frequency that management enacted
                  converge_crit  = 100    # Convergence criteria
){
    
    if(observe_type == 1 & times_observe < 2){
        stop("Need to observe at least twice for mark-recapture");   
    }
    
    user_res_opts  <- c(scaring, culling, castration, feeding, help_offspring);
    user_lnd_opts  <- c(tend_crops, kill_crops);
    pop_model      <- "IBM";
    movement       <- res_movement;
    res_types_ini  <- 1;
    time           <- 0;
    
    proc_start       <- proc.time();
    proc_check_start <- proc_start;
    
    # Set the landscape
    land_alloc   <- c(0, rep(x = 1/stakeholders, times = stakeholders));
    LANDSCAPE_r  <- make_landscape( model       = pop_model, 
                                    rows        = land_dim_1, 
                                    cols        = land_dim_2, 
                                    cell_types  = 2,
                                    cell_val_mn = 1,
                                    cell_val_sd = 0,
                                    ownership   = 1:(stakeholders + 1),
                                    owner_pr    = land_alloc
    );
    # Set the starting conditions for one resource
    starting_resources <- make_resource( model              = pop_model, 
                                         resource_quantity  = RESOURCE_ini,
                                         resource_types     = res_types_ini,
                                         rows               = land_dim_1,
                                         cols               = land_dim_2,
                                         move               = movement,
                                         rm_pr              = remove_pr,
                                         lambda             = lambda,
                                         consumption_rate   = res_consume,
                                         max_age            = max_ages[1]
    );
    # This will obviously need to be changed -- new function in initialise.R
    AGENTS   <- make_agents( model        = pop_model,
                             agent_number = 1 + stakeholders,
                             type_counts  = c(1, stakeholders),
                             vision       = agent_view,
                             rows         = land_dim_1,
                             cols         = land_dim_2,
                             move         = agent_move
    ); 
    
    Jacobian <- make_interaction_array(RESOURCES = starting_resources,
                                       LAND      = LANDSCAPE_r
    );
    Jacobian[1,2] <- -1 * res_consume; # Temporary to fix consumption rate
    
    interaction_tabl <- make_interaction_table(starting_resources, LANDSCAPE_r);
    
    COST   <- make_costs( AGENTS = AGENTS, RESOURCES = starting_resources,
                          res_opts = user_res_opts, lnd_opts = user_lnd_opts,
                          min_cost = minimum_cost);
    ACTION <- make_utilities( AGENTS = AGENTS, RESOURCES = starting_resources);
    if(land_ownership == TRUE){
        ACTION[1,5:7,] <- 1;
        ACTION[2,5:7,] <- 1;
    }
    ACTION[3,5:7,1]  <- 0;
    ACTION[1,5,2:5]  <- 0;
    ACTION[1,5,1]    <- 100;
    ACTION[2,5,2:5]  <- 100;
    ACTION[2,5,3]    <- 100;
    ACTION[1,5,1]    <- manage_target;

    AGENTS[,17]     <- user_budget;
    AGENTS[1,17]    <- manager_budget;
    
    time       <- time + 1;  # Ready for the initial time step.
    cells      <- land_dim_1 * land_dim_2; # Number of cells in the landscape
    
    ldx <- land_dim_1;
    ldy <- land_dim_2;
    ldz <- dim(LANDSCAPE_r)[3];
    rbK <- res_birth_K;
    rdK <- res_death_K;
    edg <- edge_effect;
    r_m <- res_move_type;
    rbt <- res_birth_type;
    rdt <- res_death_type;
    obt <- observe_type;
    fxo <- fixed_observe;
    tmo <- times_observe;
    o_m <- obs_move_type;
    rma <- res_min_age;
    rmo <- res_move_obs;
    Euc <- Euclidean_dist;
    prc <- res_consume;
    gap <- ga_popsize;
    gam <- ga_mingen;
    gas <- ga_seedrep;
    gal <- ga_sampleK;
    gac <- ga_chooseK;
    gmu <- ga_mutation;
    gcr <- ga_crossover;
    mva <- move_agents;
    mxa <- max_ages;
    rsi <- dim(starting_resources)[1];
    ttr <- dim(starting_resources)[2];
    agn <- dim(AGENTS)[1];
    agt <- dim(AGENTS)[2];
    lkr <- dim(interaction_tabl)[1];
    lyr <- dim(ACTION)[3];
    roc <- dim(ACTION)[1];
    coc <- dim(ACTION)[2];
    aav <- user_res_opts;
    alv <- user_lnd_opts;
    mnc <- minimum_cost;
    usb <- user_budget;
    mac <- manage_caution;
    cnv <- converge_crit;

    paras <- c(time,    # 0. The dynamic time step for each function to use 
               edg,     # 1. The edge effect (0: nothing, 1: torus)
               r_m,     # 2. Res movement (0: none, 1: unif, 2: Poisson, ...)
               rbt,     # 3. Type of birth (0: none, 1: uniform, 2: Poisson)
               rdt,     # 4. Type of death (0: none, 1: uniform, 2: K-based)
               rbK,     # 5. Carrying capacity for birth (-1 = unregulated)
               rdK,     # 6. Carrying capacity for death (-1 = unregulated)
               0,       # 7. The type of AGENT doing the observations
               obt,     # 8. The type of observing done for estimating pop.
               1,       # 9. The type of resource observed (note: dynamic)
               fxo,     # 10. Fix mark? Do observers mark exactly n resources?
               tmo,     # 11. Times resources observed during one time step
               ldx,     # 12. Land dimension on the x axis
               ldy,     # 13. Land dimension on the y axis
               o_m,     # 14. Agent movement (option same as #2)
               1,       # 15. Type category for resource observation
               rma,     # 16. Minimum age of sampling (1 excludes juveniles)
               1,       # 17. Type category for agent observation (default = 1)
               12,      # 18. Column where res seen recorded in agent array
               rmo,     # 19. Move resources while observing (0/1 = N/Y)
               Euc,     # 20. Distance is Euclidean (1) or within-cell (0)
               gap,     # 21. Population size set in the genetic algorithm
               gam,     # 22. Minimum number of generations to run a ga
               gas,     # 23. Number of replicate agents to seed a ga with
               gal,     # 24. The number of sample agents for tournament in ga
               gac,     # 25. The number of selected agents in a ga tournament
               gmu,     # 26. The mutation rate of loci in the genetic algorithm
               gcr,     # 27. The crossover rate in the genetic algorithm
               mva,     # 28. Move agents once per time step
               mxa,     # 29. Maximum age of resources
               7,       # 30. The column of the time trait in the resource array
               11,      # 31. The column for storing age in the resource array
               rsi,     # 32. The number of resources in the model
               4,       # 33. The column for resource x location on landscape
               5,       # 34. The column for resource y location on landscape
               6,       # 35. The column for the movement parameter for resource
               ldz,     # 36. The number of layers (3D depth) in the landscape
               9,       # 37. The column in resource array for growth parameter
               10,      # 38. The column in the resource array for offspring
               18,      # 39. The column to adjust the growth rate helpem
               19,      # 40. The column to adjust the offspring feedem
               ttr,     # 41. Total columns in the resource array
               17,      # 42. The column to adjust the death resource column
               8,       # 43. The column in resource array affecting removal
               1,       # 44. A column to refer to a resource type as needed
               1,       # 45. A type of resource to do interacting with land
               15,      # 46. The column in a resource array affected by land
               14,      # 47. The column in resource array of land effect size
               1,       # 48. The landscape layer interacting with a resource
               4,       # 49. The column for the agent's x location on landscape
               5,       # 50. The column for the agent's y location on landscape
               6,       # 51. The column for the movement parameter for agents
               10,      # 52. The column in agent array where marks accumulate
               12,      # 53. The column in resource array where marks accrue
               agn,     # 54. The total number of agents in the model
               agt,     # 55. The total number of agent traits (cols)
               1,       # 56. The column of resource type 1
               2,       # 57. The column of resource type 2
               3,       # 58. The column of resource type 3
               13,      # 59. The tally column of the resource array
               lkr,     # 60. The number of rows in the lookup array
               rsi,     # 61. The number of rows in the observation array
               ttr+tmo, # 62. The number of cols int he observation array
               1,       # 63. The ID of the managing agent (usually 1)
               0,       # 64. The layer of ACTION and COST where manager located
               lyr,     # 65. The number of layers in ACTION and COST arrays
               lkr - 1, # 66. The number of rows for setting action costs
               8,       # 67. The view column of the agent array
               roc,     # 68. The number of rows in the COST and ACTION arrays
               coc,     # 69. The number of cols in the COST and ACTION arrays
               4,       # 70. Col actions vary for other individuals in ga
               7,       # 71. Col actions vary for self individuals in ga
               0,       # 72. Total actions in the action array
               16,      # 73. The column to adjust the castration of a resource
               0,       # 74. Manager's projected change if resource moved
               -0.10,   # 75. Manager's projected change if resource killed
               -0.10,   # 76. Manager's projected change if resource castrated
               0.10,    # 77. Manager's projected change if resource growth +
               0.10,    # 78. Manager's projected change if resource offspring +
               0.00,    # 79. User's improvement of land (proportion)
               1,       # 80. Landscape layer on which crop yield is located
               2,       # 81. Landscape layer on which ownership is defined
               15,      # 82. Column in agent array where cell yield recorded
               0,       # 83. Temporary element to hold action array row
               0,       # 84. Temporary element to hold action array col
               0,       # 85. Temporary element to hold action array layer
               0,       # 86. Effect of landscape cell on resource growth rate
               0,       # 87. Effect of landscape cell on resource death rate
               aav[1],  # 88. Is the movement option available?
               aav[2],  # 89. Is the killing option available?
               aav[3],  # 90. Is the castration option available?
               aav[4],  # 91. Is the feedem option available?
               aav[5],  # 92. Is the helpem option availabe?
               alv[1],  # 93. Is the kill crop production option available?
               alv[2],  # 94. Is the increase crop growth option available?
               mac,     # 95. How many actions should managers assume exist?
               mnc,     # 96. What is the minimum cost for any action?
               usb,     # 97. The user budget
               cnv,     # 98. The convergence criteria of the genetic algorithm
               rsi,     # 99. Estimate of res type 1 from the observation model
               0,       # 100. Upper CI for res type 1 estimate
               0        # 101. Lower CI for res type 1 estimate
    );
    RESOURCE_REC    <- NULL;
    RESOURCES       <- starting_resources;
    OBSERVATION_REC <- NULL;
    AGENT_REC       <- NULL;
    LANDSCAPE_INI   <- LANDSCAPE_r;
    LANDSCAPE_REC   <- NULL;
    COST_REC        <- NULL;
    ACTION_REC      <- NULL;
    PARAS_REC       <- matrix(data=0, ncol = length(paras), nrow = time_max-1);
    
    print("Initialising simulations ... ");
    
    while(time < time_max){
        
        AGENTS[1,5] <- sample(x = 1:ldx, size = 1); # Move manager randomly
        AGENTS[1,6] <- sample(x = 1:ldy, size = 1); 
        
        RESOURCE_NEW      <- resource(RESOURCES  = RESOURCES,
                                      LAND       = LANDSCAPE_r,
                                      PARAS      = paras,
                                      model      = "IBM"
        ); 
        RESOURCES             <- RESOURCE_NEW[[1]];
        LANDSCAPE_r           <- RESOURCE_NEW[[2]];
        paras                 <- RESOURCE_NEW[[3]];
        
        OBSERVATION_NEW   <- observation(RESOURCES  = RESOURCES,
                                         LAND       = LANDSCAPE_r,
                                         PARAS      = paras,
                                         AGENTS     = AGENTS,
                                         inter_tabl = interaction_tabl,
                                         fix_mark   = fxo,
                                         times      = tmo,
                                         samp_age   = rma,
                                         agent_type = 0,
                                         type_cat   = 1,
                                         obs_method = obt,
                                         move_res   = rmo
        );
        OBSERVATION_r  <- OBSERVATION_NEW[[1]];
        paras          <- OBSERVATION_NEW[[3]];
        
        AGENTS_NEW        <- anecdotal(RESOURCES   = RESOURCES,
                                       LAND        = LANDSCAPE_r,
                                       PARAS       = paras,
                                       AGENTS      = AGENTS,
                                       res_type    = 1,
                                       samp_age    = rma,
                                       agent_type  = -1,
                                       type_cat    = 1,
                                       move_agents = mva
        );
        AGENTS <- AGENTS_NEW[[1]];

        if(time %% manage_freq == 0){
            MANAGER  <- manager(RESOURCES   = RESOURCES,
                                AGENTS      = AGENTS,
                                LAND        = LANDSCAPE_r, 
                                PARAS       = paras,
                                COST        = COST,
                                ACTION      = ACTION,
                                INTERACT    = Jacobian,
                                inter_tabl  = interaction_tabl,
                                OBSERVATION = OBSERVATION_r,
                                model       = "IBM"
            );
            ACTION <- MANAGER[[4]];
            COST   <- MANAGER[[5]];
        }
        
        USERS <- user(RESOURCES  = RESOURCES,
                      AGENTS     = AGENTS,
                      LAND       = LANDSCAPE_r, 
                      PARAS      = paras,
                      COST       = COST,
                      ACTION     = ACTION,
                      INTERACT   = Jacobian,
                      inter_tabl = interaction_tabl,
                      model      = "IBM"
        );
        RESOURCES    <- USERS[[1]];
        AGENTS       <- USERS[[2]];
        LANDSCAPE_r  <- USERS[[3]];
        ACTION       <- USERS[[4]];
        COST         <- USERS[[5]];
        
        RESOURCE_REC[[time]]     <- RESOURCES;
        OBSERVATION_REC[[time]]  <- OBSERVATION_NEW[[1]];
        AGENT_REC[[time]]        <- AGENTS;
        LANDSCAPE_REC[[time]]    <- LANDSCAPE_r[,,2];
        COST_REC[[time]]         <- COST;
        ACTION_REC[[time]]       <- ACTION;
        PARAS_REC[time,]         <- paras;
        
        LANDSCAPE_r <- age_land(LAND          = LANDSCAPE_r, 
                                landscape_ini = LANDSCAPE_INI, layer = 2);
        
        time              <- time + 1;
        paras[1]          <- time;
        if(dim(RESOURCES)[1] < 10){
            print("Extinction has occurred");
            break;
        }
        proc_check_end  <- proc.time();
        time_taken      <- proc_check_end - proc_check_start;
        if(time_taken[3] > 5){
            print(paste("Generation ", time, "of ", time_max));
            proc_check_start <- proc.time();
        }

        if(hunt == TRUE & time > start_hunting){
            HUNT_OUTCOME <- be_hunter(OBSERVATION_r, AGENTS, RESOURCES, 
                                      LANDSCAPE_r, paras, agent_view,
                                      times_observe);
            RESOURCES    <- HUNT_OUTCOME$RESOURCES;
            paras        <- HUNT_OUTCOME$PARAS;
        }
    }
    
    res_columns <- c("Resource_ID",
                     "Resource_type_1",
                     "Resource_type_2",
                     "Resource_type_3",
                     "Resource_loc_x",
                     "Resource_loc_y",
                     "Resource_move",
                     "Resource_time",
                     "Resource_rm_pr",
                     "Resource_growth",
                     "Resource_grown",
                     "Resource_age",
                     "Resource_marked",
                     "Resource_tally",
                     "Consumption_rate",
                     "Adjust_removal",
                     "Adjust_growth",
                     "Adjust_grown",
                     "temp_adj1",
                     "temp_adj2"
    );
    colnames(RESOURCES)    <- res_columns;
    
    proc_end   <- proc.time();
    total_time <- proc_end - proc_start;
       
    sim_results <- list(resource    = RESOURCE_REC,
                        observation = OBSERVATION_REC,
                        paras       = PARAS_REC,
                        land        = LANDSCAPE_REC,
                        time_taken  = time_taken,
                        agents      = AGENTS,
                        cost        = COST_REC,
                        action      = ACTION_REC
                        );
    
    if(plotting == TRUE){
        if(obt == 0){
            case01plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       paras  = paras,
                       ACTION = ACTION_REC,
                       COST   = COST_REC,
                       view   = agent_view,
                       times  = times_observe);
        }
        if(obt == 1){
            case01plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1],
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       ACTION = ACTION_REC,
                       COST   = COST_REC,
                       paras  = paras);
        }
        if(obt == 2){
            case23plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       ACTION = ACTION_REC,
                       paras  = paras);
        }
        if(obt == 3){
            case23plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       ACTION = ACTION_REC,
                       paras  = paras);
        }
    }

    return(sim_results);
}
################################################################################
