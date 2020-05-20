#' Resource initialisation
#'
#' Initialise the resources of the G-MSE model.
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param resource_quantity The total number of resources being initialised (e.g., the population size of the resource at the first time step)
#'@param resource_types The number of resource types that exist. Currently, only one resource type is recommended, but future versions of GMSE will include multiple resource types if requested
#'@param rows The number of rows (y-axis) on the simulated landscape; resources are randomly placed somewhere on the landscape array
#'@param cols The number of columns (x-axis) on the simulated landscape; resources are randomly placed somewhere on the landscape array
#'@param move This parameter affects the movement of resources each time step. There are multiple types of movement (see res_move_type in the gmse function), but this parameter determines the distance in cells that a resource will move
#'@param rm_pr This parameter sets the baseline probability of resource removal (death) per time step; this probability can be affected by user actions or carrying capacity, so a probability of zero does not ensure that resources will necessarily persist until the end of the simulation
#'@param lambda This is the parameter for Poisson random sampling affecting birthrate; each resource gives birth to Poisson(lambda) offspring in the resource model
#'@param consumption_rate Rate at which resource consumes crops on landscape; consumption affects the landscape by decreasing values on the landscape array (which may, e.g., be interpreted as crop production being decreased), and might also affect resource demographic parameters depending on other global options set in GMSE
#'@param max_age Maximum age allowed for a resource to be (in time steps)
#'@param times_feeding Number of times a resource moves on a landscape during a single time step in search of food
#'@return the_resources Initialised data frame of resources being modelled
#'@examples
#'resource <- make_resource(model = "IBM", resource_quantity = 100, 
#'resource_types = 1, rows = 100, cols = 100, move = 1, rm_pr = 0, lambda = 0, 
#'consumption_rate = 0.5, max_age = 5);
#'@export
make_resource <- function(model              = "IBM", 
                          resource_quantity  = 100, 
                          resource_types     = 1, 
                          rows               = 100, 
                          cols               = 100, 
                          move               = 1, 
                          rm_pr              = 0,
                          lambda             = 0,
                          consumption_rate   = 0.1,
                          max_age            = 5,
                          times_feeding      = 1
                          ){
    the_resource   <- NULL;
    if(length(consumption_rate) != resource_types){
        stop("Need consumption_rate for each resource_type")
    }
    if(model == "IBM"){
        IDs      <- seq(from = 1, to = resource_quantity, by = 1);
        type1    <- sample(x = 1:resource_types, size = resource_quantity,
                           replace = TRUE);
        type2    <- rep(x = 0, times = resource_quantity);
        type3    <- rep(x = 0, times = resource_quantity);
        xloc     <- sample(x = 0:(rows-1), size = resource_quantity, 
                           replace = TRUE);
        yloc     <- sample(x = 0:(cols-1), size = resource_quantity, 
                           replace = TRUE);
        mover    <- rep(x = move, times = resource_quantity);
        time     <- rep(x = 0, times = resource_quantity);
        remov_pr <- rep(x = rm_pr, times = resource_quantity);
        growth   <- rep(x = lambda, times = resource_quantity);
        offspr   <- rep(x = 0, times = resource_quantity); # None at init
        age      <- sample(x = 1:max_age, size = resource_quantity, 
                           replace = TRUE);
        mark     <- rep(x = 0, times = resource_quantity); # Can be marked
        tally    <- rep(x = 0, times = resource_quantity);
        consume  <- consumption_rate[type1];
        adj_mv   <- rep(x = 0, times = resource_quantity);
        adj_c    <- rep(x = 0, times = resource_quantity);
        adj_k    <- rep(x = 0, times = resource_quantity);
        adj_gr   <- rep(x = 0, times = resource_quantity);
        adj_h    <- rep(x = 0, times = resource_quantity);
        consumed <- rep(x = 0, times = resource_quantity);
        fed_col  <- rep(x = times_feeding, times = resource_quantity);
        the_resource <- cbind(IDs, type1, type2, type3, xloc, yloc, mover, time,
                              remov_pr, growth, offspr, age, mark, tally,
                              consume, adj_mv, adj_c, adj_k, adj_gr, adj_h,
                              consumed, fed_col);
    }
    if( is.null(the_resource) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return( the_resource );
}

#' Agent initialisation
#'
#' Initialise the agents of the GMSE model.
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param agent_number This is the number of agents that are set in the model; agent number does not change during the simulation, and each agent has a unique ID
#'@param type_counts A vector of how many agents there are of each type (element). The sum of this vector needs to equal the agent_number so that each agent can correctly be assigned a type. Currently, GMSE assumes that there are only two types of agents: managers (type 0) and stakeholders (type 1), and only one manager exists. Future versions of GMSE will allow for different options as requested.
#'@param move This parameter affects the movement of agents each time step. There are multiple types of movement (see obs_move_type in the gmse function), but this parameter determines the distance in cells that an agent will move. Agent movement is generally less important than resource movement, and typically does not affect how agents interact with resources
#'@param vision This parameter determines the distance around an agent's location within which it can observe resources. This is relevant for some (but not not all) types of observation in the observation model, particularly for density-based estimation (observe_type = 0 in the gmse() function). 
#'@param rows The number of rows (y-axis) on the simulated landscape; agents are randomly placed somewhere on the landscape array
#'@param cols The number of columns (x-axis) on the simulated landscape; agents are randomly placed somewhere on the landscape array
#'@param scaring This is a TRUE or FALSE value determining whether or not scaring is an option for managers and stakeholders. If so, then stakeholders that scare cause resources to be moved from their current landscape cell to a random cell on the landscape (note, it is possible that the resource could be scared back onto the stakeholder's own land again). The default value of this is FALSE.
#'@param culling This is a TRUE or FALSE value determining whether or not culling is an option for managers and stakeholders. If so, then stakeholders that cull cause the resource to be removed from the simulation permanently (i.e., killing the resource). The default value of this is TRUE.
#'@param castration This is a TRUE or FALSE value determining whether or not castration is an option for managers and stakeholders. If so, then stakeholders that castrate do not remove the resource from the simulation, but prohibit the resource from reproducing by setting its `lambda` value to zero. The default value of this is FALSE.
#'@param feeding This is a TRUE or FALSE value determining whether or not feeding is an option for managers and stakeholders. If so, then stakeholders that feed increase a resource's growth rate (lambda) for one time step by 100 percent. The default value of this is FALSE.
#'@param help_offspring This is a TRUE or FALSE value determining whether or not feeding is an option for managers and stakeholders. If so, then stakeholders that help_offspring increase a resource's offspring production for one time step by one (i.e., one more offspring is produced). The default value of this is FALSE.
#'@param tend_crops This is a TRUE or FALSE value determining whether or not tending crops on the landscape is allowed for stakeholders. If so, then stakeholders can increase one cells yield by 50 percent for each action to `tend_crops`. Actions on the landscape cannot be regulated by managers, so the cost of this action is always `minimum_cost`. The default value of this is FALSE.
#'@param tend_crop_yld The per landscape cell proportional increase in crop yield when stakeholders take one action to increase yield on their landscape. The default value is set to 0.5 (i.e., a 50 percent increase in yield on a cell).
#'@param kill_crops This is a TRUE or FALSE value determining whether or not killing crops on the landscape is allowed for stakeholders. If so, then stakeholders can remove the crop yield on a cell completely for each action to `kill_crops`. Actions on the landscape cannot be regulated by managers, so the cost of this action is always `minimum_cost`.
#'@param perceive_scare For a focal user, the perceived effect of scaring one resource on the total number of resources affecting the user
#'@param perceive_cull  For a focal user, the perceived effect of culling one resource on the total number of resources affecting the user
#'@param perceive_cast For a focal user, the perceived effect of castrating one resource on the total number of resources affecting the user
#'@param perceive_feed For a focal user, the perceived effect of feeding one resource on the total number of resources affecting the user
#'@param perceive_help For a focal user, the perceived effect of helping the offspring of one resource on the total number of resources affecting the user
#'@param perceive_tend For a focal user, the perceived effect of tending to crops on one cell of owned landscape the user's total crop yield
#'@param perceive_kill For a focal user, the perceived effect of destroying the crops on one cell of owned landscape on the user's total crop yield
#'@param manager_sense This adjusts the sensitivity that a manager assumes their actions have with respect to changes in costs (their policy). For example, given a `manage_sense` value of 0.9, if the cost of culling resources doubles, then instead of a manager assuming the the number of culled resources per user will be cut in half, the manager will instead assume that the number of resources culled will be cut by one half times eight tenths. As a general rule, a value of ca 0.8 allows the manager to predict stake-holder responses to policy accurately; future versions of GMSE could allow managers to adjust this dynamically based on simulation history.
#'@param lambda This value is the baseline population growth rate of resources. Each resource in the simulation produces Poisson(lambda) offspring in one time step within the resource model. The value of lambda might be increased or decreased by user actions, and juvenile survival can potentially be decreased by a carrying capacity placed on birth. The default value is 0.3, meaning that the average resource produces one offspring every three time steps.
#'@param res_consume The fraction of remaining biomass (e.g. crop production) that a resource consumes while occupying a landscape cell. The default value is 0.5, so if one resource occupies the cell, then landscape production is halved, if two resources occupy the cell, then landscape production drops to 0.25; if three, then production drops to 0.125, etc.
#'@param consume_repr How much from a landscape does an individual resource need to produce one offspring (default 0)?
#'@param tend_crop_yld The per landscape cell proportional increase in crop yield when stakeholders take one action to increase yield on their landscape. The default value is set to 0.5 (i.e., a 50 percent increase in yield on a cell).
#'@param landscape The landscape on which some cells are owned. This needs to have been created with the make_landscape, or be a three dimensional array in which the third layer of the array corresponds to owned cells
#'@return An initialised data frame of agents being modelled
#'@examples
#'agents <- make_agents(model = "IBM", agent_number = 2, type_counts = c(1, 1), 
#'move = 0, vision = 20, rows = 100, cols = 100);
#'@export
make_agents <- function(model              = "IBM",
                        agent_number       = 2,
                        type_counts        = c(1, 1),
                        move               = 0,
                        vision             = 20,
                        rows               = 100,
                        cols               = 100,
                        scaring            = FALSE,
                        culling            = TRUE,
                        castration         = FALSE,
                        feeding            = FALSE,
                        help_offspring     = FALSE,
                        tend_crops         = FALSE,
                        kill_crops         = FALSE,
                        perceive_scare     = NA,
                        perceive_cull      = NA,
                        perceive_cast      = NA,
                        perceive_feed      = NA,
                        perceive_help      = NA,
                        perceive_tend      = NA,
                        perceive_kill      = NA,
                        manager_sense      = 1,
                        lambda             = 0.3,
                        res_consume        = 0,
                        consume_repr       = 0,
                        tend_crop_yld      = 0.2,
                        landscape          = NA
                        ){
    if(agent_number < 2 & length(type_counts) < 2){
        print("Need >1 agent to make the array, so I'm adding a dummy agent");
        agent_number <- agent_number + 1; 
        type_counts  <- c(type_counts,1);
    }
    the_agents  <- NULL;
    agent_types <- length(type_counts);
    if(model == "IBM"){
        # First check to make sure that the input makes sense
        if(agent_number != sum(type_counts)){
            stop("ERROR: Agent number must equal sum of all agent type counts");   
        }
        IDs     <- seq(from = 1, to = agent_number, by = 1);
        type1   <- rep(x = 0:(agent_types-1), times = type_counts);
        type2   <- rep(x = 0, times = agent_number);
        type3   <- rep(x = 0, times = agent_number);
        xloc    <- sample(x = 1:rows, size = agent_number, replace = TRUE);
        yloc    <- sample(x = 1:cols, size = agent_number, replace = TRUE);
        mover   <- rep(x = move, times = agent_number);
        time    <- rep(x = 0, times = agent_number);
        view    <- rep(x = vision, times = agent_number);
        error   <- rep(x = 0, times = agent_number);
        mark    <- rep(x = 0, times = agent_number);
        tally   <- rep(x = 0, times = agent_number);
        see1    <- rep(x = 0, times = agent_number);
        see2    <- rep(x = 0, times = agent_number);
        see3    <- rep(x = 0, times = agent_number);
        yield   <- rep(x = 0, times = agent_number);
        budget  <- rep(x = 100, times = agent_number);
        p_scare <- rep(x = 0, times = agent_number);
        p_cull  <- rep(x = 0, times = agent_number);
        p_castr <- rep(x = 0, times = agent_number);
        p_feed  <- rep(x = 0, times = agent_number);
        p_help  <- rep(x = 0, times = agent_number);
        p_tend  <- rep(x = 0, times = agent_number);
        p_krop  <- rep(x = 0, times = agent_number);
        adj_b_1 <- rep(x = 0, times = agent_number);
        adj_b_2 <- rep(x = 0, times = agent_number);
        adj_b_3 <- rep(x = 0, times = agent_number);
        the_agents <- cbind(IDs, type1, type2, type3, xloc, yloc, mover, time,
                            view, error, mark, tally, see1, see2, see3, yield,
                            budget, p_scare, p_cull, p_castr, p_feed, p_help,
                            p_tend, p_krop, adj_b_1, adj_b_2, adj_b_3);
        the_agents <- agent_perceptions(the_agents, scaring, culling,
                                        castration, feeding, help_offspring,
                                        tend_crops, kill_crops, perceive_scare,
                                        perceive_cull, perceive_cast, 
                                        perceive_feed, perceive_help, 
                                        perceive_tend, perceive_kill,
                                        manager_sense, lambda, res_consume,
                                        consume_repr, tend_crop_yld, landscape);
    }
    if( is.null(the_agents) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return( the_agents );
}

agent_perceptions <- function(agents, 
                              scaring            = FALSE,
                              culling            = TRUE,
                              castration         = FALSE,
                              feeding            = FALSE,
                              help_offspring     = FALSE,
                              tend_crops         = FALSE,
                              kill_crops         = FALSE,
                              perceive_scare     = NA,
                              perceive_cull      = NA,
                              perceive_cast      = NA,
                              perceive_feed      = NA,
                              perceive_help      = NA,
                              perceive_tend      = NA,
                              perceive_kill      = NA,
                              manager_sense      = 1,
                              lambda             = 0.3,
                              res_consume        = 0,
                              consume_repr       = 0,
                              tend_crop_yld      = 0.2,
                              landscape          = NA){
    
    if(is.array(agents) == FALSE){
        stop("Something is wrong with the agents array being built");
    }
    n_agents <- dim(agents)[1];
    if(n_agents < 2){
        stop("Something is wrong with the agents array being built");
    }
    if(is.na(landscape[1]) == FALSE){
        cells <- dim(landscape[,,3])[1] * dim(landscape[,,3])[2];
    }
    E_off <- lambda;
    if(consume_repr > 0){
        E_off <- E_off + (res_consume / consume_repr);
    }
    for(i in 1:n_agents){
        if(agents[i, 2] == 0){ # If this is the manager
            agents[i, 18] <- -1 * scaring        * manager_sense * 0;
            agents[i, 19] <- -1 * culling        * manager_sense * (1 + E_off);
            agents[i, 20] <- -1 * castration     * manager_sense * E_off;
            agents[i, 21] <-  1 * feeding        * manager_sense * E_off;
            agents[i, 22] <-  1 * help_offspring * manager_sense * 1;
        }
        if(agents[i, 2] == 1){
            agents[i, 18] <- perceive_scare;
            if( is.na(perceive_scare) == TRUE ){
                agents[i, 18] <- 0; # Assume that no reason to scare
                if(is.na(landscape[1]) == FALSE){
                    land_owned <- sum(landscape[,,3] == agents[i, 1]) / cells;
                    if(land_owned > 0){
                        agents[i, 18] <- -1 * (1 - land_owned) * scaring;
                    }  # E Scaring efficacy
                } # Get the proportion of landscape cells owned
            }
            agents[i, 19] <- perceive_cull;
            if( is.na(perceive_cull) == TRUE ){
                agents[i, 19] <- -1 * (1 + E_off) * culling;
            }
            agents[i, 20] <- perceive_cast;
            if( is.na(perceive_cast) == TRUE ){
                agents[i, 20] <- -1 * E_off * castration;
            }
            agents[i, 21] <- perceive_feed;
            if( is.na(perceive_feed) == TRUE ){
                agents[i, 21] <- 1 * E_off * feeding;
            }
            agents[i, 22] <- perceive_help;
            if( is.na(perceive_help) == TRUE ){
                agents[i, 22] <- 1 * help_offspring;
            }
            agents[i, 23] <- perceive_tend;
            if( is.na(perceive_tend) == TRUE ){
                agents[i, 23] <- 1 * tend_crop_yld * tend_crops;
            }
            agents[i, 24] <- perceive_kill;
            if( is.na(perceive_kill) == TRUE ){
                agents[i, 24] <- -1 * kill_crops;
            }
        }
    }
    return(agents);
}


#' Manager and user budgets
#' 
#' Initialise manager and user budgets
#' 
#'@param agents The agents array
#'@param manager_budget The budget of a manager
#'@param user_budget What is the budget of a user
#'@param usr_budget_rng Uniform range of users budgets
#'@param budget_col Column where the budget is located
#'@return An updated agents data frame with correct budgets
manager_user_budgets <- function(agents, 
                                 manager_budget = 1000, 
                                 user_budget = 1000, 
                                 usr_budget_rng = 0,
                                 budget_col = 17){
    a_number         <- dim(agents)[1];
    budget           <- rep(x = NA, times = a_number);
    type1            <- agents[, 2];
    the_mana         <- which(type1 == 0);
    the_users        <- which(type1 > 0);
    managers         <- length(the_mana);
    users            <- length(the_users);
    budget[the_mana] <- rep(x = manager_budget, times = managers);
    ubudg            <- runif(n = users, min = user_budget - usr_budget_rng,
                                         max = user_budget + usr_budget_rng);
    ubudg[ubudg < 0]     <- 1;
    budget[the_users]    <- round(ubudg);
    agents[, budget_col] <- budget;
    return(agents);
}

#' COST initialisation
#'
#' Initialise the cost array of the G-MSE model.
#'
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param res_opts A binary vector produced by the GMSE function defining what types of stakeholder interactions with resources (scaring, culling, castration, feeding, help_offspring) are permitted
#'@param lnd_opts A binary vector produced by the GMSE function defining what types of stakeholder interactions with the landscape (tend_crops, kill_crops) are permitted
#'@param min_cost The minimum cost that any agent (stakeholder or manager) incurrs for performing one action. This value is also set as an option in the main gmse() function (minimum_cost). This cost is recommended to be set to a value of 10, which gives managers increased precision when adjusting costs. For example, if the mimimum cost for a stakeholder performing an action is low, then a small change in the mimimum cost could halve or double the number of actions performed from the manager's perspective, with no options in between; hence the benefit of having a high mimimum cost combined with a higher agent budget (see the main gmse() function)
#'@return A three dimensional array of initialised cost values for agent (manager and stakeholder) actions of the same dimensions as the ACTION array in GMSE
#'@examples
#'\dontrun{
#'COST <- make_costs( AGENTS = AGENTS, RESOURCES = starting_resources,
#'res_opts = user_res_opts, lnd_opts = user_lnd_opts, min_cost = minimum_cost);
#'}
#'@export
make_costs <- function(AGENTS, RESOURCES, res_opts, lnd_opts, min_cost){
    
    agent_IDs     <- c(-2, -1, unique(AGENTS[,1]) );
    agent_number  <- length(agent_IDs);
    res_types     <- unique(RESOURCES[,2:4]);
    
    COST_LIST <- NULL;
    
    agent  <- 1;
    agents <- agent_number - 2;
    while(agent <= agents){
        COST_LIST[[agent]] <- utility_layer(agent_IDs, agent_number, res_types);
        agent            <- agent + 1;
    }
    
    dim_u <- c( dim(COST_LIST[[1]]), length(COST_LIST) );
    
    COST <- array(data = unlist(COST_LIST), dim = dim_u);
    
    res_num    <- sum(COST[,1,1] == -2);
    res_opts_m <- matrix(data  = rep(res_opts, res_num), nrow = res_num,
                         byrow = TRUE);
    lnd_num    <- sum(COST[,1,1] == -1);
    lnd_opts_m <- matrix(data = rep(lnd_opts, lnd_num), nrow = lnd_num);
    
    for(i in 2:dim(COST)[3]){ # Exclude the manager for now
        COST[COST[,1,i]==-2,8:12,i]  <- min_cost * res_opts_m[1:res_num,];
        COST[COST[,1,i]==-1,10:11,i] <- min_cost * lnd_opts_m[1:lnd_num,];
    }
    COST[COST[,1,1] == 1, 8:12,1] <- min_cost * res_opts_m[1:res_num,];
    COST[COST[,1,1] == 1, 13, 1]  <- min_cost;
    COST[COST[,1,1] <  0, 13,  ]  <- min_cost;
    
    COST[,1:7,]    <- 100001;
    COST[COST < 1] <- 100001;
    
    return( COST );
}


#' Utility initialisation
#'
#' Initialise the utilities of the G-MSE model.
#'
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@return A three dimensional ACTION array of initialised agent (manager and stakeholder) actions of the same dimensions as the COST array in GMSE
#'@examples
#'\dontrun{
#'ACTION <- make_utilities(AGENTS = AGENTS, RESOURCES = starting_resources);
#'}
#'@export
make_utilities <- function(AGENTS, RESOURCES){

    agent_IDs     <- c(-2, -1, unique(AGENTS[,1]) );
    agent_number  <- length(agent_IDs);
    res_types     <- unique(RESOURCES[,2:4]);
    
    UTIL_LIST <- NULL;
    
    agent  <- 1;
    agents <- agent_number - 2;
    while(agent <= agents){
        UTIL_LIST[[agent]] <- utility_layer(agent_IDs, agent_number, res_types);
        agent            <- agent + 1;
    }
    
    dim_u <- c( dim(UTIL_LIST[[1]]), length(UTIL_LIST) );
    
    UTILITY <- array(data = unlist(UTIL_LIST), dim = dim_u);
    
    return( UTILITY );
}

#' Utility layer for initialisation.
#'
#' Function to initialise a layer of the UTILITY array of the G-MSE model.
#'
#'@param agent_IDs Vector of agent IDs to use (including -1 and -2, which indicate direct actions to the landscape and resources, respectively)
#'@param agent_number The number of agents to use (length of agent_IDs)
#'@param res_types The number of unique resource types (cols 2-4 of RESOURCES); for now, this should always be 1
#'@return A layer of the COST or ACTION array, as called in building either make_costs or make_utilities, respectively. This layer corresponds to the costs or actions of a single agent, with the larger array in in which it is placed including all agents
#'@examples
#'\dontrun{
#'UTIL_LIST <- utility_layer(agent_IDs, agent_number, res_types);
#'}
#'@export
utility_layer <- function(agent_IDs, agent_number, res_types){
 
    LAYER <- NULL;
    
    unique_types  <- dim(res_types)[1];
    types_data    <- lapply(X   = 1:agent_number, 
                            FUN = function(quick_rep_list) res_types);
    
    column_1    <- sort( rep(x = agent_IDs, times = unique_types) );
    columns_2_4 <- do.call(what = rbind, args = types_data);
    static_type <- cbind(column_1, columns_2_4);
    removes     <- sum(static_type[,1] == -1 & static_type[,2] > 1);
    if(removes > 0){
        remove      <- which(static_type[,1] == -1 & static_type[,2] > 1);
        static_type <- static_type[-remove,];
    }
    
    dynamic_type <- matrix(data = 0, nrow = dim(static_type)[1], ncol = 9);

    colnames(static_type)  <- c("agent", "type1", "type2", "type3");
    colnames(dynamic_type) <- c("util", "u_loc", "u_land", "movem", "castem",
                                 "killem", "feedem", "helpem", "bankem");
    
    LAYER <- cbind(static_type, dynamic_type);
    
    return( LAYER ); 
}

#' Initialise array of resource and landscape-level interactions.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param res_consume The proportion of a landscape cell that a resource consumes
#'@param consume_surv The amount that a resource needs to consume to survive a time step
#'@param consume_repr The amount that a resource needs to consume to produce one offspring
#'@param times_feeding The number of times a resource moves to feed on a cell in a time step
#'@examples
#'\dontrun{
#'Jacobian <- make_interaction_array(RESOURCES = starting_resources, 
#'LAND = LANDSCAPE_r);
#'}
#'@export
make_interaction_array <- function(RESOURCES, LAND, res_consume = 0.5, 
                                   consume_surv = 0, consume_repr = 0, 
                                   times_feeding = 1){
    resource_types  <- unique(RESOURCES[,2:4]);
    resource_count  <- dim(resource_types)[1];
    landscape_count <- dim(LAND)[3] - 2; # Maybe put all of them in later?
    total_dims      <- resource_count + landscape_count;
    INTERACTIONS    <- matrix(data = 0, nrow = total_dims, ncol = total_dims);
    
    diag(INTERACTIONS) <- 1;
    
    name_vec <- NULL;                                   
    for(i in 1:dim(resource_types)[1]){
        name_vec <- c( name_vec, 
                       paste(resource_types[i,1],
                             resource_types[i,2],
                             resource_types[i,3],
                             sep = "" )
                      );                                   
    }            
    name_vec <- c(name_vec, as.character(paste("L",1:landscape_count,sep="")));
    rownames(INTERACTIONS) <- name_vec;
    colnames(INTERACTIONS) <- name_vec;
    
    mn_land            <- 1; # mean(LAND[,, 2]);
    E_consumed         <- mn_land * (1 - (1 - res_consume)^times_feeding);
    
    INTERACTIONS[1, 2] <- -1 * E_consumed;
    
    consuming_repr     <- floor(E_consumed * consume_repr);
    consuming_survival <- E_consumed - consume_surv;
    # Note, currently reproduction happens before survival
    INTERACTIONS[2, 1] <- consuming_repr + consuming_survival;          
    
    return(INTERACTIONS);
}
                                   
#' Initialise array of resource and landscape-level interactions.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@examples
#'\dontrun{
#'interaction_tabl <- make_interaction_table(starting_resources, LANDSCAPE_r);
#'}
#'@export
make_interaction_table <- function(RESOURCES, LAND){
    
    resource_types      <- unique(RESOURCES[,2:4]);
    resource_part       <- matrix(data=0, nrow=dim(resource_types)[1], ncol=4);
    resource_part[,2:4] <- resource_types;
    
    landscape_count    <- dim(LAND)[3] - 2; # Again, maybe all in later?
    landscape_part     <- matrix(data = 0, nrow = landscape_count, ncol = 4);
    landscape_part[,1] <- 1;
    landscape_part[,2] <- 1:landscape_count;
    
    the_table <- rbind(resource_part, landscape_part);
    return(the_table);
}
