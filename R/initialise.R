#' Resource initialisation
#'
#' Function to initialise the resources of the G-MSE model
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
#'@return the_resources Initialised data frame of resources being modelled
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
                          max_age            = 5
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
        xloc     <- sample(x = 1:rows, size = resource_quantity, 
                           replace = TRUE);
        yloc     <- sample(x = 1:cols, size = resource_quantity, 
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
        the_resource <- cbind(IDs, type1, type2, type3, xloc, yloc, mover, time,
                              remov_pr, growth, offspr, age, mark, tally,
                              consume, adj_mv, adj_c, adj_k, adj_gr, adj_h);
    }
    if( is.null(the_resource) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return( the_resource );
}

#' Agent initialisation
#'
#' Function to initialise the agents of the G-MSE model
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param agent_number This is the number of agents that are set in the model; agent number does not change during the simulation, and each agent has a unique ID
#'@param type_counts A vector of how many agents there are of each type (element). The sum of this vector needs to equal the agent_number so that each agent can correctly be assigned a type. Currently, GMSE assumes that there are only two types of agents: managers (type 0) and stakeholders (type 1), and only one manager exists. Future versions of GMSE will allow for different options as requested.
#'@param move This parameter affects the movement of agents each time step. There are multiple types of movement (see obs_move_type in the gmse function), but this parameter determines the distance in cells that an agent will move. Agent movement is generally less important than resource movement, and typically does not affect how agents interact with resources
#'@param vision This parameter determines the distance around an agent's location within which it can observe resources. This is relevant for some (but not not all) types of observation in the observation model, particularly for density-based estimation (observe_type = 0 in the gmse() function). 
#'@param rows The number of rows (y-axis) on the simulated landscape; agents are randomly placed somewhere on the landscape array
#'@param cols The number of columns (x-axis) on the simulated landscape; agents are randomly placed somewhere on the landscape array
#'@return the_agents Initialised data frame of agents being modelled
#'@export
make_agents <- function(model        = "IBM",
                        agent_number = 2,
                        type_counts  = c(1,1),
                        move         = 0,
                        vision       = 20,
                        rows         = 100,
                        cols         = 100
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
        the_agents <- cbind(IDs, type1, type2, type3, xloc, yloc, mover, time,
                            view, error, mark, tally, see1, see2, see3, yield,
                            budget);
    }
    if( is.null(the_agents) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return( the_agents );
}

#' COST initialisation
#'
#' Function to initialise the cost array of the G-MSE model
#'
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param res_opts A binary vector produced by the GMSE function defining what types of stakeholder interactions with resources (scaring, culling, castration, feeding, help_offspring) are permitted
#'@param lnd_opts A binary vector produced by the GMSE function defining what types of stakeholder interactions with the landscape (tend_crops, kill_crops) are permitted
#'@param min_cost The minimum cost that any agent (stakeholder or manager) incurrs for performing one action. This value is also set as an option in the main gmse() function (minimum_cost). This cost is recommended to be set to a value of 10, which gives managers increased precision when adjusting costs. For example, if the mimimum cost for a stakeholder performing an action is low, then a small change in the mimimum cost could halve or double the number of actions performed from the manager's perspective, with no options in between; hence the benefit of having a high mimimum cost combined with a higher agent budget (see the main gmse() function)
#'@return A three dimensional array of initialised cost values for agent (manager and stakeholder) actions of the same dimensions as the ACTION array in GMSE
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
    
    res_num  <- sum(COST[,1,1] == -2);
    res_opts <- t(as.matrix(res_opts));
    lnd_num  <- sum(COST[,1,1] == -1);
    lnd_opts <- t(as.matrix(lnd_opts));
    
    for(i in 2:dim(COST)[3]){ # Exclude the manager for now
        COST[COST[,1,i]==-2,8:12,i]  <- min_cost * res_opts[1:res_num,];
        COST[COST[,1,i]==-1,10:11,i] <- min_cost * lnd_opts[1:lnd_num,];
    }
    COST[COST[,1,1] == 1, 8:12,1] <- min_cost * res_opts[1:res_num,];
    COST[COST[,1,1] == 1, 13, 1]  <- min_cost;
    COST[COST[,1,1] <  0, 13,  ]  <- min_cost;
    
    COST[,1:7,]    <- 10000;
    COST[COST < 1] <- 10000;
    
    return( COST );
}


#' Utility initialisation
#'
#' Function to initialise the utilities of the G-MSE model
#'
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@return A three dimensional ACTION array of initialised agent (manager and stakeholder) actions of the same dimensions as the COST array in GMSE
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

#' Utility layer for initialisation
#'
#' Function to initialise a layer of the UTILITY array of the G-MSE model
#'
#'@param agent_IDs Vector of agent IDs to use (including -1 and -2, which indicate direct actions to the landscape and resources, respectively)
#'@param agent_number The number of agents to use (length of agent_IDs)
#'@param res_types The number of unique resource types (cols 2-4 of RESOURCES); for now, this should always be 1
#'@return A layer of the COST or ACTION array, as called in building either make_costs or make_utilities, respectively. This layer corresponds to the costs or actions of a single agent, with the larger array in in which it is placed including all agents
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
        which(static_type[,1] == -1 & static_type[,2] > 1);
        static_type <- static_type[-remove,];
    }
    
    dynamic_type <- matrix(data = 0, nrow = dim(static_type)[1], ncol = 9);

    colnames(static_type)  <- c("agent", "type1", "type2", "type3");
    colnames(dynamic_type) <- c("util", "u_loc", "u_land", "movem", "castem",
                                 "killem", "feedem", "helpem", "bankem");
    
    LAYER <- cbind(static_type, dynamic_type);
    
    return( LAYER ); 
}


#' Initialise array of resource and landscape-level interactions
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@export
make_interaction_array <- function(RESOURCES, LAND){
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
    return(INTERACTIONS);
}
                                   
#' Initialise array of resource and landscape-level interactions
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
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
}
                                   
