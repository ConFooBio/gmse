#' Resource initialisation
#'
#' Function to initialise the resources of the G-MSE model
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param resource_quantity Number of resources being model
#'@param resource_types Number of different types of resources
#'@param rows Location of resource on LANDSCAPE (y-axis)
#'@param cols Location of resource on LANDSCAPE (x-axis)
#'@param move Parameter affecting individual movement
#'@param rm_pr Probability of resource removal (death) per time step
#'@param lambda Rate parameter for Poisson random sampling affecting birthrate
#'@param consumption_rate Rate at which resource consumes crops on landscape
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
#'@param agent_number Number of agents in the model
#'@param by_type Whether individuals are defined by type (FALSE) or row (TRUE)
#'@param type_counts Vector of how many agents of each type (element)
#'@param move A parameter that affects agent movement
#'@param vision A parameter affecting the area around its location it perceives
#'@param rows Location of agent on LANDSCAPE (y-axis)
#'@param cols Location of agent on LANDSCAPE (x-axis)
#'@param move Parameter affecting individual movement
#'@return the_agents Initialised data frame of agents being modelled
#'@export
make_agents <- function(model        = "IBM",
                        agent_number = 2,
                        by_type      = FALSE,
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

#' Utility initialisation
#'
#' Function to initialise the utilities of the G-MSE model
#'
#'@param AGENTS The agent array 
#'@param RESOURCES The resource array
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
#'@param agent_IDs Vector of agent IDs to use (including -1 and -2)
#'@param agent_number The number of agents to use (length of agent_IDs)
#'@param res_types The number of unique resource types (cols 2-4 of RESOURCES)
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
#'@param resources the resource array
#'@param landscape the landscape array
#'@export
make_interaction_array <- function(resources, landscape){
    resource_types  <- unique(resources[,2:4]);
    resource_count  <- dim(resource_types)[1];
    landscape_count <- dim(landscape)[3] - 2; # Maybe put all of them in later?
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
#'@param resources the resource array
#'@param landscape the landscape array
#'@export
make_interaction_table <- function(resources, landscape){
    
    resource_types      <- unique(resources[,2:4]);
    resource_part       <- matrix(data=0, nrow=dim(resource_types)[1], ncol=4);
    resource_part[,2:4] <- resource_types;
    
    landscape_count    <- dim(landscape)[3] - 2; # Again, maybe all in later?
    landscape_part     <- matrix(data = 0, nrow = landscape_count, ncol = 4);
    landscape_part[,1] <- 1;
    landscape_part[,2] <- 1:landscape_count;
    
    the_table <- rbind(resource_part, landscape_part);
}
                                   
