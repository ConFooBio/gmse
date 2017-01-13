#' Observation model
#'
#' A simulation of techniques (e.g., capture-mark-recapture) for estimating
#' population size and poperties.
#'
#'@param resource Resource data frame at the start of the time step
#'@param landscape Data frame at the start of the time step
#'@param paras Vector of parameter values to read into the model
#'@param agent Agent data frame at the start of the time step
#'@param types The types of data being observed
#'@param fix_mark Fixed number of individuals marked? (A number, or FALSE)
#'@param times Number of times that the observations are made
#'@param samp_age Minimum age of the resource being sampled (default = 1)
#'@param agent_type The type of agent doing the observing (default = 0)
#'@param type_cat The category of agent type (first 4 columns) doing observing
#'@param move_res Should resources be moved during observation (default = FALSE)
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frame observations at the end of the time step
observation <- function(resource   = NULL, 
                        landscape  = NULL, 
                        paras      = NULL, 
                        agent      = NULL,
                        res_type   = 1,
                        fix_mark   = FALSE,
                        times      = 1,
                        samp_age   = 1,
                        agent_type = 0,
                        type_cat   = 1,
                        move_res   = FALSE,
                        model      = "IBM"
                        ){
    check_model <- 0;
    # Use DATA as an array for mark-recapture information
    # Time-stamp the mark-recapture data so can simulate within/over years
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(resource)){
            stop("Warning: Resources need to be in an array");   
        }
        if(!is.array(landscape)){
            stop("Warning: Landscape need to be in an array");
        } # TODO: make sure paras is right length below
        if(!is.vector(paras) | !is.numeric(paras)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        if(!is.array(agent)){
            stop("Warning: Agents need to be in an array");
        }
        # If all checks out, first put the type into paras for easier input
        paras[8]  <- agent_type;
        paras[10] <- res_type;
        paras[11] <- as.numeric(fix_mark); # Note: 'FALSE' coerced to zero
        paras[12] <- times;
        paras[17] <- samp_age;
        paras[18] <- type_cat;
        paras[19] <- as.numeric(move_res);
        if(times < 1){
            stop("Need to sample at least once (else no observation)");   
        }
        # Then run the population model
        OBSERVE_OUT  <- run_observation_a( RESOURCE_c   = resource,
                                           LANDSCAPE_c  = landscape,
                                           PARAMETERS_c = paras,
                                           AGENT_c      = agent
                                           );
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(OBSERVE_OUT);
}

run_observation_a <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c){
    .Call("observation", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c);
}