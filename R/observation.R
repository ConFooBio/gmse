#' Observation model
#'
#' A simulation of techniques (e.g., capture-mark-recapture) for estimating
#' population size and poperties.
#'
#'@param resource Resource data frame at the start of the time step
#'@param landscape Data frame at the start of the time step
#'@param paras Vector of parameter values to read into the model
#'@param agent Agent data frame at the start of the time step
#'@param dat The data frame recording observations that are made
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frame observations at the end of the time step
observation <- function(resource  = NULL, 
                        landscape = NULL, 
                        paras     = NULL, 
                        agent     = NULL,
                        dat       = NULL, 
                        model     = "IBM",
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
        # If all checks out, then run the population model
        OBSERVE_OUT  <- run_observation_a( RESOURCE_c   = resource,
                                           LANDSCAPE_c  = landscape,
                                           PARAMETERS_c = paras,
                                           AGENT_c      = agent,
                                           DATA_c       = dat
                                           );
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(OBSERVE_OUT);
}

run_observation_a <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c,
                              DATA_c
                              ){
    .Call("observation", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, 
          DATA_c);
}