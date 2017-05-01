#' Manager model
#'
#' A model of manager decisions for a single time step
#'
#'@param resource Resource data frame at the start of the time step
#'@param agent Agent array at the start of the the time step
#'@param landscape Landscape array at the start of the time step
#'@param paras Vector of parameter values to read into the model
#'@param cost Array of the costs associated with each agent actions
#'@param action Array of each agents actions
#'@param Jacobian Jacobian matrix of resources & landscape layer effects
#'@param inter_table Interaction table indexing types with Jacobian matrix
#'@param observation The data table of observations to estimate abundance
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frames of user output at the end of the time step
#'@export
manager <- function(resource    = NULL,
                    agent       = NULL,
                    landscape   = NULL, 
                    paras       = NULL,
                    cost        = NULL,
                    action      = NULL,
                    Jacobian    = NULL,
                    inter_tabl  = NULL,
                    observation = NULL,
                    model       = "IBM"
) {
    check_model <- 0;
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(resource)){
            stop("Warning: Resources need to be in an array");   
        }
        if(!is.array(agent)){
            stop("Warning: Agents need to be in an array");   
        }
        if(!is.array(landscape)){
            stop("Warning: Landscape needs to be in an array");
        } # TODO: make sure paras is right length below
        if(!is.vector(paras) | !is.numeric(paras)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        if(!is.array(action)){
            stop("Warning: Action needs to be in an array");
        } 
        if(!is.array(Jacobian)){
            stop("Warning: Interaction matrix needs to be in an array");
        }
        if(!is.array(inter_tabl)){
            stop("Warning: Look-up table for interactions needs to be array");
        }         
        # If all checks out, then run the manager model
        
        MANAGER_OUT <- run_manage(RESOURCE_c    = resource,
                                  LANDSCAPE_c   = landscape,
                                  PARAMETERS_c  = paras,
                                  AGENT_c       = agent,
                                  COST_c        = cost,
                                  ACTION_c      = action,
                                  JACOBIAN_c    = Jacobian,
                                  INTERACT_c    = inter_tabl,
                                  OBSERVATION_c = observation
        );
   
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(MANAGER_OUT);
}

# Below calls the manager function from c
run_manage <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, COST_c,
                       ACTION_c, JACOBIAN_c, INTERACT_c, OBSERVATION_c){
    .Call("manager", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, COST_c,
          ACTION_c, JACOBIAN_c, INTERACT_c, OBSERVATION_c);
}