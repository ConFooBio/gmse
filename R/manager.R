#' Manager model
#'
#' A model of manager decisions for a single time step. Managers set costs for user actions.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param PARAS The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param COST A three dimensional array of cost values for agent (manager and stakeholder) actions
#'@param ACTION A three dimensional array of agent (manager and stakeholder) actions
#'@param INTERACT An interaction (Jacobian) matrix of resources & landscape layer effects
#'@param inter_tabl Interaction table indexing types with the INTERACT matrix
#'@param OBSERVATION The array of resource observations from the observation model, used to estimate abundance of resources
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param ... Other arguments to be passed to a user-defined model
#'@return The manager function outputs an R list that includes five separate arrays, including (1) an new RESOURCES array, (2) a new AGENTS array, (3) a new LAND array, (4) a new ACTIONS array, and a new (5) COST array, each of which might be affected by the user function. The new arrays can then be read back into the broader GMSE function, thereby affecting the input into the user, resource, and observation models.
#'@examples
#'\dontrun{
#'MANAGER_OUT <- run_manage(RESOURCE_c = RESOURCES, LANDSCAPE_c = LAND, 
#'PARAMETERS_c = PARAS, AGENT_c = AGENTS, COST_c = COST, ACTION_c = ACTION, 
#'JACOBIAN_c = INTERACT, INTERACT_c = inter_tabl, OBSERVATION_c = OBSERVATION);
#'}
#'@export
manager <- function(RESOURCES   = NULL,
                    AGENTS      = NULL,
                    LAND        = NULL, 
                    PARAS       = NULL,
                    COST        = NULL,
                    ACTION      = NULL,
                    INTERACT    = NULL,
                    inter_tabl  = NULL,
                    OBSERVATION = NULL,
                    model       = "IBM"
                   ) {
    check_model <- 0;
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(RESOURCES)){
            stop("Warning: Resources need to be in an array");   
        }
        if(!is.array(AGENTS)){
            stop("Warning: Agents need to be in an array");   
        }
        if(!is.array(LAND)){
            stop("Warning: Landscape needs to be in an array");
        } # TODO: make sure PARAS is right length below
        if(!is.vector(PARAS) | !is.numeric(PARAS)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        if(!is.array(COST)){
            stop("Warning: COST needs to be in an array");
        } 
        if(!is.array(ACTION)){
            stop("Warning: ACTION needs to be in an array");
        } 
        if(!is.array(INTERACT)){
            stop("Warning: Interaction matrix needs to be in an array");
        }
        if(!is.array(inter_tabl)){
            stop("Warning: Look-up table for interactions needs to be array");
        }
        if(!is.array(OBSERVATION)){
            stop("Warning: Observation needs to be in an array");
        }
        # If all checks out, then run the manager model
        MANAGER_OUT <- run_manage(RESOURCE_c    = RESOURCES,
                                  LANDSCAPE_c   = LAND,
                                  PARAMETERS_c  = PARAS,
                                  AGENT_c       = AGENTS,
                                  COST_c        = COST,
                                  ACTION_c      = ACTION,
                                  JACOBIAN_c    = INTERACT,
                                  INTERACT_c    = inter_tabl,
                                  OBSERVATION_c = OBSERVATION
        );
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    names(MANAGER_OUT) <- c("RESOURCE", "AGENTS", "LAND", "ACTIONS", "COSTS");
    return(MANAGER_OUT);
}

# Below calls the manager function from c
run_manage <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, COST_c,
                       ACTION_c, JACOBIAN_c, INTERACT_c, OBSERVATION_c){
    .Call("manager", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, COST_c,
          ACTION_c, JACOBIAN_c, INTERACT_c, OBSERVATION_c);
}