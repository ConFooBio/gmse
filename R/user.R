#' User model
#'
#' A model of user decisions for a single time step. These decisions result in stakeholder actions that can potentially affect resources and the landscape in a GMSE simulation.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param PARAS The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param COST A three dimensional array of cost values for agent (manager and stakeholder) actions
#'@param ACTION ACTION A three dimensional array of agent (manager and stakeholder) actions
#'@param INTERACT An interaction (Jacobian) matrix of resources & landscape layer effects
#'@param inter_tabl Interaction table indexing types with the INTERACT matrix
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param ... Other arguments to be passed to a user-defined model
#'@return The user function outputs an R list that includes five separate arrays, including (1) an new RESOURCES array, (2) a new AGENTS array, (3) a new LAND array, (4) a new ACTIONS array, and a new (5) COST array, each of which might be affected by the user function. The new arrays can then be read back into the broader GMSE function, thereby affecting the input into the resource, observation, and management models.
#'@examples
#'\dontrun{
#'USERS <- user(RESOURCES = RESOURCES, AGENTS = AGENTS, LAND = LANDSCAPE_r,
#'PARAS = paras, COST = COST, ACTION = ACTION, INTERACT = Jacobian, 
#'inter_tabl = interaction_tabl, model = "IBM");
#'}
#'@export
user <- function(RESOURCES  = NULL,
                 AGENTS     = NULL,
                 LAND       = NULL, 
                 PARAS      = NULL,
                 COST       = NULL,
                 ACTION     = NULL,
                 INTERACT   = NULL,
                 inter_tabl = NULL,
                 model      = "IBM"
                ){
    check_model <- 0;
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(RESOURCES)){
            stop("Warning: RESOURCES needs to be in an array");   
        }
        if(!is.array(AGENTS)){
            stop("Warning: AGENTS needs to be in an array");   
        }
        if(!is.array(LAND)){
            stop("Warning: LAND needs to be in an array");
        }
        if(!is.array(COST)){
            stop("Warning: COST needs to be in an array");
        } 
        if(!is.array(ACTION)){
            stop("Warning: ACTION needs to be in an array");
        } 
        if(!is.vector(PARAS) | !is.numeric(PARAS)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        if(dim(RESOURCES)[2] != 20){
            stop("The RESOURCES array has the wrong number of columns");
        }
        if(dim(LAND)[3] != 3){
            stop("The landscape doesn't have enough layers");
        }
        if(dim(AGENTS)[2] != 17){
            stop("The agent array has the wrong number of columns");
        }
        # If all checks out, then run the user model
        USER_OUT <- run_user(RESOURCE_c    = RESOURCES,
                             LANDSCAPE_c   = LAND,
                             PARAMETERS_c  = PARAS,
                             AGENT_c       = AGENTS,
                             COST_c        = COST,
                             ACTION_c      = ACTION,
                             JACOBIAN_c    = INTERACT,
                             INTERACT_c    = inter_tabl
        );
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    names(USER_OUT) <- c("RESOURCES", "AGENTS", "LAND", "ACTION", "COST", 
                         "PARAS");
    return(USER_OUT);
}

# Below calls the user function from c
run_user <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, COST_c,
                     ACTION_c, JACOBIAN_c, INTERACT_c){
    .Call("user", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, COST_c,
          ACTION_c, JACOBIAN_c, INTERACT_c);
}