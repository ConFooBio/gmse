#' User model
#'
#' A model of user decisions for a single time step
#'
#'@param resource Resource data frame at the start of the time step
#'@param landscape Data frame at the start of the time step
#'@param paras Vector of parameter values to read into the model
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frames of user output at the end of the time step
#'@export
user <- function(resource   = NULL,
                 agent      = NULL,
                 landscape  = NULL, 
                 paras      = NULL,
                 model      = "IBM"
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
            stop("Warning: Landscape need to be in an array");
        } # TODO: make sure paras is right length below
        if(!is.vector(paras) | !is.numeric(paras)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        # If all checks out, then run the user model
        
        USER_OUT <- run_user( RESOURCE_c    = resource,
                             LANDSCAPE_c   = landscape,
                             PARAMETERS_c  = paras,
                             AGENT_c       = agent
        );
   
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(USER_OUT);
}

# Below calls the user function from c
run_user <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c){
    .Call("user", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c);
}