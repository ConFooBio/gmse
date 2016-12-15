#' Resource model
#'
#' A population model of resource (including population) dynamics for 
#' a single time step.
#'
#'@param resource Resource data frame at the start of the time step
#'@param landscape Data frame at the start of the time step
#'@param paras Vector of parameter values to read into the model
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frame of Resource 2 at the end of the time step
#'@export
resource <- function(resource, landscape, paras, model = "IBM") {
    check_model <- 0;
    if(model == "IBM"){
        RESOURCE_OUT <- run_resource_a( RESOURCE_c   = resource,
                                        LANDSCAPE_c  = landscape,
                                        PARAMETERS_c = paras);
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(RESOURCE_OUT);
}

run_resource_a <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c){
    .Call("resource", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c);
}