#' Resource model
#'
#' A population model of resource (including population) dynamics for 
#' a single time step.
#'
#'@param RESOURCE_1 Resource 1 at the start of the time step
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frame of Resource 2 at the end of the time step
#'@export
resource <- function(resource_1, resource_2, landscape, paras, model = "IBM") {
    check_model <- 0;
    if(model == "IBM"){
        RESOURCE_OUT <- run_resource_a( RESOURCE_1_c = resource_1,
                                        LANDSCAPE_c  = landscape);
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(RESOURCE_OUT);
}

run_resource_a <- function(RESOURCE_1_c, LANDSCAPE_c){
    .Call("resource", RESOURCE_1_c, LANDSCAPE_c);
}