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
resource <- function(RESOURCE_1, RESOURCE_2, LANDSCAPE, model = "IBM") {
    if(missing(RESOURCE_1)){
        stop("Need at least one resource");   
    }
    new_resource <- NULL;
    if(model == "IBM"){
        if(missing(RESOURCE_2)){
            new_resource <- .Call("resource", RESOURCE_1, -1, LANDSCAPE);
        }else{
            stop("Cannot yet model >1 resource in an IBM");   
        }
    }
    if( is.null(new_resource) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(new_resource);
}