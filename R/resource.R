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
resource <- function(RESOURCES, model = "IBM") {
    new_resource <- NULL;
    if(model == "IBM"){
        resource_quantities <- length(RESOURCES);
        if(resource_quantities == 1){
            new_resource <- .Call("resource", RESOURCE_1);
        }else{
            stop("Cannot yet model >1 resource in an IBM");   
        }
    }
    if( is.null(new_resource) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(new_resource);
}