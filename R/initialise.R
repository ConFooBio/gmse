#' Simulation initialisation
#'
#' Function to initialise the landscape of the G-MSE model
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param resource_quantity Number of resources being model
#'@param resource_types Number of different types of resources
#'@param rows Location of individual on LANDSCAPE (y-axis)
#'@param cols Location of individual on LANDSCAPE (x-axis)
#'@param move Parameter affecting individual movement
#'@return the_resources List of data frames of resources being modelled
#'@export
initialise <- function(model, resource_quantity, resource_types = 1, 
                       rows, cols, move, rm_pr){
    the_resource   <- NULL;
    if(model == "IBM"){
        IDs      <- seq(from = 1, to = resource_quantity, by = 1);
        type     <- sample(x = 1:resource_types, size = resource_quantity,
                           replace = TRUE);
        type2    <- rep(x = 0, times = resource_quantity);
        xloc     <- sample(x = 1:rows, size = resource_quantity, 
                           replace = TRUE);
        yloc     <- sample(x = 1:cols, size = resource_quantity, 
                           replace = TRUE);
        move     <- rep(x = move, times = resource_quantity);
        time     <- rep(x = 0, times = resource_quantity);
        remov_pr <- rep(x = rm_pr, times = resource_quantity);
        the_resource <- cbind(IDs, type, type2, xloc, yloc, move, time, 
                              remov_pr);
    }
    if( is.null(the_resource) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return( the_resource );
}
