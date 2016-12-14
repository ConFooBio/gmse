#' Landscape initialisation
#'
#' Function to initialise the landscape of the G-MSE model
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param resource_quantity Number of resources being model
#'@param rows Location of individual on LANDSCAPE (y-axis)
#'@param cols Location of individual on LANDSCAPE (x-axis)
#'@param move Parameter affecting individual movement
#'@return the_resources List of data frames of resources being modelled
#'@export
initialise <- function(model, resource_quantity_1, rows, cols, move){
    the_resource_1   <- NULL;
    if(model == "IBM"){
        IDs   <- seq(from = 1, to = resource_quantity_1, by = 1);
        xloc  <- sample(x = 1:rows, size = resource_quantity_1, 
                        replace = TRUE);
        yloc  <- sample(x = 1:cols, size = resource_quantity_1, 
                        replace = TRUE);
        move  <- rep(x=move, times = resource_quantity_1);
        the_resource_1 <- cbind(IDs, xloc, yloc, move);
    }
    if( is.null(the_resource_1) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return( the_resource_1 );
}