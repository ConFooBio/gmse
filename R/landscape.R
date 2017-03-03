#' Landscape initialisation
#'
#' Function to initialise the landscape of the G-MSE model
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param cols The dimension of one side of the landscape (e.g., Latitude)
#'@param rows The dimension of the other side of the landscape (e.g., Longitude)
#'@param cell_types Scalar or vector of all possible types of landscape cells
#'@param cell_val_mn Mean cell value (e.g., defining crop output on a cell)
#'@param cell_val_sd Standard devation of cell values on a landscape
#'@return the_land A cols by rows landscape with randomly distributed cell types
#'@export
make_landscape <- function(model, rows, cols, cell_types, cell_val_mn,
                           cell_val_sd){
    the_land  <- NULL;
    if(model == "IBM"){
        if(rows < 2){
            stop("Landscape dimensions in IBM must be 2 by 2 or greater");   
        }         
        if(cols < 2){ # Check to make sure the landcape is big enough
            stop("Landscape dimensions in IBM must be 2 by 2 or greater");   
        }
        cell_count     <- cols * rows;
        the_terrain    <- sample(x = cell_types, size = cell_count, 
                                 replace = TRUE);
        the_terrain2   <- rnorm(n = cell_count, mean = cell_val_mn,
                                sd = cell_val_sd);
        alldata        <- c(the_terrain, the_terrain2);
        the_land       <- array(data = alldata, dim = c(rows, cols, 2));
    }
    if( is.null(the_land) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(the_land);
}