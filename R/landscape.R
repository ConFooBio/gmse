#' Landscape initialisation
#'
#' Initialise the landscape of the G-MSE model.
#'
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param cols The dimension of one side of the landscape (e.g., Latitude)
#'@param rows The dimension of the other side of the landscape (e.g., Longitude)
#'@param cell_types Scalar or vector of all possible types of landscape cells
#'@param cell_val_mn Mean cell value (e.g., defining crop output on a cell)
#'@param cell_val_sd Standard devation of cell values on a landscape
#'@param cell_val_max The maximum value of a cell
#'@param cell_val_min The minimum value of a cell
#'@param layers The number of layers in the 3D landscape (should usually be 3)
#'@param ownership A TRUE or FALSE whether land should be owned by stakeholders
#'@param owners The number of stakeholders in the model that own land
#'@param public_land The proportion of landscape cells that are not owned
#'@param land_var  Does distribution of land vary among users? >=0, <1
#'@return the_land A cols by rows landscape with randomly distributed cell types
#'@examples
#'land <- make_landscape(model = "IBM", rows = 10, cols = 10, cell_types = 1, 
#'cell_val_mn = 1, cell_val_sd =  0)
#'@export
make_landscape <- function(model, rows, cols, cell_types = 1, cell_val_mn = 1, 
                           cell_val_sd = 0, cell_val_max = 1, cell_val_min = 0,
                           layers = 3, ownership = FALSE, owners = 4, 
                           public_land = 0, land_var = 0){
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
        if( ownership == FALSE){
            who_owns     <- rep(x = 1, times = cell_count);
            the_terrain3 <- sort(who_owns); 
        }else{
            who_owns     <- owner_land_ssa(rows, cols, owners, public_land, land_var);
            the_terrain3 <- who_owns;
        }
        the_terrain2[the_terrain2 > cell_val_max] <- cell_val_max;
        the_terrain2[the_terrain2 < cell_val_min] <- cell_val_min;
        alldata        <- c(the_terrain, the_terrain2, the_terrain3);
        the_land       <- array(data = alldata, dim = c(rows, cols, layers));
    }
    if( is.null(the_land) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(the_land);
}

#' Count the number of owned cells of each agent
#'
#' Counts the total number of cells on a landscape owned by each agent and
#' inserts that total number into a column of the agents array.
#'
#'@param AGENTS The agents array holding agents' information
#'@param LAND The landscape on which some cells are owned
#'@param tot Column in which the total number of owned cells in AGENTS is added
#'@param own Layer of the landscape array where ownership information is held
#'@param ID Column in which the ID of an agent is held in the AGENTS array
#'@return The AGENTS array with the column of total cell count filled in.
#'@examples
#'\dontrun{
#'agents     <- make_agents(agent_number = 3, type_counts = c(1, 2))
#'land       <- make_landscape(rows = 10, cols = 10, model = "IBM", 
#'ownership = 2:3)
#'new_agents <- count_agent_cells(AGENTS = agents, LAND = land);
#'}
#'@export
count_agent_cells <- function(AGENTS, LAND = NULL, tot = 14, own = 3, ID = 1){
    if(is.null(LAND) == TRUE){
        return(AGENTS);
    }
    if(dim(LAND)[3] < 3){
        stop("The landscape needs at least three levels for land ownership");
    }
    agents <- dim(AGENTS)[1];
    for(agent in 1:agents){
        AGENTS[agent, tot] <- sum(LAND[,,own] == AGENTS[agent, ID]);
    }
    return(AGENTS);
}   

#' Age landscape
#'
#' Determines how the landscape will change over the course of one time step.
#' For now, simply reverts a specified layer back to its original values
#' In other words, e.g., crops are annual and regrow undamaged each year.
#'
#'@param LAND The name of the landscape being changed
#'@param landscape_ini The name of the original landscape replacing
#'@param layer The layer that is being affected on the landscape
#'@return the_land with one layer reset to its original cell values
#'@examples
#'\dontrun{
#'LANDSCAPE_r <- age_land(LAND = LANDSCAPE_r, landscape_ini = LANDSCAPE_INI, 
#'layer = 2);
#'}
#'@export
age_land <- function(LAND, landscape_ini, layer){

    LAND[,,layer] <- landscape_ini[,,layer];
    
    return(LAND);
}

#' Owner land SSA
#'
#' Builds a layer of the landscape with a shortest-splitline algorithm to assign
#' landscape cells among owners
#'
#'@param dim_x The number of cells on the X dimension of the landscape
#'@param dim_y The number of cells on the Y dimension of the landscape
#'@param owners Number of owners among which landscape cells will be divided
#'@param public_land The amount of land that will not be owned
#'@param land_var Does distribution of land vary among users? >=0, <1
#'@return A two dimensional array of cells with ownership values
#'@export
owner_land_ssa <- function(dim_x, dim_y, owners, public_land, land_var){
    check_val <- floor( owners / (1 - public_land) );
    if( (dim_x * dim_y) < check_val){
        warning("There are probably not enough landscape cells for users");
    }
    landscape_c_vector <- c(dim_x, dim_y, owners, public_land, land_var);
    OWNER_LAYER        <- run_landscape_a(landscape_c_vector);
    vectorise_layer    <- as.vector(OWNER_LAYER);
    return_array       <- array(data = vectorise_layer, dim = c(dim_x, dim_y));
    return(return_array);
}

run_landscape_a <- function(LANDSCAPE_PARAMETERS){
    .Call("build_ownership", LANDSCAPE_PARAMETERS);
}