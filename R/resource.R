#' Resource model
#'
#' A population model of resource (including population) dynamics for 
#' a single time step.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param PARAS The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return The resource function outputs an R list that includes three separate arrays, including (1) an new RESOURCES array, (2) a new LAND array, (3) a new PARAS array, each of which might be affected by the user function. The new arrays can then be read back into the broader GMSE function, thereby affecting the input into the observation, management, and user models.
#'@examples
#'\dontrun{
#'RESOURCE_NEW <- resource(RESOURCES = RESOURCES, LAND = LANDSCAPE_r, 
#'PARAS = paras, model = "IBM");
#'}
resource <- function(RESOURCES = NULL, 
                     LAND      = NULL, 
                     PARAS     = NULL,
                     model     = "IBM"
                     ) {
    check_model <- 0;
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(RESOURCES)){
            stop("Warning: Resources need to be in an array");   
        }
        if(!is.array(LAND)){
            stop("Warning: Landscape need to be in an array");
        } # TODO: make sure PARAS is right length below
        if(!is.vector(PARAS) | !is.numeric(PARAS)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        # If all checks out, then run the population model
        RESOURCE_OUT <- run_resource_a( RESOURCE_c   = RESOURCES,
                                        LANDSCAPE_c  = LAND,
                                        PARAMETERS_c = PARAS);
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