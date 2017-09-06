#' GMSE apply simulation
#' 
#' The gmse function is the the primary function to call to run a simulation.
#' It calls other functions that run resource, observation, management, and user
#' models in each time step. Hence while individual models can be used on their
#' own, gmse() is really all that is needed to run a simulation. 
#'
#' 
#'@param time_max This value sets the maximum number of time steps for a simulation. There are no constraints for length of time that a simulation can run. The default is 100 time steps.
#'@examples
#'sim <- gmse_apply();
#'@useDynLib GMSE
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon
#'@importFrom stats rnorm rpois
#'@export
gmse_apply <- function(){
    
}




