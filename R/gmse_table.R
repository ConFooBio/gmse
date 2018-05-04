#' GMSE table results
#' 
#' The gmse_table function takes results created from simulations of the gmse
#' and concatenates key results from a large list into a more manageable data 
#' table.
#'
#'@param gmse_sim The output of a `gmse` simulation.
#'@param individual_actions Whether or not the actions of individual users will be included as columns of the resulting table. Aggregations of user actions will always be included. By default, this is set to `FALSE`.
#'@param hide_unused_options Whether or not to hide results from policy options when creating the resulting table. If `TRUE` (default), then policy and user actions that are not allowed in a simulation will not be placed as columns. If `FALSE`, then these columns will be placed with values of `NA`.
#'@param all_timesteps Whether or not results from each time step from the simulation should be individually placed as a row in the resulting table (`TRUE` by default). If `FALSE`, then only the last row will be placed.
#'@return A table with one or more rows of results, each of which indicates a unique `gmse` simulation for a given time step. Columns represent key simulation including resource densities, observation estimates, policy, and user actions.
#'@examples
#'sim       <- gmse();
#'sim_table <- gmse_table(gmse_sim = sim);
#'@export
gmse_table <- function(gmse_sim, individual_actions = FALSE, 
                       hide_unused_options = TRUE, all_timesteps = TRUE){
    
    time_steps <- 1:length(gmse_sim$resource);
    
    
    
}