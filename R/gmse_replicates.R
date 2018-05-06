#' gmse replicate simulations
#'
#' Replicates the same simulation for a set of parameter values
#'
#'@param replicates The number of replicate simulations to be run
#'@param all_time Passes to gmse_table. If TRUE, then results from all time steps of the simulation are returned; if FALSE (default), then only results from the last time step of each simulation is returned.
#'@param hide_unused_options Passes to gmse_table. If TRUE (default), then action options (e.g., scaring, culling, etc.) that are not available are not included in the results summary. If FALSE, then they are included as `NA`
#'@param ... Parameter values to be passed to the gmse function
#'@return A simplified list that includes four elements, each of which is a table of data: 1. resources, a table showing time step in the first column, followed by resource abundance in the second column. 2. observations, a table showing time step in the first column, followed by the estimate of population size (produced by the manager) in the second column. 3. costs, a table showing time step in the first column, manager number in the second column (should always be zero), followed by the costs of each action set by the manager (policy); the far-right column indicates budget that is unused and therefore not allocated to any policy. 4. actions, a table showing time step in the first column, user number in the second column, followed by the actions of each user in the time step; additional columns indicate unused actions, crop yield on the user's land (if applicable), and the number of resources that a user successfully harvests (i.e., 'culls').
#'@examples
#'\dontrun{
#'sim_replicates <- gmse_replicates(replicates = 2, time_max = 5);
#'}
#'@export
gmse_replicates <- function(replicates, all_time = FALSE, 
                            hide_unused_options = TRUE, ...){
    result_list <- list();
    while(replicates > 0){
        gmse_iteration <- gmse(...);
        gmse_results   <- gmse_table(gmse_sim = gmse_iteration,
                                     all_time = all_time,
                                     hide_unused_options = hide_unused_options);
        result_list[[replicates]] <- gmse_results;
        replicates    <- replicates - 1;
    }
    result_table <- do.call("rbind", result_list);
    return(result_table);
}
