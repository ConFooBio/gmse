#' gmse results summary
#'
#' Summarise gmse output in a more user-friendly format
#'
#'@param gmse_results The full list as returned by the gmse function
#'@return A simplified list that includes four elements, each of which is a table of data: 1. resources, a table showing time step in the first column, followed by resource abundance in the second column. 2. observations, a table showing time step in the first column, followed by the estimate of population size (produced by the manager) in the second column. 3. costs, a table showing time step in the first column, manager number in the second column (should always be zero), followed by the costs of each action set by the manager (policy); the far-right column indicates budget that is unused and therefore not allocated to any policy. 4. actions, a table showing time step in the first column, user number in the second column, followed by the actions of each user in the time step; additional columns indicate unused actions, crop yield on the user's land (if applicable), and the number of resources that a user successfully harvests (i.e., 'culls').
#'@examples
#'\dontrun{
#'sim_summary <- gmse_summary(gmse_results = sim);
#'}
#'@export
gmse_summary <- function(gmse_results){
    time_steps <- dim(gmse_results$paras)[1];
    #--- First get the resource abundances
    res_types    <- unique(gmse_results$resource[[1]][,2]);
    resources    <- matrix(dat  = 0, nrow = time_steps, 
                           ncol = length(res_types) + 1);
    res_colna    <- rep(x = NA, times = dim(resources)[2]);
    res_colna[1] <- "time_step";
    for(i in 1:length(res_types)){
        res_colna[i+1] <- paste("type_", res_types[i], sep = "");
    }
    for(i in 1:time_steps){
        resources[i, 1] <- i;
        for(j in 1:length(res_types)){
            resources[i,j+1] <- sum(sim$resource[[i]][,2] == res_types[j]);
        }
    }
    
    
}


