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
    parameters <- gmse_results$paras[1,];
    movem      <- parameters[89];
    killem     <- parameters[90];
    castem     <- parameters[91];
    feedem     <- parameters[92];
    helpem     <- parameters[93];
    kill_crop  <- parameters[94];
    tend_crop  <- parameters[95];
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
        the_res         <- gmse_results$resource[[i]][,2]
        resources[i, 1] <- i;
        for(j in 1:length(res_types)){
            resources[i,j+1] <- sum(the_res == res_types[j]);
        }
    }
    colnames(resources) <- res_colna;
    #--- Next get the estimated abundances
    observations    <- matrix(dat  = 0, nrow = time_steps, 
                              ncol = length(res_types) + 1);
    for(i in 1:time_steps){
        manager_acts <- gmse_results$action[[i]][,,1];
        observations[i, 1] <- i;
        for(j in 1:length(res_types)){
            target_row <- which(manager_acts[,1] == -2 & 
                                manager_acts[,2] == res_types[j]);
            estim_row  <- which(manager_acts[,1] ==  1 & 
                                manager_acts[,2] == res_types[j]);
            target <- manager_acts[target_row, 5];
            adjusr <- manager_acts[estim_row,  5];
            observations[i,j+1] <- target - adjusr;
        }
    }
    colnames(observations) <- res_colna;
    #--- Next get the costs set by the manager
    costs <- matrix(dat = NA, nrow = time_steps * length(res_types), ncol = 8);
    row   <- 1;
    for(i in 1:time_steps){
        manager_acts <- gmse_results$action[[i]][,,1];
        for(j in 1:length(res_types)){
            costs[i, 1]  <- i;
            costs[i, 2]  <- res_types[j];
            estim_row    <- which(manager_acts[,1] ==  1 & 
                                  manager_acts[,2] == res_types[j]);
            if(parameters[89] == TRUE){
                costs[i, 3] <- manager_acts[estim_row,  8];
            }
            if(parameters[90] == TRUE){
                costs[i, 4] <- manager_acts[estim_row,  9];
            }
            if(parameters[91] == TRUE){
                costs[i, 5] <- manager_acts[estim_row,  10];
            }
            if(parameters[92] == TRUE){
                costs[i, 6] <- manager_acts[estim_row,  11];
            }
            if(parameters[93] == TRUE){
                costs[i, 7] <- manager_acts[estim_row,  12];
            }
            costs[row, 8] <- manager_acts[estim_row, 13] - parameters[97];
            row <- row + 1;
        }
    }
        
    cost_col <- c("time_step", "resource_type", "scaring", "culling",
                  "castration", "feeding", "helping", "unused");
    colnames(costs) <- cost_col;
    #--- Next get the actions of users
    users   <- sum(gmse_results$agents[[1]][,2] > 0);
    actions <- matrix(dat  = NA, nrow = time_steps * length(res_types) * users,
                      ncol = 13);
    for(i in 1:time_steps){
        for(j in 2:users){
            for(k in 1:length(res_types)){
                
                
            }
        }
    }
    
}




















