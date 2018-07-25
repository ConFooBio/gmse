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
    time_steps <- length(gmse_results$resource);
    parameters <- gmse_results$paras[1,];
    #--- First get the resource abundances
    res_types    <- unique(gmse_results$resource[[1]][,2]);
    resources    <- matrix(data  = 0, nrow = time_steps, 
                           ncol = length(res_types) + 1);
    res_colna    <- rep(x = NA, times = dim(resources)[2]);
    res_colna[1] <- "time_step";
    for(i in 1:length(res_types)){
        res_colna[i+1] <- paste("type_", res_types[i], sep = "");
    }
    colnames(resources) <- res_colna;
    #--- Next get estimates abd the costs set by the manager
    observations    <- matrix(data  = 0, nrow = time_steps, 
                              ncol = length(res_types) + 1);
    costs   <- matrix(data = NA, nrow = time_steps*length(res_types), 
                      ncol = 10);
    agents  <- gmse_results$agents[[1]];
    users   <- agents[agents[,2] > 0, 1];
    actions <- matrix(data  = NA, ncol = 13,
                      nrow = time_steps * length(res_types) * length(users));
    c_row  <- 1;
    a_row  <- 1;
    for(i in 1:time_steps){
        the_res            <- gmse_results$resource[[i]][,2];
        harvests           <- gmse_results$resource[[i]][,17];
        manager_acts       <- gmse_results$action[[i]][,,1];
        resources[i, 1]    <- i;
        observations[i, 1] <- i;
        land_prod          <- gmse_results$land[[i]][,,2];
        land_own           <- gmse_results$land[[i]][,,3];
        for(j in 1:length(res_types)){
            #---- Resource abundance below
            resources[i,j+1] <- sum(the_res == res_types[j]);
            #---- Manager estimates below
            target_row <- which(manager_acts[,1] == -2 & 
                                    manager_acts[,2] == res_types[j]);
            estim_row  <- which(manager_acts[,1] ==  1 & 
                                    manager_acts[,2] == res_types[j]);
            target <- manager_acts[target_row, 5];
            adjusr <- manager_acts[estim_row,  5];
            observations[i,j+1] <- target - adjusr;
            #---- Cost setting below
            costs[c_row, 1]  <- i;
            costs[c_row, 2]  <- res_types[j];
            estim_row    <- which(manager_acts[,1] ==  1 & 
                                  manager_acts[,2] == res_types[j]);
            if(parameters[89] == TRUE){
                costs[c_row, 3] <- manager_acts[estim_row,  8];
            }
            if(parameters[90] == TRUE){
                costs[c_row, 4] <- manager_acts[estim_row,  9];
            }
            if(parameters[91] == TRUE){
                costs[c_row, 5] <- manager_acts[estim_row,  10];
            }
            if(parameters[92] == TRUE){
                costs[c_row, 6] <- manager_acts[estim_row,  11];
            }
            if(parameters[93] == TRUE){
                costs[c_row, 7] <- manager_acts[estim_row,  12];
            }
            if(parameters[94] == TRUE){
                costs[c_row, 8] <- parameters[97];
            }
            if(parameters[95] == TRUE){
                costs[c_row, 9] <- parameters[97];
            }
            costs[c_row, 10] <- manager_acts[estim_row, 13] - parameters[97];
            c_row <- c_row + 1;
            #--- Action setting below
            for(k in 1:length(users)){
                usr_acts <- gmse_results$action[[i]][,,users[k]];
                actions[a_row, 1] <- i;
                actions[a_row, 2] <- users[k];
                actions[a_row, 3] <- res_types[j];
                res_row <- which(usr_acts[,1] == -2 & 
                                     usr_acts[,2] == res_types[j]);
                if(parameters[89] == TRUE){
                    actions[a_row, 4] <- usr_acts[res_row,  8];
                }
                if(parameters[90] == TRUE){
                    actions[a_row, 5] <- usr_acts[res_row,  9];
                }
                if(parameters[91] == TRUE){
                    actions[a_row, 6] <- usr_acts[res_row,  10];
                }
                if(parameters[92] == TRUE){
                    actions[a_row, 7] <- usr_acts[res_row,  11];
                }
                if(parameters[93] == TRUE){
                    actions[a_row, 8] <- usr_acts[res_row,  12];
                }
                if(j == length(res_types)){
                    if(parameters[104] > 0){
                        land_row <- which(usr_acts[,1] == -1);
                        if(parameters[95] > 0){
                            actions[a_row, 9]  <- usr_acts[land_row, 10];
                        }
                        if(parameters[94] > 0){
                            actions[a_row, 10] <- usr_acts[land_row, 11];
                        }
                    }
                    actions[a_row, 11] <- sum(usr_acts[, 13]);
                }
                if(parameters[104] > 0){
                    max_yield <- sum(land_own == users[k]);
                    usr_yield <- sum(land_prod[land_own == users[k]]);
                    actions[a_row, 12] <- 100 * (usr_yield / max_yield);
                }
                actions[a_row, 13] <- sum(harvests == users[k]);
                a_row <- a_row + 1;
            }
        }
    }
    cost_col <- c("time_step", "resource_type", "cost_scaring", "cost_culling",
                  "cost_castration", "cost_feeding", "cost_helping", 
                  "cost_tend_crop", "cost_kill_crop", "cost_unused");
    colnames(costs)        <- cost_col;
    colnames(resources)    <- res_colna;
    colnames(observations) <- res_colna;
    action_col <- c("time_step", "user_ID", "resource_type", "act_scaring", 
                    "act_culling", "act_castration", "act_feeding", 
                    "act_helping", "act_tend_crop", "act_kill_crop", 
                    "act_unused", "crop_yield", "harvested");
    colnames(actions) <- action_col;
    the_summary <- list(resources    = resources, 
                        observations = observations, 
                        costs        = costs, 
                        actions      = actions);
    return(the_summary);
}

#' GMSE table results
#' 
#' The gmse_table function takes results created from simulations of the gmse
#' and concatenates key results from a large list into a more manageable data 
#' table.
#'
#'@param gmse_sim The output of a `gmse` simulation.
#'@param hide_unused_options Whether or not to hide results from policy options when creating the resulting table. If `TRUE` (default), then policy and user actions that are not allowed in a simulation will not be placed as columns. If `FALSE`, then these columns will be placed with values of `NA`.
#'@param all_time Whether or not results from each time step from the simulation should be individually placed as a row in the resulting table (`TRUE` by default). If `FALSE`, then only the last row will be placed.
#'@return A table with one or more rows of results, each of which indicates a unique `gmse` simulation for a given time step. Columns represent key simulation including resource densities, observation estimates, policy, and user actions.
#'@examples
#'sim       <- gmse(time_max = 10);
#'sim_table <- gmse_table(gmse_sim = sim);
#'@export
gmse_table <- function(gmse_sim, hide_unused_options = TRUE, all_time = TRUE){
    
    time_steps <- 1:max(gmse_sim$paras[,1]);
    t_max      <- length(time_steps);
    sim        <- gmse_summary(gmse_sim);
    res_rows   <- sim$resources;
    estimate   <- sim$observations[,-1];
    cost_rows  <- sim$costs[,-1];
    act_rows   <- matrix(data = NA, nrow = t_max, ncol = 10);
    for(act in 1:10){
        act_rows[,act] <- tapply(sim$actions[,act + 3], sim$actions[,1], sum);
    }
    colnames(res_rows) <- c("time_step", "resources");
    colnames(act_rows) <- colnames(sim$actions[,4:13]);
    results            <- cbind(res_rows, estimate, cost_rows, act_rows);
    results            <- results[,-4];
    
    if(hide_unused_options == TRUE){
        retain  <- is.na(results[1,]) == FALSE;
        results <- results[,retain];
    }
    
    if(all_time == FALSE){
        last_step <- dim(results)[1];
        results   <- results[last_step,];
    }
    
    return(results);
}
