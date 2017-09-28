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
#'sim <- gmse_apply(time_max = 100);
#'@useDynLib GMSE
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon
#'@importFrom stats rnorm rpois
#'@export
gmse_apply <- function(resource_model    = resource, 
                       observation_model = observation, 
                       manager_model     = manager, 
                       user_model        = user, 
                       ...
                       ){
    
    if( is.function(resource_model) == "FALSE" ){
        stop( "ERROR: Resource model needs to be a function");
    }
    if( is.function(observation_model) == "FALSE" ){
        stop( "ERROR: Observation model needs to be a function");
    }
    if( is.function(manager_model) == "FALSE" ){
        stop( "ERROR: Manager model needs to be a function");
    }
    if( is.function(user_model) == "FALSE" ){
        stop( "ERROR: User model needs to be a function");
    }
    
    res_mod <- match.fun(resource_model);
    obs_mod <- match.fun(observation_model);
    man_mod <- match.fun(manager_model);
    use_mod <- match.fun(user_model);
    
    # Sort out the arguments for each function, and the rest
    all_arguments <- as.list(sys.call());
    all_arg_names <- names(all_arguments);

    res_arg_vals  <- get_arg_list( the_function   = res_mod, 
                                   all_arg_names  = all_arg_names, 
                                   all_arg_values = all_arguments
                                 );
    
    res <- do.call(what = res_mod, args = res_arg_vals);
    
    
    
    
    
    allpars <- pass_paras(...); # Will ignore non-GMSE functions
    
    inputs  <- allpars$gmse_user_input;
    paras   <- allpars$gmse_para_vect;
    
    
    
    
    return(result);
}


pass_paras <- function( time_max = 100, land_dim_1 = 100, land_dim_2 = 100,  
                        res_movement = 20, remove_pr = 0.0, lambda = 0.30, 
                        agent_view = 10, agent_move = 50, res_birth_K = 10000,
                        res_death_K = 2000, edge_effect = 1, res_move_type = 1,
                        res_birth_type = 2, res_death_type = 2, 
                        observe_type = 0, fixed_mark = 50, fixed_recapt = 150, 
                        times_observe = 1, obs_move_type  = 1, res_min_age = 0,
                        res_move_obs = TRUE, Euclidean_dist = FALSE, 
                        plotting = FALSE, hunt = FALSE, start_hunting = 95,  
                        res_consume = 0.5, ga_popsize = 100, ga_mingen = 40, 
                        ga_seedrep = 20, ga_sampleK = 20, ga_chooseK = 2,  
                        ga_mutation = 0.1, ga_crossover = 0.1, 
                        move_agents = TRUE, max_ages = 5, minimum_cost = 10,
                        user_budget = 1000, manager_budget = 1000,
                        manage_target = 1000, RESOURCE_ini = 1000, 
                        scaring = FALSE, culling = TRUE, castration = FALSE,
                        feeding = FALSE, help_offspring = FALSE, 
                        tend_crops = FALSE, tend_crop_yld = 0.2, 
                        kill_crops = FALSE, stakeholders = 4, 
                        manage_caution = 1, land_ownership = FALSE, 
                        manage_freq = 1, converge_crit = 100, 
                        manager_sense = 0.1, public_land    = 0, ...
                    ){
    
    input_list <- c(time_max, land_dim_1, land_dim_2, res_movement, remove_pr,
                    lambda, agent_view, agent_move, res_birth_K, res_death_K,
                    edge_effect, res_move_type, res_birth_type, res_death_type,
                    observe_type, fixed_mark, fixed_recapt, times_observe,
                    obs_move_type, res_min_age, res_move_obs, Euclidean_dist, 
                    plotting, hunt, start_hunting, res_consume, ga_popsize,
                    ga_mingen, ga_seedrep, ga_sampleK, ga_chooseK, ga_mutation,
                    ga_crossover, move_agents, max_ages, minimum_cost,
                    user_budget, manage_target, RESOURCE_ini, scaring, culling,
                    castration, feeding, help_offspring, tend_crops,
                    tend_crop_yld, kill_crops, stakeholders, manage_caution,
                    land_ownership, manage_freq, converge_crit, manager_sense,
                    public_land); 
    
    user_res_opts  <- c(scaring, culling, castration, feeding, help_offspring);
    user_lnd_opts  <- c(tend_crops, kill_crops);
    
    ttr <- 20;
    agn <- stakeholders + 1;
    agt <- 17;
    lkr <- 2;
    lyr <- stakeholders + 1;
    roc <- stakeholders + 1;
    coc <- 13;

    paras <- c(1, edge_effect, res_move_type, res_birth_type, res_death_type,
               res_birth_K, res_death_K, 0, observe_type, 1, fixed_mark, 
               times_observe, land_dim_1, land_dim_2, obs_move_type, 1, 
               res_min_age, 1, 12, res_move_obs, Euclidean_dist, ga_popsize, 
               ga_mingen, ga_seedrep, ga_sampleK, ga_chooseK, ga_mutation,
               ga_crossover, move_agents, max_ages, 7, 11, RESOURCE_ini, 4, 5,
               6, 3, 9, 10, 18, 19, ttr, 16, 8, 1, 1, 15, 14, 1, 4, 5, 6, 10, 
               12, agn, agt, 1, 2, 3, 13, lkr, RESOURCE_ini, ttr+times_observe, 
               1, 0, lyr, lkr-1, 8, roc, coc, 4, 7, 0, 17, 0, 
               -1*manager_sense*(1+lambda), -1*manager_sense*lambda, 1*lambda, 
               1*manager_sense, tend_crop_yld, 1, 2, 15, 0, 0, 0, 0, 0, 
               user_res_opts[1], user_res_opts[2], user_res_opts[3], 
               user_res_opts[4], user_res_opts[5], user_lnd_opts[1], 
               user_lnd_opts[2], manage_caution, minimum_cost, user_budget, 
               converge_crit, RESOURCE_ini, 0, 0, fixed_recapt, land_ownership, 
               public_land, lambda
    );
    
    return( list(gmse_user_input = as.vector(input_list), 
                 gmse_para_vect  = as.vector(paras)) 
          );
}



get_arg_list <- function(the_function, all_arg_names, all_arg_values){
    fun_args <- names(formals(the_function));
    fun_vals <- rep(x = "arg_not_found", times = length(fun_args)); 
    for(i in 1:length(fun_args)){
        for(j in 1:length(all_arg_names)){
            if(fun_args[i] == all_arg_names[j]){
                fun_vals[i] <- all_arg_values[j];
                break;
            }
        }
    }
    if( sum(fun_vals == "arg_not_found") > 0 ){
        error_str <- paste("ERROR: Cannot find the following:", 
                           fun_args[which(fun_vals == "arg_not_found")]);
        stop(error_str);
    }
    return(fun_vals);
}










f1 <- function(x, z = 0) 2*x + z;
f2 <- function(x) x*x + 1;

dv <- function(d1 = -1, d2 = -2, ...){
    return(c(d1, d2));
}

xfun <- function(f1_i, f2_i, x, y, ...){
    
    f1 <- match.fun(f1_i);
    f2 <- match.fun(f2_i);
    
    #lists all of the arguments in the function
    llv      <- as.list(sys.call()); # All values
    lln      <- names(llv);          # All names
    f1_args  <- names(formals(f1));  # Just the names of F1 arguments
    
    #Isolates the ... arguments relevant to f1
    f1_input <- rep(x = 0, times = length(f1_args));
    for(i in 1:length(lln)){
        for(j in 1:length(f1_args)){
            if(lln[i] == f1_args[j]){
                f1_input[j] <- llv[i];
            }
        }
    }

    # Runs f1 with the relevant arguments
    res <- do.call(what = f1, args = f1_input);
    
    
    
    f1x <- match.fun(f1);
    f2x <- match.fun(f2);
    
    f1r <- f1x(x);
    f2r <- f2x(y);
    
    v1  <- c(f1r, f2r)
    v2  <- dv(...);
    
    return( res );
    
}












