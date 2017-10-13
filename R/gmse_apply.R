#' GMSE apply function
#' 
#'The gmse_apply function is a flexible function that allows for user-defined sub-functions calling resource, observation, manager, and user models. Where such models are not specified, GMSE submodels 'resource', 'observation', 'manager', and 'user' are run by default. Any type of sub-model (e.g., numerical,  individual-based) is permitted as long as the input and output are appropriately specified. Only one time step is simulated per call to gmse_apply, so the function must be looped for simulation over time. Where model parameters are needed but not specified, defaults from gmse are used.
#'
#' 
#'@param res_mod The function specifying the resource model. By default, the individual-based resource model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'resource_array' or 'resource_vector', and arrays must follow the format of GMSE in terms of column number and type (if there is only one resource type, then the model can also just return a scalar value).
#'@param obs_mod The function specifying the observation model. By default, the individual-based observation model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'observation_array' or 'observation_vector', and arrays must follow the format of GMSE in terms of column number and type  (if there is only one resource type, then the model can also just return a scalar value).
#'@param man_mod The function specifying the manager model. By default, the individual-based manager model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'manager_array' or 'manager_vector', and arrays must follow the (3 dimensional) format of the 'COST' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired). User defined manager outputs will be recognised as costs by the default user model in gmse, but can be interpreted differently (e.g., total allowable catch) if specifying a custom user model.
#'@param use_mod The function specifying the user model. By default, the individual-based user model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'user_array' or 'user_vector', and arrays must follow the (3 dimensional) format of the 'ACTION' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired).
#'@param get_res How the output should be organised. The default 'basic' attempts to distill results down to their key values from submodel outputs, including resource abundances and estimates, and manager policy and actions. An option 'custom' simply returns a large list that includes the output of every submodel. Any other option (e.g. 'none') will return a massive list with all of the input, output, and parameters used to run gmse_apply.
#'@param ... Arguments passed to user-defined functions, and passed to modify default parameter values that would otherwise be called for gmse default models. Any argument that can be passed to gmse can be specified explicitly, just as if it were an argument to gmse. Similarly, any argument taken by a user-defined function should be specified, though the function will work if the user-defined function has a default that is not specified explicitly.
#'@examples
#'sim <- gmse_apply();
#'sim <- gmse_apply(stakeholders = 2);
#'@useDynLib GMSE
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon
#'@importFrom stats rnorm rpois
#'@export
gmse_apply <- function(res_mod  = resource, 
                       obs_mod  = observation, 
                       man_mod  = manager, 
                       use_mod  = user,
                       get_res  = "basic",
                       ...
                       ){

    fun_warn(res_mod, obs_mod, man_mod, use_mod);
   
    std_paras      <- pass_paras(...);
    all_args       <- as.list(sys.call());
    all_args$PARAS <- std_paras$gmse_para_vect;
    all_args$GMSE  <- formals(gmse);

    needed_args <- argument_list(res_mod, obs_mod, man_mod, use_mod, all_args);
    arg_vals    <- needed_args$all_arg_values; 
    arg_name    <- needed_args$all_arg_names;
    
    names(arg_vals) <- arg_name;
    
    # ------ RESOURCE MODEL ----------------------------------------------------
    res_args <- prep_res(arg_list = arg_vals, res_mod = res_mod);
    check_args(arg_list = res_args, the_fun = res_mod);
    res_results <- do.call(what = res_mod, args = res_args);
    res_results <- check_name_results(output   = res_results, 
                                      vec_name = "resource_vector", 
                                      mat_name = "resource_array");
    arg_vals    <- add_results(arg_list = arg_vals, output = res_results);
    arg_vals    <- fix_gmse_defaults(arg_list = arg_vals, model = res_mod);
    arg_vals    <- translate_results(arg_list = arg_vals, output = res_results);
    arg_vals    <- update_para_vec(arg_list   = arg_vals);
    check_extinction(arg_vals);
    
    # ------ OBSERVATION MODEL -------------------------------------------------
    obs_args <- prep_obs(arg_list = arg_vals, obs_mod = obs_mod);
    check_args(arg_list = obs_args, the_fun = obs_mod);
    obs_results <- do.call(what = obs_mod, args = obs_args);
    obs_results <- check_name_results(output   = obs_results,
                                      vec_name = "observation_vector", 
                                      mat_name = "observation_array");
    arg_vals    <- add_results(arg_list = arg_vals, output = obs_results);
    arg_vals    <- fix_gmse_defaults(arg_list = arg_vals, model = obs_mod);
    arg_vals    <- translate_results(arg_list = arg_vals, output = obs_results);
    arg_vals    <- update_para_vec(arg_list   = arg_vals);
    
    # ------ MANAGER MODEL -----------------------------------------------------
    man_args    <- prep_man(arg_list = arg_vals, man_mod = man_mod);
    check_args(arg_list = man_args, the_fun = man_mod);
    man_results <- do.call(what = man_mod, args = man_args);
    man_results <- check_name_results(output   = man_results, 
                                      vec_name = "manager_vector", 
                                      mat_name = "manager_array");
    arg_vals    <- add_results(arg_list = arg_vals, output = man_results);
    arg_vals    <- fix_gmse_defaults(arg_list = arg_vals, model = man_mod);
    arg_vals    <- translate_results(arg_list = arg_vals, output = man_results);
    arg_vals    <- update_para_vec(arg_list   = arg_vals);
    
    # ------ USER MODEL --------------------------------------------------------
    usr_args    <- prep_usr(arg_list = arg_vals, usr_mod = use_mod);
    check_args(arg_list = usr_args, the_fun = use_mod);
    usr_results <- do.call(what = use_mod, args = usr_args);
    usr_results <- check_name_results(output   = usr_results, 
                                      vec_name = "user_vector", 
                                      mat_name = "user_array");
    arg_vals    <- add_results(arg_list = arg_vals, output = usr_results);
    arg_vals    <- fix_gmse_defaults(arg_list = arg_vals, model = use_mod);
    arg_vals    <- translate_results(arg_list = arg_vals, output = usr_results);
    arg_vals    <- update_para_vec(arg_list   = arg_vals);
    
    res <- gmse_apply_out(arg_vals, get_res, res_mod, obs_mod, man_mod, use_mod,
                          res_results, obs_results, man_results, usr_results);
    
    return(res);    
}

################################################################################
# Subfunctions required                                                        #
################################################################################


fun_warn <- function(res_mod, obs_mod, man_mod, use_mod){
    if( is.function(res_mod) == "FALSE" ){
        stop( "ERROR: Resource model needs to be a function");
    }
    if( is.function(obs_mod) == "FALSE" ){
        stop( "ERROR: Observation model needs to be a function");
    }
    if( is.function(man_mod) == "FALSE" ){
        stop( "ERROR: Manager model needs to be a function");
    }
    if( is.function(use_mod) == "FALSE" ){
        stop( "ERROR: User model needs to be a function");
    }
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
                        manager_sense = 0.1, public_land = 0, PARAS = NULL, ...
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
    
    if(is.null(PARAS) == FALSE){
        paras <- PARAS;
        return(list(gmse_user_input = as.vector(input_list), 
                    gmse_para_vect  = as.vector(paras))
               );
    }
    
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

argument_list <- function(res_mod, obs_mod, man_mod, use_mod, oth_vals){
    oth_names   <- names(oth_vals);
    res_mod     <- check_arg_formals(res_mod);
    obs_mod     <- check_arg_formals(obs_mod);
    man_mod     <- check_arg_formals(man_mod);
    use_mod     <- check_arg_formals(use_mod);
    res_names   <- names(formals(res_mod));
    obs_names   <- names(formals(obs_mod));
    man_names   <- names(formals(man_mod)); 
    use_names   <- names(formals(use_mod));  
    f_arg_names <- c(res_names, obs_names, man_names, use_names);
    r_arg_names <- c("resource_array", "resource_vector",
                     "observation_array", "observation_vector",
                     "manager_array", "manager_vector",
                     "user_array", "user_vector");
    t_arg_names <- c(f_arg_names, r_arg_names, oth_names);
    u_arg_names <- unique(t_arg_names);
    u_arg_names <- u_arg_names[u_arg_names != ""];
    all_names   <- u_arg_names[u_arg_names != "..."];
    arg_list    <- rep(x = list(NA), times = length(all_names));
    arg_list    <- place_args(all_names, formals(res_mod), arg_list);
    arg_list    <- place_args(all_names, formals(obs_mod), arg_list);
    arg_list    <- place_args(all_names, formals(man_mod), arg_list);
    arg_list    <- place_args(all_names, formals(use_mod), arg_list);
    arg_list    <- place_args(all_names, oth_vals, arg_list);
    arg_out     <- list(all_arg_values = arg_list, all_arg_names = all_names);
    
    return(arg_out);        
}

check_arg_formals <- function(mod){
    forml <- formals(mod);
    len   <- length(forml);
    for(arg in 1:len){
        if(forml[arg] == ""){
            forml[arg] <- NA;
        }
    }
    formals(mod) <- forml;
    return(mod);
}

place_args <- function(all_names, placing_vals, arg_list){
    placing_names <- names(placing_vals);
    empty         <- identical(placing_names, NULL);
    if(empty == TRUE){
        return(arg_list);
    }
    for(i in 1:length(placing_vals)){
        place_name <- placing_names[i];
        if(place_name %in% all_names){
            place_pos <- which(all_names == place_name);
            arg_eval  <- eval(placing_vals[[i]]);
            if(is.null(arg_eval) == FALSE){
                arg_list[[place_pos]] <- eval(placing_vals[[i]]);
            }
        }
    }
    return(arg_list);
}

check_args <- function(arg_list, the_fun){
    the_fun_names <- names(formals(the_fun));
    fun_names     <- the_fun_names[the_fun_names != "..."];
    arg_names     <- names(arg_list);
    which_fun     <- deparse(substitute(the_fun));
    for(arg in 1:length(fun_names)){
        if(fun_names[arg] %in% arg_names == FALSE){
            error <- paste("ERROR: I can't find the argument ", fun_names[arg],
                           " in the function ", which_fun);
            stop(error);
        }
        arg_pos <- which(fun_names[arg] == arg_names);
        if(is.na(arg_list[arg_pos]) == TRUE){
            error <- paste("ERROR: The argument", fun_names[arg], 
                           "in the function ", which_fun, "cannot be found",
                           "or is 'NA' (which is not allowed)");
            stop(error);
        }
    }
}

prep_res <- function(arg_list, res_mod){
    if( identical(res_mod, resource) == TRUE){
        arg_list <- add_res_defaults(arg_list);
    }
    res_args <- list();
    arg_name <- names(arg_list);
    res_take <- formals(res_mod);
    res_take <- res_take[names(res_take) != "..."];
    res_name <- names(res_take);
    for(arg in 1:length(res_take)){
        arg_pos         <- which(res_name[arg] == arg_name);
        res_args[[arg]] <- arg_list[[arg_pos]];
    }
    names(res_args) <- res_name;
    return(res_args);
}

add_res_defaults <- function(arg_list){
    arg_names <- names(arg_list);
    res_pos   <- which(arg_names == "RESOURCES");
    arr_pos   <- which(arg_names == "resource_array");
    if(is.na(arg_list[[arr_pos]][1]) == FALSE){
        arg_list <- copy_args(arg_list = arg_list, from = "resource_array",
                              to = "RESOURCES");
    }
    para_pos  <- which(arg_names == "PARAS");
    paras     <- arg_list[[para_pos]];
    if(is.na(arg_list[[res_pos]][1]) == TRUE){
        dresarg <- collect_res_ini(arg_list);
        ini_res <- do.call(what = make_resource, args = dresarg);
        arg_list[[res_pos]] <- ini_res;
    }
    land_pos  <- which(arg_names == "LAND");
    if(is.na(arg_list[[land_pos]][1]) == TRUE){
        dlndarg  <- collect_land_ini(arg_list);
        ini_land <- do.call(what = make_landscape, args = dlndarg);
        arg_list[[land_pos]] <- ini_land;
    }
    return(arg_list);
}

collect_res_ini <- function(arg_list){
    make_res_list <- list();
    arg_names     <- names(arg_list);
    def_forms     <- formals(gmse);
    def_names     <- names(def_forms);
    make_res_list[[1]] <- "IBM";
    make_res_list[[2]] <- arg_list$GMSE$RESOURCE_ini;
    if("RESOURCE_ini" %in% arg_names){
        apos               <- which(arg_names == "RESOURCE_ini");
        make_res_list[[2]] <- arg_list[[apos]];
    }
    make_res_list[[3]] <- 1;
    if("res_types_ini" %in% arg_names){
        apos               <- which(arg_names == "res_types_ini");
        make_res_list[[3]] <- arg_list[[apos]];
    }
    make_res_list[[4]] <- arg_list$GMSE$land_dim_1;
    if("land_dim_1" %in% arg_names){
        apos               <- which(arg_names == "land_dim_1");
        make_res_list[[4]] <- arg_list[[apos]];
    }
    make_res_list[[5]] <- arg_list$GMSE$land_dim_2;
    if("land_dim_2" %in% arg_names){
        apos               <- which(arg_names == "land_dim_2");
        make_res_list[[5]] <- arg_list[[apos]];
    }
    make_res_list[[6]] <- arg_list$GMSE$res_movement;
    if("movement" %in% arg_names){
        apos               <- which(arg_names == "movement");
        make_res_list[[6]] <- arg_list[[apos]];
    }
    make_res_list[[7]] <- arg_list$GMSE$remove_pr;
    if("remove_pr" %in% arg_names){
        apos               <- which(arg_names == "remove_pr");
        make_res_list[[7]] <- arg_list[[apos]];
    }
    make_res_list[[8]] <- arg_list$GMSE$lambda;
    if("lambda" %in% arg_names){
        apos               <- which(arg_names == "lambda");
        make_res_list[[8]] <- arg_list[[apos]];
    }
    make_res_list[[9]] <- arg_list$GMSE$res_consume;
    if("res_consume" %in% arg_names){
        apos               <- which(arg_names == "res_consume");
        make_res_list[[9]] <- arg_list[[apos]];
    }
    make_res_list[[10]] <- arg_list$GMSE$max_ages;
    if("res_consume" %in% arg_names){
        apos               <- which(arg_names == "max_ages");
        make_res_list[[10]] <- arg_list[[apos]];
    }
    return(make_res_list);
}

collect_land_ini <- function(arg_list){
    make_lnd_list <- list();
    arg_names     <- names(arg_list);
    def_forms     <- formals(gmse);
    def_names     <- names(def_forms);
    make_lnd_list[[1]] <- "IBM";
    make_lnd_list[[2]] <- arg_list$GMSE$land_dim_1;
    if("land_dim_1" %in% arg_names){
        apos               <- which(arg_names == "land_dim_1");
        make_lnd_list[[2]] <- arg_list[[apos]];
    }
    make_lnd_list[[3]] <- arg_list$GMSE$land_dim_2;
    if("land_dim_2" %in% arg_names){
        apos               <- which(arg_names == "land_dim_2");
        make_lnd_list[[3]] <- arg_list[[apos]];
    }
    make_lnd_list[[4]] <- 1;
    make_lnd_list[[5]] <- 1;
    make_lnd_list[[6]] <- 0;
    if(arg_list$GMSE$land_ownership == TRUE){
        publand     <- arg_list$GMSE$public_land;
        stakes      <- arg_list$GMSE$stakeholders;
        stake_pr    <- (1 - publand) / stakes;
        land_alloc  <- c(publand, rep(x = stake_pr, times = stakes));
    }else{
        land_alloc  <- c(1, rep(x = 0, times = arg_list$GMSE$stakeholders)); 
    }
    make_lnd_list[[7]] <- 1:(arg_list$GMSE$stakeholders + 1);
    make_lnd_list[[8]] <- land_alloc

    return(make_lnd_list);
}

copy_args <- function(arg_list, from, to){
    arg_names <- names(arg_list);
    from_pos  <- which(arg_names == from);
    to_pos    <- which(arg_names == to);
    arg_list[[to_pos]] <- arg_list[[from_pos]];
    
    return(arg_list);
}

check_name_results <- function(output, vec_name, mat_name, the_fun){
    outname       <- names(output);
    which_fun     <- deparse(substitute(the_fun));
    if(is.null(outname) == TRUE){
        if(is.vector(output[[1]]) == TRUE){
            names(output)[[1]] <- vec_name;
        }
        if(is.matrix(output[[1]]) == TRUE){
            names(output)[[1]] <- mat_name;
        }
    }
    accepted <- c("resource_vector", "resource_array", "observation_vector",
                  "observation_array", "manager_vector", "manager_array",
                  "user_vector", "user_array", "RESOURCES", "OBSERVATION",
                  "COST", "ACTION");
    error <- TRUE;
    for(i in 1:length(output)){
        if(names(output)[i] %in% accepted){
            error <- FALSE;
        }
    }
    if(error == TRUE){
        emess <- paste("ERROR: I can't make sense of the output of the output
                       from the function ", which_fun, "-- Need to either have
                       no name, or clearly label as described in docs");
        stop(emess);
    }
    return(output);
}

fix_gmse_defaults <- function(arg_list, model){
    arg_names <- names(arg_list);
    if( identical(model, resource) == TRUE ){
        arg_list <- copy_args(arg_list, "RESOURCES", "resource_array");
    }
    if( identical(model, observation) == TRUE ){
        arg_list <- copy_args(arg_list, "OBSERVATION", "observation_array");
    }
    if( identical(model, manager) == TRUE ){
        arg_list <- copy_args(arg_list, "RESOURCES", "resource_array");
        arg_list <- copy_args(arg_list, "COST", "manager_array");
        arg_list <- copy_args(arg_list, "ACTION", "user_array");
    }
    if( identical(model, user) == TRUE ){
        arg_list <- copy_args(arg_list, "RESOURCES", "resource_array");
        arg_list <- copy_args(arg_list, "COST", "manager_array");
        arg_list <- copy_args(arg_list, "ACTION", "user_array");
    }
    return(arg_list);
}

add_results <- function(arg_list, output){
    arg_names <- names(arg_list);
    out_names <- names(output);
    for(i in 1:length(output)){
        if(out_names[[i]] %in% arg_names == TRUE){
            rep_pos <- which(arg_names == out_names[[i]]);
            arg_list[[rep_pos]] <- output[[i]];
        }else{
            arg_list_length                    <- length(arg_list) + 1;
            arg_list[[arg_list_length]]        <- output[[i]];
            names(arg_list)[[arg_list_length]] <- out_names[[i]];
        }
    }
    return(arg_list);
}

translate_results <- function(arg_list, output){
    arg_names <- names(arg_list);
    out_names <- names(output);
    for(i in 1:length(output)){
        if(out_names[[i]] %in% arg_names == TRUE){
            rep_pos <- which(arg_names == out_names[[i]]);
            arg_list[[rep_pos]] <- output[[i]];
        }
        if(out_names[[i]] == "resource_vector"){
            tot_res <- floor(output[[i]]);
            res_arr <- make_resource(resource_quantity = tot_res);
            typeNum <- length(output[[i]]);
            types   <- rep(x = 1:typeNum, times = output[[i]]);
            res_arr[,2] <- types;
            arg_list$resource_array <- res_arr;
        }
        if(out_names[[i]] == "resource_array" | out_names[[i]] == "RESOURCES"){
            typ_vec <- as.numeric(table(output[[i]][,2]));
            arg_list$resource_vector <- typ_vec;
        }
        if(out_names[[i]] == "observation_vector"){
            obs_arr <- make_resource(); # Dummy, since we already have estimate
            arg_list$observation_array <- obs_arr;
            if("PARAS" %in% arg_names == FALSE){
                stop("I can't find PARAS, and I need it");
            }
            arg_list$PARAS[9]   <- -1; # Tells manager to skip estimate
            arg_list            <- set_action_array(arg_list);
            thetar  <- arg_list$ACTION[arg_list$ACTION[,1,1]==-2, 5, 1];
            theobs  <- arg_list$observation_vector;
            arg_list$ACTION[arg_list$ACTION[,1,1]==1, 5, 1] <- thetar - theobs;
            arg_list <- gmse_apply_build_cost(arg_list);
        }
        if(out_names[[i]] == "observation_array" | 
           out_names[[i]] == "OBSERVATION"         ){
            if("PARAS" %in% arg_names == FALSE){
                stop("I can't find PARAS, and I need it");
            }
            arg_list <- estimate_abundances(arg_list);
            arg_list$PARAS[9]   <- -1; # Tells manager to skip estimate
            arg_list            <- set_action_array(arg_list);
            thetar  <- arg_list$ACTION[arg_list$ACTION[,1,1]==-2, 5, 1];
            theobs  <- arg_list$observation_vector;
            arg_list$ACTION[arg_list$ACTION[,1,1]==1, 5, 1] <- thetar - theobs;
            arg_list <- gmse_apply_build_cost(arg_list);
        }
        if(out_names[[i]] == "manager_vector"){
            thevec <- arg_list$manager_vector;
            if("ACTION" %in% arg_names){
                arg_list$ACTION[arg_list$ACTION[,1,1] == 1, 9, 1] <- thevec;
                arg_list <- copy_args(arg_list, "ACTION", "user_array");
            }
            if("COST" %in% arg_names){
                zcost  <- dim(arg_list$COST)[3];
                arg_list$COST[1:length(thevec), 9, 2:zcost] <- thevec;
                arg_list <- copy_args(arg_list, "COST", "manager_array");
            }else{
                arg_list$COST <- NA;
                arg_list <- gmse_apply_build_cost(arg_list);
                zcost    <- dim(arg_list$COST)[3];
                arg_list$COST[1:length(thevec), 9, 2:zcost] <- thevec;
                arg_list <- copy_args(arg_list, "COST", "manager_array");
            }
        }
        if(out_names[[i]] == "manager_array" | out_names[[i]] == "COST"){
            chk <- length(arg_list$ACTION[arg_list$ACTION[,1,1] == 1,1,1]);
            if( chk != length(arg_list$manager_vector) ){
                stop("manager model puts out too many resources in vec form");
            }
            rows                    <- which(arg_list$ACTION[, 1, 1] == 1);
            arg_list$manager_vector <- arg_list$ACTION[rows, 9, 1];
        }
        if(out_names[[i]] == "user_vector"){
            if(is.na(arg_list$user_array)[1] == TRUE){
                arg_list <- set_action_array(arg_list);
            }
            divuser <- arg_list$user_vector / dim(arg_list$user_array)[3];
            rows    <- which(arg_list$user_array[,1,1] == -2);
            arg_list$user_array[rows, 9, ] <- floor(divuser);
        }
        if(out_names[[i]] == "user_array" | out_names[[i]] == "ACTION"){
            if(is.na(arg_list$user_array)[1] == TRUE){
                arg_list <- set_action_array(arg_list);
            }
            u_out <- arg_list$user_array;
            rows  <- which(u_out[,1,1] == -2);
            acts  <- u_out[rows, 9, ];
            if(is.matrix(acts) == TRUE){
                allac <- apply(X = acts, MARGIN = 1, FUN = sum);  
            }else{
                allac <- sum(acts);
            }
            arg_list$user_vector <- allac;
        }
    }
    return(arg_list);
}

check_extinction <- function(arg_list){
    if(arg_list$resource_vector < 2){
        stop("Extinction has occurred -- no observation posible");
    }
    if(dim(arg_list$resource_array)[1] < 2){
        stop("Extinction has occurred -- no observation posible");
    }
}

gmse_apply_build_cost <- function(arg_list){
    arg_names <- names(arg_list);
    if("COST" %in% arg_names == FALSE){
        arg_list$COST <- NA;
        arg_names     <- names(arg_list);
    }
    if("AGENTS" %in% arg_names == FALSE | is.na(arg_list$AGENTS[1]) == TRUE){
        stop("I can't find an agent array, and I need to build the cost array");
    }
    if("resource_array" %in% arg_names == FALSE |
       is.na(arg_list$resource_array[1]) == TRUE){
        stop("I can't find a resource_array, & I need to build the cost array");
    }
    scaring <- arg_list$GMSE$scaring;
    if("scaring" %in% arg_names){
        scaring <- arg_list$scaring;
    }
    culling <- arg_list$GMSE$culling;
    if("culling" %in% arg_names){
        culling <- arg_list$culling;
    }
    castration <- arg_list$GMSE$castration;
    if("castration" %in% arg_names){
        castration <- arg_list$castration;
    }
    feeding <- arg_list$GMSE$feeding;
    if("feeding" %in% arg_names){
        feeding <- arg_list$feeding;
    }
    help_offspring <- arg_list$GMSE$help_offspring;
    if("help_offspring" %in% arg_names){
        help_offspring <- arg_list$help_offspring;
    }
    tend_crops <- arg_list$GMSE$tend_crops;
    if("tend_crops" %in% arg_names){
        tend_crops <- arg_list$tend_crops;
    }
    kill_crops <- arg_list$GMSE$kill_crops;
    if("kill_crops" %in% arg_names){
        kill_crops <- arg_list$kill_crops;
    }
    minimum_cost <- arg_list$GMSE$minimum_cost;
    if("minimum_cost" %in% arg_names){
        minimum_cost <- arg_list$minimum_cost;
    }
    user_res_opts  <- c(scaring, culling, castration, feeding, help_offspring);
    user_lnd_opts  <- c(tend_crops, kill_crops);
    AGENTS         <- arg_list$AGENTS;
    RESOURCES      <- arg_list$resource_array;
    arg_list$COST  <- make_costs( AGENTS = AGENTS, RESOURCES = RESOURCES,
                          res_opts = user_res_opts, lnd_opts = user_lnd_opts,
                          min_cost = minimum_cost);
    return(arg_list);
}

estimate_abundances <- function(arg_list){
    if(is.na(arg_list$AGENTS[1]) == FALSE){
        view <- arg_list$GMSE_agent;
    }
    if(is.na(arg_list$LAND[1]) == TRUE){
        dlndarg        <- collect_land_ini(arg_list);
        arg_list$LAND  <- do.call(what = make_landscape, args = dlndarg);
    }
    if(is.na(arg_list$AGENTS[1]) == TRUE){
        dagearg         <- collect_agent_ini(arg_list);
        arg_list$AGENTS <- do.call(what = make_agents, args = dagearg);
        arg_list$AGENTS <- add_agent_budget(arg_list$AGENTS, arg_list);
    }
    observations <- arg_list$observation_array;
    paras        <- arg_list$PARAS;
    land         <- arg_list$LAND;
    view         <- arg_list$AGENTS[1, 9];
    obs_method   <- paras[9];
    est          <- NA;
    if(obs_method == 0){
        est <- dens_est(observations, paras, view, land)$Nc;
    }
    if(obs_method == 1){
        est <- chapman_est(observations, paras)$Nc;
    }
    if(obs_method == 2 | obs_method == 3){
        est <- sum(observations[,13]);
    }
    if(is.na(est[1]) == TRUE){
        stop("I couldn't estimate population; check observe_type?");
    }
    arg_list$PARAS[100]         <- est[1];
    arg_list$observation_vector <- est;
    return(arg_list);
}

collect_agent_ini <- function(arg_list){
    make_age_list <- list();
    arg_names     <- names(arg_list);
    def_forms     <- formals(gmse);
    def_names     <- names(def_forms);
    make_age_list[[1]] <- "IBM";
    make_age_list[[2]] <- arg_list$GMSE$stakeholders + 1;
    make_age_list[[3]] <- c(1, arg_list$GMSE$stakeholders);
    if("stakeholders" %in% arg_names){
        make_age_list[[2]] <- arg_list$stakeholders + 1;
        make_age_list[[3]] <- c(1, arg_list$stakeholders);
    }
    make_age_list[[4]] <- arg_list$GMSE$agent_move;
    if("agent_move" %in% arg_names){
        make_age_list[[4]] <- arg_list$agent_move;
    }
    make_age_list[[5]] <- arg_list$GMSE$agent_view;
    if("agent_view" %in% arg_names){
        make_age_list[[5]] <- arg_list$agent_view;
    }
    make_age_list[[6]] <- arg_list$GMSE$land_dim_1;
    if("land_dim_1" %in% arg_names){
        make_age_list[[6]] <- arg_list$land_dim_1;
    }
    make_age_list[[7]] <- arg_list$GMSE$land_dim_2;
    if("land_dim_2" %in% arg_names){
        make_age_list[[7]] <- arg_list$land_dim_2;
    }
    return(make_age_list);
}

collect_itb_ini <- function(arg_list){
    make_itb_list <- list();
    arg_names     <- names(arg_list);
    def_forms     <- formals(gmse);
    def_names     <- names(def_forms);
    make_itb_list[[1]] <- NA;
    if("resource_array" %in% arg_names == TRUE){
        make_itb_list[[1]] <- arg_list$resource_array;
    }
    if(is.na(make_itb_list[[1]][1]) == TRUE){
        dresarg            <- collect_res_ini(arg_list);
        make_itb_list[[1]] <- do.call(what = make_resource, args = dresarg);
    }
    make_itb_list[[2]] <- NA;
    if("LAND" %in% arg_names == TRUE){
        make_itb_list[[2]] <- arg_list$LAND;
    }
    if(is.na(make_itb_list[[2]][1]) == TRUE){
        dlndarg            <- collect_land_ini(arg_list);
        make_itb_list[[2]] <- do.call(what = make_landscape, args = dlndarg);
    }
    
    return(make_itb_list);
}

add_obs_defaults <- function(arg_list){
    arg_names <- names(arg_list);
    res_pos   <- which(arg_names == "RESOURCES");
    arr_pos   <- which(arg_names == "resource_array");
    if(is.na(arg_list[[arr_pos]][1]) == FALSE){
        arg_list <- copy_args(arg_list = arg_list, from = "resource_array",
                              to = "RESOURCES");
    }
    if(is.na(arg_list[[res_pos]][1]) == TRUE){
        dresarg <- collect_res_ini(arg_list);
        ini_res <- do.call(what = make_resource, args = dresarg);
        arg_list[[res_pos]] <- ini_res;
    }
    para_pos  <- which(arg_names == "PARAS");
    if(is.na(arg_list[[para_pos]][1]) == TRUE){
        stop("I can't find a vector of parameters that should be initialised
              by default -- something has gone very wrong");
    }
    land_pos  <- which(arg_names == "LAND");
    if(is.na(arg_list[[land_pos]][1]) == TRUE){
        dlndarg  <- collect_land_ini(arg_list);
        ini_land <- do.call(what = make_landscape, args = dlndarg);
        arg_list[[land_pos]] <- ini_land;
    }
    agent_pos <- which(arg_names == "AGENTS");
    if(is.na(arg_list[[agent_pos]][1]) == TRUE){
        dagearg <- collect_agent_ini(arg_list);
        ini_age <- do.call(what = make_agents, args = dagearg);
        ini_age <- add_agent_budget(AGENTS = ini_age, arg_list = arg_list);
        arg_list[[agent_pos]] <- ini_age;
    }
    inter_tabl_pos <- which(arg_names == "inter_tabl");
    if(is.na(arg_list[[inter_tabl_pos]][1]) == TRUE){
        ditbarg <- collect_itb_ini(arg_list);
        ini_itb <- do.call(what = make_interaction_table, args = ditbarg);
        arg_list[[inter_tabl_pos]] <- ini_itb;
    }
    fm_pos <- which(arg_names == "fixed_mark");
    if(is.na(arg_list[[fm_pos]][1]) == TRUE){
        arg_list[[fm_pos]][1] <- arg_list$GMSE$fixed_mark;
    }
    tm_pos <- which(arg_names == "times_observe");
    if(is.na(arg_list[[tm_pos]][1]) == TRUE){
        arg_list[[tm_pos]][1] <- arg_list$GMSE$times_observe;
    }
    sm_pos <- which(arg_names == "res_min_age");
    if(is.na(arg_list[[sm_pos]][1]) == TRUE){
        arg_list[[sm_pos]][1] <- arg_list$GMSE$res_min_age;
    }
    aty_pos <- which(arg_names == "agent_type");
    if(is.na(arg_list[[aty_pos]][1]) == TRUE){
        arg_list[[aty_pos]][1] <- 0;
    }
    tca_pos <- which(arg_names == "type_cat");
    if(is.na(arg_list[[tca_pos]][1]) == TRUE){
        arg_list[[tca_pos]][1] <- 1;
    }
    ot_pos <- which(arg_names == "observe_type");
    if(is.na(arg_list[[ot_pos]][1]) == TRUE){
        arg_list[[ot_pos]][1] <- arg_list$GMSE$observe_type;
    }
    rmo_pos <- which(arg_names == "res_move_obs");
    if(is.na(arg_list[[rmo_pos]][1]) == TRUE){
        arg_list[[rmo_pos]][1] <- arg_list$GMSE$res_move_obs;
    }
    mod_pos <- which(arg_names == "model");
    if(is.na(arg_list[[mod_pos]][1]) == TRUE){
        arg_list[[mod_pos]][1] <- "IBM";
    }
    arg_list <- double_check_obs_type(arg_list);
    
    return(arg_list);
}

add_agent_budget <- function(AGENTS, arg_list){
    arg_name <- names(arg_list);
    manage_budget <- arg_list$GMSE$manager_budget;
    if("manager_budget" %in% arg_name){
        mbpos         <- which(arg_name == "manager_budget");
        manage_budget <- arg_list[[mbpos]];
    }
    user_budget <- arg_list$GMSE$user_budget;
    if("user_budget" %in% arg_name){
        ubpos         <- which(arg_name == "user_budget");
        user_budget   <- arg_list[[ubpos]];
    }
    AGENTS[AGENTS[,2]==0, 17] <- manage_budget;
    AGENTS[AGENTS[,2]==1, 17] <- user_budget;
    return(AGENTS);
}

double_check_obs_type <- function(arg_list){
    arg_names <- names(arg_list);
    if("observe_type"  %in% arg_names == FALSE | 
       "times_observe" %in% arg_names == FALSE ){
        return(arg_list);
    }
    ot_pos <- which(arg_names == "observe_type");
    tm_pos <- which(arg_names == "times_observe");
    if(arg_list[[ot_pos]][1] == 1){
        arg_list[[tm_pos]] <- 2;
    }
    return(arg_list);
}

prep_obs <- function(arg_list, obs_mod){
    if( identical(obs_mod, observation) == TRUE ){
        arg_list <- add_obs_defaults(arg_list);
    }
    obs_args <- list();
    arg_name <- names(arg_list);
    obs_take <- formals(obs_mod);
    obs_take <- obs_take[names(obs_take) != "..."];
    obs_name <- names(obs_take);
    for(arg in 1:length(obs_take)){
        arg_pos         <- which(obs_name[arg] == arg_name);
        obs_args[[arg]] <- arg_list[[arg_pos]];
    }
    names(obs_args) <- obs_name;
    return(obs_args);
}

update_para_vec <- function(arg_list){
    arg_name <- names(arg_list);
    if("PARAS" %in% arg_name == TRUE){
        arg_list$PARAS[33]  <- floor(sum(arg_list$resource_vector));
        arg_list$PARAS[100] <- arg_list$observation_vector[1];
    }
    return(arg_list);
}

set_action_array <- function(arg_list){
    arg_name <- names(arg_list);
    if("AGENTS" %in% arg_name == FALSE){
        arg_list$AGENTS <- NA;
        arg_name        <- names(arg_list);
    }
    if(is.na(arg_list$AGENTS[1]) == TRUE){
        dagearg         <- collect_agent_ini(arg_list);
        ini_age         <- do.call(what = make_agents, args = dagearg);
        ini_age         <- add_agent_budget(ini_age, arg_list);
        arg_list$AGENTS <- ini_age;
        arg_name        <- names(arg_list);
    }
    agent_pos <- which(arg_name == "AGENTS");
    res_pos   <- which(arg_name == "resource_array");
    if(is.na(arg_list[[res_pos]][1]) == TRUE){
        dresarg <- collect_res_ini(arg_list);
        ini_res <- do.call(what = make_resource, args = dresarg);
        arg_list[[res_pos]] <- ini_res;
    }
    ACTION <- make_utilities(arg_list[[agent_pos]], arg_list[[res_pos]]);
    
    manage_target <- arg_list$GMSE$manage_target;
    if("manage_target" %in% arg_name){
        mtpos         <- which(arg_name == "manage_target");
        manage_target <- arg_list[[mtpos]];
    }
    land_ownership <- arg_list$GMSE$land_ownership;
    if("land_ownership" %in% arg_name){
        lopos          <- which(arg_name == "land_ownership");
        land_ownership <- arg_list[[lopos]];
    }
    user_budget <- arg_list$GMSE$user_budget;
    if("user_budget" %in% arg_name){
        ubpos          <- which(arg_name == "user_budget");
        user_budget <- arg_list[[ubpos]];
    }
    manager_budget <- arg_list$GMSE$manager_budget;
    if("manager_budget" %in% arg_name){
        mbpos          <- which(arg_name == "manager_budget");
        manager_budget <- arg_list[[mbpos]];
    }
    stakeholder_rows             <- 2:dim(ACTION)[3];
    manager_row                  <- 1;
    ACTION[1, 5, manager_row]    <- manage_target;
    ACTION[3, 5:7 , manager_row] <- 0;
    if(land_ownership == TRUE){ # Set up utilities for land owning farmers
        ACTION[1, 5, stakeholder_rows]   <- 0;
        ACTION[2, 5, stakeholder_rows]   <- 100;
        ACTION[1, 6:7, stakeholder_rows] <- 1;
        ACTION[2, 6:7, stakeholder_rows] <- 1;
    }else{                      # Set up utilities for hunters of resources
        ACTION[1, 5, stakeholder_rows]   <- -1;
        ACTION[2, 5, stakeholder_rows]   <- 0;
    }
    arg_list[[agent_pos]][,17]     <- user_budget;
    arg_list[[agent_pos]][1,17]    <- manager_budget;
    arg_list$ACTION                <- ACTION;
    
    return(arg_list);
}

prep_man <- function(arg_list, man_mod){
    if( identical(man_mod, manager) == TRUE ){
        arg_list <- add_man_defaults(arg_list);
    }
    man_args <- list();
    arg_name <- names(arg_list);
    man_take <- formals(man_mod);
    man_take <- man_take[names(man_take) != "..."];
    man_name <- names(man_take);
    for(arg in 1:length(man_take)){
        arg_pos         <- which(man_name[arg] == arg_name);
        man_args[[arg]] <- arg_list[[arg_pos]];
    }
    names(man_args) <- man_name;
    return(man_args);
}

add_man_defaults <- function(arg_list){
    arg_names <- names(arg_list);
    res_pos   <- which(arg_names == "RESOURCES");
    arr_pos   <- which(arg_names == "resource_array");
    if(is.na(arg_list[[arr_pos]][1]) == FALSE){
        arg_list <- copy_args(arg_list = arg_list, from = "resource_array",
                              to = "RESOURCES");
    }
    if(is.na(arg_list[[res_pos]][1]) == TRUE){
        dresarg <- collect_res_ini(arg_list);
        ini_res <- do.call(what = make_resource, args = dresarg);
        arg_list[[res_pos]] <- ini_res;
    }
    para_pos  <- which(arg_names == "PARAS");
    if(is.na(arg_list[[para_pos]][1]) == TRUE){
        stop("I can't find a vector of parameters that should be initialised
              by default -- something has gone very wrong");
    }
    land_pos  <- which(arg_names == "LAND");
    if(is.na(arg_list[[land_pos]][1]) == TRUE){
        dlndarg  <- collect_land_ini(arg_list);
        ini_land <- do.call(what = make_landscape, args = dlndarg);
        arg_list[[land_pos]] <- ini_land;
    }
    agent_pos <- which(arg_names == "AGENTS");
    if(is.na(arg_list[[agent_pos]][1]) == TRUE){
        dagearg <- collect_agent_ini(arg_list);
        ini_age <- do.call(what = make_agents, args = dagearg);
        ini_age <- add_agent_budget(AGENTS = ini_age, arg_list = arg_list);
        arg_list[[agent_pos]] <- ini_age;
    }
    cost_pos <- which(arg_names == "COST");
    if(is.na(arg_list[[cost_pos]][1]) == TRUE){
        arg_list <- gmse_apply_build_cost(arg_list);
    }
    act_pos  <- which(arg_names == "ACTION");
    if(is.na(arg_list[[act_pos]][1]) == TRUE){
        arg_list <- set_action_array(arg_list);
    }
    jac_pos  <- which(arg_names == "INTERACT");
    if(is.na(arg_list[[jac_pos]][1]) == TRUE){
        arg_list <- set_interaction_array(arg_list);
    }
    inter_tabl_pos <- which(arg_names == "inter_tabl");
    if(is.na(arg_list[[inter_tabl_pos]][1]) == TRUE){
        ditbarg <- collect_itb_ini(arg_list);
        ini_itb <- do.call(what = make_interaction_table, args = ditbarg);
        arg_list[[inter_tabl_pos]] <- ini_itb;
    }
    obs_pos  <- which(arg_names == "OBSERVATION");
    if(is.na(arg_list[[obs_pos]][1]) == TRUE){ 
        arg_list <- copy_args(arg_list = arg_list, from = "observation_array",
                              to       = "OBSERVATION");
    }
    if(is.na(arg_list$OBSERVATION[1]) == TRUE){
        stop("I can't find observations for the manager model");
    }
    mod_pos <- which(arg_names == "model");
    if(is.na(arg_list[[mod_pos]][1]) == TRUE){
        arg_list[[mod_pos]][1] <- "IBM";
    }
    return(arg_list);
}

set_interaction_array <- function(arg_list){
    arg_names    <- names(arg_list);
    interact_pos <- which(arg_names == "INTERACT");
    if(is.na(arg_list[[interact_pos]][1]) == TRUE){
        ditbarg <- collect_itb_ini(arg_list);
        ini_itb <- do.call(what = make_interaction_array, args = ditbarg);
        arg_list[[interact_pos]] <- ini_itb;
    }
    res_consume <- arg_list$GMSE$res_consume;
    if("res_consume" %in% arg_names == TRUE){
        res_consume <- arg_list$res_consume;
    }
    arg_list[[interact_pos]][1,2] <- -1 * res_consume;
    return(arg_list);
}

prep_usr <- function(arg_list, usr_mod){
    if( identical(usr_mod, user) == TRUE ){
        arg_list <- add_usr_defaults(arg_list);
    }
    usr_args <- list();
    arg_name <- names(arg_list);
    usr_take <- formals(usr_mod);
    usr_take <- usr_take[names(usr_take) != "..."];
    usr_name <- names(usr_take);
    for(arg in 1:length(usr_take)){
        arg_pos         <- which(usr_name[arg] == arg_name);
        usr_args[[arg]] <- arg_list[[arg_pos]];
    }
    names(usr_args) <- usr_name;
    return(usr_args);
}

add_usr_defaults <- function(arg_list){
    arg_names <- names(arg_list);
    res_pos   <- which(arg_names == "RESOURCES");
    arr_pos   <- which(arg_names == "resource_array");
    if(is.na(arg_list[[arr_pos]][1]) == FALSE){
        arg_list <- copy_args(arg_list = arg_list, from = "resource_array",
                              to = "RESOURCES");
    }
    if(is.na(arg_list[[res_pos]][1]) == TRUE){
        dresarg <- collect_res_ini(arg_list);
        ini_res <- do.call(what = make_resource, args = dresarg);
        arg_list[[res_pos]] <- ini_res;
    }
    para_pos  <- which(arg_names == "PARAS");
    if(is.na(arg_list[[para_pos]][1]) == TRUE){
        stop("I can't find a vector of parameters that should be initialised
              by default -- something has gone very wrong");
    }
    land_pos  <- which(arg_names == "LAND");
    if(is.na(arg_list[[land_pos]][1]) == TRUE){
        dlndarg  <- collect_land_ini(arg_list);
        ini_land <- do.call(what = make_landscape, args = dlndarg);
        arg_list[[land_pos]] <- ini_land;
    }
    agent_pos <- which(arg_names == "AGENTS");
    if(is.na(arg_list[[agent_pos]][1]) == TRUE){
        dagearg <- collect_agent_ini(arg_list);
        ini_age <- do.call(what = make_agents, args = dagearg);
        ini_age <- add_agent_budget(AGENTS = ini_age, arg_list = arg_list);
        arg_list[[agent_pos]] <- ini_age;
    }
    cost_pos <- which(arg_names == "COST");
    mana_pos <- which(arg_names == "manager_array");
    if(is.na(arg_list)[[mana_pos]][1] == FALSE){
        arg_list <- copy_args(arg_list = arg_list, from = "manager_array",
                              to = "COST");
    }
    if(is.na(arg_list[[cost_pos]][1]) == TRUE){
        arg_list <- gmse_apply_build_cost(arg_list);
    }
    act_pos  <- which(arg_names == "ACTION");
    if(is.na(arg_list[[act_pos]][1]) == TRUE){
        arg_list <- set_action_array(arg_list);
    }
    jac_pos  <- which(arg_names == "INTERACT");
    if(is.na(arg_list[[jac_pos]][1]) == TRUE){
        arg_list <- set_interaction_array(arg_list);
    }
    inter_tabl_pos <- which(arg_names == "inter_tabl");
    if(is.na(arg_list[[inter_tabl_pos]][1]) == TRUE){
        ditbarg <- collect_itb_ini(arg_list);
        ini_itb <- do.call(what = make_interaction_table, args = ditbarg);
        arg_list[[inter_tabl_pos]] <- ini_itb;
    }
    mod_pos <- which(arg_names == "model");
    if(is.na(arg_list[[mod_pos]][1]) == TRUE){
        arg_list[[mod_pos]][1] <- "IBM";
    }
    return(arg_list);
}

gmse_apply_out <- function(arg_list, out, res_mod, obs_mod, man_mod, usr_mod,
                           u_res, u_obs, u_man, u_usr){
    if(out == "basic"){
        b_list  <- list();
        res_nme <- names(formals(res_mod));
        obs_nme <- names(formals(obs_mod));
        man_nme <- names(formals(man_mod));
        usr_nme <- names(formals(usr_mod));
        
        res_out <- arg_list$resource_vector;
        obs_out <- arg_list$observation_vector;
        man_out <- arg_list$manager_vector;
        if("manager_array" %in% usr_nme | "COST" %in% usr_nme){
            man_out <- get_manager_sum(arg_list);
        }
        usr_out <- arg_list$user_vector;
        if("manager_array" %in% usr_nme | "COST" %in% usr_nme){
            usr_out <- get_user_sum(arg_list);
        }
        b_list$resource_results    <- res_out;
        b_list$observation_results <- obs_out;
        b_list$manager_results     <- man_out;
        b_list$user_results        <- usr_out;
        
        return(b_list);
    }    
    if(out == "custom"){
        c_list                     <- list();
        c_list$resource_results    <- u_res;
        c_list$observation_results <- u_obs;
        c_list$manager_results     <- u_man;
        c_list$user_results        <- u_usr;
        
        return(c_list);
    }
    
    return(arg_list);
}

get_manager_sum <- function(arg_list){
    acts           <- arg_list$user_array;
    costs          <- arg_list$manager_array
    scaring        <- NA;
    culling        <- NA;
    castrating     <- NA;
    feeding        <- NA;
    help_offspring <- NA;
    u_scaring      <- arg_list$scaring;
    if(is.null(u_scaring)){
        u_scaring <- arg_list$GMSE$scaring;
    }
    if(u_scaring == TRUE){
        rows    <- which(acts[,1,1] == -2);
        scaring <- costs[rows, 8, 2];
    }
    u_culling      <- arg_list$culling;
    if(is.null(u_culling)){
        u_culling <- arg_list$GMSE$culling;
    }
    if(u_culling == TRUE){
        rows    <- which(acts[,1,1] == -2);
        culling <- costs[rows, 9, 2];
    }
    u_castrating      <- arg_list$castration;
    if(is.null(u_castrating)){
        u_castrating  <- arg_list$GMSE$castration;
    }
    if(u_castrating == TRUE){
        rows       <- which(acts[,1,1] == -2);
        castrating <- costs[rows, 10, 2];
    }
    u_feeding      <- arg_list$feeding;
    if(is.null(u_feeding)){
        u_feeding  <- arg_list$GMSE$feeding;
    }
    if(u_feeding == TRUE){
        rows       <- which(acts[,1,1] == -2);
        feeding    <- costs[rows, 10, 2];
    }
    u_help_offspring      <- arg_list$help_offspring;
    if(is.null(u_help_offspring)){
        u_help_offspring  <- arg_list$GMSE$help_offspring;
    }
    if(u_help_offspring == TRUE){
        rows           <- which(acts[,1,1] == -2);
        help_offspring <- costs[rows, 10, 2];
    }
    all_costs <- c(scaring, culling, castrating, feeding, help_offspring);
    cost_mat  <- matrix(data = all_costs, nrow = 1);
    colnames(cost_mat) <- c("scaring", "culling", "castration", "feeding",
                            "help_offspring");
    rownames(cost_mat) <- c("policy");
    
    return(cost_mat);
}

get_user_sum <- function(arg_list){
    acts           <- arg_list$user_array;
    scaring        <- NA;
    culling        <- NA;
    castrating     <- NA;
    feeding        <- NA;
    help_offspring <- NA;
    rows           <- which(acts[,1,1] == -2);
    types          <- c(acts[rows,2,]);
    act_mat        <- matrix(data = NA, nrow = length(types), ncol = 8);
    act_mat[,1]    <- types;
    u_scaring      <- arg_list$scaring;
    if(is.null(u_scaring)){
        u_scaring <- arg_list$GMSE$scaring;
    }
    if(u_scaring == TRUE){
        act_mat[,2] <- acts[rows, 8, ];
    }
    u_culling      <- arg_list$culling;
    if(is.null(u_culling)){
        u_culling <- arg_list$GMSE$culling;
    }
    if(u_culling == TRUE){
        act_mat[,3] <- acts[rows, 9, ];
    }
    u_castrating      <- arg_list$castration;
    if(is.null(u_castrating)){
        u_castrating  <- arg_list$GMSE$castration;
    }
    if(u_castrating == TRUE){
        act_mat[,4] <- acts[rows, 10, ];
    }
    u_feeding      <- arg_list$feeding;
    if(is.null(u_feeding)){
        u_feeding  <- arg_list$GMSE$feeding;
    }
    if(u_feeding == TRUE){
        act_mat[,5]    <- acts[rows, 11, ];
    }
    u_help_offspring      <- arg_list$help_offspring;
    if(is.null(u_help_offspring)){
        u_help_offspring  <- arg_list$GMSE$help_offspring;
    }
    if(u_help_offspring == TRUE){
        act_mat[,6] <- acts[rows, 12, ];
    }
    
    u_tend_crops      <- arg_list$tend_crops;
    if(is.null(u_tend_crops)){
        u_tend_crops  <- arg_list$GMSE$tend_crops;
    }
    if(u_tend_crops == TRUE){
        lrows       <- which(acts[,1,1] == -1);
        act_mat[act_mat[,1] == 1, 7] <- acts[lrows, 10, ];
    }
    u_kill_crops      <- arg_list$kill_crops;
    if(is.null(u_kill_crops)){
        u_kill_crops  <- arg_list$GMSE$kill_crops;
    }
    if(u_kill_crops == TRUE){
        lrows                        <- which(acts[,1,1] == -1);
        act_mat[act_mat[,1] == 1, 8] <- acts[lrows, 11, ];
    }
    colnames(act_mat) <- c("resource_type", "scaring", "culling", "castration", 
                            "feeding", "help_offspring", "tend_crops", 
                            "kill_crops");
    act_row_names    <- rep(x = NA, length = dim(act_mat)[1]);
    act_row_names[1] <- c("Manager");
    for(u in 2:dim(act_mat)[1]){
        act_row_names[u] <- paste("user_",u, sep = "");
    }
    rownames(act_mat) <- act_row_names;
    
    return(act_mat);
}
################################################################################

