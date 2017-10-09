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
gmse_apply <- function(res_mod  = resource, 
                       obs_mod  = observation, 
                       man_mod  = manager, 
                       use_mod  = user, 
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
    res_args <- prep_res(arg_list = arg_vals, res_mod = res_mod)
    check_args(arg_list = res_args, the_fun = res_mod);
    res_results <- do.call(what = res_mod, args = res_args);
    res_results <- check_name_results(output   = res_results, 
                                      vec_name = "resource_vector", 
                                      mat_name = "resource_array");
    arg_vals    <- add_results(arg_list = arg_vals, output = res_results);
    arg_vals    <- fix_gmse_defaults(arg_list = arg_vals, model = res_mod);
    arg_vals    <- translate_results(arg_list = arg_vals, output = res_results);
    arg_vals    <- update_para_vec(arg_list   = arg_vals);
    
    # ------ OBSERVATION MODEL -------------------------------------------------
    obs_args <- prep_obs(arg_list = arg_vals, obs_mod = obs_mod);
    check_args(arg_list = obs_args, the_fun = obs_mod);
    obs_results <- do.call(what = obs_mod, args = obs_args);
    obs_results <- check_name_results(output   = obs_results, 
                                      vec_name = "observation_vector", 
                                      mat_name = "observation_array");
    arg_vals    <- add_results(arg_list = arg_vals, output = obs_results);
    arg_vals    <- fix_gmse_defaults(arg_list = arg_vals, model = obs_mod);
    arg_vals    <- translate_results(arg_list = arg_vals, output = obs_results); # <- LEFT OFF HERE
    
    
    return(arg_vals);    
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
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

argument_list <- function(res_mod, obs_mod, man_mod, use_mod, oth_vals){
    oth_names   <- names(oth_vals);
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

check_name_results <- function(output, vec_name, mat_name){
    outname <- names(output);
    if(is.null(outname) == TRUE){
        if(is.vector(output[[1]]) == TRUE){
            names(output)[[1]] <- vec_name;
        }
        if(is.matrix(output[[1]]) == TRUE){
            names(output)[[1]] <- mat_name;
        }
    }
    return(output);
}

fix_gmse_defaults <- function(arg_list, model){
    arg_names <- names(arg_list);
    if( identical(model, resource) ){
        arg_list <- copy_args(arg_list, "RESOURCES", "resource_array");
    }
    if( identical(model, observation) ){
        arg_list <- copy_args(arg_list, "OBSERVATION", "observation_array");
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
            tot_res <- sum(arg_list[[i]]);
            res_arr <- make_resource(resource_quantity = tot_res);
            typeNum <- length(floor(arg_list[[i]]));
            types   <- rep(x = 1:typeNum, times = arg_list[[i]]);
            res_arr[,2] <- types;
            arr_pos  <- which(arg_names == "resource_array");
            arg_list[[arr_pos]] <- res_arr;
        }
        if(out_names[[i]] == "resource_array" | out_names[[i]] == "RESOURCES"){
            typ_vec <- as.numeric(table(output[[i]][,2]));
            vec_pos <- which(arg_names == "resource_vector");
            arg_list[[vec_pos]] <- typ_vec;
        }
        if(out_names[[i]] == "observation_vector"){
            obs_arr <- make_resource(); # Dummy, since we already have estimate
            arr_pos <- which(arg_names == "observation_array");
            arg_list[[arr_pos]] <- obs_arr;
            if("PARAS" %in% arg_names == TRUE){
                par_pos <- which(arg_names == "PARAS");
            }
            arg_list[[para_pos]][100] <- -1; # Tells manager to skip estimate
            arg_list                  <- set_action_array(arg_list);
            
            thetar  <- arg_list$ACTION[arg_list$ACTION[,1,1]==-2, 5, 1];
            theobs  <- arg_list$observation_vector;
            
            arg_list$ACTION[arg_list$ACTION[,1,1]==1, 5, 1] <- thetar - theobs;
            
            # Switch to the array and add to arg_list
            # Will need to get the manager function in c to recognise paras
            #     Do this by having a new 'hidden' observe_type = -1, which
            #     just uses PARAS[100] (R) PARAS[99] c to define the estimated
            #     abundance, then force whenever vector is returned
            
            # Put an if (obs_type >= 0) around lines 614-631 in manager.c
            # If obs_type = -1, ACTIONS array will come into manager.c with the
            # appropriate marginal utilities in the fourth column of the zero
            # layer (fifth col and first layer in R). Add the target to 
            # ACTION[1,5,1] (plus any other -2 values for different resource
            # types). Add the marginal utility (too high or low) to
            # ACTION[1,3,1] (again, with additional values for different
            # resources -- check to ensure this doesn't crash). 
            
            # Remember that only culling is default, so this is good so that 
            # other options will need to be forced by the gmse_apply user, as
            # many of them won't make sense.
        }
        if(out_names[[i]] == "observation_array" | 
           out_names[[i]] == "OBSERVATIONS"         ){
            # Switch to the vector and add to arg_list
            # Will need to run a function to estimate population size and
            #    return the appropriate vector from the estimate
            
            # This can be done by writing a function that recreates what's going
            # on in the first 274 lines of manager.c (some of this is already
            # available in the plotting function. In addition to the vector,
            # The estimate should also be added to PARAS[100]
        }
        if(out_names[[i]] == "manager_vector"){
            # Switch to the array and add to arg_list. Insert costs (or 
            #   converted cull abilities) into the relevant column of the COST
            #   array for resource type 1? Maybe leave up to the user.
        }
        if(out_names[[i]] == "manager_array" | out_names[[i]] == "COSTS"){
            # Switch to the vector and add to arg_list
        }
        if(out_names[[i]] == "user_vector"){
            # Switch to the array and add to arg_list
        }
        if(out_names[[i]] == "user_array" | out_names[[i]] == "ACTIONS"){
            # Switch to the vector and add to arg_list
        }
    }
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
        spos               <- which(arg_names == "stakeholders");
        make_age_list[[2]] <- arg_list[[spos]] + 1;
        make_age_list[[3]] <- c(1, arg_list[[spos]]);
    }
    make_age_list[[4]] <- arg_list$GMSE$agent_move;
    if("agent_move" %in% arg_names){
        apos               <- which(arg_names == "agent_move");
        make_age_list[[4]] <- arg_list[[apos]];
    }
    make_age_list[[5]] <- arg_list$GMSE$agent_view;
    if("agent_view" %in% arg_names){
        apos               <- which(arg_names == "agent_view");
        make_age_list[[5]] <- arg_list[[apos]];
    }
    make_age_list[[6]] <- arg_list$GMSE$land_dim_1;
    if("land_dim_1" %in% arg_names){
        apos               <- which(arg_names == "land_dim_1");
        make_age_list[[6]] <- arg_list[[apos]];
    }
    make_age_list[[7]] <- arg_list$GMSE$land_dim_2;
    if("land_dim_2" %in% arg_names){
        apos               <- which(arg_names == "land_dim_2");
        make_age_list[[7]] <- arg_list[[apos]];
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
        rpos               <- which(arg_names == "resource_array");
        make_itb_list[[1]] <- arg_list[[rpos]];
    }
    if(is.na(make_itb_list[[1]][1]) == TRUE){
        dresarg            <- collect_res_ini(arg_list);
        make_itb_list[[1]] <- do.call(what = make_resource, args = dresarg);
    }
    make_itb_list[[2]] <- NA;
    if("LAND" %in% arg_names == TRUE){
        lpos               <- which(arg_names == "LAND");
        make_itb_list[[2]] <- arg_list[[lpos]];
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
        arg_list[[agent_pos]] <- ini_age;
    }
    inter_tabl_pos <- which(arg_names == "inter_tabl");
    if(is.na(arg_list[[inter_tabl_pos]][1]) == TRUE){
        ditbarg <- collect_itb_ini(arg_list);
        ini_itb <- do.call(what = make_interaction_array, args = ditbarg);
        arg_list[[inter_tabl_pos]] <- ini_itb;
    }
    fm_pos <- which(arg_names == "fix_mark");
    if(is.na(arg_list[[fm_pos]][1]) == TRUE){
        arg_list[[fm_pos]][1] <- arg_list$GMSE$fix_mark;
    }
    tm_pos <- which(arg_names == "times");
    if(is.na(arg_list[[tm_pos]][1]) == TRUE){
        arg_list[[tm_pos]][1] <- arg_list$GMSE$times_observe;
    }
    sm_pos <- which(arg_names == "samp_age");
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
    ot_pos <- which(arg_names == "obs_method");
    if(is.na(arg_list[[ot_pos]][1]) == TRUE){
        arg_list[[ot_pos]][1] <- arg_list$GMSE$observe_type;
    }
    rmo_pos <- which(arg_names == "move_res");
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
        par_pos <- which(arg_name == "PARAS");
        res_pos <- which(arg_name == "resource_vector");
        arg_list[[par_pos]][33] <- floor(sum(arg_list[[res_pos]]));
    }
    return(arg_list);
}

set_action_array <- function(arg_list){
    arg_name <- names(arg_list);
    if("AGENTS" %in% arg_name){
        agent_pos <- which(arg_name == "AGENTS");
    }
    if(is.na(arg_list[[agent_pos]][1]) == TRUE){
        dagearg <- collect_agent_ini(arg_list);
        ini_age <- do.call(what = make_agents, args = dagearg);
        arg_list[[agent_pos]] <- ini_age;
    }
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
    
    if("ACTION" %in% arg_name){
        act_pos <- which(arg_name == "ACTION");
    }
    arg_list[[act_pos]] <- ACTION;
    
    return(arg_list);
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################




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
    fun_vals <- rep(x = NA, times = length(fun_args)); 
    for(i in 1:length(fun_args)){
        for(j in 1:length(all_arg_names)){
            if( fun_args[i] == "RESOURCES" &         # Handles an exception
                all_arg_names[j] == "resource_arr"){
                fun_vals[i] <- all_arg_values[j];
            }
            if( fun_args[i] == "resource_arr" &         # Handles an exception
                all_arg_names[j] == "RESOURCES"){
                fun_vals[i] <- all_arg_values[j];
            }
            if( identical(fun_args[i], all_arg_names[j]) == TRUE){
                fun_vals[i] <- all_arg_values[j];
            }
        }
    }
    for(i in 1:length(fun_args)){
        if(is.na(fun_vals)[i] == TRUE & formals(the_function)[i] != ""){
            fun_vals[i] <- formals(the_function)[i];
        }
    }
    return(fun_vals);
}





update_all_arguments <- function(mod_output, all_arguments, all_arg_names){
    for(i in 1:length(mod_output)){
        for(j in 1:length(all_arguments)){
            if(names(mod_output)[i] == all_arg_names[j]){
                all_arguments[j] <- mod_output[i];
            }
        }
    }
    return(all_arguments);
}














#=== SCRATCH BELOW


f1 <- function(x, z = 0, m = NULL) 2*x + z;
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
    
    return( x );
    
}







popmod <-function(resource_vector =100, sigma2_e=0.2, N_Harv=20, K=200, theta=1, r_max=1.0){
    
    resource_vec <- resource_vector;
    
    X_t0 <- resource_vec;
    
    eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
    X_star <- X_t0-N_Harv
    
    r <- (r_max*(1-(X_star/K)^theta))+eps
    X_t1 <- X_star*exp(r)
    
    PopRes <- list();
    PopRes$resource_vector <- X_t1;
    PopRes
    
}

popmod2 <-function(resource_vector =100, sigma2_e=0.2, N_Harv=20, K=200, theta=1, r_max=1.0){
    
    resource_vec <- resource_vector;
    
    X_t0 <- resource_vec;
    
    eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
    X_star <- X_t0-N_Harv
    
    r <- (r_max*(1-(X_star/K)^theta))+eps
    X_t1 <- X_star*exp(r)
    
    PopRes <- X_t1;
    PopRes
    
}

obs_mod1 <- function(scale="Abund", resource_vector=1000, bias=1, cv=0.2, LogNorm="ND"){
    
    value <- resource_vector;
    
    obs1 <-  switch(LogNorm,
                    LND={rlnorm(n=1, meanlog=log(value*bias), sdlog=cv)},
                    ND={rnorm(n=1,mean=value*bias, sd=cv*value)})
    
    obs1 <- switch(scale,
                   Abund={round(obs1)},
                   Dens={obs1})
    obs1
}

HarvDec1 <- function(HD_type="A", c=1000, qu=0.2, observation_vec=100){
    PopState_est <- observation_vec;
    TAC <- switch(HD_type,
                  A={PopState_est*qu},
                  B={ifelse(PopState_est>c, PopState_est-c, 0)},
                  C={qu},
                  D={ifelse(PopState_est>c, qu(PopState_est-c), 0)})
    
    TAC
}

#observation(RESOURCES = sim$resource[[1]], LAND = sim$land[[1]], PARAS = sim$paras[1,], AGENTS = sim$agents[[1]], inter_tabl = tbb, fix_mark = 50, times = 1, samp_age = 0, agent_type = 0, type_cat = 1, obs_method = 0, move_res = TRUE, model  = "IBM")


#test <- NULL;
#testrows <- NULL;
#for(i in 1:99){
#    test[[i]] <- gmse_apply(resource_model = resource, RESOURCES = sim$resource[[i]]);
#    testrows  <- rbind(testrows, c(i, dim(test[[i]][[2]][[1]])[1], test[[i]][[2]][[3]][33]));
#}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


argument_list <- function(res_mod, obs_mod, man_mod, use_mod, oth_vals){
    oth_names   <- names(oth_vals);
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
    arg_list    <- place_args(all_names, oth_vals, arg_list);
    arg_list    <- place_args(all_names, formals(res_mod), arg_list);
    arg_list    <- place_args(all_names, formals(obs_mod), arg_list);
    arg_list    <- place_args(all_names, formals(man_mod), arg_list);
    arg_list    <- place_args(all_names, formals(use_mod), arg_list);
    arg_out     <- list(all_arg_values = arg_list, all_arg_names = all_names);
    
    return(arg_out);        
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




replace_default_args <- function(argument_list, argument_names, from, to){
    from_names <- names(from);
    if("RESOURCES" %in% from_names){
        RESOURCE_pos <- which(argument_names == "RESOURCES");
        res_arr_pos  <- which(argument_names == "resource_array");
        res_vec_pos  <- which(argument_names == "resource_vector");
        argument_list[[res_arr_pos]] <- argument_list[[RESOURCE_pos]];
    }
    
    
    
    return( list(new_args = new_args) );
}


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


swap_vec_to_mat <- function(vec_name, mat_name, arg_list, arg_names, model){
  
  
  if(model == "resource"){
    
  }
  
  return(argument_list);
}

swap_mat_to_vec <- function(mat_name, vec_name, arg_list, arg_names, model){
  
  return(argument_list)
}

















