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
    all_arguments  <- as.list(sys.call());
    all_arg_names  <- names(all_arguments);
    res_arg_names  <- names(formals(res_mod));
    obs_arg_names  <- names(formals(obs_mod));
    man_arg_names  <- names(formals(man_mod));
    use_arg_names  <- names(formals(use_mod));
    f_arg_names    <- c(res_arg_names, obs_arg_names, man_arg_names, 
                        use_arg_names);
    f_arg_names    <- unique(f_arg_names);
    list_count     <- length(all_arguments);

    for(i in 1:length(all_arguments)){ # Needed to read in the variables
        all_arguments[[i]] <- eval(all_arguments[[i]]);
    }
    
    # ==========================================================================
    # PREPARE FOR THE RESOURCE MODEL TO BE RUN
    # ==========================================================================
    
    # Convert to names for resource() if need be
    if("resource_arr" %in% all_arg_names == TRUE & 
       "RESOURCES"    %in% all_arg_names == TRUE){
        r_m_loc       <- which(all_arg_names == "resource_arr");
        RES_loc       <- which(all_arg_names == "RESOURCES");
        all_arguments[[RES_loc]] <- all_arguments[[r_m_loc]];
    }
    if("resource_arr" %in% all_arg_names == TRUE & 
       "RESOURCES"    %in% all_arg_names == FALSE){
        r_m_loc       <- which(all_arg_names == "resource_arr");
        all_arguments[[list_count+1]] <- all_arguments[[r_m_loc]];
        all_arg_names[[list_count+1]] <- "RESOURCES";
        list_count <- list_count + 1;
    }
    if("resource_arr" %in% all_arg_names == FALSE & 
       "RESOURCES"    %in% all_arg_names == TRUE){
        RES_loc       <- which(all_arg_names == "RESOURCES");
        all_arguments[[list_count+1]] <- all_arguments[[RES_loc]];
        all_arg_names[[list_count+1]] <- "resource_arr";
        list_count <- list_count + 1;
    }
    if("resource_arr" %in% all_arg_names == FALSE & 
       "RESOURCES"    %in% all_arg_names == FALSE){
        all_arguments[[list_count+1]] <- NA
        all_arg_names[[list_count+1]] <- "resource_arr";
        list_count <- list_count + 1;
    }
    
    if("PARAS" %in% f_arg_names == TRUE & "PARAS" %in% all_arg_names == FALSE){
        allpars <- pass_paras(...); # Pass gmse linked arguments
        if("resource_arr" %in% all_arg_names == TRUE){
            resiloc <- which(all_arg_names == "resource_arr");
            if( is.na(all_arguments[[resiloc]][1])  == FALSE ){
                allpars$gmse_para_vect[33] <- dim(all_arguments[[resiloc]])[1];
            }
        }
        if("RESOURCES" %in% all_arg_names == TRUE){
            resiloc <- which(all_arg_names == "RESOURCES");
            if( is.na(all_arguments[[resiloc]][1])  == FALSE ){
                allpars$gmse_para_vect[33] <- dim(all_arguments[[resiloc]])[1];
            }
        }
        if("resource_vec" %in% all_arg_names){
            resiloc     <- which(all_arg_names == "resource_vec");
            allpars$gmse_para_vect[33] <- all_arguments[[resiloc]][1]
        }
        all_arg_names[[list_count+1]] <- "PARAS";
        all_arguments[[list_count+1]] <- allpars$gmse_para_vect;
        list_count <- list_count + 1;
    }
    
    gmse_user_input <- allpars$gmse_user_input;
    gmse_para_vect  <- allpars$gmse_para_vect;
    
    # Make a landscape if one is needed but not provided by the software user
    if("LAND" %in% f_arg_names == TRUE & "LAND" %in% all_arg_names == FALSE){
        all_arg_names[[list_count+1]] <- "LAND";
        if(gmse_para_vect[104] == TRUE){
            stake_pr    <- (1 - gmse_para_vect[105]) / (gmse_para_vect[55] - 1);
            land_alloc  <- c(gmse_para_vect[105], 
                             rep(x = stake_pr, times = gmse_para_vect[55] - 1));
        }else{
            land_alloc  <- c(1, rep(x = 0, times = gmse_para_vect[55] - 1)); 
        }
        LANDSCAPE_r  <- make_landscape( model       = "IBM", 
                                        rows        = gmse_para_vect[13], 
                                        cols        = gmse_para_vect[14], 
                                        cell_types  = 1,
                                        cell_val_mn = 1,
                                        cell_val_sd = 0,
                                        ownership   = 1:gmse_para_vect[55],
                                        owner_pr    = land_alloc
        );
        all_arguments[[list_count+1]] <- LANDSCAPE_r;
        list_count <- list_count + 1;
    }
    
    # Make agents if they are needed but not provided by the software user
    if("AGENTS" %in% f_arg_names == TRUE & 
       "AGENTS" %in% all_arg_names == FALSE){
        all_arg_names[[list_count+1]] <- "AGENTS";
        stakeholders <- 4;
        if("stakeholders" %in% all_arg_names == TRUE){
            where_stk    <- which(all_arg_names == "stakeholders");
            stakeholders <- as.numeric(all_arguments[where_stk]);
        }
        agent_view  <- 10;
        if("agent_view" %in% all_arg_names == TRUE){
            where_age  <- which(all_arg_names == "agent_view");
            agent_view <- as.numeric(all_arguments[where_age]);
        }
        agent_move  <- 0;
        if("agent_move" %in% all_arg_names == TRUE){
            where_mve  <- which(all_arg_names == "agent_move");
            agent_move <- as.numeric(all_arguments[where_mve]);
        }
        AGENTS   <- make_agents( model        = "IBM",
                                 agent_number = 1 + stakeholders,
                                 type_counts  = c(1, stakeholders),
                                 vision       = agent_view,
                                 rows         = gmse_para_vect[2],
                                 cols         = gmse_para_vect[3],
                                 move         = agent_move
        ); 
        all_arguments[[list_count+1]] <- AGENTS;
        list_count <- list_count + 1;
    }
    
    # --- Run the resource model function provided by the software user
    res_arg_vals  <- get_arg_list( the_function   = res_mod, 
                                   all_arg_names  = all_arg_names, 
                                   all_arg_values = all_arguments
                                 );
    if( identical(resource_model, resource) == TRUE ){
        res_arg_vals[[4]] <- "IBM";
        res_arg_vals[[5]] <- NULL;
    }
    
    res <- do.call(what = res_mod, args = res_arg_vals); 
    
    # ==========================================================================
    # PREPARE FOR THE OBSERVATION MODEL TO BE RUN
    # ==========================================================================
    
    res_vector_output  <- TRUE;
    if(length(res) == 1){
        names(res) <- "resource_vec";
        all_arg_names[[list_count+1]] <- "resource_vec";
        all_arguments[[list_count+1]] <- res$resource_vec;
        list_count                    <- list_count + 1;
    }
    if( "resource_arr" %in% names(res) == TRUE |
        "RESOURCES"     %in% names(res) == TRUE){
        res_vector_output <- FALSE;
    }
    if( "resource_arr" %in% names(res) == FALSE & 
        "resource_vec" %in% names(res) == FALSE){
        if("RESOURCES" %in% names(res) == FALSE){
            stop("ERROR: Resource model must return a 'resource_arr' (array) 
                 list element, a 'resource_vec' (vector) list element, or a 
                 scalar");
        }
        r_a_p <- which(all_arg_names == "resource_arr");
        all_arguments[[r_a_p]] <- res$RESOURCES;
    }
    if( "RESOURCE" %in% names(res) == TRUE & 
        "RESOURCES" %in% all_arg_names == TRUE){
        r_a_p <- which(all_arg_names == "RESOURCES");
        all_arguments[[r_a_p]] <- res$RESOURCE;
    }
    if( "resource_arr" %in% names(res)    == TRUE &
        "resource_arr" %in% all_arg_names == FALSE){
        all_arg_names[[list_count+1]] <- "resource_arr";
        all_arguments[[list_count+1]] <- res$resource_arr;
        list_count                    <- list_count + 1;
    }
    if( "resource_vec" %in% names(res)    == TRUE &
        "resource_vec" %in% all_arg_names == FALSE){
        all_arg_names[[list_count+1]] <- "resource_vec";
        all_arguments[[list_count+1]] <- res$resource_vec;
        list_count                    <- list_count + 1;
    }
    
    all_arguments <- update_all_arguments(mod_output    = res, 
                                          all_arguments = all_arguments, 
                                          all_arg_names = all_arg_names);
    
    # ADD A WAY TO CHECK FOR EXTINCTION HERE
    
    if( res_vector_output == TRUE ){
        rvec         <- floor(res$resource_vec);
        totalr       <- sum(rvec);
        mat_resource <- make_resource(resource_quantity = totalr);
        if(length(rvec) > 1){
            types <- rep(x = 1:totalr, times = rvec);
        }
        r_m_pos <- which(all_arg_names == "resource_arr")[1]
        all_arguments[[r_m_pos]] <- mat_resource;
        if("PARAS" %in% all_arg_names == TRUE){
            para_pos <- which(all_arg_names == "PARAS")[1];
            all_arguments[[para_pos]][33] <- totalr;
        }
    }
    if( res_vector_output == FALSE ){
        rvec         <- as.vector(table(res$resource_arr[,2]));
        if( is.null(res$RESOURCE) == FALSE){
            rvec         <- as.vector(table(res$RESOURCE[,2]));
        }
        all_arg_names[[list_count+1]] <- "resource_vec";
        all_arguments[[list_count+1]] <- rvec;
        list_count                    <- list_count + 1;
    }
    if("inter_tabl" %in% f_arg_names == TRUE & 
       "inter_tabl" %in% all_arg_names == FALSE){
        all_arg_names[[list_count+1]] <- "inter_tabl";
        r_a_p      <- which(all_arg_names == "resource_arr");
        l_pos      <- which(all_arg_names == "LAND");
        inter_tabl <- make_interaction_table(RESOURCES = all_arguments[[r_a_p]], 
                                             LAND      = all_arguments[[l_pos]]
                                             );
        all_arguments[[list_count+1]] <- inter_tabl;
        list_count <- list_count + 1;
    }
    
    # --- Run the observation model function provided by the software user
    obs_arg_values  <- get_arg_list( the_function   = obs_mod, 
                                     all_arg_names  = all_arg_names, 
                                     all_arg_values = all_arguments);
    
    # --- Some adjustments for running the observation model
    if( identical(observation_model, observation) == TRUE){
        pa_pos               <- which(all_arg_names == "PARAS")[1];
        obs_arg_values[[6]]  <- all_arguments[[pa_pos]][11]; 
        obs_arg_values[[7]]  <- all_arguments[[pa_pos]][12];
        obs_arg_values[[8]]  <- all_arguments[[pa_pos]][17];
        obs_arg_values[[11]] <- all_arguments[[pa_pos]][9];
        obs_arg_values[[12]] <- all_arguments[[pa_pos]][20];
        obs_arg_values[[13]] <- "IBM";
        obs_arg_values[[14]] <- NULL;
    }
    
    ## ERROR ONLY WITH resource_arr argument.
    obs <- do.call(what = obs_mod, args = obs_arg_values);
    
    # ==========================================================================
    # PREPARE FOR THE MANAGER MODEL TO BE RUN
    # ==========================================================================
    
    
    return(list(res, obs));
    
    
    obs_vector_output  <- TRUE;
    if(length(obs) == 1){
        names(obs) <- "observation_vec";
        all_arg_names[[list_count+1]] <- "observation_vec";
        all_arguments[[list_count+1]] <- obs$observation_vec;
        list_count                    <- list_count + 1;
    }
    if( "observation_arr"  %in% names(obs) == TRUE |
        "OBSERVATION"      %in% names(obs) == TRUE){
        obs_vector_output <- FALSE;
    }
    if( "observation_arr" %in% names(obs) == FALSE & 
        "observation_vec" %in% names(obs) == FALSE){
        if("OBSERVATION" %in% names(obs) == FALSE){
            stop("ERROR: Resource model must return a 'observation_arr' (array) 
                 list element, a 'observation_vec' (vector) list element, or a 
                 scalar");
        }
        o_a_p <- which(all_arg_names == "OBSERVATION");
        all_arguments[[o_a_p]] <- obs$OBSERVATION;
    }
    if( "observation_arr" %in% names(obs)    == TRUE &
        "observation_arr" %in% all_arg_names == FALSE){
        all_arg_names[[list_count+1]] <- "observation_arr";
        all_arguments[[list_count+1]] <- obs$observation_arr;
        list_count                    <- list_count + 1;
    }
    if( "observation_vec" %in% names(obs)    == TRUE &
        "observation_vec" %in% all_arg_names == FALSE){
        all_arg_names[[list_count+1]] <- "observation_vec";
        all_arguments[[list_count+1]] <- obs$observation_vec;
        list_count                    <- list_count + 1;
    }
    
    return(list(res, obs));
    
    all_arguments <- update_all_arguments(mod_output    = obs, 
                                          all_arguments = all_arguments, 
                                          all_arg_names = all_arg_names);
    
    
    
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







popmod <-function(resource_vec =100, sigma2_e=0.2, N_Harv=20, K=200, theta=1, r_max=1.0){
    
    X_t0 <- resource_vec;
    
    eps <- rnorm(1, mean=0, sd=sqrt(sigma2_e))
    X_star <- X_t0-N_Harv
    
    r <- (r_max*(1-(X_star/K)^theta))+eps
    X_t1 <- X_star*exp(r)
    
    PopRes <- list();
    PopRes$resource_vec <- X_t1;
    PopRes
    
}

obs_mod1 <- function(scale="Abund", resource_vec=1000, bias=1, cv=0.2, LogNorm="ND"){
    
    value <- resource_vec;
    
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




resource_model <- resource;
observation_model <- observation;
manager_model <- manager;
user_model <- user;








res_mod <- match.fun(resource_model);
obs_mod <- match.fun(observation_model);
man_mod <- match.fun(manager_model);
use_mod <- match.fun(user_model);

# Sort out the arguments for each function, and the rest
all_arguments  <- as.list(sys.call());
all_arg_names  <- names(all_arguments);
res_arg_names  <- names(formals(res_mod));
obs_arg_names  <- names(formals(obs_mod));
man_arg_names  <- names(formals(man_mod));
use_arg_names  <- names(formals(use_mod));
f_arg_names    <- c(res_arg_names, obs_arg_names, man_arg_names, 
                    use_arg_names);
f_arg_names    <- unique(f_arg_names);
list_count     <- length(all_arguments);

for(i in 1:length(all_arguments)){ # Needed to read in the variables
    all_arguments[[i]] <- eval(all_arguments[[i]]);
}




argument_list <- function(res_mod, obs_mod, man_mod, use_mod, ...){
    oth_vals    <- as.list(sys.call());
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
    for(i in 1:length(placing_vals)){
        place_name <- names(placing_vals[i]);
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



swap_vec_to_mat <- function(){}

swap_mat_to_vec <- function(){}

















