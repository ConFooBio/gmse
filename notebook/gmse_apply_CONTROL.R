gmse_apply_INTERIM = function(res_mod  = resource, 
                              obs_mod  = observation, 
                              man_mod  = manager, 
                              use_mod  = user,
                              get_res  = "basic",
                              old_list = NULL,
                              ...) {
    fun_warn(res_mod, obs_mod, man_mod, use_mod);
    
    if(is.null(old_list) == FALSE){
        old_list <- swap_old_gmse(old_list);
    }
    
    std_paras           <- pass_paras(old_list, ...);
    #std_paras           <- pass_paras(old_list);       # removed ellipsis to mimic function call
    all_args            <- as.list(sys.call());
    #all_args = as.list(call_bogus_for_debug(get_res = "Full", old_list = sim_old))  # replicates arguments recall
    
    check_the_var_names(all_args);
    
    all_args[["PARAS"]] <- std_paras[["gmse_para_vect"]]; 
    all_args[["ilist"]] <- std_paras[["gmse_user_input"]]; 
    all_args[["GMSE"]]  <- formals(gmse); 
    
    needed_args <- argument_list(res_mod, obs_mod, man_mod, use_mod, all_args); 
    arg_vals    <- needed_args[["all_arg_values"]]; 
    arg_name    <- needed_args[["all_arg_names"]];
    
    names(arg_vals) <- arg_name;
    
    if(is.null(old_list) == FALSE){
        arg_vals <- apply_old_gmse(arg_vals, old_list, ...);
        #arg_vals <- apply_old_gmse(arg_vals, old_list);  # removed ellipsis to mimic function call
    }
    
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
    #usr_args    <- prep_usr(arg_list = arg_vals, usr_mod = use_mod);
    #check_args(arg_list = usr_args, the_fun = use_mod);
    #usr_results <- do.call(what = use_mod, args = usr_args);
    usr_results = list(RESOURCES = arg_vals$RESOURCES,
                       AGENTS = arg_vals$AGENTS,
                       LAND = arg_vals$LAND,
                       ACTION = arg_vals$ACTION,
                       COST = arg_vals$COST,
                       PARAS = arg_vals$PARAS
                       )
    
    # usr_results$RESOURCES == arg_vals$RESOURCES
    # usr_results$AGENTS == arg_vals$AGENTS
    # usr_results$LAND == arg_vals$LAND
    # usr_results$ACTION == arg_vals$ACTION
    # usr_results$COST == arg_vals$COST
    # usr_results$PARAS == arg_vals$PARAS
    ### Adapt COST, possibly RESOURCES (culls?)
    usr_results$ACTION[1,8:12,2:dim(usr_results$ACTION)[3]] = 0  ########## RESETS ALL ACTIONS TO ZERO
    usr_results$RESOURCES[,17] = 0      ########## ENSURES no actions on resources were taken across (culls)
    usr_results$RESOURCES[,16] = 0      ########## ENSURES no actions on resources were taken across (scares)
    usr_results$RESOURCES[,18] = 0      ########## ENSURES no actions on resources were taken across (castrations)
    
    usr_results <- check_name_results(output   = usr_results, 
                                      vec_name = "user_vector", 
                                      mat_name = "user_array");
    arg_vals    <- add_results(arg_list = arg_vals, output = usr_results);
    arg_vals    <- fix_gmse_defaults(arg_list = arg_vals, model = use_mod);
    arg_vals    <- translate_results(arg_list = arg_vals, output = usr_results);
    arg_vals    <- update_para_vec(arg_list   = arg_vals);
    
    res <- gmse_apply_out(arg_vals, get_res, res_mod, obs_mod, man_mod, use_mod,
                          res_results, obs_results, man_results, usr_results);
    invisible( gc() );
    return(res);  
}

gmse_apply_UROM = function(res_mod  = resource, 
                               obs_mod  = observation, 
                               man_mod  = manager, 
                               use_mod  = user,
                               get_res  = "basic",
                               old_list = NULL,
                               ...) {
    
    fun_warn(res_mod, obs_mod, man_mod, use_mod);
    
    if(is.null(old_list) == FALSE){
        old_list <- swap_old_gmse(old_list);
    }
    
    std_paras           <- pass_paras(old_list, ...);
    #all_args = as.list(call_bogus_for_debug(get_res = "Full", old_list = sim_old))
    all_args            <- as.list(sys.call());
    
    check_the_var_names(all_args);
    
    all_args[["PARAS"]] <- std_paras[["gmse_para_vect"]]; 
    all_args[["ilist"]] <- std_paras[["gmse_user_input"]]; 
    all_args[["GMSE"]]  <- formals(gmse); 
    
    needed_args <- argument_list(res_mod, obs_mod, man_mod, use_mod, all_args); 
    arg_vals    <- needed_args[["all_arg_values"]]; 
    arg_name    <- needed_args[["all_arg_names"]];
    
    names(arg_vals) <- arg_name;
    
    if(is.null(old_list) == FALSE){
        arg_vals <- apply_old_gmse(arg_vals, old_list, ...);
    }
    
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
    
    ###### APPARENTLY "user actions" are reset following the observation model!
    ###### So if we want a return of actions taken, we need to do this here.
    PREV_ACTS = arg_vals$ACTION
    
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
    
    res <- gmse_apply_out(arg_vals, get_res, res_mod, obs_mod, man_mod, use_mod,
                          res_results, obs_results, man_results, usr_results);
    res$PREV_ACTS = PREV_ACTS
    
    invisible( gc() );
    return(res);
    
}

user_NULL <- function(RESOURCES  = NULL,
                      AGENTS     = NULL,
                      LAND       = NULL, 
                      PARAS      = NULL,
                      COST       = NULL,
                      ACTION     = NULL){
    
    USER_OUT = list(RESOURCES, AGENTS, LAND, ACTION, COST, PARAS)
    names(USER_OUT) <- c("RESOURCES", "AGENTS", "LAND", "ACTION", "COST","PARAS");

    return(USER_OUT);
}

observed_suggested = function(dat) {
    out = list(observed = round(dat$basic_output$observation_results,2), 
               culling = dat[["COST"]][1,9,2:dim(dat$COST)[3]],
               scaring = dat[["COST"]][1,8,2:dim(dat$COST)[3]])
    return(out)
}

set_man_costs = function(dat, newcost) {
    ### Currently implemented for culling and scaring only
    
    ### Check names of newcost
    valid_costs = c("culling","scaring")
    if(sum(!(names(newcost) %in% valid_costs)) != 0) stop("Invalid costs specified.")
    
    n_users = dim(dat$COST)[3]-1
    
    ### Get positions of actions in COST array
    pos = list()
    pos$culling = cbind(rep(1,n_users),rep(9,n_users))
    pos$scaring = cbind(rep(1,n_users),rep(8,n_users))
    
    ### Repeat costs for each user in cases where new cost was given as a scalar:
    newcost = lapply(newcost, function(x) { 
        if(length(x)==1) { x = rep(x, n_users) } else { x = x } 
        })
    ### Check length of given new cost vectors. If they are not now all the same, stop.
    check_length = unlist(lapply(newcost, function(x) length(x)!=n_users))
    if(sum(check_length)>0) {
        which_wrong = names(which(check_length))
        which_wrong = paste(which_wrong, collapse = ", ")
        stop(paste("New costs given for: '", which_wrong, "' not equal to number of users.", sep = ""))
    }
    
    for(i in 1:length(newcost)) {
         costname_i = names(newcost)[i]
         cost_i = newcost[[i]]
         dat[["COST"]][cbind(pos[[costname_i]],2:(n_users+1))] = cost_i
    }
    
    ### Ensure `manager_array` is the same as `COST`
    dat[["manager_array"]] = dat[["COST"]]
    dat[["manager_vector"]] = unique(newcost[["culling"]])
    return(dat)
}

##### init_man_control is intended as a "setup" for manager control gmse_apply() runs as tested in
##### gmse_apply_CONTROL_EXAMPLE.R. The idea is to run K-1 iterations of "default" gmse_apply() with given parameters,
##### followed by a single run of gmse_apply_interim.

init_man_control = function(K = 5) {
    
    K1 = K-1 # Because the "interim" time step will be the fifth, where actions aren't yet taken (pending user input)
    print("K1 set")
    
    sim_old = gmse_apply(get_res = "Full", 
                         land_ownership = TRUE, 
                         observe_type = 3, 
                         res_move_obs = FALSE, 
                         res_death_K = 1500,
                         lambda = 0.275,
                         manage_target = 1000, 
                         stakeholders = 4
    )
    print("First gmse_apply() call success")
    
    output = gmse_apply_summary(sim_old, include = c("res","obs","culls","scares", "cull_cost", "scare_cost", "RES_CULLS"))
    
    assign("sim_old", sim_old, env = .GlobalEnv)
    
    #### 1. First K1-1 time steps as normal to set up population run:
    for(i in 2:K1) {
        sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
        output = gmse_apply_summary(sim_new, output)
        sim_old = sim_new
    }
    
    #### 2. Run "interim" time step where user actions as surpressed:
    sim_new = gmse_apply_INTERIM(get_res = "Full", old_list = sim_old)
    output = gmse_apply_summary(sim_new, output) 
    # Reset selected output for interim time step (as no actions have been taken)
    output[nrow(output),c("culls","scares","cull_cost","scare_cost","RES_CULLS")] = NA
    
    init_steps = list(output = output, 
                      observed_suggested = observed_suggested(sim_new))
    return(init_steps)
}


############################# UTILITY FUNCTIONS - FOR DEBUGGING PURPOSES ONLY ########################################

testing_wrapper = function(K = 1) {
    
    test_old = gmse_apply(get_res = "Full", 
                          land_ownership = LAND_OWNERSHIP, 
                          scaring = SCARING, 
                          manage_target = MANAGE_TARGET, 
                          res_death_K = RES_DEATH_K, 
                          lambda = LAMBDA)
    output = gmse_apply_summary(test_old, include = c("res","obs","culls","scares"))
    print("bleep")
    for(i in 1:K) {
        test_new = gmse_apply(get_res = "Full", old_list = test_old)
        output = gmse_apply_summary(test_new, output)
        test_old = test_new    
    }

    return(output)

}

call_bogus_for_debug = function(res_mod  = resource, 
         obs_mod  = observation, 
         man_mod  = manager, 
         use_mod  = user,
         get_res  = "basic",
         old_list = NULL,
         ...) {
    return(sys.call())
}

check_res = function(m) {
    return(cbind(nrow(m$resource_array), nrow(m$RESOURCES), m$resource_vector, m$observation_vector))
}

check_acts = function(m) {
    return(list(users = t(m$ACTION[1,8:12,2:5]), manager = m$ACTION[1,8:12,1]))
}

check_costs = function(m) {
    return(list(users = t(m$COST[1,8:12,2:5]), manager = m$COST[3,8:12,1]))
}

test_resource = function(resource_vector) {
    return(resource_vector)
}
