#' gmse_apply_INTERIM
#'
#' A modified implementatiom of gmse_apply() which in effect "skips" the user model, simply passing through the usual data structures unmodified. This ensures that the function output overall is exactly the same as vanilla gmse_apply() but without any user actions enacted, ensuring any remaining non-zero actions are reset to zero.
#'
#'@param res_mod The function specifying the resource model. By default, the individual-based resource model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'resource_array' or 'resource_vector', and arrays must follow the format of GMSE in terms of column number and type (if there is only one resource type, then the model can also just return a scalar value).
#'@param obs_mod The function specifying the observation model. By default, the individual-based observation model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'observation_array' or 'observation_vector', and arrays must follow the format of GMSE in terms of column number and type  (if there is only one resource type, then the model can also just return a scalar value).
#'@param man_mod The function specifying the manager model. By default, the individual-based manager model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'manager_array' or 'manager_vector', and arrays must follow the (3 dimensional) format of the 'COST' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired). User defined manager outputs will be recognised as costs by the default user model in gmse, but can be interpreted differently (e.g., total allowable catch) if specifying a custom user model.
#'@param use_mod The function specifying the user model. By default, the individual-based user model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'user_array' or 'user_vector', and arrays must follow the (3 dimensional) format of the 'ACTION' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired).
#'@param get_res How the output should be organised. The default 'basic' attempts to distill results down to their key values from submodel outputs, including resource abundances and estimates, and manager policy and actions. An option 'custom' simply returns a large list that includes the output of every submodel. Any other option (e.g. 'none') will return a large list with all of the input, output, and parameters used to run gmse_apply. This list will also include a list element named 'basic_output', which will display the default results.
#'@param old_list A an existing list of results from gmse_apply, produced by setting `get_res = TRUE` to be included in the function. The parameter and data structures from the previous run will be applied to the new run of gmse_apply, thereby making it easy to loop multiple generations. Additional arguments passed to `...` will over-ride those stored in the old list, allowing global parameter values to be updated (e.g., sub-models used, management options, genetic algorithm parameters). Note that if these arguments are passed, the function will attempt to work with them even if it means removing previous list elements (e.g., if a new number of stakeholders is passed through stakeholder = new_value, then an entirely new AGENT array and user and manager arrays will need to be built).
#'@param ... Arguments passed to user-defined functions, and passed to modify default parameter values that would otherwise be called for gmse default models. Any argument that can be passed to gmse can be specified explicitly, just as if it were an argument to gmse. Similarly, any argument taken by a user-defined function should be specified, though the function will work if the user-defined function has a default that is not specified explicitly.
#'@details To integrate across different types of submodels, gmse_apply translates between vectors and arrays between each submodel. For example, because the default GMSE observation model requires a resource array with particular requirements for column identites, when a resource model subfunction returns a vector, or a list with a named element 'resource_vector', this vector is translated into an array that can be used by the observation model. Specifically, each element of the vector identifies the abundance of a resource type (and hence will usually be just a single value denoting abundance of the only focal population). If this is all the information provided, then a resource_array will be made with default GMSE parameter values with an identical number of rows to the abundance value (floored if the value is a non-integer; non-default values can also be put into this transformation from vector to array if they are specified in gmse_apply, e.g., through an argument such as lambda = 0.8). Similarly, a 'resource_array' is also translated into a vector after the default individual-based resource model is run, should the observation model require simple abundances instead of an array. The same is true of 'observation_vector' and 'observation_array' objects returned by observation models, of 'manager_vector' and 'manager_array' (i.e., COST) objects returned by manager models, and of 'user_vector' and 'user_array' (i.e., ACTION) objects returned by user models. At each step, a translation between the two is made, with necessary adjustments that can be tweaked through arguments to gmse_apply when needed.
#'@return A matrix of gmse_apply values, summarised.
#'@examples
#'\dontrun{
#'sim_summary <- sim_old <- gmse_apply(get_res = "Full", scaring = FALSE, land_ownership = T);
#'gmse_apply_summary(data = sim_old, include = c("res","obs","culls","yield"))
#'}
#'@export
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
    res$PREV_ACTS = PREV_ACTS

    invisible( gc() );
    return(res);  
}

#' gmse_apply_UROM
#'
#' A modified implementatiom of gmse_apply(), which runs submodels in the order of User, Resource, Observation, Manager (UROM), as opposed to the usual order of resource, observation, manager user. This is intended to be run following a run of gmse_apply_INTERIM(), which in effect "ended" with costs set by a "manager" - this could be direct user input. As a result, user actions are the first to be evaluated here, again ending with costs set by a manager - which again may be overridden. In addition to the change in order of operation, a further data structure (PREV_ACTS) is appended to the output structure; this is the ACTION array as it was before actions were reset again - i.e. the actions users took when gmse_apply_UROM() was called.
#'
#'@param res_mod The function specifying the resource model. By default, the individual-based resource model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'resource_array' or 'resource_vector', and arrays must follow the format of GMSE in terms of column number and type (if there is only one resource type, then the model can also just return a scalar value).
#'@param obs_mod The function specifying the observation model. By default, the individual-based observation model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'observation_array' or 'observation_vector', and arrays must follow the format of GMSE in terms of column number and type  (if there is only one resource type, then the model can also just return a scalar value).
#'@param man_mod The function specifying the manager model. By default, the individual-based manager model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'manager_array' or 'manager_vector', and arrays must follow the (3 dimensional) format of the 'COST' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired). User defined manager outputs will be recognised as costs by the default user model in gmse, but can be interpreted differently (e.g., total allowable catch) if specifying a custom user model.
#'@param use_mod The function specifying the user model. By default, the individual-based user model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'user_array' or 'user_vector', and arrays must follow the (3 dimensional) format of the 'ACTION' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired).
#'@param get_res How the output should be organised. The default 'basic' attempts to distill results down to their key values from submodel outputs, including resource abundances and estimates, and manager policy and actions. An option 'custom' simply returns a large list that includes the output of every submodel. Any other option (e.g. 'none') will return a large list with all of the input, output, and parameters used to run gmse_apply. This list will also include a list element named 'basic_output', which will display the default results.
#'@param old_list A an existing list of results from gmse_apply, produced by setting `get_res = TRUE` to be included in the function. The parameter and data structures from the previous run will be applied to the new run of gmse_apply, thereby making it easy to loop multiple generations. Additional arguments passed to `...` will over-ride those stored in the old list, allowing global parameter values to be updated (e.g., sub-models used, management options, genetic algorithm parameters). Note that if these arguments are passed, the function will attempt to work with them even if it means removing previous list elements (e.g., if a new number of stakeholders is passed through stakeholder = new_value, then an entirely new AGENT array and user and manager arrays will need to be built).
#'@param ... Arguments passed to user-defined functions, and passed to modify default parameter values that would otherwise be called for gmse default models. Any argument that can be passed to gmse can be specified explicitly, just as if it were an argument to gmse. Similarly, any argument taken by a user-defined function should be specified, though the function will work if the user-defined function has a default that is not specified explicitly.
#'@details To integrate across different types of submodels, gmse_apply translates between vectors and arrays between each submodel. For example, because the default GMSE observation model requires a resource array with particular requirements for column identites, when a resource model subfunction returns a vector, or a list with a named element 'resource_vector', this vector is translated into an array that can be used by the observation model. Specifically, each element of the vector identifies the abundance of a resource type (and hence will usually be just a single value denoting abundance of the only focal population). If this is all the information provided, then a resource_array will be made with default GMSE parameter values with an identical number of rows to the abundance value (floored if the value is a non-integer; non-default values can also be put into this transformation from vector to array if they are specified in gmse_apply, e.g., through an argument such as lambda = 0.8). Similarly, a 'resource_array' is also translated into a vector after the default individual-based resource model is run, should the observation model require simple abundances instead of an array. The same is true of 'observation_vector' and 'observation_array' objects returned by observation models, of 'manager_vector' and 'manager_array' (i.e., COST) objects returned by manager models, and of 'user_vector' and 'user_array' (i.e., ACTION) objects returned by user models. At each step, a translation between the two is made, with necessary adjustments that can be tweaked through arguments to gmse_apply when needed.
#'@return A matrix of gmse_apply values, summarised.
#'@examples
#'\dontrun{
#'sim_summary <- sim_old <- gmse_apply(get_res = "Full", scaring = FALSE, land_ownership = T);
#'gmse_apply_summary(data = sim_old, include = c("res","obs","culls","yield"))
#'}
#'@export
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
    
    if(sum(arg_vals$RESOURCES[,17]==0)<2) {
        stop("Extinction has occurred - population wiped out (users killed more individuals than were present)!")
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
#rm(all_args, arg_vals, man_args, man_results, needed_args, obs_args, obs_results, old_list, res, res_args, res_results, std_paras, usr_args, usr_results, arg_name)

#' observed_suggested
#'
#' Function to extract observed number of resources and costs for actions "proposed" by the manager model. Should work with any gmse_apply(get_res="Full") object, but intended for use following a run of gmse_apply_INTERIM() or gmse_apply_UROM().
#'
#'@param dat gmse_apply(get_res="Full") style list.
#'@details Function to extract observed number of resources and costs for actions "proposed" by the manager model. Should work with any gmse_apply(get_res="Full") object, but intended for use following a run of gmse_apply_INTERIM() or gmse_apply_UROM().
#'@return A list with three elements: observed, culling and scaring, which is the observed number of resources, and the cost suggested for culling and scaring actions in the given gmse_apply_ run.
#'@examples
#'\dontrun{
#'sim_old <- gmse_apply(get_res = "Full", scaring = TRUE);
#'observed_suggested(sim_old);
#'}
#'@export
observed_suggested = function(dat) {
    out = list(observed = round(dat$basic_output$observation_results,2), 
               culling = dat[["COST"]][1,9,2:dim(dat$COST)[3]],
               scaring = dat[["COST"]][1,8,2:dim(dat$COST)[3]])
    return(out)
}

#' set_man_costs
#'
#' Function to manually override user action costs set in a given gmse_apply_ output list. Currently implemented only for culling and scaring actions. If given single values for either culling or scaring, this value is repeated for all users. 
#'
#'@param dat gmse_apply(get_res="Full") style list.
#'@param newcost list of two elements, named culling and scaring respectively. Each element can either be a single value or a vector of length equal to the number of user/stakeholders. If the former, the given value is used as the new cost for that action, for all users. If the latter, user-specific costs are set (not yet tested).
#'@details Function to manually override user action costs set in a given gmse_apply_ output list. Currently implemented only for culling and scaring actions. If given single values for either culling or scaring, this value is repeated for all users.
#'@return gmse_apply_ style list as input, but with the cost array overwritten with the new costs.
#'@examples
#'\dontrun{
#'sim_old <- gmse_apply(get_res = "Full", scaring = TRUE);
#'sim_old$COST[1,8:12,2:5]
#'sim_old_updated <- set_man_costs(sim_old, newcost = list(culling = 999, scaring = 999))
#'sim_old_updated$COST[1,8:12,2:5]
#'sim_old_updated <- set_man_costs(sim_old, newcost = list(culling = c(10,20,30,40), scaring = 999))
#'sim_old_updated$COST[1,8:12,2:5]
#'}
#'@export
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

#' append_UROM_output
#'
#' Appends output to an output table set up by gmse_apply_summary() but treats this as "part-finished line"; i.e. following a gmse_apply_UROM() run, so following user-resource-observation-manager run. Assumes last output line is blank apart from true and observed resource pop size. Costs for the last line will be set as input as set by user during the previous time step (costs). Actions will be set as those taken by users in the new sim step in dat, and a new line will be added  with new true and observed resource counts.
#'
#'@param dat gmse_apply_ style list with output to summarise and append to previous gmse_apply_summary(output).
#'@param costs updated manager costs, as set by user in the preceding time step.
#'@param old_output existing gmse_apply_summary() output data to append to.
#'@details Appends output to an output table set up by gmse_apply_summary() but treats this as "part-finished line"; i.e. following a gmse_apply_UROM() run, so following user-resource-observation-manager run. Assumes last output line is blank apart from true and observed resource pop size. Costs for the last line will be set as input as set by user during the previous time step (costs). Actions will be set as those taken by users in the new sim step in dat, and a new line will be added  with new true and observed resource counts.
#'@return Updated gmse_apply_summary() output structure. The last line of the new output will be blank (NA) apart from the new true and observed resource population sizes. The actions taken and costs set will be added to the previous line, as they represent what happened in the previous time step.
#'@examples
#'\dontrun{
#'step1 <- gmse_apply(get_res = "Full", scaring = TRUE);
#'
#'}
#'@export
append_UROM_output = function(dat, costs, old_output) {
    # Costs as set previously:
    old_output[nrow(old_output),"cull_cost"] = costs$culling
    old_output[nrow(old_output),"scare_cost"] = costs$scaring
    # Actions as enacted previously:
    old_output[nrow(old_output),c("scares","culls","tend_crops")] = get_acts(dat$PREV_ACTS)[c("scares","culls","tend_crops")]
    # Add a new line for newly observed population, and add current counts:
    old_output = rbind(old_output, old_output[nrow(old_output),])
    old_output[nrow(old_output),] = NA
    old_output[nrow(old_output),c("res","obs")] = gmse_apply_summary(dat, include = c("res","obs"))
    return(old_output)
}

##### init_man_control is intended as a "setup" for manager control gmse_apply() runs as tested in
##### gmse_apply_CONTROL_EXAMPLE.R. The idea is to run K-1 iterations of "default" gmse_apply() with given parameters,
##### followed by a single run of gmse_apply_interim.

#' init_man_control
#'
#' init_man_control is a "set-up" for "manager control" gmse_apply() runs. It essentially wraps gmse_apply() and gmse_apply(INTERIM) calls; running K-1 gmse_apply() iterations followed by a single gmse_apply_INTERIM() call. The last resulting gmse_apply_ list struture is one ready for a gmse_apply_UROM() call, having finished with a call to the manager model.
#'
#'@param costs updated manager costs, as set by user in the preceding time step.
#'@param old_output existing gmse_apply_summary() output data to append to.
#'@details init_man_control is a "set-up" for "manager control" gmse_apply() runs. It essentially wraps gmse_apply() and gmse_apply(INTERIM) calls; running K-1 gmse_apply() iterations followed by a single gmse_apply_INTERIM() call. The last resulting gmse_apply_ list struture is one ready for a gmse_apply_UROM() call, having finished with a call to the manager model.
#'@return A list with three elements: gmse_list which is each K gmse_apply output list, summary, which is the output summary as produced by gmse_apply_summary() and observed_suggested which is the output of observed_suggested() as run on the final gmse_apply_INTERIM() call.
#'@examples
#'\dontrun{
#'step1 <- gmse_apply(get_res = "Full", scaring = TRUE);
#'
#'}
#'@export
init_man_control = function(K = 5) {
    
    gmse_list = list()
    
    K1 = K-1 # Because the "interim" time step will be the fifth, where actions aren't yet taken (pending user input)

    sim_old = gmse_apply(get_res = "Full", 
                         land_ownership = LAND_OWNERSHIP,
                         tend_crops = TEND_CROPS,
                         scaring = SCARING,
                         culling = CULLING,
                         observe_type = OBSERVE_TYPE, 
                         res_move_obs = RES_MOVE_OBS, 
                         res_death_K = RES_DEATH_K,
                         res_death_type = RES_DEATH_TYPE,
                         lambda = LAMBDA,
                         remove_pr = REMOVE_PR,
                         manage_target = MANAGE_TARGET, 
                         stakeholders = STAKEHOLDERS,
                         user_budget = USER_BUDGET,
                         manager_budget = MANAGER_BUDGET
    )
    
    output = gmse_apply_summary(sim_old, include = c("res","obs","culls","scares","tend_crops","cull_cost", "scare_cost","yield"))
    
    PREV_YIELD = tapply(sim_old$LAND[,,2], sim_old$LAND[,,3], mean)
    sim_old$LAND[,,2] = 1   ## Resent landscape yield
    
    gmse_list[[1]] = sim_old
    
    #### 1. First K1-1 time steps as normal to set up population run:
    for(i in 2:K1) {
        sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
        output = gmse_apply_summary(sim_new, output)
        sim_old = sim_new
        PREV_YIELD = rbind(PREV_YIELD, tapply(sim_old$LAND[,,2], sim_old$LAND[,,3], mean))
        sim_old$LAND[,,2] = 1   ## Resent landscape yield
        gmse_list[[i]] = sim_old
    }
    
    #### 2. Run "interim" time step where user actions as surpressed:
    sim_new = gmse_apply_INTERIM(get_res = "Full", old_list = sim_old)
    output = gmse_apply_summary(sim_new, output) 
    # Reset selected output for interim time step (as no actions have been taken)
    output[nrow(output),c("culls","scares","tend_crops","cull_cost","scare_cost")] = NA
    
    # Append last "previous yield:
    PREV_YIELD = rbind(PREV_YIELD, tapply(sim_new$LAND[,,2], sim_new$LAND[,,3], mean))
    
    gmse_list[[length(gmse_list)+1]] = sim_new
    
    init_steps = list(gmse_list = gmse_list,
                      prev_yield = PREV_YIELD,
                      summary = output, 
                      observed_suggested = observed_suggested(sim_new))
    return(init_steps)
}

### Takes an action array and returns a vector of the total number of actions summed across stakeholders
#' get_acts
#'
#' Takes an ACTION array from gmse_apply_ style output, and returns a vector of the total number of actions summed across stakeholders.
#'
#'@param actions ACTION array from gmse_apply_ style output.
#'@details Takes an ACTION array from gmse_apply_ style output, and returns a vector of the total number of actions summed across stakeholders.
#'@return A vector with six elements, being the sum of scaring, culling, castration, feeding, helping, and tend_crop actions, across all users. 
#'@examples
#'\dontrun{
#'step1 <- gmse_apply(get_res = "Full", scaring = TRUE, tend_crops = TRUE);
#'get_acts(step1$ACTION)
#'
#'}
#'@export
get_acts = function(actions) {
    acts = apply(actions[1,8:12,2:dim(actions)[3]],1,sum)
    acts = c(acts, sum(actions[2,10,2:dim(actions)[3]]))
    names(acts) = c("scares","culls","castrations","feeds","helps","tend_crops")
    return(acts)
}

