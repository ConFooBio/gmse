#' GMSE apply function
#' 
#'The gmse_apply function is a flexible function that allows for user-defined sub-functions calling resource, observation, manager, and user models. Where such models are not specified, GMSE submodels 'resource', 'observation', 'manager', and 'user' are run by default. Any type of sub-model (e.g., numerical,  individual-based) is permitted as long as the input and output are appropriately specified. Only one time step is simulated per call to gmse_apply, so the function must be looped for simulation over time. Where model parameters are needed but not specified, defaults from gmse are used.
#'
#' 
#'@param res_mod The function specifying the resource model. By default, the individual-based resource model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'resource_array' or 'resource_vector', and arrays must follow the format of GMSE in terms of column number and type (if there is only one resource type, then the model can also just return a scalar value).
#'@param obs_mod The function specifying the observation model. By default, the individual-based observation model from gmse is called with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'observation_array' or 'observation_vector', and arrays must follow the format of GMSE in terms of column number and type  (if there is only one resource type, then the model can also just return a scalar value).
#'@param man_mod The function specifying the manager model. By default, the individual-based manager model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'manager_array' or 'manager_vector', and arrays must follow the (3 dimensional) format of the 'COST' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired). User defined manager outputs will be recognised as costs by the default user model in gmse, but can be interpreted differently (e.g., total allowable catch) if specifying a custom user model.
#'@param use_mod The function specifying the user model. By default, the individual-based user model that calls the genetic algorithm from gmse is used with default parameter values. User-defined functions must either return an unnamed matrix or vector, or return a named list in which one list element is named either 'user_array' or 'user_vector', and arrays must follow the (3 dimensional) format of the 'ACTION' array in GMSE in terms of column numbers and types, with appropriate rows for interactions and layers for agents (see documentation of GMSE for constructing these, if desired).
#'@param get_res How the output should be organised. The default 'basic' attempts to distill results down to their key values from submodel outputs, including resource abundances and estimates, and manager policy and actions. An option 'custom' simply returns a large list that includes the output of every submodel. Any other option (e.g. 'none') will return a large list with all of the input, output, and parameters used to run gmse_apply. This list will also include a list element named 'basic_output', which will display the default results.
#'@param old_list A an existing list of results from gmse_apply, produced by setting `get_res = TRUE` to be included in the function. The parameter and data structures from the previous run will be applied to the new run of gmse_apply, thereby making it easy to loop multiple generations. Additional arguments passed to `...` will over-ride those stored in the old list, allowing global parameter values to be updated (e.g., sub-models used, management options, genetic algorithm parameters). Note that if these arguments are passed, the function will attempt to work with them even if it means removing previous list elements (e.g., if a new number of stakeholders is passed through stakeholder = new_value, then an entirely new AGENT array and user and manager arrays will need to be built).
#'@param ... Arguments passed to user-defined functions, and passed to modify default parameter values that would otherwise be called for gmse default models. Any argument that can be passed to gmse can be specified explicitly, just as if it were an argument to gmse. Similarly, any argument taken by a user-defined function should be specified, though the function will work if the user-defined function has a default that is not specified explicitly.
#'@details To integrate across different types of submodels, gmse_apply translates between vectors and arrays between each submodel. For example, because the default GMSE observation model requires a resource array with particular requirements for column identites, when a resource model subfunction returns a vector, or a list with a named element 'resource_vector', this vector is translated into an array that can be used by the observation model. Specifically, each element of the vector identifies the abundance of a resource type (and hence will usually be just a single value denoting abundance of the only focal population). If this is all the information provided, then a resource_array will be made with default GMSE parameter values with an identical number of rows to the abundance value (floored if the value is a non-integer; non-default values can also be put into this transformation from vector to array if they are specified in gmse_apply, e.g., through an argument such as lambda = 0.8). Similarly, a 'resource_array' is also translated into a vector after the default individual-based resource model is run, should the observation model require simple abundances instead of an array. The same is true of 'observation_vector' and 'observation_array' objects returned by observation models, of 'manager_vector' and 'manager_array' (i.e., COST) objects returned by manager models, and of 'user_vector' and 'user_array' (i.e., ACTION) objects returned by user models. At each step, a translation between the two is made, with necessary adjustments that can be tweaked through arguments to gmse_apply when needed.
#'
#'Parameter changes are accommodated by rebuilding data structures whenever necessary. For example, if the number of stakeholders is changed (and by including an argument 'stakeholders' to gmse_apply, it is assumed that stakeholders are changing even the value is identical to what is in the old_list), then a new array of agents will be built. If landscape dimensions are changed (i.e., if the argument 'land_dim_1' or 'land_dim_2' is included), then a new landscape willl be built. For custom defined GMSE sub-functions, arguments passed to '...' will not be found or updated, so changes to arguments of custom functions should be made directly to the 'old_list', or the use of old_list should be avoided.
#'@examples
#'\dontrun{
#'sim <- gmse_apply();
#'sim <- gmse_apply(stakeholders = 2);
#'sim <- gmse_apply(obs_mod = function(resource_vector) rnorm(1, resource_vector, 10));
#'}
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
                       old_list = NULL,
                       ...
                       ){

    fun_warn(res_mod, obs_mod, man_mod, use_mod);

    if(is.null(old_list) == FALSE){
        old_list <- swap_old_gmse(old_list);
    }
    
    std_paras      <- pass_paras(old_list, ...);
    all_args       <- as.list(sys.call());
    all_args$PARAS <- std_paras$gmse_para_vect;
    all_args$ilist <- std_paras$gmse_user_input; 
    all_args$GMSE  <- formals(gmse);
    
    needed_args <- argument_list(res_mod, obs_mod, man_mod, use_mod, all_args);
    arg_vals    <- needed_args$all_arg_values; 
    arg_name    <- needed_args$all_arg_names;

    names(arg_vals) <- arg_name;

    if(is.null(old_list) == FALSE){
        arg_vals <- apply_old_gmse(arg_vals, old_list, ...);
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

swap_old_gmse   <- function(ol){
    names_old <- names(ol);
    if("resource_array" %in% names_old == TRUE){
        if(identical(ol[["resource_array"]], ol[["RESOURCES"]]) == FALSE){
            ol[["RESOURCES"]] <- ol[["resource_array"]];
        }
        if(identical(ol[["resource_array"]], ol[["RESOURCE"]]) == FALSE){
            ol[["RESOURCE"]] <- ol[["resource_array"]];
        }
    }
    if("observation_array" %in% names_old == TRUE){
        if(identical(ol[["observation_array"]], ol[["OBSERVATION"]]) == FALSE){
            ol[["OBSERVATION"]] <- ol[["observation_array"]];
        }
    }
    if("manager_array" %in% names_old == TRUE){
        if(identical(ol[["manager_array"]], ol[["COST"]]) == FALSE){
            ol[["COST"]] <- ol[["manager_array"]];
        }
    }
    if("user_array" %in% names_old == TRUE){
        if(identical(ol[["user_array"]], ol[["ACTION"]]) == FALSE){
            ol[["ACTION"]] <- ol[["user_array"]];
        }
    }
    return(ol);
}

update_old_gmse <- function(arg_vals, ol, list_add){
    names_old  <- names(ol);
    names_arg  <- names(arg_vals);
    names_add  <- names(list_add);
    if("manager_sense" %in% names_add){
        ol$manager_sense     <- list_add$manager_sense;
    }
    if("time_max" %in% names_add){
        stop("ERROR: time_max cannot be changed when old_list is included");
    }
    if("land_dim_1" %in% names_add){
        ol$LAND       <- NA;
        ol$land_dim_1 <- list_add$land_dim_1;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[13] <- list_add$land_dim_1;
        }
    }
    if("land_dim_2" %in% names_add){
        ol$LAND       <- NA;
        ol$land_dim_2 <- list_add$land_dim_2;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[14] <- list_add$land_dim_2;
        }
    }
    if("res_movement" %in% names_add){
        if(is.null(ol$RESOURCES) == FALSE & is.na(ol$RESOURCES)[1] == FALSE){
            ol$RESOURCES[,7] <- list_add$res_movement;
        }
        if(is.null(ol$resource_array)  == FALSE & 
           is.na(ol$resource_array)[1] == FALSE){
            ol$resource_array[,7] <- list_add$res_movement;
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[3] <- list_add$res_movement;
        }
        ol$res_movement <- list_add$res_movement;
    }
    if("remove_pr" %in% names_add){
        if(is.null(ol$RESOURCES) == FALSE & is.na(ol$RESOURCES)[1] == FALSE){
            ol$RESOURCES[,9] <- list_add$remove_pr;
        }
        if(is.null(ol$resource_array)  == FALSE & 
           is.na(ol$resource_array)[1] == FALSE){
            ol$resource_array[,9] <- list_add$remove_pr;
        }
        ol$remove_pr <- list_add$remove_pr;
    }
    if("lambda" %in% names_add){
        if(is.null(ol$RESOURCES) == FALSE & is.na(ol$RESOURCES)[1] == FALSE){
            ol$RESOURCES[,10] <- list_add$lambda;
        }
        if(is.null(ol$resource_array)  == FALSE & 
           is.na(ol$resource_array)[1] == FALSE){
            ol$resource_array[,10] <- list_add$lambda;
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            mas <- ol$GMSE$manager_sense
            if(is.null(ol$manager_sense) == FALSE){
                if(is.na(ol$manager_sense) == FALSE){
                    mas <- ol$manager_sense;
                }
            }
            ol$PARAS[76]  <- -1*mas*(1+lambda);
            ol$PARAS[77]  <- -1*mas*lambda;
            ol$PARAS[78]  <-  1*lambda;
            ol$PARAS[101] <- lambda;
        }
        ol$lambda <- list_add$lambda;
    }
    if("agent_view" %in% names_add){
        if(is.null(ol$AGENTS) == FALSE & is.na(ol$AGENTS)[1] == FALSE){
            ol$AGENTS[,9] <- list_add$agent_view;
        }
        ol$agent_view <- list_add$agent_view;
    }
    if("agent_move" %in% names_add){
        if(is.null(ol$AGENTS) == FALSE & is.na(ol$AGENTS)[1] == FALSE){
            ol$AGENTS[,7] <- list_add$agent_move;
        }
        ol$agent_view <- list_add$agent_move;
    }
    if("res_birth_K" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[6] <- list_add$res_birth_K;
        }
        ol$res_birth_K <- list_add$res_birth_K;
    }
    if("res_death_K" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[7] <- list_add$res_death_K;
        }
        ol$res_death_K <- list_add$res_death_K;
    }
    if("edge_effect" %in% names_add){
        stop("ERROR: Cannot change edge_effect (must be torus)");
    }
    if("res_move_type" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[3] <- list_add$res_move_type;
        }
        ol$res_move_type <- list_add$res_move_type;
    }
    if("res_birth_type" %in% names_add){
        stop("ERROR: Only res_birth_type = 2 is allowed by GMSE");
    }
    if("res_death_type" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[5] <- list_add$res_death_type;
        }
        ol$res_death_type <- list_add$res_death_type;
    }
    if("observe_type" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[9] <- list_add$observe_type;
        }
        ol$observe_type <- list_add$observe_type;
    }
    if("fixed_mark" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[11] <- list_add$fixed_mark;
        }
        ol$fixed_mark <- list_add$fixed_mark;
    }
    if("fixed_recapt" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[103] <- list_add$fixed_recapt;
        }
        ol$fixed_recapt <- list_add$fixed_recapt;
    }
    if("times_observe" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[12] <- list_add$times_observe;
        }
        ol$times_observe <- list_add$times_observe;
    }
    if("obs_move_type" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[15] <- list_add$obs_move_type;
        }
        ol$obs_move_type <- list_add$obs_move_type;
    }
    if("res_min_age" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[17] <- list_add$res_min_age;
        }
        ol$res_min_age <- list_add$res_min_age;
    }
    if("res_move_obs" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[20] <- list_add$res_move_obs;
        }
        ol$res_move_obs  <- list_add$res_move_obs;
    }
    if("Euclidean_dist" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[21] <- list_add$Euclidean_dist;
        }
        ol$Euclidean_dist <- list_add$Euclidean_dist;
    }
    if("res_consume" %in% names_add){
        if(is.null(ol$RESOURCES) == FALSE & is.na(ol$RESOURCES)[1] == FALSE){
            ol$RESOURCES[,15] <- list_add$res_consume;
        }
        if(is.null(ol$resource_array)  == FALSE & 
           is.na(ol$resource_array)[1] == FALSE){
            ol$resource_array[,15] <- list_add$res_consume;
        }
        ol$res_consume <- list_add$res_consume;
    }
    if("ga_popsize" %in% names_add){
        if(list_add$ga_popsize < 2){
            stop("ERROR: ga_popsize must be an integer > 2");
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[22] <- list_add$ga_popsize;
        }
        ol$ga_popsize <- list_add$ga_popsize;
    }
    if("ga_mingen" %in% names_add){
        if(list_add$ga_mingen < 2){
            stop("ERROR: ga_mingen must be an integer > 2");
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[23] <- list_add$ga_mingen;
        }
        ol$ga_mingen <- list_add$ga_mingen;
    }
    if("ga_seedrep" %in% names_add){
        if(list_add$ga_seedrep < 0){
            stop("ERROR: ga_mingen must be a non-negative integer");
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[24] <- list_add$ga_seedrep;
        }
        ol$ga_seedrep <- list_add$ga_seedrep;
    }
    if("ga_sampleK" %in% names_add){
        if(list_add$ga_sampleK < 0){
            stop("ERROR: ga_sampleK must be a non-negative integer");
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[25] <- list_add$ga_sampleK;
        }
        ol$ga_sampleK <- list_add$ga_sampleK;
    }
    if("ga_chooseK" %in% names_add){
        if(list_add$ga_chooseK < 0){
            stop("ERROR: ga_chooseK must be a non-negative integer");
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[26] <- list_add$ga_chooseK;
        }
        ol$ga_chooseK <- list_add$ga_chooseK;
    }
    if("ga_mutation" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[27] <- list_add$ga_mutation;
        }
        ol$ga_mutation <- list_add$ga_mutation;
    }
    if("ga_crossover" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[28] <- list_add$ga_crossover;
        }
        ol$ga_crossover <- list_add$ga_crossover;
    }
    if("move_agents" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[29] <- list_add$move_agents;
        }
        ol$move_agents <- list_add$move_agents;
    }
    if("max_ages" %in% names_add){
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[30] <- list_add$max_ages;
        }
        ol$max_ages <- list_add$max_ages;
    }
    if("minimum_cost" %in% names_add){
        ol$COST          <- NA;
        ol$manager_array <- NA;
        ol$ACTION        <- NA;
        ol$user_array    <- NA;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[97] <- list_add$minimum_cost;
        }
        ol$minimum_cost <- list_add$minimum_cost;
    }
    if("user_budget" %in% names_add){
        if(is.null(ol$AGENTS) == FALSE & is.na(ol$AGENTS)[1] == FALSE){
            ol$AGENTS[ol$AGENTS[,2]==1, 17] <-list_add$user_budget;
        }
        if(is.null(ol$AGENT) == FALSE & is.na(ol$AGENT)[1] == FALSE){
            ol$AGENTS[ol$AGENT[,2]==1, 17] <- list_add$user_budget;
        }
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[98] <- list_add$user_budget;
        }
        ol$user_budget <- list_add$user_budget;
    }
    if("manager_budget" %in% names_add){
        if(is.null(ol$AGENTS) == FALSE & is.na(ol$AGENTS)[1] == FALSE){
            ol$AGENTS[ol$AGENTS[,2]==0, 17] <- list_add$manager_budget;
        }
        if(is.null(ol$AGENT) == FALSE & is.na(ol$AGENT)[1] == FALSE){
            ol$AGENTS[ol$AGENT[,2]==0, 17] <- list_add$manager_budget;
        }
        ol$manager_budget <- list_add$manager_budget;
    }
    if("manage_target" %in% names_add){
        ol$COST          <- NA;
        ol$manager_array <- NA;
        ol$ACTION        <- NA;
        ol$user_array    <- NA;
        ol$manage_target <- list_add$manage_target;
    }
    if("RESOURCE_ini" %in% names_add){
        stop("ERROR: Should not be initialising resources if using old_list");
    }
    if("scaring" %in% names_add){
        ol$COST          <- NA;
        ol$manager_array <- NA;
        ol$ACTION        <- NA;
        ol$user_array    <- NA;
        ol$scaring       <- list_add$scaring;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[89] <- list_add$scaring;
        }
    }
    if("culling" %in% names_add){
        ol$COST          <- NA;
        ol$manager_array <- NA;
        ol$ACTION        <- NA;
        ol$user_array    <- NA;
        ol$culling       <- list_add$culling;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[90] <- list_add$culling;
        }
    }
    if("castration" %in% names_add){
        ol$COST          <- NA;
        ol$manager_array <- NA;
        ol$ACTION        <- NA;
        ol$user_array    <- NA;
        ol$castration    <- list_add$castration;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[91] <- list_add$castration;
        }
    }
    if("feeding" %in% names_add){
        ol$COST          <- NA;
        ol$manager_array <- NA;
        ol$ACTION        <- NA;
        ol$user_array    <- NA;
        ol$feeding       <- list_add$feeding;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[92] <- list_add$feeding;
        }
    }
    if("help_offspring" %in% names_add){
        ol$COST           <- NA;
        ol$manager_array  <- NA;
        ol$ACTION         <- NA;
        ol$user_array     <- NA;
        ol$help_offspring <- list_add$help_offspring;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[93] <- list_add$help_offspring;
        }
    }
    if("kill_crops" %in% names_add){
        ol$COST           <- NA;
        ol$manager_array  <- NA;
        ol$ACTION         <- NA;
        ol$user_array     <- NA;
        ol$kill_crops     <- list_add$kill_crops;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[94] <- list_add$kill_crops;
        }
    }
    if("tend_crops" %in% names_add){
        ol$COST           <- NA;
        ol$manager_array  <- NA;
        ol$ACTION         <- NA;
        ol$user_array     <- NA;
        ol$tend_crops     <- list_add$tend_crops;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[95] <- list_add$tend_crops;
        }
    }
    if("tend_crop_yld" %in% names_add){
        ol$COST              <- NA;
        ol$manager_array     <- NA;
        ol$ACTION            <- NA;
        ol$user_array        <- NA;
        ol$tend_crop_yld     <- list_add$tend_crop_yld;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[80] <- list_add$tend_crop_yld;
        }
    }
    if("stakeholders" %in% names_add){
        ol$AGENTS         <- NA;
        ol$AGENT          <- NA;
        ol$COST           <- NA;
        ol$manager_array  <- NA;
        ol$ACTION         <- NA;
        ol$user_array     <- NA;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[55] <- list_add$stakeholders + 1;
            ol$PARAS[66] <- list_add$stakeholders + 1;
            ol$PARAS[69] <- list_add$stakeholders + 3;
        }
        ol$stakeholders     <- list_add$stakeholders;
    }
    if("manage_caution" %in% names_add){
        ol$manage_caution     <- list_add$manage_caution;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[96] <- list_add$manage_caution;
        }
    }
    if("land_ownership" %in% names_add){
        ol$LAND           <- NA;
        ol$COST           <- NA;
        ol$manager_array  <- NA;
        ol$ACTION         <- NA;
        ol$user_array     <- NA;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[104] <- list_add$land_ownership;
        }
        ol$land_ownership     <- list_add$land_ownership;
    }
    if("manage_freq" %in% names_add){
        stop("ERROR: manage_freq cannot be changed when old_list is included");
    }
    if("converge_crit" %in% names_add){
        ol$converge_crit     <- list_add$converge_crit;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[99] <- list_add$converge_crit;
        }
    }
    if("public_land" %in% names_add){
        ol$LAND           <- NA;
        ol$COST           <- NA;
        ol$manager_array  <- NA;
        ol$ACTION         <- NA;
        ol$user_array     <- NA;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[105] <- list_add$public_land;
        }
        ol$public_land     <- list_add$public_land;
    }
    if("group_think" %in% names_add){
        ol$group_think     <- list_add$group_think;
        if(is.null(ol$PARAS) == FALSE & is.na(ol$PARAS)[1] == FALSE){
            ol$PARAS[102] <- list_add$group_think;
        }
    }
    return(ol);
}


apply_old_gmse <- function(arg_vals, old_list,  ...){
    
    input_list <- arg_vals$ilist; 
    paras_errors(input_list);
    
    list_add   <- as.list(match.call())[-1];
    names_old  <- names(old_list);
    names_arg  <- names(arg_vals);
    names_add  <- names(list_add);
    
    added      <- length(list_add);
    
    if(added > 2){
        for(i in 3:length(list_add)){
            list_add[[i]] <- eval(list_add[[i]]);
        }
    }

    old_list   <- update_old_gmse(arg_vals, old_list, list_add);
    
    old_length <- length(old_list);
    arg_length <- length(arg_vals);
    add_length <- length(list_add);
    
    for(i in 1:arg_length){
        if(names_arg[[i]] %in% names_old == FALSE){
            old_length                    <- old_length + 1;
            old_list[[old_length]]        <- arg_vals[[i]];
            names(old_list)[old_length]   <- names_arg[[i]];
        }
    }

    old_list$arg_vals <- NULL;
    old_list$old_list <- NULL;
    return(old_list);
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

pass_paras <- function( old_list = NULL, time_max = 100, land_dim_1 = 100, 
                        land_dim_2 = 100,  res_movement = 20, remove_pr = 0.0, 
                        lambda = 0.30, agent_view = 10, agent_move = 50, 
                        res_birth_K = 10000, res_death_K = 2000, 
                        edge_effect = 1, res_move_type = 1, res_birth_type = 2, 
                        res_death_type = 2, observe_type = 0, fixed_mark = 50, 
                        fixed_recapt = 150, times_observe = 1, 
                        obs_move_type = 1, res_min_age = 0, res_move_obs = TRUE, 
                        Euclidean_dist = FALSE, plotting = FALSE, hunt = FALSE, 
                        start_hunting = 95, res_consume = 0.5, ga_popsize = 100, 
                        ga_mingen = 40, ga_seedrep = 20, ga_sampleK = 20, 
                        ga_chooseK = 2, ga_mutation = 0.1, ga_crossover = 0.1, 
                        move_agents = TRUE, max_ages = 5, minimum_cost = 10,
                        user_budget = 1000, manager_budget = 1000,
                        manage_target = 1000, RESOURCE_ini = 1000, 
                        scaring = FALSE, culling = TRUE, castration = FALSE,
                        feeding = FALSE, help_offspring = FALSE, 
                        tend_crops = FALSE, tend_crop_yld = 0.2, 
                        kill_crops = FALSE, stakeholders = 4, 
                        manage_caution = 1, land_ownership = FALSE, 
                        manage_freq = 1, converge_crit = 1, 
                        manager_sense = 0.1, public_land = 0, 
                        group_think = FALSE, PARAS = NULL, ...
){
    
    if(is.null(PARAS) == FALSE){
        stop("ERROR: Do not specify the PARAS vector directly; this will most
              likely cause R to crash.");
    }
    
    input_list <- c(time_max, land_dim_1, land_dim_2, res_movement, remove_pr,
                    lambda, agent_view, agent_move, res_birth_K, res_death_K,
                    edge_effect, res_move_type, res_birth_type, res_death_type,
                    observe_type, fixed_mark, fixed_recapt, times_observe,
                    obs_move_type, res_min_age, res_move_obs, Euclidean_dist, 
                    plotting, hunt, start_hunting, res_consume, ga_popsize,
                    ga_mingen, ga_seedrep, ga_sampleK, ga_chooseK, ga_mutation,
                    ga_crossover, move_agents, max_ages, minimum_cost,
                    user_budget, manager_budget, manage_target, RESOURCE_ini, 
                    scaring, culling, castration, feeding, help_offspring, 
                    tend_crops, tend_crop_yld, kill_crops, stakeholders, 
                    manage_caution, land_ownership, manage_freq, converge_crit, 
                    manager_sense, public_land, group_think); 
    
    paras_errors(input_list);
    
    ldims  <- land_errors(input_list, ...);
    stakes <- agent_errors(input_list, ldims, ...);
    action_errors(input_list, stakes, ...);
    old_list_errors(old_list, ...);
    
    if(observe_type == 1){
        times_observe <- 2;
    }
    
    user_res_opts  <- c(scaring, culling, castration, feeding, help_offspring);
    user_lnd_opts  <- c(tend_crops, kill_crops);
    
    ttr <- 20;
    agn <- stakeholders + 1;
    agt <- 17;
    lkr <- 2;
    lyr <- stakeholders + 1;
    roc <- stakeholders + 3;
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
               -1*manager_sense, -1*manager_sense*lambda, 1*lambda, 
               1*manager_sense, tend_crop_yld, 1, 2, 15, 0, 0, 0, 0, 0, 
               user_res_opts[1], user_res_opts[2], user_res_opts[3], 
               user_res_opts[4], user_res_opts[5], user_lnd_opts[1], 
               user_lnd_opts[2], manage_caution, minimum_cost, user_budget, 
               converge_crit, RESOURCE_ini, lambda, group_think, fixed_recapt, 
               land_ownership, public_land
    );
    
    return( list(gmse_user_input = as.vector(input_list), 
                 gmse_para_vect  = as.vector(paras))
    );
}


old_list_errors <- function(old_list = NULL, RESOURCES = NULL, ACTION = NULL,
                            resource_array = NULL, LAND = NULL, COST = NULL,
                            manager_array  = NULL, user_array = NULL, 
                            PARAS = NULL, ...){
    if(is.null(old_list) == FALSE){
        if(is.null(RESOURCES) == FALSE){
            stop("ERROR: Do not specify RESOURCES if using an old_list");
        }
        if(is.null(ACTION) == FALSE){
            stop("ERROR: Do not specify ACTION if using an old_list");
        }
        if(is.null(resource_array) == FALSE){
            stop("ERROR: Do not specify resource_array if using an old_list");
        }
        if(is.null(LAND) == FALSE){
            stop("ERROR: Do not specify LAND if using an old_list");
        }
        if(is.null(COST) == FALSE){
            stop("ERROR: Do not specify COST if using an old_list");
        }
        if(is.null(manager_array) == FALSE){
            stop("ERROR: Do not specify manager_array if using an old_list");
        }
        if(is.null(user_array) == FALSE){
            stop("ERROR: Do not specify user_array if using an old_list");
        }
    }
}

land_errors <- function(input_list, LAND = NULL, PARAS = NULL, ...){
    arguments <- as.list(match.call());          
    in_list   <- eval(arguments$input_list);
    arg_names <- names(arguments);          
    ld1_u     <- in_list[2];
    ld2_u     <- in_list[3];    
    ld1_p     <- NA;
    ld2_p     <- NA;
    ld1_l     <- NA;
    ld2_l     <- NA;
    ld1       <- ld1_u;
    ld2       <- ld2_u;
    if(is.null(PARAS) == FALSE){
        ld1_p    <- PARAS[13];
        ld2_p    <- PARAS[14];
        ld1      <- ld1_p;
        ld2      <- ld2_p;
    }
    if(is.null(LAND) == FALSE){
        ld1_l    <- dim(LAND)[1];
        ld2_l    <- dim(LAND)[2];
        ld1      <- ld1_l;
        ld2      <- ld2_l;
        if(is.null(ld1_l) == TRUE | is.null(ld2_l) == TRUE){
            stop("ERROR: LAND dimensions are unclear -- needs to be an array");
        }
    }
    if(is.na(ld1_u[1]) == FALSE & is.na(ld1_p[1]) == FALSE){
        if(ld1_u != ld1_p){
            stop("ERROR: PARAS and land_dim_1 disagree abount landscape size");
        }
    }
    if(is.na(ld1_u[1]) == FALSE & is.na(ld1_l[1]) == FALSE){
        if(ld1_u != ld1_l){
            stop("ERROR: LAND and land_dim_1 disagree about landscape size");
        }
    }
    if(is.na(ld1_l[1]) == FALSE & is.na(ld1_p[1]) == FALSE){
        if(ld1_l != ld1_p){
            stop("ERROR: PARAS and LAND disagree about landscape size (dim 1)");
        }
    }
    if(is.na(ld2_u[1]) == FALSE & is.na(ld2_p[1]) == FALSE){
        if(ld2_u != ld2_p){
            stop("ERROR: PARAS and land_dim_2 disagree abount landscape size");
        }
    }
    if(is.na(ld2_u[1]) == FALSE & is.na(ld2_l[1]) == FALSE){
        if(ld2_u != ld2_l){
            stop("ERROR: LAND and land_dim_2 disagree about landscape size");
        }
    }
    if(is.na(ld2_l[1]) == FALSE & is.na(ld2_p[1]) == FALSE){
        if(ld2_l != ld2_p){
            stop("ERROR: PARAS and LAND disagree about landscape size (dim 2)");
        }
    }
    output <- c(ld1, ld2);
    return(output);
}

agent_errors <- function(input_list, ldims, ...){
    land_dim_1 <- ldims[1];
    land_dim_2 <- ldims[2];
    arguments  <- as.list(match.call()); 
    arg_names  <- names(arguments);
    in_list    <- eval(arguments$input_list);
    il_names   <- names(in_list);
    as_stake   <- NA;                                                           
    stakes     <- in_list[49];                                                        
    if("AGENT" %in% arguments | "AGENT" %in% arg_names){
        warning("Warning: You've included 'AGENT' as an argument -- did you mean
               'AGENTS' instead to insert an agent array?", noBreaks. = TRUE);
    }
    if("AGENTS" %in% arg_names){
        stake_pos <- which(arg_names == "AGENTS")[1];
        loc_stake <- eval(arguments[[stake_pos]]);
        as_stake  <- sum(loc_stake[,2] > 0);
        if(is.na(land_dim_1[1]) == FALSE & is.na(land_dim_2[1]) == FALSE){
            max_d1 <- max(loc_stake[,5]);
            max_d2 <- max(loc_stake[,6]);
            if(max_d1 > land_dim_1 | max_d2 > land_dim_2){
                stop("ERROR: Some agents (manager or stakeholders) are located
                      off of the landscape -- either the landscape is too small
                      or the agent positions are set to be too large");
            }
        }
    }
    if(is.na(as_stake[1]) == FALSE){
        if(stakes != as_stake){
            stop("ERROR: AGENT and stakeholders disagree about how many
                  stakeholders should exist in the model. If you have included
                  your own AGENT array, make sure that 'stakeholders' is set to 
                  the appropriate number of type 1 (col 2) agents");
        }
    }
    return(stakes);
}

action_errors <- function(input_list, stakes, ...){
    agents     <- NA;
    if(is.na(stakes[1]) == FALSE){
        agents     <- stakes + 1;
    }
    res_arr    <- NA;
    res_types  <- NA;
    arguments  <- as.list(match.call());
    arg_names  <- names(arguments);
    in_list    <- eval(arguments$input_list);
    il_names   <- names(in_list);
    if("RESOURCES" %in% arg_names){
        res_pos <- which(arg_names == "RESOURCES")[1];
        res_arr <- eval(arguments[[res_pos]]);
        if(is.null(dim(res_arr)) == TRUE){
            stop("ERROR: Dimensions of the RESOURCE array are unclear");
        }
    }
    if("resource_array" %in% arg_names){
        res_pos <- which(arg_names == "resource_array")[1];
        res_arr <- eval(arguments[[res_pos]]);
        if(is.null(dim(res_arr))){
            stop("ERROR: Dimensions of the resource_array are unclear");
        }
    }
    if(is.na(res_arr[1]) == FALSE){
        res_types <- length(unique(res_arr[,2]));
    }
    if("ACTION" %in% arg_names){
        act_pos <- which(arg_names == "ACTION")[1];
        act_arr <- eval(arguments[[act_pos]]);
        if(is.null(dim(act_arr)) == TRUE){
            stop("ERROR: Dimensions of the ACTION array are unclear");
        }
        if(is.na(agents[1]) == FALSE){
            if(agents != dim(act_arr)[3]){
                stop("The ACTION array has a different number of layers
                      than there are total agents (manager plus stakeholders).
                      Input arguments are contradictory.");
            }
        }
        if(is.na(res_types[1]) == FALSE){
            act_res_types <- length(unique(act_arr[,2,1]));
            if(act_res_types != res_types){
                stop("The number of resource types in the ACTION array
                      contradicts something else that was set as an argument
                      (most likely a resource array)");
            }
        }
        if("COST" %in% arg_names){
            cost_pos <- which(arg_names == "COST")[1];
            cost_arr <- eval(arguments[[cost_pos]]);
            if(is.null(dim(cost_arr)) == TRUE){
                stop("ERROR: Dimensions of the COST array are unclear");
            }
            if(identical(dim(cost_arr), dim(act_arr)) == FALSE){
                stop("The dimensions of the COST and ACTION arrays need to be
                      identical");
            }
        }
        if("manager_array" %in% arg_names){
            cost_pos <- which(arg_names == "manager_array")[1];
            cost_arr <- eval(arguments[[cost_pos]]);
            if(is.null(dim(cost_arr)) == TRUE){
                stop("ERROR: Dimensions of the manager_array are unclear");
            }
            if(identical(dim(cost_arr), dim(act_arr)) == FALSE){
                stop("The dimensions of the manager_array and 
                      ACTION array need to be identical");
            }
        }
    }
    if("user_array" %in% arg_names){
        act_pos <- which(arg_names == "user_array")[1];
        act_arr <- eval(arguments[[act_pos]]);
        if(is.null(dim(act_arr))){
            stop("ERROR: Dimensions of the user_array are unclear");
        }
        if(is.na(agents[1]) == FALSE){
            if(agents != dim(act_arr)[3]){
                stop("The user_array has a different number of layers
                      than there are total agents (manager plus stakeholders).
                      Input arguments are contradictory.");
            }
        }
        if(is.na(res_types[1]) == FALSE){
            act_res_types <- length(unique(act_arr[,2,1]));
            if(act_res_types != res_types){
                stop("The number of resource types in the user_array
                      contradicts something else that was set as an argument
                      (most likely a resource array)");
            }
        }
        if("COST" %in% arg_names){
            cost_pos <- which(arg_names == "COST")[1];
            cost_arr <- eval(arguments[[cost_pos]]);
            if(identical(dim(cost_arr), dim(act_arr)) == FALSE){
                stop("The dimensions of COST and user_array need to be
                      identical");
            }
        }
        if("manager_array" %in% arg_names){
            cost_pos <- which(arg_names == "manager_array")[1];
            cost_arr <- eval(arguments[[cost_pos]]);
            if(identical(dim(cost_arr), dim(act_arr)) == FALSE){
                stop("The dimensions of the manager_array and 
                      user_array need to be identical");
            }
        }
    }
    if("COST" %in% arg_names){
        cpos <- which(arg_names == "COST")[1];
        carr <- eval(arguments[[cpos]]);
        if(is.null(dim(carr)) == TRUE){
            stop("Incorrect dimensions set for the COST array");
        }
        if(dim(carr)[3] != 3){
            stop("Incorrect dimensions set for the COST array");
        }
    }
    if("manager_array" %in% arg_names){
        cpos <- which(arg_names == "manager_array")[1];
        carr <- eval(arguments[[cpos]]);
        if(is.null(dim(carr)) == TRUE){
            stop("Incorrect dimensions set for the manager_array");
        }
        if(dim(carr)[3] != 3){
            stop("Incorrect dimensions set for the manager_array");
        }
    }
    if("ACTION" %in% arg_names){
        apos <- which(arg_names == "ACTION")[1];
        aarr <- eval(arguments[[apos]]);
        if(is.null(dim(aarr)) == TRUE){
            stop("Incorrect dimensions set for the ACTION array");
        }
        if(dim(aarr)[3] != 3){
            stop("Incorrect dimensions set for the ACTION array");
        }
    }
    if("user_array" %in% arg_names){
        apos <- which(arg_names == "user_array")[1];
        aarr <- eval(arguments[[apos]]);
        if(is.null(dim(aarr)) == TRUE){
            stop("Incorrect dimensions set for the user_array");
        }
        if(dim(aarr)[3] != 3){
            stop("Incorrect dimensions set for the user_array");
        }
    }
}

paras_errors <- function(input_list){
    if(input_list[7] < 0){
        stop("ERROR: Agents cannot see negative distance");
    }
    if(input_list[8] < 0){
        stop("ERROR: Agents cannot move negative distance");
    }
    if(input_list[9] < 1 | input_list[10] < 1){
        stop("ERROR: Carrying capacities must be positive");
    }
    if(input_list[11] != 1){
        stop("ERROR: Only torus landscape possible in GMSE (edge_effect == 1)");
    }
    if(input_list[12] < 0 | input_list[12] > 3){
        stop("ERROR: Unrecognised GMSE res_move_type");
    }
    if(input_list[13] != 2){
        stop("ERROR: Currently, only res_birth_type == 2 is allowed by GMSE");
    }
    if(input_list[14] < 1 | input_list[14] > 3){
        stop("ERROR: Unrecognised GMSE res_death_type");
    }
    if(input_list[15]  <  0 | input_list[15] > 3){
        stop("ERROR: Unrecognised observe_type for GMSE observation model");
    }
    if(input_list[19]  <  0 | input_list[19] > 3){
        stop("ERROR: Unrecognised obs_move_type for GMSE observation model");
    }
    if(input_list[20]  <  0){
        stop("ERROR: Minimum resource age cannot be less than zero");
    }
    if(input_list[21]  <  0 | input_list[21] > 1){
        stop("ERROR: res_move_obs must be TRUE/FALSE");
    }
    if(input_list[27]  <  0){
        stop("ERROR: ga_popsize must be an integer greater than zero");
    }
    if(input_list[28]  <  0){
        stop("ERROR: ga_mingen must be an integer greater than zero");
    }
    if(input_list[29]  <  0){
        stop("ERROR: ga_seedrep must be an integer greater than zero");
    }
    if(input_list[30]  <  0){
        stop("ERROR: ga_sampleK must be greater than zero");
    }
    if(input_list[31]  <  0){
        stop("ERROR: ga_chooseK must be greater than zero");
    }
    if(is.na(input_list[27]) == FALSE & input_list[27] %% 1 != 0){
        stop("ERROR: ga_popsize must be an integer greater than zero");
    }
    if(is.na(input_list[28]) == FALSE & input_list[28] %% 1 != 0){
        stop("ERROR: ga_mingen must be an integer greater than zero");
    }
    if(is.na(input_list[29]) == FALSE & input_list[29] %% 1 != 0){
        stop("ERROR: ga_seedrep must be an integer greater than zero");
    }
    if(is.na(input_list[30]) == FALSE & input_list[30] %% 1 != 0){
        stop("ERROR: ga_sampleK must be greater than zero");
    }
    if(is.na(input_list[31]) == FALSE & input_list[31] %% 1 != 0){
        stop("ERROR: ga_chooseK must be greater than zero");
    }
    if(input_list[37] < 1 | input_list[37] > 100000){
        stop("User budget needs to be between 1 and 100000");
    }
    if(input_list[38] < 1 | input_list[37] > 100000){
        stop("Manager budget needs to be between 1 and 100000");
    }
    if(input_list[40]  <  1){
        stop("ERROR: Must have a positive number of initial resources");
    }
    if(input_list[41]  <  0 | input_list[41] > 1){
        stop("ERROR: scaring must be TRUE/FALSE");
    }
    if(input_list[42]  <  0 | input_list[42] > 1){
        stop("ERROR: culling must be TRUE/FALSE");
    }
    if(input_list[43]  <  0 | input_list[43] > 1){
        stop("ERROR: castration must be TRUE/FALSE");
    }
    if(input_list[44]  <  0 | input_list[44] > 1){
        stop("ERROR: feeding must be TRUE/FALSE");
    }
    if(input_list[45]  <  0 | input_list[45] > 1){
        stop("ERROR: help_offspring must be TRUE/FALSE");
    }
    if(input_list[46]  <  0 | input_list[46] > 1){
        stop("ERROR: tend_crops must be TRUE/FALSE");
    }
    if(input_list[48]  <  0 | input_list[48] > 1){
        stop("ERROR: help_offspring must be TRUE/FALSE");
    }
    if(input_list[49] < 1){
        stop("ERROR: Need at least 1 stakeholder");
    }
    if(input_list[51] < 0 | input_list[51] > 1){
        stop("ERROR: land_ownership must be TRUE/FALSE");
    }
    if(input_list[52] < 1 ){
        stop("ERROR: manage_freq must be at least 1");
    }
    if(input_list[55] < 0 | input_list[55] > 1){
        stop("ERROR: public_land must be between 0 and 1");
    }
    if(input_list[56] < 0 | input_list[56] > 1){
        stop("ERROR: group_think must be TRUE/FALSE");
    }
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
    if(which_fun == "man_mod"){
        check_manager_res_types(arg_list);
    }
}

check_manager_res_types <- function(arg_list){    
    res_types <- unique(arg_list$OBSERVATION[,2]);
    if(length(res_types) > 2){
        stop("The GMSE manager function cannot yet handle more than two
              resource types. Email the package creator and tell them that you
              want this feature in gmse_apply, or add it as a GitHub issue");
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
    if("max_ages" %in% arg_names){
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
        make_lnd_list[[2]] <- arg_list$land_dim_1;
    }
    make_lnd_list[[3]] <- arg_list$GMSE$land_dim_2;
    if("land_dim_2" %in% arg_names){
        make_lnd_list[[3]] <- arg_list$land_dim_2;
    }
    make_lnd_list[[4]] <- 1;
    make_lnd_list[[5]] <- 1;
    make_lnd_list[[6]] <- 0;
    make_lnd_list[[7]] <- 1;
    make_lnd_list[[8]] <- 0
    make_lnd_list[[9]] <- 3;
    land_is_owned <- arg_list$GMSE$land_ownership;
    if("land_ownership" %in% arg_names){
        land_is_owned <- arg_list$land_ownership;
    }
    stakeholders <- arg_list$GMSE$stakeholders;
    if("stakeholders" %in% arg_names){
        stakeholders <- arg_list$stakeholders;
    }
    public_land <- arg_list$GMSE$public_land;
    if("public_land" %in% arg_names){
        public_land <- arg_list$public_land;
    }
    if(land_is_owned == TRUE){
        stake_pr    <- (1 - public_land) / stakeholders;
        land_alloc  <- c(public_land, rep(x = stake_pr, times = stakeholders));
    }else{
        land_alloc  <- c(1, rep(x = 0, times = stakeholders)); 
    }
    make_lnd_list[[10]] <- 1:(stakeholders + 1);
    make_lnd_list[[11]] <- land_alloc;
    
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
        temp_out <- list();
        if(is.vector(output[[1]]) == TRUE){
            temp_out[[1]]        <- output;
            names(temp_out)[[1]] <- vec_name;
        }
        if(is.matrix(output[[1]]) == TRUE){
            temp_out[[1]]        <- output;
            names(temp_out)[[1]] <- mat_name;
        }
        output <- temp_out;
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
            rep_pos <- which(arg_names == out_names[[i]])[1];
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
            res_arr <- make_resource(resource_quantity = sum(tot_res));
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
            res_tys     <- length(arg_list$observation_vector);
            obs_arr     <- make_resource(resource_quantity = 10*res_tys);
            res_idd     <- rep(x = 1:res_tys, each = 10);
            obs_arr[,2] <- res_idd;
            arg_list$observation_array <- obs_arr;
            if("PARAS" %in% arg_names == FALSE){
                stop("I can't find PARAS, and I need it");
            }
            arg_list$PARAS[9]   <- -1; # Tells manager to skip estimate
            arg_list            <- set_action_array(arg_list);
            thetar  <- arg_list$ACTION[arg_list$ACTION[,1,1] == -2, 5, 1];
            theobs  <- arg_list$observation_vector;
            arg_list$ACTION[arg_list$ACTION[,1,1]==1, 5, 1] <- thetar - theobs;
            arg_list <- gmse_apply_build_cost(arg_list);
        }
        if(out_names[[i]] == "observation_array" | 
           out_names[[i]] == "OBSERVATION"         ){               
            if("PARAS" %in% arg_names == FALSE){
                stop("I can't find PARAS, and I need it");
            }
            arg_list            <- estimate_abundances(arg_list);
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
            chk  <- length(arg_list$ACTION[arg_list$ACTION[,1,1] == 1,1,1]);
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
    if(sum(arg_list$resource_vector) < 2){
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
    res_types    <- unique(observations[,2]);                                  
    if(length(res_types) == 0){ ### INCLUDE TYPES A PRIORI FROM RESOURCE LIST
        arg_list$PARAS[100]         <- 0;
        arg_list$observation_vector <- 0; 
        return(arg_list);
    }
    est          <- rep(x = NA, times = length(res_types));
    for(i in res_types){                                                       
        esti       <- NA;
        obs_subset <- observations[observations[,2] == i,];                    
        obs_sub_ar <- is.array(obs_subset);
        if(obs_method == 0 & obs_sub_ar == TRUE){
            esti <- dens_est(obs_subset, paras, view, land)$Nc;                
        }
        if(obs_method == 1 & obs_sub_ar == TRUE){
            esti <- chapman_est(obs_subset, paras)$Nc;
        }
        if( (obs_method == 2 | obs_method == 3) & obs_sub_ar == TRUE ){
            esti <- sum(obs_subset[,13]);
        }
        if(length(res_types) > 1 & is.na(esti[1]) == TRUE){
            esti[1] <- 0;
        }
        if(is.na(esti[1]) == TRUE){
            stop("I couldn't estimate population; check observe_type?
                  Might not be enough resources to estimate (e.g., if there
                  is more than one type of resources)");
        }
        est[i] <- esti;
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
        user_budget    <- arg_list[[ubpos]];
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
        arg_list <- get_old_actions(arg_list);
        arg_list <- get_old_costs(arg_list);
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

get_old_costs <- function(arg_list){
    cols_cost   <- dim(arg_list$COST)[2];
    lays_cost   <- dim(arg_list$COST)[3];
    user_places <- which(arg_list$AGENTS[,2] > 0);
    old_costs   <- sum(arg_list$COST[,8:cols_cost,user_places]);
    if( is.null(arg_list$basic_output) == FALSE ){
        cost_vector  <- as.vector(arg_list$basic_output$manager_results[1,2:6]);
        cost_vector[is.na(cost_vector)] <- 100001;
        arg_list$COST[1,8:12,2:lays_cost] <- cost_vector;
    }
    return(arg_list);
}

get_old_actions <- function(arg_list){
    cols_action <- dim(arg_list$ACTION)[2];
    user_places <- which(arg_list$AGENTS[,2] > 0);
    old_actions <- sum(arg_list$ACTION[,8:cols_action,user_places]);
    if( old_actions <= 0 & is.null(arg_list$basic_output) == FALSE ){
        tot_actions <- apply(X = arg_list$basic_output$user_results, MARGIN = 2, 
                             FUN = sum);
        act_vector  <- as.vector(tot_actions[2:6]);
        act_vector[is.na(act_vector)] <- 0;
        arg_list$ACTION[1,8:12,2] <- act_vector;
        man_vector <- as.vector(arg_list$basic_output$manager_results[1,2:6]);
        man_vector[is.na(man_vector)] <- arg_list$GMSE$minimum_cost;
        arg_list$ACTION[3,8:13,1] <- c(man_vector, arg_list$GMSE$minimum_cost);
    }
    return(arg_list);
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
        stop("I can't find observations for the manager model. The manager
              might have failed to observe resources. This might be because
              resources are low, or because the manager is off-landscape");
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

    if(out == "custom"){
        c_list                     <- list();
        c_list$resource_results    <- u_res;
        c_list$observation_results <- u_obs;
        c_list$manager_results     <- u_man;
        c_list$user_results        <- u_usr;
        
        return(c_list);
    }
        
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
    
    if(out == "basic"){
        return(b_list);
    }    

    arg_list$basic_output <- b_list;
    
    return(arg_list);
}

get_manager_sum <- function(arg_list){
    acts           <- arg_list$user_array;
    costs          <- arg_list$manager_array;
    res_types      <- length(arg_list$resource_vector);
    scaring        <- rep(x = NA, times = res_types);
    culling        <- rep(x = NA, times = res_types);
    castrating     <- rep(x = NA, times = res_types);
    feeding        <- rep(x = NA, times = res_types);
    help_offspring <- rep(x = NA, times = res_types);
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
    cost_mat  <- matrix(data = all_costs, nrow = res_types);
    cost_mat  <- cbind(1:res_types, cost_mat);
    colnames(cost_mat) <- c("resource_type", "scaring", "culling", "castration",
                            "feeding", "help_offspring");
    cost_row_names <- rep(x = NA, length = dim(cost_mat)[1]);
    for(i in 1:dim(cost_mat)[1]){
        cost_row_names[i] <- paste("policy_",i, sep = "");
    }
    rownames(cost_mat) <- cost_row_names;
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
    res_types        <- unique(acts[,2,1]);
    act_row_names[1:length(res_types)] <- c("Manager");
    u_start  <- length(res_types) + 1;
    u        <- 1;
    t_count  <- 1;
    for(i in u_start:dim(act_mat)[1]){
        act_row_names[i] <- paste("user_",u, sep = "");
        t_count          <- t_count + 1;
        if(t_count > length(res_types)){
            t_count <- 1;
            u       <- u + 1;
        }
    }
    rownames(act_mat) <- act_row_names;
    
    return(act_mat);
}
################################################################################
