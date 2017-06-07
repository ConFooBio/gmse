#' Observation model
#'
#' A simulation of techniques (e.g., capture-mark-recapture) for estimating
#' population size and poperties.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param PARAS The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param inter_table Interaction table indexing types with the INTERACT matrix
#'@param fix_mark Fixed number of individuals marked? (A number, or FALSE)
#'@param times Number of times that the observations are made (e.g., managers go out sampling n times in an area of the landscape)
#'@param samp_age Minimum age of the resource being sampled (default = 1)
#'@param agent_type The type of agent doing the observing (default = 0)
#'@param type_cat The category of agent type (first 4 columns) doing observing; this will almost always be 1, so type 0 agents (managers, of which there is always one by default) will perform the observations
#'@param obs_method The type of method used to do the observing. For types of observation exist: (1) Density based observation, where observers count all of the resources within a subset of the landscape (the manager function can then later estimate total resource number from this estimate). (2) Mark-recapture based observation, where observers tag a fixed number of randomly sampled resources on the landscape some number of `times`; some of these resources marks are later interpreted as marks ('fix_mark') while the rest are interpreted as recaptures. (3) Transect based observation, where observers sample a linear transect, observing all resources on the transect one row of landscape cells at a time, until all landscape cells are sampled; between samples, resources might move generating observation error. (4) Block based sampling, which is very similar to Transect based sampling; here observers instead sample square blocks of a landscape, counting resources one block at a time, until the whole landscape is sampled; between samples resources might move generating observation error.
#'@param move_res Defines whether or not resources move during observation (default = FALSE). Note that if this is FALSE, then observation methods (obs_method) 3 and 4 produce no observation error 
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frame observations at the end of the time step
observation <- function(RESOURCES  = NULL, 
                        LAND       = NULL, 
                        PARAS      = NULL, 
                        AGENTS     = NULL,
                        inter_tabl = NULL,
                        fix_mark   = FALSE,
                        times      = 1,
                        samp_age   = 1,
                        agent_type = 0,
                        type_cat   = 1,
                        obs_method = 0,
                        move_res   = FALSE,
                        model      = "IBM"
                        ){
    check_model <- 0;
    # Use DATA as an array for mark-recapture information
    # Time-stamp the mark-recapture data so can simulate within/over years
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(RESOURCES)){
            stop("Warning: Resources need to be in an array");   
        }
        if(!is.array(LAND)){
            stop("Warning: Landscape need to be in an array");
        } # TODO: make sure paras is right length below
        if(!is.vector(PARAS) | !is.numeric(PARAS)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        if(!is.array(AGENTS)){
            stop("Warning: Agents need to be in an array");
        }
        # If all checks out, first put the type into PARAS for easier input
        PARAS[8]  <- agent_type;
        PARAS[9]  <- obs_method;
        PARAS[11] <- as.numeric(fix_mark); # Note: 'FALSE' coerced to zero
        PARAS[12] <- times;
        PARAS[17] <- samp_age;
        PARAS[18] <- type_cat;
        PARAS[20] <- as.numeric(move_res);
        if(times < 1){
            stop("Need to sample at least once (else no observation)");   
        }
        # Then run the population model
        OBSERVE_OUT  <- run_observation_a( RESOURCE_c   = RESOURCES,
                                           LANDSCAPE_c  = LAND,
                                           PARAMETERS_c = PARAS,
                                           AGENT_c      = AGENTS,
                                           INTERACT_c   = inter_tabl
                                           );
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(OBSERVE_OUT);
}

run_observation_a <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c,
                              INTERACT_c){
    .Call("observation", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, 
          INTERACT_c);
}