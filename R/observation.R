#' Observation model
#'
#' A simulation of techniques (e.g., capture-mark-recapture) for estimating
#' population size and poperties.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param PARAS The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param inter_tabl Interaction table indexing types with the INTERACT matrix
#'@param fixed_mark Fixed number of individuals marked? (A number, or FALSE)
#'@param times_observe Number of times that the observations are made (e.g., managers go out sampling n times in an area of the landscape)
#'@param res_min_age Minimum age of the resource being sampled (default = 1)
#'@param agent_type The type of agent doing the observing (default = 0)
#'@param type_cat The category of agent type (first 4 columns) doing observing; this will almost always be 1, so type 0 agents (managers, of which there is always one by default) will perform the observations
#'@param observe_type The type of method used to do the observing. For types of observation exist: (1) Density based observation, where observers count all of the resources within a subset of the landscape (the manager function can then later estimate total resource number from this estimate). (2) Mark-recapture based observation, where observers tag a fixed number of randomly sampled resources on the landscape some number of `times`; some of these resources marks are later interpreted as marks ('fixed_mark') while the rest are interpreted as recaptures. (3) Transect based observation, where observers sample a linear transect, observing all resources on the transect one row of landscape cells at a time, until all landscape cells are sampled; between samples, resources might move generating observation error. (4) Block based sampling, which is very similar to Transect based sampling; here observers instead sample square blocks of a landscape, counting resources one block at a time, until the whole landscape is sampled; between samples resources might move generating observation error.
#'@param res_move_obs Defines whether or not resources move during observation (default = FALSE). Note that if this is FALSE, then observation methods (observe_type) 3 and 4 produce no observation error 
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@param ... Other arguments to be passed to a user-defined model
#'@return The observation function outputs an R list that includes three separate arrays, including (1) an new OBSERVATION array that holds observed resources and their traits with additional columns indicating when the resources were observed (relevant, e.g., for mark-recapture), (2) a new AGENTS array, and (3) a new PARAS array, each of which might be affected by the user function.  The new arrays can then be read back into the broader GMSE function, thereby affecting the input into the management, user, and resource models.
#'@examples
#'\dontrun{
#'OBSERVATION_NEW <- observation(RESOURCES = RESOURCES, LAND = LANDSCAPE_r,
#'PARAS = paras, AGENTS = AGENTS, inter_tabl = interaction_tabl, fixed_mark = fxo,
#'times_observe = tmo, res_min_age = rma, agent_type = 0, type_cat   = 1, observe_type = obt,
#'res_move_obs   = rmo);
#'}
#'@export
observation <- function(RESOURCES     = NULL, 
                        LAND          = NULL, 
                        PARAS         = NULL, 
                        AGENTS        = NULL,
                        inter_tabl    = NULL,
                        fixed_mark    = FALSE,
                        times_observe = 1,
                        res_min_age   = 0,
                        agent_type    = 0,
                        type_cat      = 1,
                        observe_type  = 0,
                        res_move_obs  = FALSE,
                        model         = "IBM"
                       ){
    check_model <- 0;
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
        if(dim(RESOURCES)[2] != 20){
            stop("The RESOURCES array has the wrong number of columns");
        }
        if(dim(LAND)[3] != 3){
            stop("The landscape doesn't have enough layers");
        }
        # If all checks out, first put the type into PARAS for easier input
        PARAS[8]  <- agent_type;
        PARAS[9]  <- observe_type;
        PARAS[11] <- as.numeric(fixed_mark); # Note: 'FALSE' coerced to zero
        PARAS[12] <- times_observe;
        PARAS[17] <- res_min_age;
        PARAS[18] <- type_cat;
        PARAS[20] <- as.numeric(res_move_obs);
        if(times_observe < 1){
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
    names(OBSERVE_OUT) <- c("OBSERVATION", "AGENTS", "PARAS");
    return(OBSERVE_OUT);
}

run_observation_a <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c,
                              INTERACT_c){
    .Call("observation", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c, 
          INTERACT_c);
}