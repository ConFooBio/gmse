#' Anecdotal model
#'
#' A simulation of how many resources of a particular type are in the vicinity 
#' of each agent -- this produces a kind of anecdotal evidence for each agent 
#' around their circle of view. It also potentially moves the agents during a 
#' time step.
#'
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param PARAS The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param AGENTS The array of agents produced in the main gmse() function
#'@param res_type The type of resources being observed (default = 1)
#'@param samp_age Minimum age of the resource being sampled (default = 1)
#'@param agent_type The type of agent doing the observing (default = 0)
#'@param type_cat The category of agent type (first 4 columns) doing observing; this will almost always be 1, so type 0 agents (managers, of which there is always one by default) will be affected
#'@param move_agents Whether or not agents are moved during the run of anecodtal
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return The anecdotal function outputs an R list that includes two separate arrays, including (1) a new AGENTS array and (3) a new PARAS array, each of which might be affected by the anecdotal function.  The new arrays can then be read back into the broader GMSE function, thereby affecting the input into the management, user, resource, and observation models.
#'@examples
#'\dontrun{
#'AGENTS_NEW <- anecdotal(RESOURCES = RESOURCES, LAND = LANDSCAPE_r, 
#'PARAS = paras, AGENTS = AGENTS, res_type = 1, samp_age = rma, agent_type = -1,
#'type_cat = 1, move_agents = mva);
#'}
#'@export
anecdotal <- function(RESOURCES   = NULL, 
                      LAND        = NULL, 
                      PARAS       = NULL, 
                      AGENTS      = NULL,
                      res_type    = 1,
                      samp_age    = 1,
                      agent_type  = 0,
                      type_cat    = 1,
                      move_agents = FALSE,
                      model       = "IBM"
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
        PARAS[10] <- res_type;
        PARAS[17] <- samp_age;
        PARAS[18] <- type_cat;
        PARAS[29] <- move_agents;
        # Then run the population model
        ANECDOTAL_OUT  <- run_anecdotal_a( RESOURCE_c   = RESOURCES,
                                           LANDSCAPE_c  = LAND,
                                           PARAMETERS_c = PARAS,
                                           AGENT_c      = AGENTS
        );
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(ANECDOTAL_OUT);
}

run_anecdotal_a <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c){
    .Call("anecdotal", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c, AGENT_c);
}