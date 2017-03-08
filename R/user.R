#' User model
#'
#' A model of user decisions for a single time step
#'
#'@param resource Resource data frame at the start of the time step
#'@param landscape Data frame at the start of the time step
#'@param paras Vector of parameter values to read into the model
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frame of Resource 2 at the end of the time step
#'@export
user <- function(resource   = NULL,
                 agent      = NULL,
                 landscape  = NULL, 
                 paras      = NULL,
                 model      = "IBM"
) {
    check_model <- 0;
    if(model == "IBM"){
        # Relevant warnings below if the inputs are not of the right type
        if(!is.array(resource)){
            stop("Warning: Resources need to be in an array");   
        }
        if(!is.array(agent)){
            stop("Warning: Agents need to be in an array");   
        }
        if(!is.array(landscape)){
            stop("Warning: Landscape need to be in an array");
        } # TODO: make sure paras is right length below
        if(!is.vector(paras) | !is.numeric(paras)){
            stop("Warning: Parameters must be in a numeric vector");
        }
        # If all checks out, then run the population model
        paras[20] <- as.numeric(move_res);

        #======================================================================
        # TEMPORARY R CODE TO DO USER ACTIONS (WILL BE RUN FROM C EVENTUALLY)
        #======================================================================
        
        for(agent_ID in 1:dim(agent)[1]){
            owned_cells <- sum(landscape[,,3] == agent_ID);
            if(owned_cells > 0){ # If the agent owns some land
                a_xloc <- agent[agent_ID, 5];
                a_yloc <- agent[agent_ID, 6];
                while(agent[agent_ID,1] != landscape[a_xloc, a_yloc, 3]){
                    agent_xloc <- floor( runif(n = 1) * dim(landscape)[1] );
                    agent_yloc <- floor( runif(n = 1) * dim(landscape)[2] );
                }
            } # Place the agent somewhere on its own landscape
            
            
            
        }
        
        #======================================================================

        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(USER_OUT);
}

# Below is to be included later

#run_user <- function(RESOURCE_c, AGENT_c, LANDSCAPE_c, PARAMETERS_c){
#    .Call("resource", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c);
#}