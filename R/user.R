#' User model
#'
#' A model of user decisions for a single time step
#'
#'@param resource Resource data frame at the start of the time step
#'@param landscape Data frame at the start of the time step
#'@param paras Vector of parameter values to read into the model
#'@param model The type of model being applied (Currently only individual-based
#' -- i.e., 'agent-based' -- models are allowed)
#'@return Data frame of user output at the end of the time step
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

        #======================================================================
        # TEMPORARY R CODE TO DO USER ACTIONS (WILL BE RUN FROM C EVENTUALLY)
        #======================================================================
        
        for(agent_ID in 1:dim(agent)[1]){
            owned_cells <- sum(landscape[,,3] == agent_ID);
            # --- Put the agent on its own land
            if(owned_cells > 0){ # If the agent owns some land
                a_xloc <- agent[agent_ID, 5];
                a_yloc <- agent[agent_ID, 6];
                while(agent[agent_ID,1] != landscape[a_xloc, a_yloc, 3]){
                    a_xloc <- sample(x = 1:dim(landscape)[1], size = 1);
                    a_yloc <- sample(x = 1:dim(landscape)[2], size = 1);
                }
                agent[agent_ID, 5] <- a_xloc;
                agent[agent_ID, 6] <- a_yloc;
            } 
            # --- count up yield on cells
            agent_yield <- 0;
            xdim        <- dim(landscape[,,3])[1]
            ydim        <- dim(landscape[,,3])[2]
            for(i in 1:xdim){
                for(j in 1:ydim){
                    if(landscape[i,j,3] == agent[agent_ID,1]){
                        agent_yield <- agent_yield + landscape[i,j,2];
                    }
                }
            }
            agent[agent_ID, 16] <- agent_yield
        }
        USER_OUT <- list(resource, landscape, agent);
        # TODO: User actions are next...
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