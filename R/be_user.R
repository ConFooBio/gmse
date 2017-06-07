#' Become a hunter on the landscape
#' 
#' This function allows the user of the GMSE software to insert themselves as a hunter in the simulation, allowing them to cull some number of resources in a time step as observed by the agent whose ID is 2
#' 
#'@param OBSERVATION The observation array produced by the observation function within GMSE
#'@param AGENT The array of agents produced in the main gmse() function
#'@param RESOURCES The resources array produced by the resource function within GMSE
#'@param LAND The landscape array on which interactions between resources and agents occur
#'@param PARAS The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param view The distance within which agents are able to observe resources on the landscape
#'@param times The number of times that resources are observed in the observation model of GMSE
#'@return the_land A cols by rows landscape with randomly distributed cell types
#'@export
be_hunter <- function(OBSERVATION, AGENT, RESOURCES, LAND, PARAS, view, times){
    seeit    <- AGENT[2,13];
    view     <- view;
    count    <- dens_est(OBSERVATION, view, LAND, times)$Nc;
    count    <- floor(count);
    line0    <- paste("Year: ", RESOURCES[1,8]);
    line1    <- paste("The manager says the population size is ",count);
    line2    <- paste("You observe ",seeit," animals on the farm");
    line3    <- paste("Enter the number of animals to shoot");
    cat("\n");
    cat(line0);
    cat("\n");
    cat(line1);
    cat("\n");
    cat(line2);
    cat("\n");
    cat(line3);
    cat("\n");
    shot_char   <- readLines(con=stdin(),1);
    shooting    <- as.numeric(shot_char);
    while(is.na(shooting)){
        cat("Need to shoot a natural number -- try again");
        shot_char   <- readLines(con=stdin(),1);
        shooting    <- as.numeric(shot_char);
    }
    if(shooting > seeit){
        shooting <- seeit;
        cat("You can't shoot animals that you can't see");
        cat("\n");
        response <- paste(seeit," animals shot");
        cat(response);
        cat("\n");
    }
    if(shooting > 0){
        ress      <- dim(RESOURCES)[1];
        hunted    <- sample(x=1:ress, size = shooting, replace = FALSE);
        RESOURCES <- RESOURCES[-hunted,];
    }
    PARAS[33] <- dim(RESOURCES)[1];
    return(list(RESOURCES = RESOURCES, PARAS = PARAS));
}