####################################################################
## A bit of code to read out and allow input to the observation
####################################################################
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