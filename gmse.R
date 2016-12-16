#' General function for now
#'
#' Function to run G-MSE model
#'
#'@export


setwd("~/Dropbox/projects/gmse");

dyn.load('src/resource.so') # Just keep this here for now.

source("R/initialise.R");
source("R/landscape.R");
source("R/resource.R");

pop_model       <- "IBM";
RESOURCE_ini    <- 100;
time_max        <- 200;
time            <- 0;
land_dim_1      <- 20;
land_dim_2      <- 20;
movement        <- 1;
res_types_ini   <- 1; 

# Set the landscape
LANDSCAPE_r  <- make_landscape( model      = pop_model, 
                                rows       = land_dim_1, 
                                cols       = land_dim_2, 
                                cell_types = 0 
                              );

# Set the starting conditions for one resource
starting_resources <- initialise( model              = pop_model, 
                                  resource_quantity  = RESOURCE_ini,
                                  resource_types     = res_types_ini,
                                  rows               = land_dim_1,
                                  cols               = land_dim_2,
                                  move               = movement
                                );

time       <- time + 1;  # Ready for the initial time step.
parameters <- c(time,    # The dynamic time step for each function to use 
                1);      # The edge effect (0: nothing, 1: torus)

RESOURCE_REC <- NULL;
RESOURCES    <- starting_resources;
while(time < time_max){
   RESOURCE_NEW  <- resource(resource   = RESOURCES,
                             landscape  = LANDSCAPE_r,
                             paras      = parameters,
                             model      = "IBM"
                             );
   RESOURCES     <- RESOURCE_NEW;
   RESOURCE_REC  <- rbind(RESOURCE_REC, RESOURCES);
   time          <- time + 1;
   parameters[1] <- time;
}




colnames(RESOURCE_REC) <- c("Resource_ID",
                            "Resource_type_1",
                            "Resource_type_2",
                            "Resource_loc_x",
                            "Resource_loc_y",
                            "Resource_move",
                            "Resource_time");



# For fun, for now, let's see the indivdiuals move around
for(i in 1:(time_max-1)){
    res_t <- RESOURCE_REC[RESOURCE_REC[,7]==i,];
    par(mar=c(0,0,0,0))
    plot(x=res_t[,4], y=res_t[,5], pch=20, col=res_t[,1], xlim=c(0,20),
         ylim=c(0,20),xaxt="n",yaxt="n", cex=2);
    p1 <- proc.time()
    Sys.sleep(0.1)
    proc.time() - p1 # The cpu usage should be negligible
}



