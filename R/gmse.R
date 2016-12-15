setwd("Dropbox/projects/gmse");

dyn.load('src/resource.so') # Just keep this here for now.

source("R/initialise.R");
source("R/landscape.R");
source("R/resource.R");

pop_model       <- "IBM";
RESOURCE_ini    <- 100;
time_max        <- 10;
time            <- 0;
land_dim_1      <- 10;
land_dim_2      <- 10;
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
parameters <- c(time, 0);

RESOURCE_REC <- NULL;
RESOURCES    <- starting_resources;
while(time < time_max){
   RESOURCE_NEW  <- resource(resource   = RESOURCES,
                             landscape  = LANDSCAPE_r,
                             paras      = parameters,
                             model      = "IBM"
                             );
   RESOURCES     <- RESOURCE_NEW;
   timestamp     <- cbind(RESOURCES, rep(x = time, length = dim(RESOURCES)[1]));
   RESOURCE_REC  <- rbind(RESOURCE_REC, timestamp);
   time          <- time + 1;
}








# For fun, for now, let's see the indivdiuals move around
