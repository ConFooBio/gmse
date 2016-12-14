dyn.load('src/resource.so') # Just keep this here for now.



setwd("Dropbox/projects/gmse");

source("R/initialise.R");
source("R/landscape.R");
source("R/resource.R");



pop_model       <- "IBM";
RESOURCE_1_ini  <- 100;
time_max        <- 2;
time            <- 0;
land_dim_1      <- 10;
land_dim_2      <- 10;
movement        <- 1;

# Set the landscape
LANDSCAPE_r  <- make_landscape( model      = pop_model, 
                                rows       = land_dim_1, 
                                cols       = land_dim_2, 
                                cell_types = 0 
                              );

# Set the starting conditions for one resource
starting_cond_1 <- initialise( model               = pop_model, 
                               resource_quantity_1 = RESOURCE_1_ini,
                               rows                = land_dim_1,
                               cols                = land_dim_2,
                               move                = movement
                             );


RESOURCE_REC <- NULL;
RESOURCE_1_r <- starting_cond_1;
while(time < time_max){
   RESOURCE_NEW  <- resource(resource_1 = RESOURCE_1_r,
                             landscape  = LANDSCAPE_r,
                             model      = "IBM"
                             );
   RESOURCES     <- RESOURCE_NEW;
   RESOURCE_REC  <- rbind(RESOURCE_REC[[1]], RESOURCE_NEW[[1]]);
   time <- time + 1;
}