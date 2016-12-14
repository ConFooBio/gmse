





dyn.load('resource.so') # Just keep this here for now.

pop_model       <- "IBM";
RESOURCE_1_ini  <- 100;
time_max        <- 2;
time            <- 0;
land_dim_1      <- 10;
land_dim_2      <- 10;

# Set the landscape
LANDSCAPE  <- make_landscape( model      = pop_model, 
                              rows       = land_dim_1, 
                              cols       = land_dim_2, 
                              cell_types = 0 
                            );

# Set the starting conditions for one resource
starting_cond_1 <- initialise( model               = pop_model, 
                               resource_quantity_1 = RESOURCE_1_ini,
                               rows                = land_dim_1,
                               cols                = land_dim_2
                             );


RESOURCE_REC <- NULL;
RESOURCE_1   <- starting_cond_1;
while(time < time_max){
   RESOURCE_NEW  <- resource(RESOURCE_1 = RESOURCE_1,
                             LANDSCAPE  = LANDSCAPE,
                             model      = "IBM"
                             );
   RESOURCES     <- RESOURCE_NEW;
   RESOURCE_REC  <- rbind(RESOURCE_REC[[1]], RESOURCE_NEW[[1]]);
   time <- time + 1;
}