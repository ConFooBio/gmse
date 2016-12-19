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
time_max        <- 100;
time            <- 0;
land_dim_1      <- 20;
land_dim_2      <- 20;
movement        <- 0.2;
res_types_ini   <- 1;
remove_pr       <- 0.001;
lambda          <- 2;

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
                                  move               = movement,
                                  rm_pr              = remove_pr
                                );

time       <- time + 1;  # Ready for the initial time step.
cells      <- land_dim_1 * land_dim_2; # Number of cells in the landscape

parameters <- c(time,    # 0. The dynamic time step for each function to use 
                1,       # 1. The edge effect (0: nothing, 1: torus)
                2,       # 2. Type of movement (0: none, 1: uniform, 2: Poisson)
                2,       # 3. Type of birth (0: none, 1: uniform, 2: Poisson)
                1,       # 4. Type of death (0: none, 1: uniform)
                cells    # 5. Carrying capacity for birth
                );
                
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
   if(dim(RESOURCES)[1] < 10){
       print("Extinction has occurred");
       break;
   }
}




colnames(RESOURCE_REC) <- c("Resource_ID",
                            "Resource_type_1",
                            "Resource_type_2",
                            "Resource_loc_x",
                            "Resource_loc_y",
                            "Resource_move",
                            "Resource_time",
                            "Resource_rm_pr",
                            "Resource_growth",
                            "Resource_grown"
                            );



# For fun, for now, let's see the indivdiuals move around
for(i in 1:(time_max-1)){
    res_t <- RESOURCE_REC[RESOURCE_REC[,7]==i,];
    par(mar=c(0,0,0,0))
    plot(x=res_t[,4], y=res_t[,5], pch=20, col=res_t[,1], xlim=c(-2,22),
         ylim=c(-2,22),xaxt="n",yaxt="n", cex=2);
    p1 <- proc.time()
    Sys.sleep(0.1)
    proc.time() - p1 # The cpu usage should be negligible
}



