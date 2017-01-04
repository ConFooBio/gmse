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
source("R/observation.R");

proc_start <- proc.time();


pop_model       <- "IBM";
RESOURCE_ini    <- 100;
time_max        <- 100;
time            <- 0;
land_dim_1      <- 100;
land_dim_2      <- 100;
movement        <- 0.1;
res_types_ini   <- 1;
remove_pr       <- 0.0;
lambda          <- 0.6;

# Set the landscape
LANDSCAPE_r  <- make_landscape( model      = pop_model, 
                                rows       = land_dim_1, 
                                cols       = land_dim_2, 
                                cell_types = 2
                              );

# Set the starting conditions for one resource
starting_resources <- make_resource( model              = pop_model, 
                                     resource_quantity  = RESOURCE_ini,
                                     resource_types     = res_types_ini,
                                     rows               = land_dim_1,
                                     cols               = land_dim_2,
                                     move               = movement,
                                     rm_pr              = remove_pr
                                   );

# This will obviously need to be changed -- new function in initialise.R
AGENTS   <- matrix(data=0, nrow=2, ncol=2);

time       <- time + 1;  # Ready for the initial time step.
cells      <- land_dim_1 * land_dim_2; # Number of cells in the landscape

parameters <- c(time,    # 0. The dynamic time step for each function to use 
                1,       # 1. The edge effect (0: nothing, 1: torus)
                1,       # 2. Type of movement (0: none, 1: uniform, 2: Poisson)
                2,       # 3. Type of birth (0: none, 1: uniform, 2: Poisson)
                2,       # 4. Type of death (0: none, 1: uniform, 2: K-based)
                cells,   # 5. Carrying capacity for birth (-1 = unregulated)
                400,     # 6. Carrying capacity for death (-1 = unregulated)
                0,       # 7. The type of AGENT doing the observations
                0        # 8. The type of observing done for estimating pop.
                );

# Create a warning somewhere if population size is not regulated
                
RESOURCE_REC    <- NULL;
RESOURCES       <- starting_resources;
OBSERVATION_REC <- NULL;
while(time < time_max){
   RESOURCE_NEW      <- resource(resource   = RESOURCES,
                                 landscape  = LANDSCAPE_r,
                                 paras      = parameters,
                                 model      = "IBM"
                                 );
   RESOURCES         <- RESOURCE_NEW;
   RESOURCE_REC      <- rbind(RESOURCE_REC, RESOURCES);
   
   OBSERVATION_NEW   <- observation(resource   = RESOURCES,
                                    landscape  = LANDSCAPE_r,
                                    paras      = parameters,
                                    agent      = AGENTS
                                    );
   OBSERVATION_REC   <- rbind(OBSERVATION_REC, OBSERVATION_NEW);
   time          <- time + 1;
   parameters[1] <- time;
   if(dim(RESOURCES)[1] < 10){
       print("Extinction has occurred");
       break;
   }
}

proc_end <- proc.time();


colnames(RESOURCE_REC) <- c("Resource_ID",
                            "Resource_type_1",
                            "Resource_type_2",
                            "Resource_type_3",
                            "Resource_loc_x",
                            "Resource_loc_y",
                            "Resource_move",
                            "Resource_time",
                            "Resource_rm_pr",
                            "Resource_growth",
                            "Resource_grown",
                            "Resource_age"
                            );



# Actually put the individuals on the landscape with function below
ind_to_land <- function(inds, landscape){
    ind_rep <- max(landscape) + 1;

    for(i in 1:dim(inds)[1]){
        x <- as.numeric(inds[i,5]);
        y <- as.numeric(inds[i,6]);
        landscape[y,x] <- ind_rep;
    }
    
    return(landscape);
}

gens <- NULL;
abun <- NULL;
land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 

aged_res <- RESOURCE_REC[RESOURCE_REC[,12] > 0,];
ymaxi    <- max(tapply(aged_res[,8],aged_res[,8],length)) + 100;
for(i in 1:(time_max-1)){
    res_t <- RESOURCE_REC[RESOURCE_REC[,8]==i,];
    if(i > 1){
        res_t <- res_t[res_t[,12] > 0,]; # Only look at res not just added
    }
    gens  <- c(gens, i);
    abun  <- c(abun, dim(res_t)[1]);
    par(mfrow=c(2,1),mar=c(0,0,0,0));
    indis  <- ind_to_land(inds=res_t, landscape=LANDSCAPE_r);
    image(indis, col=land_cols, xaxt="n", yaxt="n");
    par(mar=c(4,4,1,1));
    plot(x=gens, y=abun, pch=20, type="l", lwd=2, ylim=c(0, ymaxi),
         xlim=c(0,time_max), xlab="Time Step", ylab="Abundance");
    abline(h=parameters[7], col="red", lwd=0.8, lty="dashed");
    Sys.sleep(0.1);
}



print(proc_end - proc_start);




# The code below can be used to do the same as above, but see individuals move
for(i in 1:(time_max-1)){
    res_t <- RESOURCE_REC[RESOURCE_REC[,7]==i,];
    if(i > 1){
        res_t <- res_t[res_t[,11] > 0,]; # Only look at res not just added
    }
    gens  <- c(gens, i);
    abun  <- c(abun, dim(res_t)[1]);
    par(mfrow=c(2,1),mar=c(0,0,0,0));
    plot(x=res_t[,4], y=res_t[,5], pch=20, col=res_t[,1], xlim=c(-2,100),
         ylim=c(-2,100),xaxt="n",yaxt="n", cex=2);
    par(mar=c(4,4,1,1));
    plot(x=gens, y=abun, pch=20, type="l", lwd=2, ylim=c(0, ymaxi),
         xlim=c(0,time_max), xlab="Time Step", ylab="Abundance");
    abline(h=parameters[7], col="red", lwd=0.8, lty="dashed");
    Sys.sleep(0.1);    
}






