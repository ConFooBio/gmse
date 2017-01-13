#' General function for now
#'
#' Function to run G-MSE model
#'
#'@export


setwd("~/Dropbox/projects/gmse");

# Compiled using the following
# R CMD SHLIB -o gmse.so resource.c observation.c
dyn.load('src/gmse.so') # Just keep this here for now.

source("R/initialise.R");
source("R/landscape.R");
source("R/resource.R");
source("R/observation.R");
source("R/anecdotal.R");

proc_start <- proc.time();


pop_model       <- "IBM";
RESOURCE_ini    <- 100;
time_max        <- 100;
time            <- 0;
land_dim_1      <- 100;
land_dim_2      <- 100;
movement        <- 4;
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
                                     rm_pr              = remove_pr,
                                     lambda             = lambda
                                   );

# This will obviously need to be changed -- new function in initialise.R
AGENTS   <- make_agents( model        = pop_model,
                         agent_number = 2,
                         type_counts  = c(1,1),
                         vision       = 1,
                         rows         = land_dim_1,
                         cols         = land_dim_2,
                         move         = 50 # Make sure <= landscape dims
                        );

time       <- time + 1;  # Ready for the initial time step.
cells      <- land_dim_1 * land_dim_2; # Number of cells in the landscape

ldx <- land_dim_1;
ldy <- land_dim_2;

parameters <- c(time,    # 0. The dynamic time step for each function to use 
                1,       # 1. The edge effect (0: nothing, 1: torus)
                3,       # 2. Res movement (0: none, 1: unif, 2: Poisson, ...)
                2,       # 3. Type of birth (0: none, 1: uniform, 2: Poisson)
                2,       # 4. Type of death (0: none, 1: uniform, 2: K-based)
                cells,   # 5. Carrying capacity for birth (-1 = unregulated)
                400,     # 6. Carrying capacity for death (-1 = unregulated)
                0,       # 7. The type of AGENT doing the observations
                0,       # 8. The type of observing done for estimating pop.
                1,       # 9. The type of resource observed (note: dynamic)
                0,       # 10. Fix mark? Do observers mark exactly n resources?
                0,       # 11. Times resources observed during one time step
                ldx,     # 12. Land dimension on the x axis
                ldy,     # 13. Land dimension on the y axis
                1,       # 14. Agent movement (option same as #2)
                1,       # 15. Type category for resource observation
                1,       # 16. Minimum age of sampling (1 excludes juveniles)
                1,       # 17. Type category for agent observation (default = 1)
                12,      # 18. Column where res seen recorded in agent array
                1        # 19. Move resources while observing (0/1 = N/Y)
                );

# Create a warning somewhere if population size is not regulated

RESOURCE_REC    <- NULL;
RESOURCES       <- starting_resources;
OBSERVATION_REC <- NULL;
while(time < time_max){
   RESOURCE_NEW      <- resource(resource   = RESOURCES,
                                 landscape  = LANDSCAPE_r,
                                 paras      = parameters,
                                 move_res   = TRUE,
                                 model      = "IBM"
                                 );
   RESOURCES         <- RESOURCE_NEW;
   RESOURCE_REC      <- rbind(RESOURCE_REC, RESOURCES);
   
   OBSERVATION_NEW   <- observation(resource   = RESOURCES,
                                    landscape  = LANDSCAPE_r,
                                    paras      = parameters,
                                    agent      = AGENTS,
                                    res_type   = 1,      # Resource(s) observed
                                    fix_mark   = 0,      # Fixed or view-based
                                    times      = 1,      # Times observed
                                    samp_age   = 1,      # Minimum resource age
                                    agent_type = 0,      # Agent type
                                    type_cat   = 1,      # Type category (row)
                                    obs_method = 1,      # How res are observed
                                    move_res   = TRUE   # Do resources move
                                    );
   
   # anecdotal is a bit useless right now, but included here anyway. 
   AGENTS            <- anecdotal(resource   = RESOURCES,
                                  landscape  = LANDSCAPE_r,
                                  paras      = parameters,
                                  agent      = AGENTS,
                                  res_type   = 1,
                                  samp_age   = 1,
                                  agent_type = -1,
                                  type_cat   = 1
                                  );
   
   OBSERVATION_REC   <- rbind(OBSERVATION_REC, OBSERVATION_NEW);
   time              <- time + 1;
   parameters[1]     <- time;
   if(dim(RESOURCES)[1] < 10){
       print("Extinction has occurred");
       break;
   }
}

proc_end <- proc.time();


res_columns <- c("Resource_ID",
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
                 "Resource_age",
                 "Resource_marked",
                 "Resource_tally"
                );

colnames(RESOURCE_REC)    <- res_columns;

# Give this it's own place in an analysis file later
cmr_estimate <- function(obs, year){
    dat <- obs[obs[,8]==year,];
    K_tot   <- dim(dat)[1];
    k_cau   <- dat[dat[,13]==2,];
    
}

chapman_est <- function(observation, marks = 1, recaptures = 1){
    mcols  <- seq(from = 15, to = 15 + (marks-1), by = 1);
    rcols  <- seq(from = max(mcols+1), to = max(mcols+1)+(recaptures-1), by=1);
    if(marks > 1){
        mrked <- apply(X=observation[,mcols], MARGIN = 1, FUN = sum);
        mrked <- mrked > 0;
    }else{
        mrked <- observation[,mcols];   
    }
    if(recaptures > 1){
        recpt <- apply(X=observation[,rcols], MARGIN = 1, FUN = sum);
        recpt <- recpt > 0;
    }else{
        recpt <- observation[,rcols];   
    }
    n      <- sum(mrked);
    K      <- sum(recpt);
    recapt <- mrked + recpt;
    k      <- sum(recapt == 2);
    Nc     <- ((n + 1) * (K + 1) / (k + 1)) - 1;
    Nc     <- floor(Nc);
    a      <- ((n+1)*(K+1)*(n-k)*(K-k));
    b      <- ((k+1)*(k+1)*(k+2));
    varNc  <- a/b;
    lci    <- Nc - (1.965 * sqrt(varNc));
    uci    <- Nc + (1.965 * sqrt(varNc));
    return(list(Nc=Nc,lci=lci,uci=uci));
}

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




###########################################################
## Plot this way when looking at transect type sampling
###########################################################
gens <- NULL;
abun <- NULL;
est  <- NULL;
lci  <- NULL;
uci  <- NULL;
land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 

aged_res <- RESOURCE_REC[RESOURCE_REC[,12] > 0,];
ymaxi    <- max(tapply(aged_res[,8],aged_res[,8],length)) + 325;
for(i in 1:(time_max-1)){
    res_t <- RESOURCE_REC[RESOURCE_REC[,8]==i,];
    obs_t <- OBSERVATION_REC[OBSERVATION_REC[,8]==i,];
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
    new_est   <- sum(OBSERVATION_REC[OBSERVATION_REC[,8]==i,13]);
    est       <- c(est, new_est);
    points(x=gens, y=est, pch=20, type="l", lwd=2, col="cyan4");
    abline(h=parameters[7], col="red", lwd=0.8, lty="dashed");
    points(x=gens, y=abun, pch=20, type="l", lwd=3, col="black");
    Sys.sleep(0.1);
}






####################################################################
## Plot this way when looking at view or mark-recapture sampling
####################################################################
gens <- NULL;
abun <- NULL;
est  <- NULL;
lci  <- NULL;
uci  <- NULL;
land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 

aged_res <- RESOURCE_REC[RESOURCE_REC[,12] > 0,];
ymaxi    <- max(tapply(aged_res[,8],aged_res[,8],length)) + 325;
for(i in 1:(time_max-1)){
    res_t <- RESOURCE_REC[RESOURCE_REC[,8]==i,];
    obs_t <- OBSERVATION_REC[OBSERVATION_REC[,8]==i,];
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
    if(!is.null(obs_t)){
        analysis <- chapman_est(observation=obs_t, marks=3, recaptures=9);
        est      <- c(est, analysis$Nc);
        lci      <- c(lci, analysis$lci);
        uci      <- c(uci, analysis$uci);
    }
    polygon(y=c(lci,rev(uci)),x=c(gens,rev(gens)),border=NA,col="lightblue");
    points(x=gens, y=est, pch=20, type="l", lwd=2, col="cyan4");
    abline(h=parameters[7], col="red", lwd=0.8, lty="dashed");
    points(x=gens, y=abun, pch=20, type="l", lwd=3, col="black");
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






