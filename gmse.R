#' General function for now
#'
#' Function to run G-MSE model
#'
#'@export
rm(list=ls(all=TRUE))

setwd("~/Dropbox/projects/gmse");

# Compiled using the following
# R CMD SHLIB -o gmse.so resource.c observation.c
dyn.load('src/gmse.so') # Just keep this here for now.

source("R/initialise.R");
source("R/landscape.R");
source("R/resource.R");
source("R/observation.R");
source("R/anecdotal.R");


################################################################################

gmse( observe_type = 3,
      agent_view   = 10,
      res_death_K  = 800,
      plotting     = TRUE
);

################################################################################
# PRIMARY FUNCTION (gmse) FOR RUNNING A SIMULATION
# NOTE: RELIES ON SOME OTHER FUNCTIONS BELOW: MIGHT WANT TO READ WHOLE FILE
################################################################################
gmse <- function( time_max       = 100,   # Max number of time steps in sim
                  land_dim_1     = 100,   # x dimension of the landscape
                  land_dim_2     = 100,   # y dimension of the landscape
                  res_movement   = 1,     # How far do resources move
                  remove_pr      = 0.0,   # Density independent resource death
                  lambda         = 0.9,   # Resource growth rate
                  agent_view     = 10,    # Number cells agent view around them
                  agent_move     = 50,    # Number cells agent can move
                  res_birth_K    = 10000, # Carrying capacity applied to birth
                  res_death_K    = 400,   # Carrying capacity applied to death
                  edge_effect    = 1,     # What type of edge on the landscape
                  res_move_type  = 2,     # What type of movement for resources
                  res_birth_type = 2,     # What type of birth for resources
                  res_death_type = 2,     # What type of death for resources
                  observe_type   = 0,     # Type of observation used
                  fixed_observe  = 1,     # How many obs (if type = 1)
                  times_observe  = 1,     # How many times obs (if type = 0)
                  obs_move_type  = 1,     # Type of movement for agents
                  res_min_age    = 1,     # Minimum age recorded and observed
                  res_move_obs   = TRUE,  # Move resources while observing
                  Euclidean_dist = FALSE, # Use Euclidean distance in view
                  plotting       = TRUE   # Plot the results
){
    pop_model       <- "IBM";
    RESOURCE_ini    <- 100;
    movement        <- res_movement;
    res_types_ini   <- 1;   
    
    time            <- 0;
    
    proc_start <- proc.time();
    
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
                             vision       = agent_view,
                             rows         = land_dim_1,
                             cols         = land_dim_2,
                             move         = agent_move
    );  
    
    time       <- time + 1;  # Ready for the initial time step.
    cells      <- land_dim_1 * land_dim_2; # Number of cells in the landscape
    
    ldx <- land_dim_1;
    ldy <- land_dim_2;
    rbK <- res_birth_K;
    rdK <- res_death_K;
    edg <- edge_effect;
    r_m <- res_move_type;
    rbt <- res_birth_type;
    rdt <- res_death_type;
    obt <- observe_type;
    fxo <- fixed_observe;
    tmo <- times_observe;
    o_m <- obs_move_type;
    rma <- res_min_age;
    rmo <- res_move_obs;
    Euc <- Euclidean_dist;

    paras <- c(time,    # 0. The dynamic time step for each function to use 
               edg,     # 1. The edge effect (0: nothing, 1: torus)
               r_m,     # 2. Res movement (0: none, 1: unif, 2: Poisson, ...)
               rbt,     # 3. Type of birth (0: none, 1: uniform, 2: Poisson)
               rdt,     # 4. Type of death (0: none, 1: uniform, 2: K-based)
               rbK,     # 5. Carrying capacity for birth (-1 = unregulated)
               rdK,     # 6. Carrying capacity for death (-1 = unregulated)
               0,       # 7. The type of AGENT doing the observations
               obt,     # 8. The type of observing done for estimating pop.
               1,       # 9. The type of resource observed (note: dynamic)
               fxo,     # 10. Fix mark? Do observers mark exactly n resources?
               tmo,     # 11. Times resources observed during one time step
               ldx,     # 12. Land dimension on the x axis
               ldy,     # 13. Land dimension on the y axis
               o_m,     # 14. Agent movement (option same as #2)
               1,       # 15. Type category for resource observation
               rma,     # 16. Minimum age of sampling (1 excludes juveniles)
               1,       # 17. Type category for agent observation (default = 1)
               12,      # 18. Column where res seen recorded in agent array
               rmo,     # 19. Move resources while observing (0/1 = N/Y)
               Euc      # 20. Distance is Euclidean (1) or within-cell (0)
    );
    RESOURCE_REC    <- NULL;
    RESOURCES       <- starting_resources;
    OBSERVATION_REC <- NULL;
    while(time < time_max){
        RESOURCE_NEW      <- resource(resource   = RESOURCES,
                                      landscape  = LANDSCAPE_r,
                                      paras      = paras,
                                      move_res   = TRUE,
                                      model      = "IBM"
        );
        RESOURCES         <- RESOURCE_NEW;
        RESOURCE_REC      <- rbind(RESOURCE_REC, RESOURCES);
        
        OBSERVATION_NEW   <- observation(resource   = RESOURCES,
                                         landscape  = LANDSCAPE_r,
                                         paras      = paras,
                                         agent      = AGENTS,
                                         res_type   = 1, # Resource(s) observed
                                         fix_mark   = fxo,
                                         times      = tmo,
                                         samp_age   = rma,
                                         agent_type = 0,
                                         type_cat   = 1,
                                         obs_method = obt,
                                         move_res   = rmo
        );
        
        # anecdotal is a bit useless right now, but included here anyway. 
        AGENTS            <- anecdotal(resource   = RESOURCES,
                                       landscape  = LANDSCAPE_r,
                                       paras      = paras,
                                       agent      = AGENTS,
                                       res_type   = 1,
                                       samp_age   = rma,
                                       agent_type = -1,
                                       type_cat   = 1
        );
        
        OBSERVATION_REC   <- rbind(OBSERVATION_REC, OBSERVATION_NEW);
        time              <- time + 1;
        paras[1]          <- time;
        if(dim(RESOURCES)[1] < 10){
            print("Extinction has occurred");
            break;
        }
    }
    
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
    
    proc_end   <- proc.time();
    time_taken <- proc_end - proc_start;
    
    
    sim_results <- list(resource    = RESOURCE_REC,
                        observation = OBSERVATION_REC,
                        paras       = paras,
                        land        = LANDSCAPE_r,
                        time_taken  = time_taken,
                        agents      = AGENTS
                        );
    
    if(plotting == TRUE){
        if(obt == 0){
            case01plot(res   = RESOURCE_REC, 
                       obs   = OBSERVATION_REC, 
                       land  = LANDSCAPE_r, 
                       paras = paras, 
                       view  = agent_view);
        }
        if(obt == 1){
            case01plot(res   = RESOURCE_REC, 
                       obs   = OBSERVATION_REC, 
                       land  = LANDSCAPE_r, 
                       paras = paras);
        }
        if(obt == 2){
            case23plot(res   = RESOURCE_REC, 
                       obs   = OBSERVATION_REC, 
                       land  = LANDSCAPE_r, 
                       paras = paras);
        }
        if(obt == 3){
            case23plot(res   = RESOURCE_REC, 
                       obs   = OBSERVATION_REC, 
                       land  = LANDSCAPE_r, 
                       paras = paras);
        }
    }

    
    return(sim_results);
}
################################################################################
################################################################################


################################################################################
# Helps estimate mark-recapture -- give it's own place in an analysis file later
################################################################################
cmr_estimate <- function(obs, year){
    dat <- obs[obs[,8]==year,];
    K_tot   <- dim(dat)[1];
    k_cau   <- dat[dat[,13]==2,];
    
}

################################################################################
# Chapman estimator for capture-mark-recapture
################################################################################
chapman_est <- function(observation, marks = 1, recaptures = 1){
    mcols  <- seq(from = 16, to = 16 + (marks-1), by = 1);
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

################################################################################
# Actually put the individuals on the landscape with function below
################################################################################
ind_to_land <- function(inds, landscape){
    ind_rep <- max(landscape) + 1;

    for(i in 1:dim(inds)[1]){
        x <- as.numeric(inds[i,5]);
        y <- as.numeric(inds[i,6]);
        landscape[y,x] <- ind_rep;
    }
    
    return(landscape);
}

################################################################################
# Density estimator
################################################################################
dens_est <- function(observation = obs_t, view = view, land = land){
    vision <- (2*view) + 1;
    area   <- vision * vision;
    cells  <- dim(land)[1] * dim(land)[2];
    if(area > cells){
        area <- cells;   
    }
    prp    <- dim(observation)[1] / area;
    est    <- prp * cells;
    lcp    <- prp - 1.96 * sqrt((1/(vision*vision))*prp*(1-prp));
    ucp    <- prp + 1.96 * sqrt((1/(vision*vision))*prp*(1-prp));
    lci    <- cells * lcp
    uci    <- cells * ucp;
    return(list(Nc=est, lci=lci, uci=uci));
}

###########################################################
## Plot this way when looking at transect type sampling
###########################################################
case23plot <- function(res, obs, land, paras){
    gens <- NULL;
    abun <- NULL;
    est  <- NULL;
    lci  <- NULL;
    uci  <- NULL;
    land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 

    aged_res <- res[res[,12] > 0,];
    ymaxi    <- max(tapply(aged_res[,8],aged_res[,8],length)) + 325;
    time_max <- max(res[,8]);
    for(i in 1:(time_max-1)){
        res_t <- res[res[,8]==i,];
        obs_t <- obs[obs[,8]==i,];
        if(i > 1){
            res_t <- res_t[res_t[,12] > 0,]; # Only look at res not just added
        }
        gens  <- c(gens, i);
        abun  <- c(abun, dim(res_t)[1]);
        par(mfrow=c(2,1),mar=c(0,0,0,0));
        indis  <- ind_to_land(inds=res_t, landscape=land);
        image(indis, col=land_cols, xaxt="n", yaxt="n");
        par(mar=c(4,4,1,1));
        plot(x=gens, y=abun, pch=20, type="l", lwd=2, ylim=c(0, ymaxi),
             xlim=c(0,time_max), xlab="Time Step", ylab="Abundance");
        new_est   <- sum(obs[obs[,8]==i,13]);
        est       <- c(est, new_est);
        points(x=gens, y=est, pch=20, type="l", lwd=2, col="cyan4");
        abline(h=paras[7], col="red", lwd=0.8, lty="dashed");
        points(x=gens, y=abun, pch=20, type="l", lwd=3, col="black");
        Sys.sleep(0.1);
    }
}


####################################################################
## Plot this way when looking at view or mark-recapture sampling
####################################################################
case01plot <- function(res, obs, land, paras, view = NULL){
    gens <- NULL;
    abun <- NULL;
    est  <- NULL;
    lci  <- NULL;
    uci  <- NULL;
    land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 
    
    case  <- paras[9];
    tiobs <- paras[12];
    if(case == 1 & tiobs < 2){
        return("No RMR possible"); 
    }
    mrk <- floor(tiobs / 2);
    rcp <- tiobs - mrk;

    aged_res <- res[res[,12] > 0,];
    ymaxi    <- max(tapply(aged_res[,8],aged_res[,8],length)) + 325;
    time_max <- max(res[,8]);
    for(i in 1:(time_max-1)){
        res_t <- res[res[,8]==i,];
        obs_t <- obs[obs[,8]==i,];
        if(i > 1){
            res_t <- res_t[res_t[,12] > 0,]; # Only look at res not just added
        }
        gens  <- c(gens, i);
        abun  <- c(abun, dim(res_t)[1]);
        par(mfrow=c(2,1),mar=c(0,0,0,0));
        indis  <- ind_to_land(inds=res_t, landscape=land);
        image(indis, col=land_cols, xaxt="n", yaxt="n");
        par(mar=c(4,4,1,1));
        plot(x=gens, y=abun, pch=20, type="l", lwd=2, ylim=c(0, ymaxi),
             xlim=c(0,time_max), xlab="Time Step", ylab="Abundance");
        if(!is.null(obs_t) & case == 1){
            analysis <- chapman_est(observation=obs_t, marks=mrk, 
                                    recaptures=rcp);
            est      <- c(est, analysis$Nc);
            lci      <- c(lci, analysis$lci);
            uci      <- c(uci, analysis$uci);
        }
        if(!is.null(obs_t) & !is.null(view) & case == 0){
            analysis <- dens_est(observation=obs_t, view=view, land=land);
            est      <- c(est, analysis$Nc);
            lci      <- c(lci, analysis$lci);
            uci      <- c(uci, analysis$uci);
        }
        polygon(y=c(lci,rev(uci)),x=c(gens,rev(gens)),border=NA,
                col="lightblue");
        points(x=gens, y=est, pch=20, type="l", lwd=2, col="cyan4");
        abline(h=paras[7], col="red", lwd=0.8, lty="dashed");
        points(x=gens, y=abun, pch=20, type="l", lwd=3, col="black");
        Sys.sleep(0.1);
    }
}


