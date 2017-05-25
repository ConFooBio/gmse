#' General function for now
#'
#' Function to run G-MSE model
#'
#'@export
rm(list=ls(all=TRUE));

################################################################################

setwd("~/Dropbox/projects/gmse");

# Compiled using the following
# R CMD SHLIB -o gmse.so resource.c observation.c user.c game.c utilities.c
#   manager.c
dyn.load('src/gmse.so') # Just keep this here for now.

source("R/initialise.R");
source("R/landscape.R");
source("R/resource.R");
source("R/observation.R");
source("R/user.R");
source("R/anecdotal.R");
source("R/manager.R");

################################################################################

################################################################################
# PRIMARY FUNCTION (gmse) FOR RUNNING A SIMULATION
# NOTE: RELIES ON SOME OTHER FUNCTIONS BELOW: MIGHT WANT TO READ WHOLE FILE
################################################################################
gmse <- function( time_max       = 100,   # Max number of time steps in sim
                  land_dim_1     = 100,   # x dimension of the landscape
                  land_dim_2     = 100,   # y dimension of the landscape
                  res_movement   = 1,     # How far do resources move
                  remove_pr      = 0.0,   # Density independent resource death
                  lambda         = 0.05,  # Resource growth rate
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
                  plotting       = TRUE,  # Plot the results
                  hunt           = FALSE, # Does the user hunt resources?
                  start_hunting  = 0,     # What generation hunting starts
                  res_consume    = 0.5,   # Pr. landscape cell consumed by res
                  ga_popsize     = 100,   # Pop size in genetic algorithm
                  ga_mingen      = 20,    # Minimum generations in a ga run
                  ga_seedrep     = 10,    # How many copies to seed a ga with
                  ga_sampleK     = 10,    # Random sample size in ga tournament
                  ga_chooseK     = 2,     # Select from sample in ga tournament
                  ga_mutation    = 0.1,   # Mutation rate in genetic algorithm
                  ga_crossover   = 0.1,   # Crossover rate in genetic algorithm
                  move_agents    = TRUE,  # Move agents once per time step
                  max_ages       = 5      # Maximum age of any resource(s)
){
    
    if(observe_type == 1 & times_observe < 2){
        stop("Need to observe at least twice for mark-recapture");   
    }
    
    pop_model       <- "IBM";
    RESOURCE_ini    <- 100;
    movement        <- res_movement;
    res_types_ini   <- 1;
    
    time            <- 0;
    
    proc_start <- proc.time();
    
    # Set the landscape
    LANDSCAPE_r  <- make_landscape( model       = pop_model, 
                                    rows        = land_dim_1, 
                                    cols        = land_dim_2, 
                                    cell_types  = 2,
                                    cell_val_mn = 1,
                                    cell_val_sd = 0,
                                    ownership   = 1:5,
                                    owner_pr    = c(0, 0.25, 0.25, 0.25, 0.25)
    );
    # Set the starting conditions for one resource
    starting_resources <- make_resource( model              = pop_model, 
                                         resource_quantity  = RESOURCE_ini,
                                         resource_types     = res_types_ini,
                                         rows               = land_dim_1,
                                         cols               = land_dim_2,
                                         move               = movement,
                                         rm_pr              = remove_pr,
                                         lambda             = lambda,
                                         consumption_rate   = res_consume,
                                         max_age            = max_ages[1]
    );
    # This will obviously need to be changed -- new function in initialise.R
    AGENTS   <- make_agents( model        = pop_model,
                             agent_number = 5,
                             type_counts  = c(1, 4),
                             vision       = agent_view,
                             rows         = land_dim_1,
                             cols         = land_dim_2,
                             move         = agent_move
    ); 
    
    Jacobian <- make_interaction_array(resources = starting_resources,
                                       landscape = LANDSCAPE_r
    );
    Jacobian[1,2] <- -1 * res_consume; # Temporary to fix consumption rate
    Jacobian[2,1] <- 0;
    
    interaction_tabl <- make_interaction_table(starting_resources, LANDSCAPE_r);
    
    COST   <- make_utilities( AGENTS = AGENTS, RESOURCES = starting_resources);
    COST[COST < 1] <- 1; # Need this until a proper make_cost function is made
    COST[,1:7,]      <- 10000;
    COST[,11:12,2:5] <- 10000;
    COST[,8:13,]     <- 1;
    COST[2,8:13,]    <- 10000;
    COST[,8:13,1]    <- 10000;
    COST[3:7,8:13,2:5] <- 10000;
    COST[3,8:13,1]   <- 1;
    COST[COST < 1]   <- 1;
    ACTION <- make_utilities( AGENTS = AGENTS, RESOURCES = starting_resources);
    ACTION[1:2,5:7,] <- 1;
    ACTION[3,5:7,1]  <- 0;
    ACTION[1,5,2:5]  <- 0;
    ACTION[1,5,1]    <- 100;
    ACTION[2,5,2:5]  <- 100;
    ACTION[2,5,3]    <- 100;
    ACTION[1,5,1]    <- 200;   ###### CONTROL HOW MUCH MANAGER LIKES RESOURCES
    ACTION[3:7,5:13,2:5]   <- 0;
    AGENTS[,17]     <- 1200;
    AGENTS[1,17]    <- 300;
    
    time       <- time + 1;  # Ready for the initial time step.
    cells      <- land_dim_1 * land_dim_2; # Number of cells in the landscape
    
    ldx <- land_dim_1;
    ldy <- land_dim_2;
    ldz <- dim(LANDSCAPE_r)[3];
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
    prc <- res_consume;
    gap <- ga_popsize;
    gam <- ga_mingen;
    gas <- ga_seedrep;
    gal <- ga_sampleK;
    gac <- ga_chooseK;
    gmu <- ga_mutation;
    gcr <- ga_crossover;
    mva <- move_agents;
    mxa <- max_ages;
    rsi <- dim(starting_resources)[1];
    ttr <- dim(starting_resources)[2];
    agn <- dim(AGENTS)[1];
    agt <- dim(AGENTS)[2];
    lkr <- dim(interaction_tabl)[1];
    lyr <- dim(ACTION)[3];
    roc <- dim(ACTION)[1];
    coc <- dim(ACTION)[2];

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
               Euc,     # 20. Distance is Euclidean (1) or within-cell (0)
               gap,     # 21. Population size set in the genetic algorithm
               gam,     # 22. Minimum number of generations to run a ga
               gas,     # 23. Number of replicate agents to seed a ga with
               gal,     # 24. The number of sample agents for tournament in ga
               gac,     # 25. The number of selected agents in a ga tournament
               gmu,     # 26. The mutation rate of loci in the genetic algorithm
               gcr,     # 27. The crossover rate in the genetic algorithm
               mva,     # 28. Move agents once per time step
               mxa,     # 29. Maximum age of resources
               7,       # 30. The column of the time trait in the resource array
               11,      # 31. The column for storing age in the resource array
               rsi,     # 32. The number of resources in the model
               4,       # 33. The column for resource x location on landscape
               5,       # 34. The column for resource y location on landscape
               6,       # 35. The column for the movement parameter for resource
               ldz,     # 36. The number of layers (3D depth) in the landscape
               9,       # 37. The column in resource array for growth parameter
               10,      # 38. The column in the resource array for offspring
               16,      # 39. The column to adjust the growth rate resource col
               17,      # 40. The column to adjust the offspring resource col
               ttr,     # 41. Total columns in the resource array
               15,      # 42. The column to adjust the death resource column
               8,       # 43. The column in resource array affecting removal
               1,       # 44. A column to refer to a resource type as needed
               1,       # 45. A type of resource to do interacting with land
               15,      # 46. The column in a resource array affected by land
               14,      # 47. The column in resource array of land effect size
               1,       # 48. The landscape layer interacting with a resource
               4,       # 49. The column for the agent's x location on landscape
               5,       # 50. The column for the agent's y location on landscape
               6,       # 51. The column for the movement parameter for agents
               10,      # 52. The column in agent array where marks accumulate
               12,      # 53. The column in resource array where marks accrue
               agn,     # 54. The total number of agents in the model
               agt,     # 55. The total number of agent traits (cols)
               1,       # 56. The column of resource type 1
               2,       # 57. The column of resource type 2
               3,       # 58. The column of resource type 3
               13,      # 59. The tally column of the resource array
               lkr,     # 60. The number of rows in the lookup array
               rsi,     # 61. The number of rows in the observation array
               ttr+tmo, # 62. The number of cols int he observation array
               1,       # 63. The ID of the managing agent (usually 1)
               0,       # 64. The layer of ACTION and COST where manager located
               lyr,     # 65. The number of layers in ACTION and COST arrays
               lkr - 1, # 66. The number of rows for setting action costs
               8,       # 67. The view column of the agent array
               roc,     # 68. The number of rows in the COST and ACTION arrays
               coc,     # 69. The number of cols in the COST and ACTION arrays
               4,       # 70. Col actions vary for other individuals in ga
               7        # 71. Col actions vary for self individuals in ga
    );
    RESOURCE_REC    <- NULL;
    RESOURCES       <- starting_resources;
    OBSERVATION_REC <- NULL;
    AGENT_REC       <- NULL;
    LANDSCAPE_INI   <- LANDSCAPE_r;
    LANDSCAPE_REC   <- NULL;
    COST_REC        <- NULL;
    ACTION_REC      <- NULL;
    
    while(time < time_max){
        
        AGENTS[1,5] <- sample(x = 1:ldx, size = 1); # Change this later?
        AGENTS[1,6] <- sample(x = 1:ldy, size = 1); 
        
        RESOURCE_NEW      <- resource(resource   = RESOURCES,
                                      landscape  = LANDSCAPE_r,
                                      paras      = paras,
                                      move_res   = TRUE,
                                      model      = "IBM"
        ); 
        RESOURCES             <- RESOURCE_NEW[[1]];
        LANDSCAPE_r           <- RESOURCE_NEW[[2]];
        paras                 <- RESOURCE_NEW[[3]];
        
        OBSERVATION_NEW   <- observation(resource   = RESOURCES,
                                         landscape  = LANDSCAPE_r,
                                         paras      = paras,
                                         agent      = AGENTS,
                                         inter_tabl = interaction_tabl,
                                         fix_mark   = fxo,
                                         times      = tmo,
                                         samp_age   = rma,
                                         agent_type = 0,
                                         type_cat   = 1,
                                         obs_method = obt,
                                         move_res   = rmo
        );
        OBSERVATION_r  <- OBSERVATION_NEW[[1]];
        paras          <- OBSERVATION_NEW[[3]];
        
        # anecdotal is a bit useless right now, but included here anyway. 
        AGENTS_NEW        <- anecdotal(resource    = RESOURCES,
                                       landscape   = LANDSCAPE_r,
                                       paras       = paras,
                                       agent       = AGENTS,
                                       res_type    = 1,
                                       samp_age    = rma,
                                       agent_type  = -1,
                                       type_cat    = 1,
                                       move_agents = mva
        );
        AGENTS <- AGENTS_NEW[[1]];

        MANAGER  <- manager(resource    = RESOURCES,
                            agent       = AGENTS,
                            landscape   = LANDSCAPE_r, 
                            paras       = paras,
                            cost        = COST,
                            action      = ACTION,
                            Jacobian    = Jacobian,
                            inter_tabl  = interaction_tabl,
                            observation = OBSERVATION_r,
                            model       = "IBM"
        );
        ACTION <- MANAGER[[4]];
        COST   <- MANAGER[[5]];
        COST[COST < 1] <- 1;
        
        USERS <- user(resource   = RESOURCES,
                      agent      = AGENTS,
                      landscape  = LANDSCAPE_r, 
                      paras      = paras,
                      cost       = COST,
                      action     = ACTION,
                      Jacobian   = Jacobian,
                      inter_tabl = interaction_tabl,
                      model      = "IBM"
        );
        RESOURCES    <- USERS[[1]];
        AGENTS       <- USERS[[2]];
        LANDSCAPE_r  <- USERS[[3]];
        ACTION       <- USERS[[4]];
        COST         <- USERS[[5]];
        
        RESOURCE_REC[[time]]     <- RESOURCES;
        OBSERVATION_REC[[time]]  <- OBSERVATION_NEW[[1]];
        AGENT_REC[[time]]        <- AGENTS;
        LANDSCAPE_REC[[time]]    <- LANDSCAPE_r[,,2];
        COST_REC[[time]]         <- COST;
        ACTION_REC[[time]]       <- ACTION;
        
        LANDSCAPE_r <- age_land(landscape = LANDSCAPE_r, 
                                landscape_ini = LANDSCAPE_INI, layer = 2);
        
        time              <- time + 1;
        paras[1]          <- time;
        if(dim(RESOURCES)[1] < 10){
            print("Extinction has occurred");
            break;
        }

        if(hunt == TRUE & time > start_hunting){
            HUNT_OUTCOME <- be_hunter(OBSERVATION_r, AGENTS, RESOURCES, 
                                      LANDSCAPE_r, paras, agent_view,
                                      times_observe);
            RESOURCES    <- HUNT_OUTCOME$RESOURCES;
            paras        <- HUNT_OUTCOME$PARAS;
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
                     "Resource_tally",
                     "Consumption_rate",
                     "Adjust_removal",
                     "Adjust_growth",
                     "Adjust_grown"
    );
    colnames(RESOURCES)    <- res_columns;
    
    proc_end   <- proc.time();
    time_taken <- proc_end - proc_start;
    
    sim_results <- list(resource    = RESOURCE_REC,
                        observation = OBSERVATION_REC,
                        paras       = paras,
                        land        = LANDSCAPE_REC,
                        time_taken  = time_taken,
                        agents      = AGENTS,
                        cost        = COST_REC,
                        action      = ACTION_REC
                        );
    
    if(plotting == TRUE){
        if(obt == 0){
            case01plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       paras  = paras,
                       ACTION = ACTION_REC,
                       COST   = COST_REC,
                       view   = agent_view,
                       times  = times_observe);
        }
        if(obt == 1){
            case01plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1],
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       ACTION = ACTION_REC,
                       COST   = COST_REC,
                       paras  = paras);
        }
        if(obt == 2){
            case23plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,                       
                       paras  = paras);
        }
        if(obt == 3){
            case23plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,                       
                       paras  = paras);
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
    mcols  <- seq(from = 19, to = 19 + (marks-1), by = 1);
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
    ind_rep  <- max(landscape) + 1;
    
    for(i in 1:dim(inds)[1]){
        x <- as.numeric(inds[i,6]);
        y <- as.numeric(inds[i,5]);
        landscape[y,x] <- ind_rep;
    }
    
    return(landscape);
}

################################################################################
# Density estimator
################################################################################
dens_est <- function(observation = obs_t, view = view, land = land, times = 1){
    vision  <- (2*view) + 1;
    area    <- vision * vision;
    cells   <- dim(land)[1] * dim(land)[2];
    if(area > cells){
        area <- cells;   
    }
    area    <- area * times;
    endrow  <- 19 + times;
    tot_obs <- sum(observation[,17:endrow]);
    prp     <- tot_obs / area;
    est     <- prp * cells;
    lcp     <- prp - 1.96 * sqrt((1/(vision*vision))*prp*(1-prp));
    ucp     <- prp + 1.96 * sqrt((1/(vision*vision))*prp*(1-prp));
    lci     <- cells * lcp
    uci     <- cells * ucp;
    return(list(Nc=est, lci=lci, uci=uci, test = tot_obs));
}

###########################################################
## Plot this way when looking at transect type sampling
###########################################################
case23plot <- function(res, obs, land1, land2, land3, agents, paras){
    gens <- NULL;
    abun <- NULL;
    est  <- NULL;
    lci  <- NULL;
    uci  <- NULL; 
    lnds <- NULL;
    ages <- NULL;
    land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 

    minK <- min(paras[6:7]);
    
    ymaxi    <- 2 * minK;
    time_max <- length(res);
    for(i in 1:(time_max-1)){
        res_t    <- res[[i]];
        obs_t    <- obs[[i]];
        lnd_t    <- land2[[i]] * 100;
        age_t    <- agents[[i]];
        if(i > 1){
            res_t <- res_t[res_t[,12] >= paras[17],];
        }
        gens  <- c(gens, i);
        abun  <- c(abun, dim(res_t)[1]);
        lnds  <- c(lnds, mean(lnd_t));
        ages  <- rbind(ages, age_t[,16]);
        par(mfrow=c(2,2),mar=c(0,0,0,0));
        # ------------- Panel 1 (upper left)
        indis  <- ind_to_land(inds=res_t, landscape=land1);
        image(indis, col=land_cols, xaxt="n", yaxt="n");
        # ------------- Panel 2 (upper right)
        col_num <- max(land3);
        image(land3, col=topo.colors(col_num), xaxt="n", yaxt="n");    
        # ------------- Panel 3 (lower left)
        par(mar=c(4,4,1,4));
        plot(x=gens, y=abun, pch=20, type="l", lwd=2, ylim=c(0, ymaxi),
             xlim=c(0,time_max), xlab="Time Step", ylab="Abundance",
             cex.lab=1.25);
        new_est   <- sum(obs_t[,13]);
        est       <- c(est, new_est);
        points(x=gens, y=est, pch=20, type="l", lwd=2, col="cyan4");
        abline(h=paras[7], col="red", lwd=0.8, lty="dashed");
        points(x=gens, y=abun, pch=20, type="l", lwd=3, col="black");
        par(new=TRUE);
        plot(x=gens, y=lnds, pch=20, type="l", lwd=3, col="orange", xlab = "",
             xlim=c(0, time_max), ylim = c(0, 100), xaxt="n", yaxt="n", 
             ylab = "");
        axis(side=4, at=c(0, 25, 50, 75, 100));
        mtext("Mean % Yield", side = 4, line = 2.4);
        # ------------ Panel 4 (lower right);
        par(mar=c(4,6,1,1));
        cell_number <- dim(land3)[1] * dim(land3)[2]
        max_yield   <- floor( cell_number / (dim(age_t)[1]) )
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, max_yield),
             xlim=c(0,time_max), xlab="Time Step", ylab="Stake-holder yield",
             cex.lab=1.25);
        stake_colors <- topo.colors( dim(age_t)[1] );
        for(stakeholder in 1:dim(ages)[2]){
            points(x=gens, y=ages[,stakeholder], type="l", lwd=2, 
                   col = stake_colors[stakeholder]);
        }
        Sys.sleep(0.1);
    }
}


####################################################################
## Plot this way when looking at view or mark-recapture sampling
####################################################################
case01plot <- function(res, obs, land1, land2, land3, agents, paras, ACTION,
                       COST, view = NULL, times = 1){
    gens <- NULL;
    abun <- NULL;
    est  <- NULL;
    lci  <- NULL;
    uci  <- NULL;
    lnds <- NULL;
    ages <- NULL;
    land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 
    
    case  <- paras[9];
    tiobs <- paras[12];
    if(case == 1 & tiobs < 2){
        return("No RMR possible"); 
    }
    mrk <- floor(tiobs / 2);
    rcp <- tiobs - mrk;
    
    minK <- min(paras[6:7]);

    ymaxi    <- 2 * minK;
    time_max <- length(res);
    for(i in 1:(time_max-1)){
        res_t    <- res[[i]];
        obs_t    <- obs[[i]];
        lnd_t    <- land2[[i]] * 100;
        age_t    <- agents[[i]];
        if(i > 1){
            res_t <- res_t[res_t[,12] >= paras[17],];
        }
        gens  <- c(gens, i);
        abun  <- c(abun, dim(res_t)[1]);
        lnds  <- c(lnds, mean(lnd_t));
        ages  <- rbind(ages, age_t[,16]);
        par(mfrow=c(3,2),mar=c(0,0,0,0));
        # ------------- Panel 1 (upper left)
        if(abun[i] > 0){
            indis  <- ind_to_land(inds=res_t, landscape=land1);
            image(indis, col=land_cols, xaxt="n", yaxt="n");
        }else{
            image(land1, col=land_cols, xaxt="n", yaxt="n");
        }
        # ------------- Panel 2 (upper right)
        col_num <- max(land3);
        image(land3, col=topo.colors(col_num), xaxt="n", yaxt="n");
        # ------------- Panel 3 (middle left)
        par(mar=c(4,5,1,4));
        plot(x=gens, y=abun, pch=20, type="l", lwd=2, ylim=c(0, ymaxi),
             xlim=c(0,time_max), xlab="Time Step", ylab="Abundance",
             cex.lab=1.25);
        if(!is.null(obs_t) & case == 1){
            analysis <- chapman_est(observation=obs_t, marks=mrk, 
                                    recaptures=rcp);
            est      <- c(est, analysis$Nc);
            lci      <- c(lci, analysis$lci);
            uci      <- c(uci, analysis$uci);
        }
        if(!is.null(obs_t) & !is.null(view) & case == 0){
            analysis <- dens_est(observation=obs_t, view=view, land=land1, 
                                 times = times);
            est      <- c(est, analysis$Nc);
            lci      <- c(lci, analysis$lci);
            uci      <- c(uci, analysis$uci);
        }
        polygon(y=c(lci,rev(uci)),x=c(gens,rev(gens)), border=NA,
                col="lightblue");
        points(x=gens, y=est, pch=20, type="l", lwd=2, col="cyan4");
        abline(h=paras[7], col="red", lwd=0.8, lty="dashed");
        abline(h=ACTION[[1]][1,5,1], col=topo.colors(1), lwd=0.8, lty="dashed");
        points(x=gens, y=abun, pch=20, type="l", lwd=3, col="black");
        par(new=TRUE);
        plot(x=gens, y=lnds, pch=20, type="l", lwd=3, col="orange", xlab = "",
             xlim=c(0, time_max), ylim = c(0, 100), xaxt="n", yaxt="n", 
             ylab = "");
        axis(side=4, at=c(0, 25, 50, 75, 100));
        mtext("Mean % Yield", side = 4, line = 2.4);
        # ------------ Panel 4 (middle right);
        par(mar=c(4,6,1,1));
        cell_number <- dim(land3)[1] * dim(land3)[2];
        max_yield   <- cell_number; #floor( cell_number / (dim(age_t)[1]) )
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, max_yield),
             xlim=c(0,time_max), xlab="Time Step", ylab="Stake-holder yield",
             cex.lab=1.25);
        stake_colors <- topo.colors( dim(age_t)[1] );
        for(stakeholder in 1:dim(ages)[2]){
            points(x=gens, y=ages[,stakeholder], type="l", lwd=2, 
                   col = stake_colors[stakeholder]);
        }
        # ------------- Panel 5 (lower left)
        res_costs <- matrix(data = 0, nrow = i, ncol = 5);
        for(j in 1:i){
            res_costs[j,1] <- ACTION[[j]][3,8,1];
            res_costs[j,2] <- ACTION[[j]][3,9,1];
            res_costs[j,3] <- ACTION[[j]][3,10,1];
            res_costs[j,4] <- ACTION[[j]][3,11,1];
            res_costs[j,5] <- ACTION[[j]][3,12,1];
        }
        par(mar=c(4,5,1,4));
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, 200),
             xlim=c(0,time_max), xlab="Time Step", ylab="Cost of actions",
             cex.lab=1.25);
        points(x=gens, y=res_costs[,1], type="l", col="green", lwd=2);
        points(x=gens, y=res_costs[,2], type="l", col="indianred1", lwd=2);
        points(x=gens, y=res_costs[,3], type="l", col="indianred3", lwd=2);
        points(x=gens, y=res_costs[,4], type="l", col="deepskyblue1", lwd=2);
        points(x=gens, y=res_costs[,5], type="l", col="deepskyblue2", lwd=2);
        # ------------- Panel 5 (lower left)
        res_acts <- matrix(data = 0, nrow = i, ncol = 5);
        for(j in 1:i){
            for(k in 2:dim(ACTION[[j]])[3]){
                res_acts[j,1] <- res_acts[j,1] + ACTION[[j]][1,8,k];
                res_acts[j,2] <- res_acts[j,2] + ACTION[[j]][1,9,k];
                res_acts[j,3] <- res_acts[j,3] + ACTION[[j]][1,10,k];
                res_acts[j,4] <- res_acts[j,4] + ACTION[[j]][1,11,k];
                res_acts[j,5] <- res_acts[j,5] + ACTION[[j]][1,12,k];
            }
        }
        par(mar=c(4,6,1,1));
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, 300),
             xlim=c(0,time_max), xlab="Time Step", ylab="Actions made",
             cex.lab=1.25);
        points(x=gens, y=res_acts[,1], type="l", col="green", lwd=2);
        points(x=gens, y=res_acts[,2], type="l", col="indianred1", lwd=2);
        points(x=gens, y=res_acts[,3], type="l", col="indianred3", lwd=2);
        points(x=gens, y=res_acts[,4], type="l", col="deepskyblue1", lwd=2);
        points(x=gens, y=res_acts[,5], type="l", col="deepskyblue2", lwd=2);
        # -------------
        Sys.sleep(0.1);
    }
}

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

################################################################################

sim <- gmse( observe_type  = 0,
             agent_view    = 20,
             res_death_K   = 400,
             plotting      = TRUE,
             hunt          = FALSE,
             start_hunting = 95,
             lambda        = 0.3,
             fixed_observe = 10,
             times_observe = 20,
             land_dim_1    = 100,
             land_dim_2    = 100,
             res_consume   = 0.5,
             time_max      = 100,
             res_move_obs  = TRUE,
             max_ages      = 5,   
             ga_mingen     = 40,   
             ga_seedrep    = 20
);

################################################################################
