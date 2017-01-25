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

time_max       <- 100;   # Max number of time steps in sim
land_dim_1     <- 100;   # x dimension of the landscape
land_dim_2     <- 100;   # y dimension of the landscape
res_movement   <- 1;     # How far do resources move
remove_pr      <- 0.0;   # Density independent resource death
lambda         <- 0.9;   # Resource growth rate
agent_view     <- 10;    # Number cells agent view around them
agent_move     <- 50;    # Number cells agent can move
res_birth_K    <- 10000; # Carrying capacity applied to birth
res_death_K    <- 400;   # Carrying capacity applied to death
edge_effect    <- 1;     # What type of edge on the landscape
res_move_type  <- 2;     # What type of movement for resources
res_birth_type <- 2;     # What type of birth for resources
res_death_type <- 2;     # What type of death for resources
observe_type   <- 0;     # Type of observation used
fixed_observe  <- 1;     # How many obs (if type = 1)
times_observe  <- 1;     # How many times obs (if type = 0)
obs_move_type  <- 1;     # Type of movement for agents
res_min_age    <- 1;     # Minimum age recorded and observed
res_move_obs   <- TRUE;  # Move resources while observing
Euclidean_dist <- FALSE; # Use Euclidean distance in view
plotting       <- TRUE;  # Plot the results

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





RESOURCES <- split(RESOURCES, rep(1:ncol(RESOURCES), each = nrow(RESOURCES)))


RESOURCE_NEW      <- resource(resource   = RESOURCES,
                              landscape  = LANDSCAPE_r,
                              paras      = paras,
                              move_res   = TRUE,
                              model      = "IBM"
);





resource <- function(resource  = NULL, 
                     landscape = NULL, 
                     paras     = NULL,
                     move_res  = TRUE,
                     model     = "IBM"
) {
    check_model <- 0;
    if(model == "IBM"){
        paras[20] <- as.numeric(move_res);
        
        RESOURCE_OUT <- run_resource_a( RESOURCE_c   = resource,
                                        LANDSCAPE_c  = landscape,
                                        PARAMETERS_c = paras);
        check_model <- 1;
    }
    if(check_model == 0){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return(RESOURCE_OUT);
}

run_resource_a <- function(RESOURCE_c, LANDSCAPE_c, PARAMETERS_c){
    .Call("resource", RESOURCE_c, LANDSCAPE_c, PARAMETERS_c);
}




make_resource <- function(model              = "IBM", 
                          resource_quantity  = 100, 
                          resource_types     = 1, 
                          rows               = 100, 
                          cols               = 100, 
                          move               = 1, 
                          rm_pr              = 0,
                          lambda             = 0
){
    the_resource   <- NULL;
    if(model == "IBM"){
        IDs      <- seq(from = 1, to = resource_quantity, by = 1);
        type1    <- sample(x = 1:resource_types, size = resource_quantity,
                           replace = TRUE);
        type2    <- rep(x = 0, times = resource_quantity);
        type3    <- rep(x = 0, times = resource_quantity);
        xloc     <- sample(x = 1:rows, size = resource_quantity, 
                           replace = TRUE);
        yloc     <- sample(x = 1:cols, size = resource_quantity, 
                           replace = TRUE);
        mover    <- rep(x = move, times = resource_quantity);
        time     <- rep(x = 0, times = resource_quantity);
        remov_pr <- rep(x = rm_pr, times = resource_quantity);
        growth   <- rep(x = lambda, times = resource_quantity);
        offspr   <- rep(x = 0, times = resource_quantity); # None at init
        age      <- rep(x = 0, times = resource_quantity); # Start age zero
        mark     <- rep(x = 0, times = resource_quantity); # Can be marked
        tally    <- rep(x = 0, times = resource_quantity); 
        the_resource <- list(IDs, type1, type2, type3, xloc, yloc, mover, time, 
                              remov_pr, growth, offspr, age, mark, tally);
    }
    if( is.null(the_resource) ){
        stop("Invalid model selected (Must be 'IBM')");
    }
    return( the_resource );
}





act_agent <- function(times){
    while(times > 0){
        cat("\n\n\n How many geese do you shoot? \n\n");
        shot_char   <- readLines(con=stdin(),1);
        shot_num    <- as.numeric(shot_char);
        gross_prod  <- rpois(n=1, lambda=100);
        net_prod    <- gross_prod - (2 * shot_num);
        cat("\n");
        output      <- paste("Net production = ", net_prod);
        print(output);
        times       <- times - 1;
    }
}





################################################################################
################################################################################
################################################################################
# Testing list versus array efficiency

# ARRAY FIRST:
sam <- sample(x = 1:100, size = 14000, replace = TRUE);
dat <- matrix(data=sam, ncol=14);

obs <- NULL;

proc_start <- proc.time();

time <- 1000;
while(time > 0){
   obs   <- rbind(obs, dat);
   time  <- time - 1;
}

proc_end   <- proc.time();
time_taken <- proc_end - proc_start;
# TIME TAKEN: 14.09 seconds

# NOW LIST:
sam <- sample(x = 1:100, size = 14000, replace = TRUE);
dat <- matrix(data=sam, ncol=14);

obs <- list();

proc_start <- proc.time();

time <- 1000;
elem <- 1;
i    <- 1;
while(time > 0){
    obs[[i]] <- dat;
    i        <- i + 1;
    time     <- time - 1;
}

proc_end   <- proc.time();
time_taken <- proc_end - proc_start;
# TIME TAKEN: 0.005 seconds

################################################################################


sim <- gmse( observe_type  = 0,
             agent_view    = 20,
             res_death_K   = 400,
             res_birth_K   = 100000,
             plotting      = TRUE,
             hunt          = FALSE,
             start_hunting = 95,
             time_max      = 100,
             times_observe = 1
);










