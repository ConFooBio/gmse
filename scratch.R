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



















################################################################################
################################################################################
################################################################################
################################################################################

mbox <- function(x0, x1, y0, y1){
    xx <- seq(from=x0, to=x1, length.out = 100);
    yy <- seq(from=y0, to=y1, length.out = 100);
    xd <- c(rep(x0, 100), xx, rep(x1,100), rev(xx));
    yd <- c(yy, rep(y1,100), rev(yy), rep(y0, 100));
    return(list(x=xd, y=yd));
}

par(mar=c(0,0,0,0));
# ===============================================================
plot(x=0, y=0, type="n", xlim=c(0,200), ylim=c(0,100), xaxt="n", yaxt="n",
     xlab="",ylab="");
text(x=22, y=95, labels="Data Frame", col="red");
text(x=90, y=45, labels="Fitness function", col="blue");
text(x=165, y=95, labels="Genetic algorithm", col="goldenrod4");
abox <- mbox(x0 = 0, x1 = 45, y0 = 70, y1 = 90);
polygon(x=abox$x, y=abox$y, lwd=3, border="red", col="rosybrown1");
bbox <- mbox(x0 = 65, x1 = 120, y0 = 10, y1 = 40);
polygon(x=bbox$x, y=bbox$y, lwd=3, border="blue", col="lightsteelblue1");
cbox <- mbox(x0 = 140, x1 = 195, y0 = 0, y1 = 90);
polygon(x=cbox$x, y=cbox$y, lwd=3, border="goldenrod4", col="gold3");
dbox <- mbox(x0 = 142, x1 = 193, y0 = 75, y1 = 88);
polygon(x=dbox$x, y=dbox$y, lwd=3, border="goldenrod4", col="white");
ebox <- mbox(x0 = 142, x1 = 193, y0 = 63, y1 = 73);
polygon(x=ebox$x, y=ebox$y, lwd=3, border="goldenrod4", col="white");
fbox <- mbox(x0 = 142, x1 = 193, y0 = 51, y1 = 61);
polygon(x=fbox$x, y=fbox$y, lwd=3, border="goldenrod4", col="white");
gbox <- mbox(x0 = 142, x1 = 193, y0 = 39, y1 = 49);
polygon(x=gbox$x, y=gbox$y, lwd=3, border="goldenrod4", col="white");
hbox <- mbox(x0 = 142, x1 = 193, y0 = 27, y1 = 37);
polygon(x=hbox$x, y=hbox$y, lwd=3, border="goldenrod4", col="white");
ibox <- mbox(x0 = 142, x1 = 193, y0 = 15, y1 = 25);
polygon(x=ibox$x, y=ibox$y, lwd=3, border="goldenrod4", col="white");
jbox <- mbox(x0 = 142, x1 = 193, y0 = 3, y1 = 13);
polygon(x=jbox$x, y=jbox$y, lwd=3, border="goldenrod4", col="white");
box1 <-  mbox(x0 = 67, x1 = 118, y0 = 22, y1 = 38);
polygon(x=box1$x, y=box1$y, lwd=3, border="blue", col="white");
box2 <-  mbox(x0 = 67, x1 = 118, y0 = 12, y1 = 20);
polygon(x=box2$x, y=box2$y, lwd=3, border="blue", col="white");
text(x=22, y=80, labels="UTILITY", col="black");
text(x=92, y=32, labels="Adj RES", col="black");
text(x=92, y=27, labels="values", col="black");
text(x=92, y=16, labels="E[t+1] RES", col="black");
text(x=167, y=84, labels="add row 10X", col="black");
text(x=167, y=79, labels="to 90 rand", col="black");
text(x=167, y=69, labels="crossover", col="black");
text(x=167, y=57, labels="mutation", col="black");
text(x=167, y=44, labels="check fitness", col="black");
text(x=167, y=32, labels="tournament", col="black");
text(x=167, y=20, labels="replace", col="black");
text(x=167, y=8, labels="converge?", col="black");
arrows(x0=46, x1=138, y0=85, y1=85, lwd=2, length=0.15);
arrows(x0=139, x1=122, y0=44, y1=36, lwd=2, length=0.15);
arrows(x0=139, x1=14.75, y0=5, y1=5, lwd=2, length=0.0);
arrows(x0=15, x1=15, y0=5, y1=70, lwd=2, length=0.15);
arrows(x0=55, x1=55, y0=5, y1=67, lwd=2, length=0.0);
arrows(x0=55, x1=138, y0=67, y1=67, lwd=2, length=0.15);
text(x=10, y=35, labels="Yes", col="black", srt=90);
text(x=50, y=35, labels="No", col="black", srt=90);
arrows(x0=121, x1=128.75, y0=16, y1=16, lwd=2, length=0);
arrows(x0=128, x1=128, y0=16, y1=31, lwd=2, length=0);
arrows(x0=128.75, x1=138, y0=31, y1=31, lwd=2, length=0.15);

################################################################################
################################################################################
################################################################################
################################################################################




















par(mar=c(0,0,0,0),mfrow=c(2,1));
plot(x=0, y=0, type="n", xlim=c(0,100), ylim=c(0,100), xaxt="n", yaxt="n",
     xlab="",ylab="");
# ------- G-MSE logo below:
text(x=0,y=97,pos=4,family="mono",labels="  ____       __  __ ____  _____ ");
text(x=0,y=90,pos=4,family="mono",labels=" / ___|     |  \\/  / ___|| ____|");
text(x=0,y=83,pos=4,family="mono",labels="| |  _ ____ | |\\/| \\___ \\|  _|  ");
text(x=0,y=76,pos=4,family="mono",labels="| |_| |____|| |  | |___) | |___ ");
text(x=0,y=69,pos=4,family="mono",labels=" \\____|     |_|  |_|____/|_____|");
lines(x=1:100,y=rep(99,100),lwd=3,col="red");
lines(x=1:100,y=rep(64,100),lwd=3,col="blue");
lines(x=rep(0,36),y=64:99,lwd=3,col="yellow");
lines(x=rep(100,36),y=99:64,lwd=3,col="orange");
polygon(x=c(rep(14,16),14:40,rep(40,16),40:14), # box
        y=c(40:55,rep(40,27),55:40,rep(55,27)),
        lwd=3,border="black");
text(x=27,y=50,labels="Long-term data", cex=0.65);
text(x=27,y=45,labels="input as CSV file", cex=0.65);
polygon(x=c(rep(45,16),45:90,rep(90,16),90:45), # box
        y=c(40:55,rep(40,46),55:40,rep(55,46)),
        lwd=3,border="black");
text(x=65,y=50,labels="Set key parameters using", cex=0.65);
text(x=65,y=45,labels="any of the below", cex=0.65);
arrows(x0=65, x1=1, y0=40, y1=31, lwd=2, length=0.00);
arrows(x0=65, x1=100, y0=40, y1=31, lwd=2, length=0.00);

polygon(x=c(rep(1,20),1:15,rep(15,20),15:1), # box
        y=c(11:30,rep(30,15),30:11,rep(11,15)),
        lwd=3,border="black");
text(x=8,y=25,labels="Browser", cex=0.65);
text(x=8,y=20,labels="Interface", cex=0.65);

polygon(x=c(rep(21,20),21:35,rep(35,20),35:21), # box
        y=c(11:30,rep(30,15),30:11,rep(11,15)),
        lwd=3,border="black");
text(x=28,y=25,labels="R", cex=0.65);
text(x=28,y=20,labels="Interface", cex=0.65);

polygon(x=c(rep(41,20),41:100,rep(100,20),100:41), # box
        y=c(11:30,rep(30,60),30:11,rep(11,60)),
        lwd=3,border="black");
text(x=68,y=25,labels="Separate calls to high-level R files", cex=0.65);
text(x=68,y=20,labels="R files call from C independently", cex=0.65);

arrows(x0=15, x1=20, y0=20, y1=20, lwd=2, length=0.08);
arrows(x0=35, x1=41, y0=20, y1=20, lwd=2, length=0.08);
arrows(x0=68, x1=1, y0=11, y1=2, lwd=2, length=0.00);
arrows(x0=65, x1=100, y0=11, y1=2, lwd=2, length=0.00);

text(x=56, y=0, cex=0.60,
     labels="(Use long-term data in Natural Resource Model before MSE)"
);

# ===============================================================
plot(x=0, y=0, type="n", xlim=c(0,100), ylim=c(0,100), xaxt="n", yaxt="n",
     xlab="",ylab="");
# Manager model box
polygon(x=c(rep(1,30),1:30,rep(30,30),30:1), # box
        y=c(70:99,rep(99,30),99:70,rep(70,30)),
        lwd=3,border="red");
# User model box
polygon(x=c(rep(70,30),70:99,rep(99,30),99:70), # box
        y=c(70:99,rep(99,30),99:70,rep(70,30)),
        lwd=3,border="orange");
# Obeservation model box
polygon(x=c(rep(1,30),1:30,rep(30,30),30:1), # box
        y=c(1:30,rep(30,30),30:1,rep(1,30)),
        lwd=3,border="yellow");
# Natural resources model
polygon(x=c(rep(70,30),70:99,rep(99,30),99:70), # box
        y=c(1:30,rep(30,30),30:1,rep(1,30)),
        lwd=3,border="blue");
# Natural resources model
polygon(x=c(rep(36,30),36:65,rep(65,30),65:36), # box
        y=c(36:65,rep(65,30),65:36,rep(36,30)),
        lwd=3,border="green");
arrows(x0=15, x1=35, y0=30, y1=45, lwd=2, length=0.15);
arrows(x0=30, x1=45, y0=85, y1=67, lwd=2, length=0.15);
arrows(x0=85, x1=85, y0=70, y1=30, lwd=2, length=0.15);
arrows(x0=70, x1=30, y0=15, y1=15, lwd=2, length=0.15);
arrows(x0=36, x1=23, y0=50, y1=69, lwd=2, length=0.15);
arrows(x0=52, x1=69, y0=66, y1=80, lwd=2, length=0.15);
text(x=38, y=80, labels="Policy", srt=-38);
text(x=50, y=20, labels="Monitoring");
text(x=22, y=40, labels="Indicators", srt=28);
text(x=90, y=52, labels="Off-take", srt=-90);
# Manager model details: 
text(x=15, y=95, cex=0.75,labels="Manager model");
text(x=15, y=90, cex=0.75,labels="file: manager.c");
text(x=15, y=85, cex=0.75,labels="In: Obs. Data");
text(x=15, y=80, cex=0.75,labels="Out: Game para.");
# Bayesion priors details: 
text(x=50, y=60, labels="Genetic algorithm", cex=0.8, col="darkgreen");
text(x=50, y=50, cex=0.75,labels="Update manager");
text(x=50, y=45, cex=0.75,labels="or user strategy");
text(x=50, y=40, cex=0.75,labels="file: game.c");
# Manager model details: 
text(x=85, y=95, cex=0.75,labels="User model");
text(x=85, y=90, cex=0.75,labels="file: user.c");
# Natural resources model details: 
text(x=85, y=25, cex=0.75,labels="Natural");
text(x=85, y=20, cex=0.75,labels="resources model");
text(x=85, y=15, cex=0.75,labels="file: resources.c");
# Natural resources model details: 
text(x=15, y=25, cex=0.75,labels="Observ. model");
text(x=15, y=20, cex=0.75,labels="file: observe.c");



sim <- gmse( observe_type  = 0,
             agent_view    = 20,
             res_death_K   = 400,
             plotting      = TRUE,
             hunt          = FALSE,
             land_dim_1    = 40
);















