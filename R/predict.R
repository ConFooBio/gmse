###############################################################################
### Logistic growth model for Islay barnacle geese using the optim function ###
### March 2018 ################################################################
###############################################################################

# This script takes a standardised dataset and performs the following steps:
# 1) Applies a correction to recent records of area of improved grassland to make them consistent with earlier records
# 2) Scales the variables that are used as effects on goose number
# 3) Creates a hunting bag variable consisting of birds shot on greenland and iceland
# 4) Optimises a logistic population growth model that accounts for climate, land-use and management effects
# 5) Provides optimised parameter estimates for this model

#################################
##### Set working directory #####
#################################

#setwd("/Users/jeremycusack/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset")
setwd("/Users/jc130/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset")
#setwd("~/Dropbox/Islay_goose_data_from_Tom_Jan_2018/Dataset")





function(data, time_row, count_cols, harvest_cols, coeff_cols){
    harv_mat <- data[,harvest_cols];
    harvest  <- apply(X = harv_mat, MARGIN = 1, FUN = sum);
    for(i in coeff_cols){
        data[,i] <- scale(data[,i]);
    }
    
}

warn_coeffs <- function(coeffs, coeff_pars){
    if(length(coeff_pars) <= 2){
        coeff_pars <- NULL;
    }
    if( is.null(coeffs) == TRUE & is.null(coeff_pars) == FALSE ){
        stop("No data for coefficient parameterisation");
    }
    if( is.null(coeffs) == FALSE & is.null(coeff_pars) == TRUE ){
        stop("Missing coefficient parameters");
    }
    if( is.null(coeffs) == FALSE & is.null(coeff_pars) == FALSE ){
        if( is.matrix(coeffs) == FALSE ){
            stop("The argument 'coeffs' needs to be a matrix");
        }
        d_coeff <- dim(coeffs)[2];
        d_pars  <- length(coeff_pars);
        if(d_coeff != d_pars){
            stop("Need the same number of coefficients as in parameters");
        }
    }
}

warn_harvest <- function(N_val, harvest){
    if( is.null(harvest) == FALSE ){
        if( length(harvest) != length(N_val) ){
            stop("N_val and harvest vectors should be the same length");
        }
    }
}

fix_K_val <- function(K_val, N_val){
    if( length(K_val) == 1 ){
        K_val <- rep(x = K_val, times = length(N_val));
    }
    if( length(K_val) != length(N_val) ){
        stop("Different size N and K vectors");
    }
    return(K_val);
}

logistic_mod <- function(data, params){
    N_val      <- data[,1];
    warn_coeffs(coeffs = coeffs, coeff_pars = params);
    warn_harvest(N_val = N_val,  harvest = harvest);
    par_n      <- length(params);
    r_val      <- params[1];
    K_val      <- params[2];
    coeff_pars <- 0;
    if(par_n > 2){
        coeff_pars <- params[3:par_n];
    }
    K_val <- fix_K_val(K_val, N_val);
    time_steps   <- length(N_val);
    N_prediction <- rep(x = NA, times = time_steps);
    for(time in N_start:time_steps){
        log_growth <- r_val * N_val[time] * (1 - N_val[time] / K_val[time]);
        coeff_adj  <- 0;
        if( is.null(coeffs) == FALSE ){
            for(i in 1:dim(coeffs)[2]){
                coeff_adj <- coeff_adj + coeff_pars[i] * coeffs[time, i];
            }
        }
        harv_adj <- 0;
        if( is.null(harvest) == FALSE ){
            harv_adj <- harvest[time];
        }
        N_prediction[time] <- log_growth + N_val[time] + coeff_adj - harv_adj;
    }
    N_pred <- N_prediction[-1];
    N_tail <- N_val[-1];
    SqDev  <- (N_pred -  N_tail) * (N_pred -  N_tail) / N_pred;
    SSDev  <- sum(SqDev);
    Opt_r  <- (1 / SSDev) * 1000;
    return(Opt_r);
}


inits <- c(1, 100, 0);
data  <- data[,1:3];

optim(par = inits, fn = logistic_mod, data = data, method = "BFGS",
      control = list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
                     pgtol = 0),
      hessian = TRUE);



best.P<-optim(par=inits.P,fn=mod,data=data,method="BFGS",
              control=list(trace=1,fnscale=-1,maxit=1000,factr=1e-8,pgtol=0),
              hessian=T)






mod <- function(params,data){
    
    # Parameters
    rmax = params[1]              # Maximum growth rate
    K = params[2]                 # Carrying capacity
    a = params[3]                 # Effect of precipitation on Greenland in August
    b = params[4]                 # Effect of temperature on Greenland in August
    c = params[5]                 # Effect of temperature on Islay the previous winter
    d = params[6]                 # Effect of area of improved grassland two years prior
    
    # Model
    N.pred <- rep(NA,dim(data)[1])
    for (t in 3:dim(data)[1]){
        N.pred[t] <- rmax*data$y[t-1]*((1-data$y[t-1]/(K*data$AIG[t-1]))+a*data$AugRain[t-1]+b*data$AugTemp[t-1]+c*data$IslayTemp[t-1]+d*data$AIG.sc[t-2])+data$y[t-1]-data$HB[t-1]
    }
    
    SOS <- 1/sum((N.pred[3:29]-data$y[3:29])^2/N.pred[3:29])*1000                  # Optimise sum of squares
    return(SOS)                                                          
}



##################### We agreed to use the mean monthly count for a given winter to dampen the variability across months and 
##### Load data ##### ensure consistency with Tom's paper.However, because of this, we have to add the number of birds shot
##################### on Islay back into the mean count.

data <- read.csv("Standardised_dataset_IslayGBG.csv")                 # Load dataset
data$y <- data$Count+data$IslayCull                                   # Count data from 1987 to 2015 + individuals culled on Islay

####################################################################### AIG was measured in one way until 2009, and then in
##### Create continuous time series of area of improved grassland ##### another post 2009, This section tries to make the two
####################################################################### consistent with each other. It fits a logistic model to
# pre-2009 data, thenuses it to predict a value for 2010, which
# is then used to rescale data post 2009

AIGs <- data$AIG[1:22]                                                # Take the years before the change
tii <- 1:22                                                           # Corresponding time variable
DATg <- data.frame(AIGs,tii)                                          # Create dataframe with grass and time
coef(lm(logit(AIGs/8000)~tii,data=DATg))                              # Obtain starting values for nls
lmodg<-nls(AIGs~phi1/(1+exp(-(phi2+phi3*tii))),
           start=list(phi1=7000,phi2=1.22,phi3=0.09),
           data=DATg,trace=TRUE)                                      # Fit logistic function to grass time series pre 2009
newx <- data.frame(tii=23)                                            # Predict one year ahead
pg <- predict(lmodg,newdata=newx)                                     # Predict one year ahead
dif <- data$AIG[23:29][1]-pg                                          # Derive difference between the predicted value of AIG and the value for the first year afterthe change of method
data$AIG[23:29] <- data$AIG[23:29]-dif                                # Apply this difference to the remaining years after the change

################################
##### Scale used variables #####
################################

data$AugTemp<-as.numeric(scale(data$AugTemp))
data$IslayTemp<-as.numeric(scale(data$IslayTemp))
data$AugRain<-as.numeric(scale(data$AugRain))
data$AIG.sc<-as.numeric(scale(data$AIG))

#######################
##### Hunting bag #####
#######################

data$HB <- data$IcelandCull+data$GreenlandCull

##########################################
##### Optimise logistic growth model #####
##########################################

### MODEL - K dependent on grass + all climate effects and grass
### N[t+1] = rmax * N[t] * ((1-N[t]/K*grass[t-1])+clim.vars[t-1]+grass[t-2]) + N[t] - H[t]

mod <- function(params,data){
  
  # Parameters
  rmax = params[1]              # Maximum growth rate
  K = params[2]                 # Carrying capacity
  a = params[3]                 # Effect of precipitation on Greenland in August
  b = params[4]                 # Effect of temperature on Greenland in August
  c = params[5]                 # Effect of temperature on Islay the previous winter
  d = params[6]                 # Effect of area of improved grassland two years prior
  
  # Model
  N.pred <- rep(NA,dim(data)[1])
  for (t in 3:dim(data)[1]){
    N.pred[t] <- rmax*data$y[t-1]*((1-data$y[t-1]/(K*data$AIG[t-1]))+a*data$AugRain[t-1]+b*data$AugTemp[t-1]+c*data$IslayTemp[t-1]+d*data$AIG.sc[t-2])+data$y[t-1]-data$HB[t-1]
  }
  
  SOS <- 1/sum((N.pred[3:29]-data$y[3:29])^2/N.pred[3:29])*1000                  # Optimise sum of squares
  return(SOS)                                                          
}

### Solve
inits.P <- c(0.1,6,0,0,0,0)                                                      # Provide starting values for optimisation                             
best.P<-optim(par=inits.P,fn=mod,data=data,method="BFGS",
              control=list(trace=1,fnscale=-1,maxit=1000,factr=1e-8,pgtol=0),
              hessian=T)

### Plot predictions from optimised model
rmax=best.P$par[1]
K=best.P$par[2]
a=best.P$par[3]
b=best.P$par[4]
c=best.P$par[5]
d=best.P$par[6]
Npred <- rep(NA,dim(data)[1])
for (t in 3:dim(data)[1]){
  Npred[t] <- rmax*data$y[t-1]*((1-data$y[t-1]/(K*data$AIG[t-1]))+a*data$AugRain[t-1]+b*data$AugTemp[t-1]+c*data$IslayTemp[t-1]+d*data$AIG.sc[t-2])+data$y[t-1]-data$HB[t-1]
}
yrs <- 1987:2015
plot(data$y~yrs,pch=1,ylim=c(10000,60000),
     las=2,xlab="Year",ylab="Population size")                   # Observed time series
points(Npred~yrs,pch=19,col="red")                               # Predicted time series
points(data$y[2:28]~yrs[3:29],pch=19,col="blue")                 # The model performs better than just taking the previous year count






