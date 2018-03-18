
logit <- function(p){
    size <- length(p);
    resv <- rep(x = NA, length = size)
    for(i in 1:size){
        if(p[i] >= 0 & p[i] <= 1){
            resv[i] <- log(p[i] / (1 - p[i]));   
        } 
    }
    return(resv);
}


goose_rescale_AIG <- function(data, years = 22){
  
    AIGs   <- data$AIG[1:years];         # Take the years before the change
    tii    <- 1:years;                   # Corresponding time variable
    DATg   <- data.frame(AIGs,tii);      # Create dataframe with grass and time
    get_cf <- coef(object = lm(logit(AIGs/8000)~tii, data = DATg)); # Vals
    cf_p2  <- as.numeric(get_cf[1]);
    cf_p3  <- as.numeric(get_cf[2]);
    lmodg  <- nls(formula = AIGs~phi1/(1+exp(-(phi2+phi3*tii))),
                  data    = DATg, trace = FALSE,
                  start   = list(phi1 = 7000, phi2 = cf_p2, phi3 = cf_p3));
    newx   <- data.frame(tii = years + 1);             # Predict one year ahead
    pg     <- predict(object = lmodg, newdata = newx); 
    dif    <- data$AIG[(years+1):dim(data)[1]][1]-pg;  
   
    data$AIG[(years+1):dim(data)[1]] <- data$AIG[(years+1):dim(data)[1]] - dif;
    return(data);
}

goose_clean_data <- function(file){
  
    data   <- read.csv(file);                # Load dataset
    data$y <- data$Count+data$IslayCull;     # Count data + culled
    data   <- goose_rescale_AIG(data = data, years = 22);
  
    data$AugTemp   <- as.numeric( scale(data$AugTemp) );
    data$IslayTemp <- as.numeric( scale(data$IslayTemp) );
    data$AugRain   <- as.numeric( scale(data$AugRain) );
    data$AIG.sc    <- as.numeric( scale(data$AIG) );
    data$HB        <- data$IcelandCull+data$GreenlandCull;
  
    return(data);
}  

goose_growth <- function(para, data){
  
  N_pred <- goose_pred(para = para, data = data);
  
  DEV    <- N_pred[3:data_rows] - data$y[3:data_rows];
  sq_Dev <- DEV * DEV;
  pr_sum <- sum( sq_Dev / N_pred[3:data_rows] );
  SS_tot <- (1 / pr_sum) * 1000;
  return(SS_tot);
}

goose_pred <- function(para, data){
  r_val        <- para[1]; # Maximum growth rate
  K_val        <- para[2]; # Carrying capacity
  G_rain_coeff <- para[3]; # Effect of precipitation on Greenland in August
  G_temp_coeff <- para[4]; # Effect of temperature on Greenland in August
  I_temp_coeff <- para[5]; # Effect of temperature on Islay the previous winter
  AIG_2_yrs    <- para[6]; # Effect of area of improved grassland 2 years prior
  hunting_bag  <- para[7]; # Effect of hunting bag
  
  data_rows <- dim(data)[1];
  N_pred    <- rep(x = NA, times = data_rows);
  for(time in 3:data_rows){
      goose_repr   <- r_val * data$y[time - 1];
      goose_dens   <- 1 - (data$y[time -1] / (K_val * data$AIG[time - 1]));
      goose_now    <- data$y[time - 1];
      G_rain_adj   <- G_rain_coeff * data$AugRain[time - 1];
      G_temp_adj   <- G_temp_coeff * data$AugTemp[time - 1];
      I_temp_adj   <- I_temp_coeff * data$IslayTemp[time - 1];
      AIG_2_adj    <- AIG_2_yrs    * data$AIG.sc[time - 2];
      adjusted     <- G_rain_adj + G_temp_adj + I_temp_adj + AIG_2_adj
      hunted       <- hunting_bag  * goose_now;
      N_pred[time] <- goose_repr * (goose_dens + adjusted) + goose_now - hunted;
  }
  
  return(N_pred);
}

get_goose_paras <- function(data, init_params = NULL){
    if( is.null(init_params) == TRUE ){
        init_params    <- c(0.1,6,0,0,0,0,0);
    }
    contr_paras    <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
                           pgtol = 0);
    get_parameters <- optim(par = init_params, fn = goose_growth, data = data,
                            method = "BFGS", control = contr_paras, 
                            hessian = TRUE);
    return(get_parameters);
}

goose_plot_pred <- function(data, year_start = 1987, ylim = c(10000, 60000),
                            plot = TRUE){
    params <- get_goose_paras(data = data);
    Npred  <- goose_pred(para = params$par, data = data);
    yrs    <- year_start:(year_start + length(data$y) - 1);
    if(plot == TRUE){
        plot(x =  yrs, y = data$y, pch = 1, ylim = ylim, cex.lab = 1.5,
             xlab="Year", ylab="Population size")         # Observed time series
        points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
        oend <- length(data$y);
        points(x = yrs[3:oend], y = data$y[2:(oend - 1)], pch = 19, 
               col = "blue");
    }
    return(Npred);
}

goose_predict_and_plot <- function(file, plot = TRUE){
    dat    <- read.csv("Standardised_dataset_IslayGBG.csv");
    data   <- goose_clean_data(file);
    goosep <- goose_plot_pred(data = data, plot = plot);
    return(goosep);
}

predicted <- goose_predict_and_plot(file = "Standardised_dataset_IslayGBG.csv");
