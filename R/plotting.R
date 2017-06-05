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
chapman_est <- function(observation, paras){
    marks       <- paras[12];
    recaptures  <- paras[11];
    obs_start   <- paras[42] + 1;
    marks_end   <- obs_start + paras[11];
    mcols       <- obs_start:marks_end;
    rcols       <- (marks_end + 1):dim(observation)[2];
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
dens_est <- function(observation = obs_t, paras, view = view, land = land){
    vision  <- (2*view) + 1;
    area    <- vision * vision;
    cells   <- dim(land)[1] * dim(land)[2];
    if(area > cells){
        area <- cells;   
    }
    area    <- area * paras[12];
    ob_strt <- paras[42] + 1;
    endrow  <- dim(observation)[2];
    tot_obs <- sum(observation[,21:endrow]);
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
            analysis <- chapman_est(observation=obs_t, paras = paras);
            est      <- c(est, analysis$Nc);
            lci      <- c(lci, analysis$lci);
            uci      <- c(uci, analysis$uci);
        }
        if(!is.null(obs_t) & !is.null(view) & case == 0){
            analysis <- dens_est(observation=obs_t, paras=paras, view=view, 
                                 land=land1);
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
        # ------------- Panel 6 (lower right)
        res_acts <- matrix(data = 0, nrow = i, ncol = 7);
        for(j in 1:i){
            for(k in 2:dim(ACTION[[j]])[3]){
                res_acts[j,1] <- res_acts[j,1] + ACTION[[j]][1,8,k];
                res_acts[j,2] <- res_acts[j,2] + ACTION[[j]][1,9,k];
                res_acts[j,3] <- res_acts[j,3] + ACTION[[j]][1,10,k];
                res_acts[j,4] <- res_acts[j,4] + ACTION[[j]][1,11,k];
                res_acts[j,5] <- res_acts[j,5] + ACTION[[j]][1,12,k];
                res_acts[j,6] <- res_acts[j,6] + ACTION[[j]][2,10,k];
                res_acts[j,7] <- res_acts[j,7] + ACTION[[j]][2,11,k];
            }
        }
        par(mar=c(4,6,1,1));
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, paras[98]),
             xlim=c(0,time_max), xlab="Time Step", ylab="Actions made",
             cex.lab=1.25);
        points(x=gens, y=res_acts[,1], type="l", col="green", lwd=2);
        points(x=gens, y=res_acts[,2], type="l", col="indianred1", lwd=2);
        points(x=gens, y=res_acts[,3], type="l", col="indianred3", lwd=2);
        points(x=gens, y=res_acts[,4], type="l", col="deepskyblue1", lwd=2);
        points(x=gens, y=res_acts[,5], type="l", col="deepskyblue2", lwd=2);
        points(x=gens, y=res_acts[,6], type="l", lty= "dotted", col="purple", 
               lwd=3);
        points(x=gens, y=res_acts[,7], type="l", lty= "dotted", col="orange", 
               lwd=3);
        # -------------
        Sys.sleep(0.1);
    }
}