#' Chapman estimator of mark-recapture
#'
#' Estimates population size using simulated mark-recapture data produced by the
#' observation model of GMSE
#'
#'@param observation The array of resource observations from the observation model, used to estimate abundance of resources
#'@param paras The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@return The Chapman estimator (which is also performed GMSE in the manager function) returns a list that includes resource population size estimates along with 95% confidence intervals
#'@export
chapman_est <- function(observation, paras){
    marks       <- 1;
    recaptures  <- paras[12] - 1;
    mcols       <- paras[42] + 2;
    rcols       <- (paras[42] + 3):dim(observation)[2];
    mrked       <- observation[,mcols];
    if(recaptures > 1){
        recpt <- apply(X=observation[,rcols], MARGIN = 1, FUN = sum);
        recpt <- recpt > 0;
    }else{
        recpt <- observation[,rcols];   
    }
    n       <- sum(mrked);
    K       <- sum(recpt);
    recapt  <- mrked + recpt;
    k       <- sum(recapt == 2);
    Nc      <- ((n + 1) * (K + 1) / (k + 1)) - 1;
    Nc      <- floor(Nc);
    
    a      <- ((n+1)*(K+1)*(n-k)*(K-k));
    b      <- ((k+1)*(k+1)*(k+2));
    varNc  <- a/b;
    lci    <- Nc - (1.965 * sqrt(varNc));
    uci    <- Nc + (1.965 * sqrt(varNc));

    return(list(Nc=Nc,lci=lci,uci=uci));
}

#' Plot resource position on a landscape image output
#'
#' Actually put the individuals on the landscape with function below
#'
#'@param inds A single time step of resources from GMSE
#'@param land The landscape array on which interactions between resources and agents occur
#'@return Returns a landscape in which resources are embedded for a timestep for plotting purposes
#'@export
ind_to_land <- function(inds, land){
    ind_rep  <- max(land) + 1;
    
    for(i in 1:dim(inds)[1]){
        x <- as.numeric(inds[i,6]);
        y <- as.numeric(inds[i,5]);
        land[y,x] <- ind_rep;
    }
    
    return(land);
}

#' Density estimator of resource abundance
#'
#' Estimates population size using simulated data produced by the
#' observation model of GMSE -- it assumes that the density of resources
#' observed on the subset of the landscape sampled equals the density on the 
#' whole landscape
#'
#'@param observation The array of resource observations from the observation model, used to estimate abundance of resources
#'@param paras The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param view This parameter determines the distance around an agent's location within which it can observe resources. 
#'@param land The landscape array on which interactions between resources and agents occur
#'@return The density estimator (which is also performed GMSE in the manager function) returns a list that includes resource population size estimates along with 95% confidence intervals
#'@export
dens_est <- function(observation, paras, view = view, land = land){
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
    lcp     <- prp - 1.96 * sqrt(prp / (vision * vision));
    ucp     <- prp + 1.96 * sqrt(prp / (vision * vision));
    lci     <- cells * lcp
    uci     <- cells * ucp;
    # sum_vec <- apply(X = observation[,21:endrow], MARGIN = 2, FUN = sum);
    # pr_vec  <- sum_vec / (area / paras[12]);
    # mn_pr   <- mean(pr_vec) * cells;
    # CI_pr   <- 1.96 * sd(pr_vec) / sqrt( length(pr_vec) );

    return(list(Nc = est, lci = lci, uci = uci, test = tot_obs));
} 

#' Plot results for transect-based sampling
#'
#' Produce six panels on a plot showing resource distribution, owned land, resource dynamics and estimates, stake-holder yield, and action costs and actions made.
#' 
#'@param res The resources array produced by the resource function within GMSE
#'@param obs The array of resource observations from the observation model, used to estimate abundance of resources
#'@param land1 The first layer of the 3D landscape array, which indicates values of terrain for plotting (as of now, terrain values have no effect on the simulation and only exist for display purposes)
#'@param land2 The full list showing all layers of the landscape in each time step of GMSE
#'@param land3 The third layer of the 3D landscape array, which indicates agent ownership of the land
#'@param agents The array of agents produced in the main gmse() function
#'@param paras The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param COST A three dimensional array of cost values for agent (manager and stakeholder) actions
#'@param ACTION A three dimensional array of agent (manager and stakeholder) actions
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon legend
#'@importFrom stats rnorm rpois
#'@return This function plots the dynamics of GMSE resource, observation, managemer, and user models in six separate sub-panels. (1) Upper left panel: Shows the locations of resources on the landscape (black dots); landscape terrain is also shown in brown, but at the moment, this is only cosmetic and does not reflect anything occurring in the model. (2) Upper right panel: Shows ownership of land by agents; land is divided proportional based on parameters set in gmse() and colours correspond with other subplots. If agent utilities and actions are restricted to land (`land_ownership` in the gmse() function), then this gives some idea of where actions are being performed and where resources are affecting the landscape. (3) Middle left panel: Shows the actual population abundance (black solid line) and the population abundance estimated by the manager (blue solid line) over time. The dotted red line shows the resource carrying capacity (death-based) and the dotted blue line shows the target for resource abundance as set in the gmse() function; the orange line shows the total percent yield of the landscape (i.e., 100 percent means that resources have not decreased yield at all, 0 percent means that resources have completely destroyed all yield). (4) Middle right panel: Shows the raw landscape yield for each stakeholder (can be ignored if `land_ownership` is FALSE) over time; colours correspond to land ownership shown in the upper right panel. (5) Lower left panel: The cost of stakeholders performing actions over time, as set by the manager. (6) Lower right panel: The total number of actions performed by all stakeholders over time.
#'@export
case23plot <- function(res, obs, land1, land2, land3, agents, paras, COST,
                       ACTION){
    gens <- NULL;
    abun <- NULL;
    est  <- NULL;
    lci  <- NULL;
    uci  <- NULL; 
    lnds <- NULL;
    ages <- NULL;
    land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 
    cols      <- c("green", "indianred1", "indianred3", "deepskyblue1",
                   "deepskyblue2");
    
    max_action <- 0;
    max_cost   <- 0;
    for(i in 1:length(res)){
        act_check <- ACTION[[i]][,8:12,];
        act_check[act_check > 10000] <- -1;
        act_comb <- apply(X = act_check, MARGIN = c(1,2), FUN = sum);
        gen_max_action <- max(act_comb);
        if(gen_max_action > max_action){
            max_action <- gen_max_action;
        }
        cost_check <- COST[[i]][,8:12,];
        cost_check[cost_check >= 10000] <- -1;
        gen_max_cost   <- max(cost_check);
        if(gen_max_cost > max_cost){
            max_cost <- gen_max_cost;
        }
    }
    
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
        indis  <- ind_to_land(inds=res_t, land=land1);
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
        abline(h=ACTION[[1]][1,5,1], col=topo.colors(1), lwd=0.8, lty="dashed");
        points(x=gens, y=abun, pch=20, type="l", lwd=3, col="black");
        par(new=TRUE);
        plot(x=gens, y=lnds, pch=20, type="l", lwd=3, col="orange", xlab = "",
             xlim=c(0, time_max), ylim = c(0, 100), xaxt="n", yaxt="n", 
             ylab = "");
        axis(side=4, at=c(0, 25, 50, 75, 100));
        mtext("Mean % Yield", side = 4, line = 2.4);
        # ------------ Panel 4 (middle right);
        par(mar=c(4,5,1,1));
        cell_number <- dim(land3)[1] * dim(land3)[2];
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, 100),
             xlim=c(0,time_max), xlab="Time Step", ylab="Stake-holder % yield",
             cex.lab=1.25);
        stake_colors <- topo.colors( dim(age_t)[1] );
        for(stakeholder in 1:dim(ages)[2]){
            max_yield   <- sum(land3 == stakeholder);
            if(max_yield > 0 & paras[104] > 0){
                agent_yield <- 100 * (ages[,stakeholder] / max_yield);
                points(x = gens, y = agent_yield, type="l", lwd=2, 
                       col = stake_colors[stakeholder]);
            }
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
        par(mar=c(4,5,1,5), xpd = TRUE);
        y_upper_limit <- max_cost + (0.25 * max_cost);
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, y_upper_limit),
             xlim=c(0,time_max), xlab="Time Step", ylab="Cost of actions",
             cex.lab=1.25);
        if(paras[89] == 1){
            points(x=gens, y=res_costs[,1], type="l", col=cols[1], lwd=2);
        }
        if(paras[90] == 1){
            points(x=gens, y=res_costs[,2], type="l", col=cols[2], lwd=2);
        }
        if(paras[91] == 1){
            points(x=gens, y=res_costs[,3], type="l", col=cols[3], lwd=2);
        }
        if(paras[92] == 1){
            points(x=gens, y=res_costs[,4], type="l", col=cols[4], lwd=2);
        }
        if(paras[93] == 1){
            points(x=gens, y=res_costs[,5], type="l", col=cols[5], lwd=2);
        }
        legend(x = time_max + (time_max * 0.02), y = y_upper_limit, 
               fill = c(cols[1:5], "purple", "orange"), horiz = FALSE,
               legend = c("scaring", "culling", "castration", "feeding", 
                          "helping", "tend crop", "kill crop"), bty = "n");
        par(xpd = FALSE);
        # ------------- Panel 6 (lower right)
        res_acts <- matrix(data = 0, nrow = i, ncol = 7);
        for(j in 1:i){
            for(k in 2:dim(ACTION[[j]])[3]){
                res_acts[j,1] <- res_acts[j,1] + ACTION[[j]][1,8,k] - paras[96];
                res_acts[j,2] <- res_acts[j,2] + ACTION[[j]][1,9,k] - paras[96];
                res_acts[j,3] <- res_acts[j,3] + ACTION[[j]][1,10,k]- paras[96];
                res_acts[j,4] <- res_acts[j,4] + ACTION[[j]][1,11,k]- paras[96];
                res_acts[j,5] <- res_acts[j,5] + ACTION[[j]][1,12,k]- paras[96];
                res_acts[j,6] <- res_acts[j,6] + ACTION[[j]][2,10,k]- paras[96];
                res_acts[j,7] <- res_acts[j,7] + ACTION[[j]][2,11,k]- paras[96];
            }
        }
        par(mar=c(4,5,1,1));
        y_upper_limit <- max_action + (0.25 * max_action);
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, y_upper_limit),
             xlim=c(0,time_max), xlab="Time Step", ylab="Actions made",
             cex.lab=1.25);
        if(paras[89] == 1){
            points(x=gens, y=res_acts[,1], type="l", col=cols[1], lwd=2);
        }
        if(paras[90] == 1){
            points(x=gens, y=res_acts[,2], type="l", col=cols[2], lwd=2);
        }
        if(paras[91] == 1){
            points(x=gens, y=res_acts[,3], type="l", col=cols[3], lwd=2);
        }
        if(paras[92] == 1){
            points(x=gens, y=res_acts[,4], type="l", col=cols[4], lwd=2);
        }
        if(paras[93] == 1){
            points(x=gens, y=res_acts[,5], type="l", col=cols[5], lwd=2);
        }
        if(paras[94] == 1){
            points(x=gens, y=res_acts[,6], type="l", lty= "solid", col="purple", 
                   lwd=3);
        }
        if(paras[95] == 1){
            points(x=gens, y=res_acts[,7], type="l", lty= "solid", col="orange", 
                   lwd=3);
        }
        # -------------
        Sys.sleep(0.1);
    }
}


####################################################################
## Plot this way when looking at view or mark-recapture sampling
####################################################################
#' Plot results for density-based or mark-recapture sampling
#'
#' Produce six panels on a plot showing resource distribution, owned land, resource dynamics and estimates, stake-holder yield, and action costs and actions made.
#' 
#'@param res The resources array produced by the resource function within GMSE
#'@param obs The array of resource observations from the observation model, used to estimate abundance of resources
#'@param land1 The first layer of the 3D landscape array, which indicates values of terrain for plotting (as of now, terrain values have no effect on the simulation and only exist for display purposes)
#'@param land2 The full list showing all layers of the landscape in each time step of GMSE
#'@param land3 The third layer of the 3D landscape array, which indicates agent ownership of the land
#'@param agents The array of agents produced in the main gmse() function
#'@param paras The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param ACTION A three dimensional array of agent (manager and stakeholder) actions
#'@param COST A three dimensional array of cost values for agent (manager and stakeholder) actions
#'@param view The distance that an agent can see on a landscape
#'@param times The number of times that resources are sampled per time step
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon legend
#'@importFrom stats rnorm rpois
#'@return This function plots the dynamics of GMSE resource, observation, managemer, and user models in six separate sub-panels. (1) Upper left panel: Shows the locations of resources on the landscape (black dots); landscape terrain is also shown in brown, but at the moment, this is only cosmetic and does not reflect anything occurring in the model. (2) Upper right panel: Shows ownership of land by agents; land is divided proportional based on parameters set in gmse() and colours correspond with other subplots. If agent utilities and actions are restricted to land (`land_ownership` in the gmse() function), then this gives some idea of where actions are being performed and where resources are affecting the landscape. (3) Middle left panel: Shows the actual population abundance (black solid line) and the population abundance estimated by the manager (blue solid line; shading indicates 95 percent confidence intervals) over time. The dotted red line shows the resource carrying capacity (death-based) and the dotted blue line shows the target for resource abundance as set in the gmse() function; the orange line shows the total percent yield of the landscape (i.e., 100 percent means that resources have not decreased yield at all, 0 percent means that resources have completely destroyed all yield). (4) Middle right panel: Shows the raw landscape yield for each stakeholder (can be ignored if `land_ownership` is FALSE) over time; colours correspond to land ownership shown in the upper right panel. (5) Lower left panel: The cost of stakeholders performing actions over time, as set by the manager. (6) Lower right panel: The total number of actions performed by all stakeholders over time.
#'@export
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
    cols      <- c("green", "indianred1", "indianred3", "deepskyblue1",
                   "deepskyblue2");
   
    case  <- paras[9];
    tiobs <- paras[12];
    if(case == 1 & tiobs < 2){
        return("No RMR possible"); 
    }
    mrk <- floor(tiobs / 2);
    rcp <- tiobs - mrk;
    
    max_action <- 0;
    max_cost   <- 0;
    for(i in 1:length(res)){
        act_check <- ACTION[[i]][,8:12,];
        act_check[act_check > 10000] <- -1;
        act_comb <- apply(X = act_check, MARGIN = c(1,2), FUN = sum);
        gen_max_action <- max(act_comb);
        if(gen_max_action > max_action){
            max_action <- gen_max_action;
        }
        cost_check <- COST[[i]][,8:12,];
        cost_check[cost_check >= 10000] <- -1;
        gen_max_cost   <- max(cost_check);
        if(gen_max_cost > max_cost){
            max_cost <- gen_max_cost;
        }
    }
    
    minK <- min(paras[6:7]);
    
    ymaxi    <- minK + (minK * (1 + res[[1]][1,10])); # Add for birth rate
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
            indis  <- ind_to_land(inds=res_t, land=land1);
            image(indis, col=land_cols, xaxt="n", yaxt="n");
        }else{
            image(land1, col=land_cols, xaxt="n", yaxt="n");
        }
        # ------------- Panel 2 (upper right)
        col_num <- max(land3);
        image(land3, col=topo.colors(col_num), xaxt="n", yaxt="n");
        # ------------- Panel 3 (middle left)
        par(mar=c(4,5,1,5));
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
        par(mar=c(4,5,1,1));
        cell_number <- dim(land3)[1] * dim(land3)[2];
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, 100),
             xlim=c(0,time_max), xlab="Time Step", ylab="Stake-holder % yield",
             cex.lab=1.25);
        stake_colors <- topo.colors( dim(age_t)[1] );
        for(stakeholder in 1:dim(ages)[2]){
            max_yield   <- sum(land3 == stakeholder);
            if(max_yield > 0 & paras[104] > 0){
                agent_yield <- 100 * (ages[,stakeholder] / max_yield);
                points(x = gens, y = agent_yield, type="l", lwd=2, 
                       col = stake_colors[stakeholder]);
            }
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
        par(mar=c(4,5,1,5), xpd = TRUE);
        y_upper_limit <- max_cost + (0.25 * max_cost);
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, y_upper_limit),
             xlim=c(0,time_max), xlab="Time Step", ylab="Cost of actions",
             cex.lab=1.25);
        if(paras[89] == 1){
            points(x=gens, y=res_costs[,1], type="l", col=cols[1], lwd=2);
        }
        if(paras[90] == 1){
            points(x=gens, y=res_costs[,2], type="l", col=cols[2], lwd=2);
        }
        if(paras[91] == 1){
            points(x=gens, y=res_costs[,3], type="l", col=cols[3], lwd=2);
        }
        if(paras[92] == 1){
            points(x=gens, y=res_costs[,4], type="l", col=cols[4], lwd=2);
        }
        if(paras[93] == 1){
            points(x=gens, y=res_costs[,5], type="l", col=cols[5], lwd=2);
        }
        legend(x = time_max + (time_max * 0.02), y = y_upper_limit, 
               fill = c(cols[1:5], "purple", "orange"), horiz = FALSE,
               legend = c("scaring", "culling", "castration", "feeding", 
               "helping", "tend crop", "kill crop"), bty = "n");
        par(xpd = FALSE);
        # ------------- Panel 6 (lower right)
        res_acts <- matrix(data = 0, nrow = i, ncol = 7);
        for(j in 1:i){
            for(k in 2:dim(ACTION[[j]])[3]){
                res_acts[j,1] <- res_acts[j,1] + ACTION[[j]][1,8,k] - paras[96];
                res_acts[j,2] <- res_acts[j,2] + ACTION[[j]][1,9,k] - paras[96];
                res_acts[j,3] <- res_acts[j,3] + ACTION[[j]][1,10,k]- paras[96];
                res_acts[j,4] <- res_acts[j,4] + ACTION[[j]][1,11,k]- paras[96];
                res_acts[j,5] <- res_acts[j,5] + ACTION[[j]][1,12,k]- paras[96];
                res_acts[j,6] <- res_acts[j,6] + ACTION[[j]][2,10,k]- paras[96];
                res_acts[j,7] <- res_acts[j,7] + ACTION[[j]][2,11,k]- paras[96];
            }
        }
        par(mar=c(4,5,1,1));
        y_upper_limit <- max_action + (0.25 * max_action);
        plot(x=gens, y=gens, pch=20, type="n", lwd=2, ylim=c(0, y_upper_limit),
             xlim=c(0,time_max), xlab="Time Step", ylab="Actions made",
             cex.lab=1.25);
        if(paras[89] == 1){
            points(x=gens, y=res_acts[,1], type="l", col=cols[1], lwd=2);
        }
        if(paras[90] == 1){
            points(x=gens, y=res_acts[,2], type="l", col=cols[2], lwd=2);
        }
        if(paras[91] == 1){
            points(x=gens, y=res_acts[,3], type="l", col=cols[3], lwd=2);
        }
        if(paras[92] == 1){
            points(x=gens, y=res_acts[,4], type="l", col=cols[4], lwd=2);
        }
        if(paras[93] == 1){
            points(x=gens, y=res_acts[,5], type="l", col=cols[5], lwd=2);
        }
        if(paras[94] == 1){
            points(x=gens, y=res_acts[,6], type="l", lty= "solid", col="purple", 
                   lwd=3);
        }
        if(paras[95] == 1){
            points(x=gens, y=res_acts[,7], type="l", lty= "solid", col="orange", 
                   lwd=3);
        }
        # -------------
        Sys.sleep(0.1);
    }
}