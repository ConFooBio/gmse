#' Chapman estimator of mark-recapture
#'
#' Estimates population size using simulated mark-recapture data produced by the
#' observation model of GMSE.
#'
#'@param observation The array of resource observations from the observation model, used to estimate abundance of resources
#'@param paras The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@return The Chapman estimator (which is also performed GMSE in the manager function) returns a list that includes resource population size estimates along with 95% confidence intervals
#'@examples
#'\dontrun{
#'analysis <- chapman_est(observation=obs_t, paras = paras);
#'}
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
#' Places individuals (simulated resources) on the landscape for plotting.
#'
#'@param inds A single time step of resources from GMSE
#'@param land The landscape array on which interactions between resources and agents occur
#'@return Returns a landscape in which resources are embedded for a timestep for plotting purposes
#'@examples
#'\dontrun{
#'indis  <- ind_to_land(inds=res_t, land=land1);
#'}
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
#' whole landscape.
#'
#'@param observation The array of resource observations from the observation model, used to estimate abundance of resources
#'@param paras The vector of parameters that hold global and dynamic parameter values used by GMSE
#'@param view This parameter determines the distance around an agent's location within which it can observe resources. 
#'@param land The landscape array on which interactions between resources and agents occur
#'@return The density estimator (which is also performed GMSE in the manager function) returns a list that includes resource population size estimates along with 95% confidence intervals
#'@examples
#'\dontrun{
#'analysis <- dens_est(observation = obs_t, paras = paras, view = view,
#'land = land1);
#'}
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
    tot_obs <- sum(observation[,ob_strt:endrow]);
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
#' Produce six panels on a plot showing resource distribution, owned land, resource dynamics and estimates, stake-holder yield, and action costs and actions made. This plot is run internally within the gmse function, and should not be used to plot results stored after running the gmse function (for this, use plot_gmse_results).
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
#'@examples
#'\dontrun{
#'case23plot(res = RESOURCE_REC, obs = OBSERVATION_REC, 
#'land1 = LANDSCAPE_r[,,1], land2 = LANDSCAPE_REC, land3  = LANDSCAPE_r[,,3], 
#'agents = AGENT_REC, COST = COST_REC, ACTION = ACTION_REC, paras  = paras);
#'}
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
#' Produce six panels on a plot showing resource distribution, owned land, resource dynamics and estimates, stake-holder yield, and action costs and actions made. This plot is run internally within the gmse function, and should not be used to plot results stored after running the gmse function (for this, use plot_gmse_results).
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
#'@examples
#'\dontrun{
#'case01plot(res = RESOURCE_REC, obs = OBSERVATION_REC, 
#'land1 = LANDSCAPE_r[,,1], land2  = LANDSCAPE_REC, land3  = LANDSCAPE_r[,,3], 
#'agents = AGENT_REC, paras = paras, ACTION = ACTION_REC, COST = COST_REC, 
#'view = agent_view, times = times_observe);
#'}
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
            indis  <- ind_to_land(inds = res_t, land = land1);
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

####################################################################
## Plot this way when looking at view or mark-recapture sampling (quick version)
####################################################################
#' Plot the results of a gmse simulation
#'
#' Produce six panels on a plot showing resource distribution, owned land, resource dynamics and estimates, stake-holder yield, and action costs and actions made. 
#' 
#'@param res The resources array produced by the resource function within GMSE
#'@param obs The array of resource observations from the observation model, used to estimate abundance of resources
#'@param land The full list showing all layers of the landscape in each time step of GMSE
#'@param agents The array of agents produced in the main gmse() function
#'@param paras The array of parameters that hold global and dynamic parameter values used by GMSE
#'@param ACTION A three dimensional array of agent (manager and stakeholder) actions
#'@param COST A three dimensional array of cost values for agent (manager and stakeholder) actions
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon legend
#'@importFrom graphics text
#'@importFrom stats rnorm rpois
#'@return This function plots the dynamics of GMSE resource, observation, managemer, and user models in six separate sub-panels. (1) Upper left panel: Shows the locations of resources on the landscape (black dots); landscape terrain is also shown in brown, but at the moment, this is only cosmetic and does not reflect anything occurring in the model. (2) Upper right panel: Shows ownership of land by agents; land is divided proportional based on parameters set in gmse() and colours correspond with other subplots. If agent utilities and actions are restricted to land (`land_ownership` in the gmse() function), then this gives some idea of where actions are being performed and where resources are affecting the landscape. (3) Middle left panel: Shows the actual population abundance (black solid line) and the population abundance estimated by the manager (blue solid line; shading indicates 95 percent confidence intervals) over time. The dotted red line shows the resource carrying capacity (death-based) and the dotted blue line shows the target for resource abundance as set in the gmse() function; the orange line shows the total percent yield of the landscape (i.e., 100 percent means that resources have not decreased yield at all, 0 percent means that resources have completely destroyed all yield). (4) Middle right panel: Shows the raw landscape yield for each stakeholder (can be ignored if `land_ownership` is FALSE) over time; colours correspond to land ownership shown in the upper right panel. (5) Lower left panel: The cost of stakeholders performing actions over time, as set by the manager. (6) Lower right panel: The total number of actions performed by all stakeholders over time.
#'@examples
#'\dontrun{
#'plot_gmse_results(res = sim$resource, obs = sim$observation, land = sim$land, 
#'sim$agents, sim$paras, ACTION = sim$action, COST = sim$cost, 
#'observe_type = 0);
#'}
#'@export
plot_gmse_results <- function(res, obs, land, agents, paras, ACTION, COST){
    
    para_vec <- paras[1,]
    
    times <- 1;
    view  <- agents[[1]][1, 9];
    
    land1 <- NULL;
    land3 <- NULL;
    land2 <- land;
    
    for(i in 1:length(land2)){
        land1 <- land2[[i]][,,1];
        land3 <- land2[[i]][,,3];
    }
    
    max_time <- length(res) - 1;
        
    gens <- NULL;
    abun <- NULL;
    est  <- NULL;
    lci  <- NULL;
    uci  <- NULL;
    lnds <- NULL;
    ages <- NULL;
    stky <- NULL;
    land_cols <- c("#F2F2F2FF", "#ECB176FF", "#000000"); 
    cols      <- c("green", "indianred1", "indianred3", "deepskyblue1",
                   "deepskyblue2");
    
    case  <- para_vec[9];
    tiobs <- para_vec[12];
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
    
    ymaxi <- max(paras[,33]) + (0.1 * (max(paras[,33])));
    if(ymaxi < para_vec[7]){
        ymaxi <- para_vec[7] + (0.1 * para_vec[7]);
    }
    
    time_max <- length(res);
    for(i in 1:(time_max-1)){
        res_t    <- res[[i]];
        obs_t    <- obs[[i]];
        lnd_t    <- land2[[i]][,,2] * 100;
        age_t    <- agents[[i]];
        if(i > 1){
            res_t <- res_t[res_t[,12] >= para_vec[17],];
        }
        gens  <- c(gens, i);
        abun  <- c(abun, dim(res_t)[1]);
        lnds  <- c(lnds, mean(lnd_t));
        ages  <- rbind(ages, age_t[,16]);
        if(!is.null(obs_t) & case == 1){
            analysis <- chapman_est(observation = obs_t, paras = para_vec);
            est      <- c(est, analysis$Nc);
            lci      <- c(lci, analysis$lci);
            uci      <- c(uci, analysis$uci);
        }
        if(!is.null(obs_t) & !is.null(view) & case == 0){
            analysis <- dens_est(observation = obs_t, paras = para_vec, 
                                 view = view, land = land1);
            est      <- c(est, analysis$Nc);
            lci      <- c(lci, analysis$lci);
            uci      <- c(uci, analysis$uci);
        }
    }
    for(stakeholder in 1:dim(ages)[2]){
        max_yield   <- sum(land3 == stakeholder);
        agent_yield <- rep(x = NA, max_time);
        if(max_yield > 0 & para_vec[104] > 0){
            agent_yield <- 100 * (ages[,stakeholder] / max_yield);
        }
        if(para_vec[104] > 0){
            stky[[stakeholder]] <- agent_yield;
        }
    }
    
    if(case > 1){
        est <- paras[,100];
    }
    par(mfrow=c(3,2),mar=c(0,0,0,0));
    # ------------- Panel 1 (upper left)
    if(abun[max_time] > 0){
        indis  <- ind_to_land(inds = res_t, land = land1);
        image(indis, col = land_cols, xaxt="n", yaxt="n");
    }else{
        image(land1, col = land_cols, xaxt="n", yaxt="n");
    }
    # ------------- Panel 2 (upper right)
    col_num <- max(land3);
    image(land3, col = topo.colors(col_num), xaxt="n", yaxt="n");
    # ------------- Panel 3 (middle left)
    par(mar = c(4, 5, 1, 5));
    plot(x = gens, y = abun, pch = 20, type="l", lwd = 2, ylim = c(0, ymaxi),
         xlim=c(0, time_max), xlab = "Time Step", ylab = "Abundance",
         cex.lab=1.25);
    if(case == 0 | case == 1){
        polygon(y = c(lci,rev(uci)), x = c(gens, rev(gens)), border = NA,
                col="lightblue");
        points(x = gens, y = est, pch = 20, type = "l", lwd = 2, col = "cyan4");
    }else{
        points(x = gens, y = est[-time_max], pch = 20, type = "l", lwd = 2, 
               col = "cyan4");
    }
    abline(h = para_vec[7], col = "red", lwd = 0.8, lty = "dashed");
    abline(h = ACTION[[1]][1,5,1], col = topo.colors(1), lwd = 0.8, 
           lty = "dashed");
    points(x = gens, y = abun, pch = 20, type = "l", lwd = 3, col = "black");
    par(new = TRUE);
    plot(x = gens, y = lnds, pch = 20, type = "l", lwd = 3, col = "orange", 
         xlab = "", xlim = c(0, time_max), ylim = c(0, 100), xaxt="n", yaxt="n", 
         ylab = "");
    axis(side = 4, at = c(0, 25, 50, 75, 100));
    mtext("Mean % Yield", side = 4, line = 2.4);
    # ------------ Panel 4 (middle right);
    par(mar = c(4, 5, 1, 1));
    cell_number <- dim(land3)[1] * dim(land3)[2];
    plot(x = gens, y = gens, pch = 20, type = "n", lwd = 2, ylim = c(0, 100),
         xlim = c(0, time_max), xlab = "Time Step", 
         ylab = "Stake-holder % yield", cex.lab = 1.25);
    stake_colors <- topo.colors( dim(age_t)[1] );
    if(para_vec[104] > 0){
        for(stakeholder in 1:dim(ages)[2]){
            points(x = gens, y = stky[[stakeholder]], type="l", lwd=2, 
                   col = stake_colors[stakeholder]);
        }
    }
    # ------------- Panel 5 (lower left)
    res_costs <- matrix(data = 0, nrow = max_time, ncol = 5);
    for(j in 1:max_time){
        res_costs[j,1] <- ACTION[[j]][3,8,1];
        res_costs[j,2] <- ACTION[[j]][3,9,1];
        res_costs[j,3] <- ACTION[[j]][3,10,1];
        res_costs[j,4] <- ACTION[[j]][3,11,1];
        res_costs[j,5] <- ACTION[[j]][3,12,1];
    }
    par(mar = c(4, 5, 1, 5), xpd = TRUE);
    y_upper_limit <- max_cost + (0.25 * max_cost);
    plot(x = gens, y = gens, pch = 20, type = "n", lwd = 2, 
         ylim = c(0, y_upper_limit), xlim = c(0, time_max), xlab = "Time Step", 
         ylab = "Cost of actions", cex.lab = 1.25);
    if(para_vec[89] == 1){
        points(x = gens, y = res_costs[,1], type = "l", col = cols[1], lwd = 2);
    }
    if(para_vec[90] == 1){
        points(x = gens, y = res_costs[,2], type = "l", col = cols[2], lwd = 2);
    }
    if(para_vec[91] == 1){
        points(x = gens, y = res_costs[,3], type = "l", col = cols[3], lwd = 2);
    }
    if(para_vec[92] == 1){
        points(x = gens, y = res_costs[,4], type = "l", col = cols[4], lwd = 2);
    }
    if(para_vec[93] == 1){
        points(x = gens, y = res_costs[,5], type = "l", col = cols[5], lwd = 2);
    }
    legend(x = time_max + (time_max * 0.02), y = y_upper_limit, 
           fill = c(cols[1:5], "purple", "orange"), horiz = FALSE,
           legend = c("scaring", "culling", "castration", "feeding", 
                      "helping", "tend crop", "kill crop"), bty = "n");
    par(xpd = FALSE);
    # ------------- Panel 6 (lower right)
    res_acts <- matrix(data = 0, nrow = time_max, ncol = 7);
    gens     <- 1:time_max;
    for(j in 1:time_max){
        for(k in 2:dim(ACTION[[j]])[3]){
            res_acts[j,1] <- res_acts[j,1] + ACTION[[j]][1,8,k] - para_vec[96];
            res_acts[j,2] <- res_acts[j,2] + ACTION[[j]][1,9,k] - para_vec[96];
            res_acts[j,3] <- res_acts[j,3] + ACTION[[j]][1,10,k]- para_vec[96];
            res_acts[j,4] <- res_acts[j,4] + ACTION[[j]][1,11,k]- para_vec[96];
            res_acts[j,5] <- res_acts[j,5] + ACTION[[j]][1,12,k]- para_vec[96];
            res_acts[j,6] <- res_acts[j,6] + ACTION[[j]][2,10,k]- para_vec[96];
            res_acts[j,7] <- res_acts[j,7] + ACTION[[j]][2,11,k]- para_vec[96];
        }
    }
    par(mar=c(4,5,1,1));
    y_upper_limit <- max_action + (0.25 * max_action);
    plot(x = gens, y = gens, pch = 20, type = "n", lwd = 2, 
         ylim = c(0, y_upper_limit), xlim = c(0, time_max), xlab = "Time Step", 
         ylab = "Actions made", cex.lab = 1.25);
    if(para_vec[89] == 1){
        points(x = gens, y = res_acts[,1], type = "l", col = cols[1], lwd = 2);
    }
    if(para_vec[90] == 1){
        points(x = gens, y = res_acts[,2], type = "l", col = cols[2], lwd = 2);
    }
    if(para_vec[91] == 1){
        points(x = gens, y = res_acts[,3], type = "l", col = cols[3], lwd=2);
    }
    if(para_vec[92] == 1){
        points(x=gens, y=res_acts[,4], type="l", col=cols[4], lwd=2);
    }
    if(para_vec[93] == 1){
        points(x = gens, y = res_acts[,5], type = "l", col = cols[5], lwd = 2);
    }
    if(para_vec[94] == 1){
        points(x = gens, y = res_acts[,6], type = "l", lty = "solid", 
               col = "purple", lwd = 3);
    }
    if(para_vec[95] == 1){
        points(x = gens, y = res_acts[,7], type = "l", lty = "solid", 
               col = "orange", lwd = 3);
    }
}

####################################################################
## Plot the effort of each user
####################################################################
#' Plot the effort made by each user for each action
#'
#' Produce a five panel plot in which each panel compares the permissiveness of each action (scaring, culling, etc.) from the manager with the effort put into each action by individual users.
#' 
#'@param agents The array of agents produced in the main gmse() function
#'@param paras The array of parameters that hold global and dynamic parameter values used by GMSE
#'@param ACTION A three dimensional array of agent (manager and stakeholder) actions
#'@param COST A three dimensional array of cost values for agent (manager and stakeholder) actions
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon legend
#'@importFrom stats rnorm rpois
#'@return This function plots the permissiveness that each manager exhibits for each user action (scaring, culling, etc.) and the effort that each individual user puts into each action over time. On the left axis, permissiveness is calculated as 100 minus the percent of the manager's budget put into increasing the cost of a particular action, so, e.g., if a manager puts all of their effort into increasing the cost of culling, then permissiveness of culling is 0; if they put none of their effort into increasing the cost of culling, then permissiveness of culling is 100. On the right axis, percentage of user action expended is the percent of a user's budget put into a particular action (note, these might not add up to 100 because users are not forced to use their entire budget). Coloured lines that are above black lines could potentially (cautiously) be interpreted as conflict between managers and users.
#'@examples
#'\dontrun{
#'plot_gmse_effort(sim$agents, sim$paras, ACTION = sim$action, COST = sim$cost);
#'}
#'@export
plot_gmse_effort <- function(agents, paras, ACTION, COST){
    
    cols      <- c("green", "indianred1", "indianred3", "deepskyblue1",
                   "deepskyblue2");
    
    users    <- dim(agents[[1]])[1];
    max_time <- length(ACTION);
    
    para_vec <- paras[1,];
    
    allowed  <- sum(para_vec[89:93]);

    scar_cst  <- matrix(data = 0, nrow = max_time, ncol = users);
    cull_cst  <- matrix(data = 0, nrow = max_time, ncol = users);
    cast_cst  <- matrix(data = 0, nrow = max_time, ncol = users);
    feed_cst  <- matrix(data = 0, nrow = max_time, ncol = users);
    help_cst  <- matrix(data = 0, nrow = max_time, ncol = users);
    
    scar_act  <- matrix(data = 0, nrow = max_time, ncol = users);
    cull_act  <- matrix(data = 0, nrow = max_time, ncol = users);
    cast_act  <- matrix(data = 0, nrow = max_time, ncol = users);
    feed_act  <- matrix(data = 0, nrow = max_time, ncol = users);
    help_act  <- matrix(data = 0, nrow = max_time, ncol = users);

    scar_eff  <- matrix(data = 0, nrow = max_time, ncol = users);
    cull_eff  <- matrix(data = 0, nrow = max_time, ncol = users);
    cast_eff  <- matrix(data = 0, nrow = max_time, ncol = users);
    feed_eff  <- matrix(data = 0, nrow = max_time, ncol = users);
    help_eff  <- matrix(data = 0, nrow = max_time, ncol = users);    

    act_costs  <- matrix(data = 0, nrow = max_time, ncol = 5);
    pol_effort <- matrix(data = 0, nrow = max_time, ncol = 5);
    min_cost   <- para_vec[97];
    
    for(time in 1:max_time){
        #-- Scaring cost and actions
        scar_cst[time, 1] <- COST[[time]][3, 8, 1];
        for(user in 2:users){
            scar_cst[time, user] <- COST[[time]][1, 8, user];
        }
        scar_act[time, 1] <- ACTION[[time]][3, 8, 1] - min_cost;
        for(user in 2:users){
            scar_act[time, user] <- ACTION[[time]][1, 8, user];
        }
        scar_eff[time,] <- scar_act[time,]*scar_cst[time,]/agents[[time]][,17];
        #-- Culling cost and actions
        cull_cst[time, 1] <- COST[[time]][3, 9, 1];
        for(user in 2:users){
            cull_cst[time, user] <- COST[[time]][1, 9, user];
        }
        cull_act[time, 1] <- ACTION[[time]][3, 9, 1] - min_cost;
        for(user in 2:users){
            cull_act[time, user] <- ACTION[[time]][1, 9, user];
        }
        cull_eff[time,] <- cull_act[time,]*cull_cst[time,]/agents[[time]][,17];
        #-- Castration cost and actions
        cast_cst[time, 1] <- COST[[time]][3, 10, 1];
        for(user in 2:users){
            cast_cst[time, user] <- COST[[time]][1, 10, user];
        }
        cast_act[time, 1] <- ACTION[[time]][3, 10, 1] - min_cost;
        for(user in 2:users){
            cast_act[time, user] <- ACTION[[time]][1, 10, user];
        }
        cast_eff[time,] <- cast_act[time,]*cast_cst[time,]/agents[[time]][,17];
        #-- Feeding cost and actions
        feed_cst[time, 1] <- COST[[time]][3, 11, 1];
        for(user in 2:users){
            feed_cst[time, user] <- COST[[time]][1, 11, user];
        }
        feed_act[time, 1] <- ACTION[[time]][3, 11, 1] - min_cost;
        for(user in 2:users){
            feed_act[time, user] <- ACTION[[time]][1, 11, user];
        }
        feed_eff[time,] <- feed_act[time,]*feed_cst[time,]/agents[[time]][,17];
        #-- Helping cost and actions
        help_cst[time, 1] <- COST[[time]][3, 12, 1];
        for(user in 2:users){
            help_cst[time, user] <- COST[[time]][1, 12, user];
        }
        help_act[time, 1] <- ACTION[[time]][3, 12, 1] - min_cost;
        for(user in 2:users){
            help_act[time, user] <- ACTION[[time]][1, 12, user];
        }
        help_eff[time,] <- help_act[time,]*help_cst[time,]/agents[[time]][,17];
    }

    # -- Turn these into percentages:
    scar_eff <- scar_eff * 100;
    cull_eff <- cull_eff * 100;
    cast_eff <- cast_eff * 100;
    feed_eff <- feed_eff * 100;
    help_eff <- help_eff * 100;
    
    par(mfrow = c(5, 1), mar = c(0, 0, 0, 0), oma = c(6, 6, 4, 6));
    y1 <- 100;
    y2 <- 130;
    lcex  <- 1.5
    if(max_time < 40){
        lcex <- 1 + lcex * (max_time / 100);
    }
    #---- Scaring
    if(para_vec[89] == 1){
        plot(x = 1:max_time, y = 100 - scar_eff[,1], type = "l", lwd = 2, 
             cex.axis = 1.5, xaxt = "n", ylim = c(0, y2), yaxt = "n");
        axis(side = 2, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
        par(new = TRUE);
        plot(x = 1:max_time, y = scar_eff[,2], type = "n", lwd = 2, 
             ylim = c(0, y2), xaxt="n", yaxt="n", 
             cex.axis = 1.5);
        for(stakeholder in 2:users){
            points(x = 1:max_time, y = scar_eff[,stakeholder], type = "l", 
                   lwd = 1, col = cols[1]);
        }
        axis(side = 4, at = c(50, 100),  labels = c(50, 100), cex.axis = 1.5);
        legend(x = 1, y = 135, fill = cols[1:5], horiz = TRUE,
               legend = c("scaring", "culling", "castration", "feeding", 
                          "helping"), bty = "n", cex = lcex);
        abline(h = 105, lwd = 2);
    }else{
        plot(x = 1:max_time, y = scar_eff[,1], type = "n", xaxt= "n", 
             yaxt = "n", ylim = c(0, y2));
        text(x = 2, y = 90, cex = 2, labels = "No scaring allowed",
             pos = 4);
        abline(h = 105, lwd = 2);
        legend(x = 1, y = 135, fill = cols[1:5], horiz = TRUE,
               legend = c("scaring", "culling", "castration", "feeding", 
                          "helping"), bty = "n", cex = lcex);
    }
    #---- Culling
    if(para_vec[90] == 1){
        plot(x = 1:max_time, y = 100 - cull_eff[,1], type = "l", lwd = 2, 
             cex.axis = 1.5, xaxt = "n", ylim = c(0, y1), yaxt = "n");
        axis(side = 2, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
        par(new = TRUE);
        plot(x = 1:max_time, y = cull_eff[,2], type = "n", lwd = 2, 
             ylim = c(0, y1), xaxt="n", yaxt="n", cex.axis = 1.5);
        for(stakeholder in 2:users){
            points(x = 1:max_time, y = cull_eff[,stakeholder], type = "l", 
                   lwd = 1, col = cols[2]);
        }
        axis(side = 4, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
    }else{
        plot(x = 1:max_time, y = cull_act[,1], type = "n", xaxt= "n", 
             yaxt = "n", ylim = c(0, 100));
        text(x = 2, y = 90, cex = 2, labels = "No culling allowed",
             pos = 4);
    }
    #---- Castrating
    if(para_vec[91] == 1){
        plot(x = 1:max_time, y = 100 - cast_eff[,1], type = "l", lwd = 2, 
             cex.axis = 1.5, xaxt = "n", ylim = c(0, y1), yaxt = "n");
        axis(side = 2, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
        par(new = TRUE);
        plot(x = 1:max_time, y = cast_eff[,2], type = "n", lwd = 2, 
             ylim = c(0, y1), xaxt="n", yaxt="n", cex.axis = 1.5);
        for(stakeholder in 2:users){
            points(x = 1:max_time, y = cast_eff[,stakeholder], type = "l", 
                   lwd = 1, col = cols[3]);
        }
        axis(side = 4, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
    }else{
        plot(x = 1:max_time, y = cast_act[,1], type = "n", xaxt= "n", 
             yaxt = "n", ylim = c(0, 100));
        text(x = 2, y = 90, cex = 2, labels = "No castration allowed",
             pos = 4);
    }
    #---- Axes labels
    mtext("Manager's permissiveness of user action", 
          side = 2, line = 3.5, cex = 1.5, col = "black");
    mtext("Percentage of user action effort expended", 
          side = 4, line = 3.5, cex = 1.5, col = "black");
    #---- Feeding
    if(para_vec[92] == 1){
        plot(x = 1:max_time, y = 100 - feed_eff[,1], type = "l", lwd = 2, 
             cex.axis = 1.5, xaxt = "n", ylim = c(0, y1), yaxt = "n");
        axis(side = 2, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
        par(new = TRUE);
        plot(x = 1:max_time, y = feed_eff[,2], type = "n", lwd = 2, 
             ylim = c(0, y1), xaxt="n", yaxt="n", cex.axis = 1.5);
        for(stakeholder in 2:users){
            points(x = 1:max_time, y = feed_eff[,stakeholder], type = "l", 
                   lwd = 1, col = cols[4]);
        }
        axis(side = 4, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
    }else{
        plot(x = 1:max_time, y = feed_act[,1], type = "n", xaxt= "n", 
             yaxt = "n", ylim = c(0, 100));
        text(x = 2, y = 90, cex = 2, labels = "No feeding allowed",
             pos = 4);
    }
    #---- Helping
    if(para_vec[93] == 1){
        plot(x = 1:max_time, y = 100 - help_eff[,1], type = "l", lwd = 2, 
             cex.axis = 1.5, xaxt = "n", ylim = c(0, y1), yaxt = "n");
        axis(side = 2, at = c(50, y2), labels = c(50, 100), cex.axis = 1.5);
        par(new = TRUE);
        plot(x = 1:max_time, y = help_eff[,2], type = "n", lwd = 2, 
             ylim = c(0, y1), xaxt="n", yaxt="n", cex.axis = 1.5);
        for(stakeholder in 2:users){
            points(x = 1:max_time, y = help_eff[,stakeholder], type = "l", 
                   lwd = 1, col = cols[5]);
        }
        axis(side = 4, at = c(50, 100), labels = c(50, 100), cex.axis = 1.5);
    }else{
        plot(x = 1:max_time, y = help_act[,1], type = "n", cex.axis = 1.5,
             yaxt = "n", ylim = c(0, 100));
        text(x = 2, y = 90, cex = 2, labels = "No helping offspring allowed",
             pos = 4);
    }    
    mtext("Time step", side = 1, line = 3.5, cex = 1.5, col = "black");
}



