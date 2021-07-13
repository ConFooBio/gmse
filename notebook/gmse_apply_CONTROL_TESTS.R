rm(list=ls())

call_bogus_for_debug = function(res_mod  = resource, 
                                obs_mod  = observation, 
                                man_mod  = manager, 
                                use_mod  = user,
                                get_res  = "basic",
                                old_list = NULL,
                                ...) {
    return(sys.call())
}


list.to.df = function(l) {
    return(data.frame(matrix(unlist(l), nrow=length(l), byrow=T)))
}

### Takes a list of GMSE apply objects and extracts the mean or total yields across all users
get_init_yields = function(x, type = "mean") {
    if(type == "mean") {
        mn_ylds = lapply(x, function(x) tapply(x$LAND[,,2], x$LAND[,,3],mean))
        mn_ylds = list.to.df(mn_ylds)
        return(mn_ylds)
    }
}



plot_pop = function(dat, yield_dat = NULL, yrange = 10, track_range = TRUE, extinction_message = FALSE) {
    obs = dat[,"obs"]
    t = 1:nrow(dat)
    if(length(obs)<yrange) {
        obs = c(obs, rep(NA, yrange-length(obs)))
        t = c(t, (tail(t,1)+1):yrange)
    }
    if(length(obs)>yrange & track_range == TRUE) {
        obs = tail(obs, yrange)
        t = tail(t, yrange)
    }
    
    par(mar = c(5,5,2,5))
    
    plot(t, obs, ylim = c(0, max(obs, na.rm=T)*2), 
         type = "b", pch = 21, col = "black", bg = "grey", lwd = 2, xaxt = "n",
         xlab = "Time step", ylab = "Observed population size", cex.axis = 1.5, cex.lab = 2)
    axis(1, at = t, cex.lab = 1.5)
    points(tail(t[!is.na(obs)],1),tail(obs[!is.na(obs)],1), 
           pch = 21, col = "black", bg = "red", lwd = 2, cex = 2)
    
    if(!is.null(yield_dat)) {
        par(new = T)
        yield_ylim = c(0, round(max(yield_dat)*1.25,2))
        #yield_pos = seq(yield_ylim[1],yield_ylim[2],(yield_ylim[2]-yield_ylim[1])/6)
        #yield_labs = yield_pos*100
        plot(yield_dat[,1], type = "n", col = "darkgreen", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = yield_ylim)
        apply(yield_dat, 2, function(x) lines(x, col = "darkgreen"))
        #axis(4, at = yield_pos, labels = yield_labs, col.ticks = "darkgreen")
        axis(4, col.ticks = "darkgreen")
        mtext("Yield %",side = 4,cex = 2, line = 3, col = "darkgreen")
        abline(h=1, col = "darkgreen", lty = "dashed")
    }
    
    # The below needs amending for when yield data is added.
    if(extinction_message == TRUE) {
    if(!is.null(yield_dat)) {
      ext_mess_pos = max(yield_dat)*1.1
    } else {
      ext_mess_pos = max(obs, na.rm=T)*1.8
    }

    text(x = length(obs)/2, 
         y = ext_mess_pos,
         "Population wiped out!", 
         col = "#D35E60", cex = 2.5)
  }
  
}

plot_land = function(x) {
    # Pick colors
    land_cols = grey.colors(length(table(x)))
    land_cols = sample(land_cols, replace = FALSE)

    if(sum(x == 1)>0) land_cols[1] = "#FFFFFF"
    
    image(x = x, col = land_cols, yaxt = "n", xaxt = "n")  
}

placeResources = function(res, xd, yd) {
    land_res = matrix(0, nrow = xd, ncol = yd)
    for(i in 1:nrow(res)) {
        land_res[res[i,5]+1,res[i,6]+1] = land_res[res[i,5]+1,res[i,6]+1]+1
    }
    land_res[land_res==0] = NA
    
    return(land_res)
}

plot_land_res = function(land, resources) {
    plot_land(land[,,3])
    par(new = T)
    res_positions = placeResources(res = prev$RESOURCES, xd = dim(prev$LAND[,,3])[1], yd = dim(prev$LAND[,,3])[1])
    image(res_positions, col = "darkred", xaxt = "n", yaxt = "n")
}

GMSE_PARAS = list(LAND_OWNERSHIP = TRUE,
                  TEND_CROPS = TRUE,
                  SCARING = TRUE,
                  CULLING = TRUE,
                  TEND_CROP_YLD = runif(1, 0.1, 0.75),
                  OBSERVE_TYPE = 0,
                  RES_MOVE_OBS = TRUE,
                  RES_DEATH_K = round(runif(1, 1000, 6000)),
                  RES_DEATH_TYPE = 3,
                  LAMBDA = runif(1, 0.2, 0.4),
                  REMOVE_PR = runif(1, 0, 0.1),
                  MANAGE_TARGET = 2000,
                  STAKEHOLDERS = round(runif(1, 4, 12)),
                  USER_BUDGET = 1500,
                  MANAGER_BUDGET = 1000,
                  LAND_DIM_1 = 100,
                  LAND_DIM_2 = 100,
                  RESOURCE_INI = 1000,
                  OWNERSHIP_VAR = 0.5,
                  PUBLIC_LAND = 0,
                  USR_BUDGET_RNG = 0
)
 
### Initial time steps:
init_steps = init_man_control(K = 5, gmse_paras = GMSE_PARAS)
# No. stakeholderS:
lapply(init_steps$gmse_list, function(x) x$stakeholders) # Test para extraction 
# Land ownership check:
table(init_steps$gmse_list[[1]]$LAND[,,3])
lapply(init_steps$gmse_list, function(x) table(x$LAND[,,3]))

# Appends output:
output = init_steps$summary 
# Extracts last time step (last `old_list`)
prev = init_steps$gmse_list[[length(init_steps$gmse_list)]]
# Print output:
init_steps$summary
init_steps$observed_suggested
init_steps$prev_yield
yields = init_steps$prev_yield
#get_init_yields(init_steps$gmse_list)


#par(mfrow = c(1,2))
plot_pop(output, track_range = FALSE, yield_dat = yields)
#plot_land_res(prev$LAND, prev$RESOURCES)

budget_as_input = list(culling = 50, scaring = 50)
# ### Attempt at translating from BUDGET allocated to costs set:
# budget_as_input = list(culling = 50, scaring = 50)
# 
# budget2cost = function(budgetAllocated, minimum_cost) {
#   return(budgetAllocated/10 + minimum_cost)
# }
# 
# ### User input
# costs_as_input = list(culling = budgetAllocated_to_cost(budget_as_input$culling, minimum_cost = 10), 
#                       scaring = budgetAllocated_to_cost(budget_as_input$culling, minimum_cost = 10))

prev = set_man_costs(prev, newcost = budget_as_input)

### Run next time step:
nxt = try({gmse_apply_UROM(get_res = "Full", old_list = prev)}, silent = TRUE)

if(class(nxt)!="try-error") {
    # Add appropriate outputs.
    output = append_UROM_output(dat = nxt, costs = budget_as_input, old_output = output)
    yields = rbind(yields, tapply(nxt$LAND[,,2], nxt$LAND[,,3], mean))
    
    # Reset time step
    nxt$LAND[,,2] = 1   # Reset landscape yield
    prev = nxt
    
    ### Output new results:
    observed_suggested(prev)
    output
    #par(mfrow = c(1,2))
    plot_pop(output, track_range = FALSE, yield_dat = yields)
    #plot_land_res(prev$LAND, prev$RESOURCES)
} else {
    print("STOP - POPULATION WIPED OUT")
    output
}
    
