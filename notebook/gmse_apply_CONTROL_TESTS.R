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

plot_pop = function(dat, yrange = 10, track_range = TRUE) {
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
    plot(t, obs, ylim = c(0, max(obs, na.rm=T)*2), 
         type = "b", pch = 21, col = "black", bg = "grey", lwd = 2, xaxt = "n",
         xlab = "Time step", ylab = "Observed resource population size")
    axis(1, at = t)
    points(tail(t[!is.na(obs)],1),tail(obs[!is.na(obs)],1), 
           pch = 21, col = "black", bg = "red", lwd = 2, cex = 1.25)
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

LAND_OWNERSHIP = TRUE
TEND_CROPS = TRUE
SCARING = TRUE
CULLING = TRUE
OBSERVE_TYPE = 1
RES_MOVE_OBS = TRUE
RES_DEATH_K = 3000
LAMBDA = 0.3
MANAGE_TARGET = 1500
STAKEHOLDERS = 11
USER_BUDGET = 1500
MANAGER_BUDGET = 1000
RES_DEATH_TYPE = 3
REMOVE_PR = 0.10

### Initial time steps:
init_steps = init_man_control(K = 5)
# Appends output:
output = init_steps$summary 
# Extracts last time step (last `old_list`)
prev = init_steps$gmse_list[[length(init_steps$gmse_list)]]
# Print output:
init_steps$summary
init_steps$observed_suggested

par(mfrow = c(1,2))
plot_pop(output, track_range = FALSE)
plot_land_res(prev$LAND, prev$RESOURCES)

### User input
costs_as_input = list(culling = 10, scaring = 10)
prev = set_man_costs(prev, newcost = costs_as_input)

### Run next time step:
nxt = try({gmse_apply_UROM(get_res = "Full", old_list = prev)}, silent = TRUE)

if(class(nxt)!="try-error") {
    # Add appropriate outputs.
    output = append_UROM_output(dat = nxt, costs = costs_as_input, old_output = output)
    # Reset time step
    prev = nxt
    
    ### Output new results:
    observed_suggested(prev)
    output
    par(mfrow = c(1,2))
    plot_pop(output, track_range = FALSE)
    plot_land_res(prev$LAND, prev$RESOURCES)
} else {
    print("STOP - POPULATION WIPED OUT")
    output
}
    
