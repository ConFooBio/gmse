rm(list=ls())
source("notebook/gmse_apply_CONTROL.R")
source("R/gmse_apply.R")
rm(gmse_apply)

J = 3

init_man_control()

### Default gmse_apply() runs:
OUT_DEFAULT = list()

for(j in 1:J){
    print(j)
    sim_old = gmse_apply(get_res = "Full", 
                         land_ownership = TRUE, 
                         observe_type = 3, 
                         res_move_obs = FALSE, 
                         res_death_K = 1500, 
                         lambda = 0.275,
                         manage_target = 1000, 
                         stakeholders = 4)
    output = gmse_apply_summary(sim_old, include = (c("res","obs","culls","scares")))
    
    for(i in 2:20) {
        sim_new = gmse_apply(get_res = "Full", old_list = sim_old)
        output = gmse_apply_summary(sim_new, output)
        sim_old = sim_new
    }
    
    OUT_DEFAULT[[j]] = output[,1]
}

###The same number of iterations of but using the init_man_control() function, for comparison. #### NOTE - CHECK THE
###DEFAULT PARAMETERS in init_man_control() ARE THE SAME AS USED FOR THE DEFAULT gmse_apply() LOOP ABOVE!

OUT_INIT = list()
for(j in 1:J) {
    print(j)
    out = init_man_control(K = 20)
    OUT_INIT[[j]] = out$output[,1]
}

### PLOT THE COMPARISON BETWEEN gmse_apply() AND init_man_control() RUNS:
hi1 = max(unlist(lapply(OUT_DEFAULT, max)))
hi2 = max(unlist(lapply(OUT_INIT, max)))
yhi = ceiling(max(hi1, hi2)*1.05)
lo1 = min(unlist(lapply(OUT_DEFAULT, min)))
lo2 = min(unlist(lapply(OUT_INIT, min)))
ylo = floor(min(lo1, lo2)*0.95)

plot(OUT_DEFAULT[[1]], type = "n", ylim = c(ylo,yhi))
lapply(OUT_DEFAULT, function(x) lines(x))
lapply(OUT_INIT, function(x) lines(x, col = "red"))


