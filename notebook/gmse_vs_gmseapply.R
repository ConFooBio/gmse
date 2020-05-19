### The idea here is to run a number of replicates (reps) of gmse and gmse_apply runs, each with the same parameters.

### The purpose is twofold - (1) test gmse() functionality vs gmse_apply() to ensure both are returning the same 
###  results, given the same parameters; and (2) test parameter "pass-through" - that is, are parameter values that are 
### passed to either gmse() or gmse_apply() as variables (as opposed to hard-coded values) correctly collected and
###  processed?

### In each replication, the population size for each time step is extracted and added to an output matrix,
###  separately for gmse() and gmse_apply(). This should produce effectively the same population trajectories,
###  or, given enough replication, at least trajectories that are not significantly different from each other.

### Run and tested using GMSE rev branch on 19/05/2020

rm(list=ls())
reps = 10
nyears = 10

ld1 = 100
ld2 = 100
res_move = 20
rem_pr = 0
l = 0.3
a_view = 10
a_move = 50
rbK = 100000
rdK = 2000
e_effect = 1
r_move_type = 1
r_birth_type = 2
r_death_type = 2
obs_type = 0
f_mark = 100 
f_recapt = 500
t_observe = 1 
o_move_type = 1
r_min_age = 0
r_move_obs = FALSE
e_dist = FALSE
plotme = FALSE
huntme = FALSE
start_hunt = 95
r_consume = 0.5
ga_pops = 100
ga_ming = 40
ga_seedr = 20
ga_sampK = 20
ga_chseK = 2
ga_mut = 0.1
ga_cross = 0.1
m_agents = TRUE
m_ages = 5
min_cost = 10
u_budget = 1000
m_budget = 1000
m_target = 1000
RES_ini = 1000
scare = FALSE
cull = TRUE 
castr = FALSE
feed = FALSE
help_offs = FALSE
t_crops = FALSE
t_crop_yld = 0.2
k_crops = FALSE 
s = 4
m_caution = 1
l_ownership = FALSE
m_freq = 1
con_crit = 0.1
m_sense = 0.9
p_land = 0
g_think = FALSE
age_rep = 1
u_bud_r = 0
act_thres = 0
bud_bonus = 0
cons_surv = 0
cons_repr = 0
t_feeding = 1
o_var = 0
p_scare = NA
p_cull = NA
p_cast = NA
p_feed = NA
p_help = NA
p_tend = NA
p_kill  = NA

### Using gmse()

out_gmse = matrix(NA, nrow = reps, ncol = nyears)
out_gmse_ALL = list()
for(z in 1:reps) {
  
  gmse_z = gmse(time_max = nyears, land_dim_1 = ld1, land_dim_2 = ld2, res_movement = res_move, remove_pr = rem_pr, 
                lambda = l, agent_view = a_view, agent_move = a_move, res_birth_K = rbK, res_death_K = rdK, 
                edge_effect = e_effect, res_move_type = r_move_type, res_birth_type = r_birth_type, 
                res_death_type = r_death_type, observe_type = obs_type, fixed_mark = f_mark, fixed_recapt = f_recapt, 
                times_observe = t_observe, obs_move_type = o_move_type, res_min_age = r_min_age, 
                res_move_obs = r_move_obs, Euclidean_dist = e_dist, plotting = plotme, hunt = huntme, 
                start_hunting = start_hunt, res_consume = r_consume, ga_popsize = ga_pops, ga_mingen = ga_ming, 
                ga_seedrep = ga_seedr, ga_sampleK = ga_sampK, ga_chooseK = ga_chseK, ga_mutation = ga_mut, 
                ga_crossover = ga_cross, move_agents = m_agents, max_ages = m_ages, minimum_cost = min_cost,
                user_budget = u_budget, manager_budget = m_budget,manage_target = m_target, RESOURCE_ini = RES_ini, 
                scaring = scare, culling = cull, castration = castr,feeding = feed, help_offspring = help_offs, 
                tend_crops = t_crops, tend_crop_yld = t_crop_yld, kill_crops = k_crops, stakeholders = s, 
                manage_caution = m_caution, land_ownership = l_ownership, 
                manage_freq = m_freq, converge_crit = con_crit, 
                manager_sense = m_sense, public_land = p_land, 
                group_think = g_think, age_repr = age_rep, usr_budget_rng = u_bud_r,
                action_thres = act_thres, budget_bonus = bud_bonus, consume_surv = cons_surv,
                consume_repr = cons_repr, times_feeding = t_feeding, 
                ownership_var = o_var, perceive_scare = p_scare, 
                perceive_cull = p_cull, perceive_cast = p_cast, 
                perceive_feed = p_feed, perceive_help = p_help, 
                perceive_tend = p_tend, perceive_kill  = p_kill)
  out_gmse[z, ] = unlist(lapply(gmse_z$resource, nrow))
  out_gmse_ALL[[z]] = gmse_z
}

### gmse_apply()

out_gmseapply = matrix(NA, nrow = reps, ncol = nyears)
for(z in 1:reps) {
  print(sprintf("Rep %d", z))
  z_old = gmse_apply(get_res = "Full", land_dim_1 = ld1, land_dim_2 = ld2, res_movement = res_move, remove_pr = rem_pr, 
                     lambda = l, agent_view = a_view, agent_move = a_move, res_birth_K = rbK, res_death_K = rdK, 
                     edge_effect = e_effect, res_move_type = r_move_type, res_birth_type = r_birth_type, 
                     res_death_type = r_death_type, observe_type = obs_type, fixed_mark = f_mark, fixed_recapt = f_recapt, 
                     times_observe = t_observe, obs_move_type = o_move_type, res_min_age = r_min_age, 
                     res_move_obs = r_move_obs, Euclidean_dist = e_dist, plotting = plotme, hunt = huntme, 
                     start_hunting = start_hunt, res_consume = r_consume, ga_popsize = ga_pops, ga_mingen = ga_ming, 
                     ga_seedrep = ga_seedr, ga_sampleK = ga_sampK, ga_chooseK = ga_chseK, ga_mutation = ga_mut, 
                     ga_crossover = ga_cross, move_agents = m_agents, max_ages = m_ages, minimum_cost = min_cost,
                     user_budget = u_budget, manager_budget = m_budget,manage_target = m_target, RESOURCE_ini = RES_ini, 
                     scaring = scare, culling = cull, castration = castr,feeding = feed, help_offspring = help_offs, 
                     tend_crops = t_crops, tend_crop_yld = t_crop_yld, kill_crops = k_crops, stakeholders = s, 
                     manage_caution = m_caution, land_ownership = l_ownership, 
                     manage_freq = m_freq, converge_crit = con_crit, 
                     manager_sense = m_sense, public_land = p_land, 
                     group_think = g_think, age_repr = age_rep, usr_budget_rng = u_bud_r,
                     action_thres = act_thres, budget_bonus = bud_bonus, consume_surv = cons_surv,
                     consume_repr = cons_repr, times_feeding = t_feeding, 
                     ownership_var = o_var, perceive_scare = p_scare, 
                     perceive_cull = p_cull, perceive_cast = p_cast, 
                     perceive_feed = p_feed, perceive_help = p_help, 
                     perceive_tend = p_tend, perceive_kill  = p_kill)
  out_gmseapply[z,1] = z_old$basic_output$resource_results
  
  for(i in 2:nyears) {
    z_new = gmse_apply(get_res = "Full", old_list = z_old)
    out_gmseapply[z,i] = z_new$basic_output$resource_results
    z_old = z_new
  }
  rm(z_old, z_new)
}

### Plotting the results.
ylo = 0.9*min(c(min(out_gmse),min(out_gmseapply)))
yhi = 1.2*max(c(max(out_gmse),max(out_gmseapply)))
par(mfrow=c(1,1))
plot(out_gmse[1,], type = "n", ylim = c(ylo, yhi), main = "")
legend(x = 4, y = yhi, legend = c("gmse()","gmse_apply()"), col = c("red","blue"), fill = c("red","blue"))
apply(out_gmse, 1, function(x) lines(x, col = "red"))
apply(out_gmseapply, 1, function(x) lines(x, col = "blue"))

