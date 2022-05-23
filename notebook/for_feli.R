sim_sco  <- gmse_apply(get_res = "Full", max_age = 200, lambda = 0, 
                       RESOURCE_ini = 2000, user_budget = 10);

sim_ice  <- gmse_apply(get_res = "Full", max_age = 200, lambda = 2, 
                       RESOURCE_ini = 2000, user_budget = 10);

the_res <- sim_sco$resource_array;

sim_sum_1 <- matrix(data = NA, nrow = 30, ncol = 5);
for(time_step in 1:30){
  sim_sco   <- gmse_apply(get_res = "Full", old_list = sim_sco);
  
  sim_ice$PARAS[33]           <- dim(sim_sco$resource_array)[1];
  sim_ice$PARAS[62]           <- dim(sim_sco$resource_array)[1];
  sim_ice$resource_array      <- sim_sco$resource_array;
  sim_ice$resource_array[,10] <- 2;

  sim_ice   <- gmse_apply(get_res = "Full", old_list = sim_ice);

  print(c(time_step, dim(sim_sco$resource_array)[1], dim(sim_ice$resource_array)[1]));
    
  sim_sco$PARAS[33]           <- dim(sim_ice$resource_array)[1];
  sim_sco$PARAS[62]           <- dim(sim_ice$resource_array)[1];
  sim_sco$resource_array      <- sim_ice$resource_array;
  sim_sco$resource_array[,10] <- 0;
}



