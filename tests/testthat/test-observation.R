library("testthat");
library("GMSE");
context("Observation model");

test_that("Dimensions of observation arrays in returned list are correct", {
    skip_on_cran();
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 100, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    land <- make_landscape(model        = "IBM", 
                           rows         = 10, 
                           cols         = 10, 
                           cell_types   = 1, 
                           cell_val_mn  = 1, 
                           cell_val_sd  = 0, 
                           cell_val_max = 1, 
                           cell_val_min = 1,
                           layers       = 3, 
                           ownership    = 1,
                           owners       = 1,
                           public_land  = 0
    );
    
    paras <- c(0, 1, 1, 2, 2, 100, 100, 0, 0, 1, 10, 20, 10, 10, 1, 1, 1, 1, 12,
               1, 0, 100, 10, 20, 20, 2, 0.1, 0.1, 0, 5, 7, 11, 100, 4, 5, 6, 3,
               9, 10, 18, 19, 20, 17, 8, 1, 1, 15, 14, 1, 4, 5, 6, 10, 12, 2, 
               17, 1, 2, 3, 13, 3, -1, -1, 1, 0, 2, 2, 8, 7, 13, 4, 7, 0, 17, 
               17, 18, 19, 20, 21, 0.5, 1, 2, 15, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
               1, 1, 1, 1, 10, 1000, 100, 100, 0, 0, 10, 0, 0, 0, 1, 0, 0, 0, 0,
               1, 16, 1000, 10, 20, 0, 0, 21, 0, 13, 1, 22, 23, 1, 0, 0, 
               24, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    interaction_tabl <- make_interaction_table(res, land);
    
    obs   <- observation(RESOURCES      = res,
                         LAND           = land,
                         PARAS          = paras,
                         AGENTS         = agents,
                         inter_tabl     = interaction_tabl,
                         fixed_mark     = paras[11],
                         times_observe  = paras[12],
                         res_min_age    = paras[17],
                         agent_type     = 0,
                         type_cat       = 1,
                         observe_type   = paras[9],
                         res_move_obs   = paras[20]
    );
    
    expect_equal(dim(obs[[1]]), c(100, 43));
    expect_equal(dim(obs[[2]]), c(2, 27));
    expect_equal(length(obs[[3]]), 140);
})

test_that("Observation model updates new parameters", {
    skip_on_cran();
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 100, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    land <- make_landscape(model        = "IBM", 
                           rows         = 10, 
                           cols         = 10, 
                           cell_types   = 1, 
                           cell_val_mn  = 1, 
                           cell_val_sd  = 0, 
                           cell_val_max = 1, 
                           cell_val_min = 1,
                           layers       = 3, 
                           ownership    = 1,
                           owners       = 1,
                           public_land  = 0
    );
    
    paras <- c(0, 1, 1, 2, 2, 100, 100, 0, 0, 1, 10, 20, 10, 10, 1, 1, 1, 1, 12,
               1, 0, 100, 10, 20, 20, 2, 0.1, 0.1, 0, 5, 7, 11, 100, 4, 5, 6, 3,
               9, 10, 18, 19, 20, 17, 8, 1, 1, 15, 14, 1, 4, 5, 6, 10, 12, 2, 
               17, 1, 2, 3, 13, 3, -1, -1, 1, 0, 2, 2, 8, 7, 13, 4, 7, 0, 17, 
               17, 18, 19, 20, 21, 0.5, 1, 2, 15, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
               1, 1, 1, 1, 10, 1000, 100, 100, 0, 0, 10, 0, 0, 0, 1, 0, 0, 0, 0,
               1, 16, 1000, 10, 20, 0, 0, 21, 0, 13, 1, 22, 23, 1, 0, 0, 
               24, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    
    interaction_tabl <- make_interaction_table(res, land);
    
    obs   <- observation(RESOURCES      = res,
                         LAND           = land,
                         PARAS          = paras,
                         AGENTS         = agents,
                         inter_tabl     = interaction_tabl,
                         fixed_mark     = paras[11],
                         times_observe  = paras[12],
                         res_min_age    = paras[17],
                         agent_type     = 0,
                         type_cat       = 1,
                         observe_type   = paras[9],
                         res_move_obs   = paras[20]
    );
    
    expect_equal(sum(obs[[3]][1:61] - paras[1:61]), 0);
    expect_equal(obs[[3]][62] - paras[62], 101);
    expect_equal(obs[[3]][63] - paras[63], 44);
    expect_equal(sum(obs[[3]][64:102] - paras[64:102]), 0);
})

test_that("Resources are marked correctly", {
    skip_on_cran();
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 100, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    land <- make_landscape(model        = "IBM", 
                           rows         = 10, 
                           cols         = 10, 
                           cell_types   = 1, 
                           cell_val_mn  = 1, 
                           cell_val_sd  = 0, 
                           cell_val_max = 1, 
                           cell_val_min = 1,
                           layers       = 3, 
                           ownership    = 1,
                           owners       = 1,
                           public_land  = 0
    );
    
    paras <- c(0, 1, 1, 2, 2, 100, 100, 0, 0, 1, 10, 20, 10, 10, 1, 1, 1, 1, 12,
               1, 0, 100, 10, 20, 20, 2, 0.1, 0.1, 0, 5, 7, 11, 100, 4, 5, 6, 3,
               9, 10, 18, 19, 20, 17, 8, 1, 1, 15, 14, 1, 4, 5, 6, 10, 12, 2, 
               17, 1, 2, 3, 13, 3, -1, -1, 1, 0, 2, 2, 8, 7, 13, 4, 7, 0, 17, 
               17, 18, 19, 20, 21, 0.5, 1, 2, 15, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
               1, 1, 1, 1, 10, 1000, 100, 100, 0, 0, 10, 0, 0, 0, 1, 0, 0, 0, 0,
               1, 16, 1000, 10, 20, 0, 0, 21, 0, 13, 1, 22, 23, 1, 1, 0, 0, 
               24, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    
    interaction_tabl <- make_interaction_table(res, land);
    
    obs   <- observation(RESOURCES      = res,
                         LAND           = land,
                         PARAS          = paras,
                         AGENTS         = agents,
                         inter_tabl     = interaction_tabl,
                         fixed_mark     = paras[11],
                         times_observe  = paras[12],
                         res_min_age    = paras[17],
                         agent_type     = 0,
                         type_cat       = 1,
                         observe_type   = paras[9],
                         res_move_obs   = paras[20]
    );
    
    expect_equal(max(obs[[1]][,28:41]), 1);
})
