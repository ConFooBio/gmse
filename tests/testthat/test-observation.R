library(GMSE);
context("Observation model");

set.seed(1);
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
                       owner_pr     = NULL
);

paras <- c(0, 1, 1, 2, 2, 100, 100, 0, 0, 1, 10, 20, 10, 10, 1, 1, 1, 1, 12, 1, 
           0, 100, 10, 20, 20, 2, 0.1, 0.1, 0, 5, 7, 11, 100, 4, 5, 6, 3, 9, 10,  
           18, 19, 20, 17, 8, 1, 1, 15, 14, 1, 4, 5, 6, 10, 12, 2, 17, 1, 2, 3, 
           13, 3, -1, -1, 1, 0, 2, 2, 8, 7, 13, 4, 7, 0, 16, 0, -0.1, -0.1, 0.1,  
           0.1, 0.5, 1, 2, 15, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 10, 1000, 
           100, 100, 0, 0);

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

test_that("Dimensions of observation arrays in returned list are correct", {
    expect_equal(dim(obs[[1]]), c(100, 41));
    expect_equal(dim(obs[[2]]), c(2, 17));
    expect_equal(length(obs[[3]]), 102);
})

test_that("Observation model updates new parameters", {
    expect_equal(sum(obs[[3]][1:61] - paras[1:61]), 0);
    expect_equal(obs[[3]][62] - paras[62], 101);
    expect_equal(obs[[3]][63] - paras[63], 42);
    expect_equal(sum(obs[[3]][64:102] - paras[64:102]), 0);
})

test_that("Observation model has managers marking resources", {
    expect_equal(obs[[2]][1,11], 1483);
    expect_equal(obs[[2]][2,11], 0);
})

test_that("Resources are marked correctly", {
    expect_equal(min(obs[[1]][,28:41]), 1);
    expect_equal(max(obs[[1]][,28:41]), 1);
})

test_that("Resources can move during observation", {
    expect_equal(min(obs[[1]][,5] - res[,5]), -8);
    expect_equal(min(obs[[1]][,6] - res[,6]), -9);
    expect_equal(max(obs[[1]][,5] - res[,5]),  8);
    expect_equal(max(obs[[1]][,6] - res[,6]),  9);
})

set.seed(Sys.time())