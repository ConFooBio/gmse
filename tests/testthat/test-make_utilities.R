library("testthat");
library("GMSE");
context("Action array initialisation");

test_that("Dimensions of action array are correct", {
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    res_opts  <- c(1, 1, 1, 1, 1);
    lnd_opts  <- c(1, 1);
    min_cost  <- 10;
    
    action <- make_utilities(agents, res);
    
    expect_equal(dim(action), c(7, 13, 2));
})

test_that("Values on the action array are accurate", {
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    res_opts  <- c(1, 1, 1, 1, 1);
    lnd_opts  <- c(1, 1);
    min_cost  <- 10;
    
    action <- make_utilities(agents, res);
    
    expect_equal(sum(action[,3:13,]), 0);
})

test_that("IDs on the action array are accurate", {
    ID_vec <- c(-2, -2, -1, 1, 1, 2, 2);
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    res_opts  <- c(1, 1, 1, 1, 1);
    lnd_opts  <- c(1, 1);
    min_cost  <- 10;
    
    action <- make_utilities(agents, res);
    
    expect_equal(action[,1,1], ID_vec);
    expect_equal(action[,1,2], ID_vec);
})
