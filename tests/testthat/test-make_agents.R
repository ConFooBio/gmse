library("testthat");
library("GMSE");
context("Agent initialisation");

test_that("Dimensions of agent array are correct", {
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    expect_equal(dim(agents), c(2, 24));
})

test_that("Resource IDs are correct", {
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 2,
                            type_counts  = c(1,1),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    
    expect_equal(agents[,1], 1:2);
})

test_that("Variable budgets are made", {
    agents  <-  make_agents(model        = "IBM",
                            agent_number = 12,
                            type_counts  = c(1,11),
                            move         = 0,
                            vision       = 20,
                            rows         = 100,
                            cols         = 100
    )
    agents <- manager_user_budgets(agents, manager_budget = 4000, 
                                   user_budget = 4000, usr_budget_rng = 2000);
    
    expect_equal(as.numeric(agents[1, 17]), 4000);
    expect_equal(var(agents[2:12, 17]) > 0, TRUE);
})

