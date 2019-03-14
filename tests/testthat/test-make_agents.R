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
    
    expect_equal(dim(agents), c(2, 17));
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
