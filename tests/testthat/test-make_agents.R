library(GMSE);
context("Agent initialisation");

set.seed(1);
agents  <-  make_agents(model        = "IBM",
                        agent_number = 2,
                        type_counts  = c(1,1),
                        move         = 0,
                        vision       = 20,
                        rows         = 100,
                        cols         = 100
)

test_that("Dimensions of agent array are correct", {
    expect_equal(dim(res), c(2, 17));
})

test_that("Resource IDs are correct", {
    expect_equal(res[,1], 1:2);
})

set.seed(Sys.time())