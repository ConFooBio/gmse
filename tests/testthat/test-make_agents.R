library(GMSE);
context("Agent initialisation");
suppressWarnings(RNGversion("3.5.0"));
RNGversion("3.4.0");
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
    expect_equal(dim(agents), c(2, 17));
})

test_that("Resource IDs are correct", {
    expect_equal(agents[,1], 1:2);
})

set.seed(Sys.time())