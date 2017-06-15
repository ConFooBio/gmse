library(GMSE);
context("Action and cost layer initialisation");

agent_IDs    <- -2:4;
agent_number <- length(agent_IDs);
res_types    <- rbind(c(1, 0, 0), c(2, 0, 0));

layer <- utility_layer(agent_IDs, agent_number, res_types);

test_that("Action and cost layers initialised with correct dimensions", {
    expect_equal(dim(layer), c(8, 13));
})

test_that("Action and cost layers initialised with zeros", {
    expect_equal(sum(layer[,3:13]), 0);
})

test_that("Action and cost layers initialised with correct agents", {
    expect_equal(c(min(layer[,1]),max(layer[,1])), c(1,4));
})

test_that("Action and cost layers initialised with correct resources", {
    expect_equal(c(min(layer[,2]),max(layer[,2])), c(1,2));
})

set.seed(Sys.time())