library(GMSE);
context("Main gmse function");

set.seed(1);
sim <- gmse(time_max = 3, plotting = FALSE);

test_that("Dimensions of simulation list output are correct", {
    expect_equal(length(sim), 8);
})

test_that("Dimensions of simulation resource output are correct", {
    expect_equal(length(sim[[1]]), 3);
})

test_that("Dimensions of simulation observation output are correct", {
    expect_equal(length(sim[[2]]), 3);
})

test_that("Dimensions of simulation paras output are correct", {
    expect_equal(dim(sim[[3]])[2], 105);
})

test_that("Dimensions of simulation landscape output are correct", {
    expect_equal(length(sim[[4]]), 3);
    expect_equal(dim(sim[[4]][[1]]), c(100, 100, 3));
})

test_that("Dimensions of time elapsed are correct", {
    expect_equal(length(sim[[5]]), 5);
})

test_that("Dimensions of simulation agent output are correct", {
     expect_equal(dim(sim[[6]][[1]]), c(5, 17));
})

test_that("Dimensions of simulation cost array output are correct", {
    expect_equal(dim(sim[[7]][[1]]), c(7, 13, 5));
})

test_that("Dimensions of simulation cost array output are correct", {
    expect_equal(length(sim[[8]]), 3);
    expect_equal(dim(sim[[8]][[1]]), c(7, 13, 5));
})

set.seed(Sys.time())
