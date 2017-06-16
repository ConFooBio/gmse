library(GMSE);
context("Main gmse function");

set.seed(1);
sim <- gmse(time_max = 3, plotting = FALSE);

test_that("Dimensions of simulation list output are correct", {
    expect_equal(length(sim), 8);
})

test_that("Dimensions of simulation resource output are correct", {
    expect_equal(length(sim[[1]]), 2);
    expect_equal(dim(sim[[1]][[1]]), c(208, 20));
    expect_equal(dim(sim[[1]][[2]]), c(219, 20));
})

test_that("Dimensions of simulation observation output are correct", {
    expect_equal(length(sim[[2]]), 2);
    expect_equal(dim(sim[[2]][[1]]), c(151, 61));
    expect_equal(dim(sim[[2]][[2]]), c(169, 61));
})

test_that("Dimensions of simulation paras output are correct", {
    expect_equal(length(sim[[3]]), 204);
    expect_equal(dim(sim[[3]]), c(2, 102));
})

test_that("Dimensions of simulation landscape output are correct", {
    expect_equal(length(sim[[4]]), 2);
    expect_equal(dim(sim[[4]][[1]]), c(100, 100));
    expect_equal(dim(sim[[4]][[2]]), c(100, 100));
})

test_that("Dimensions of time elapsed are correct", {
    expect_equal(length(sim[[5]]), 5);
})

test_that("Dimensions of simulation agent output are correct", {
     expect_equal(dim(sim[[6]]), c(5, 17));
})

test_that("Dimensions of simulation cost array output are correct", {
    expect_equal(length(sim[[7]]), 2);
    expect_equal(dim(sim[[7]][[1]]), c(7, 13, 5));
    expect_equal(dim(sim[[7]][[2]]), c(7, 13, 5));
})

test_that("Dimensions of simulation cost array output are correct", {
    expect_equal(length(sim[[8]]), 2);
    expect_equal(dim(sim[[8]][[1]]), c(7, 13, 5));
    expect_equal(dim(sim[[8]][[2]]), c(7, 13, 5));
})

set.seed(Sys.time())