library("testthat");
library("GMSE");
context("Main gmse function");

test_that("Dimensions of simulation list output are correct", {
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(length(sim), 8);
})

test_that("Dimensions of simulation resource output are correct", {
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(length(sim[[1]]), 3);
})

test_that("Dimensions of simulation observation output are correct", {
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(length(sim[[2]]), 3);
})

test_that("Dimensions of simulation paras output are correct", {
    skip_on_cran();
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(dim(sim[[3]])[2], 136);
})

test_that("Dimensions of simulation landscape output are correct", {
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(length(sim[[4]]), 3);
    expect_equal(dim(sim[[4]][[1]]), c(100, 100, 3));
})

test_that("Dimensions of time elapsed are correct", {
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(length(sim[[5]]), 5);
})

test_that("Dimensions of simulation agent output are correct", {
    skip_on_cran();
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(dim(sim[[6]][[1]]), c(5, 27));
})

test_that("Dimensions of simulation cost array output are correct", {
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(dim(sim[[7]][[1]]), c(7, 13, 5));
})

test_that("Dimensions of simulation cost array output are correct", {
    sim <- gmse(time_max = 3, plotting = FALSE);
    expect_equal(length(sim[[8]]), 3);
    expect_equal(dim(sim[[8]][[1]]), c(7, 13, 5));
})

test_that("Consumption based resource use is working", {
    sim   <- gmse(time_max = 4, times_feeding = 4, consume_surv = 0.1, 
                  consume_repr = 0.4, res_consume = 0.2, land_dim_1 = 10,
                  land_dim_2 = 10, plotting = FALSE);
    cons  <- mean(sim$land[[4]][,,2]) < 1;
    expect_equal(cons, TRUE);
})
