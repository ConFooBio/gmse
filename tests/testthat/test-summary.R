library(GMSE);
context("Summary functions");

set.seed(1);
gmse_sim <- gmse(time_max = 3, plotting = FALSE);
gmse_sum <- gmse_summary(gmse_sim);
gmse_tab <- gmse_table(gmse_sim);

test_that("Dimensions of gmse_summary list output are correct", {
    expect_equal(length(gmse_sum), 4);
})

test_that("Dimensions of gmse_summary resource output are correct", {
    expect_equal(dim(gmse_sum$resources), c(3,2));
})

test_that("Dimensions of simulation observation output are correct", {
    expect_equal(dim(gmse_sum$observations), c(3,2));
})

test_that("Dimensions of simulation cost output are correct", {
    expect_equal(dim(gmse_sum$costs), c(3,10));
})

test_that("Dimensions of simulation action output are correct", {
    expect_equal(dim(gmse_sum$actions), c(12,13));
})

test_that("Dimensions of gmse_tab output are correct", {
    expect_equal(dim(gmse_tab), c(3,8));
})

set.seed(Sys.time())