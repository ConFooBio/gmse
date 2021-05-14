library("testthat");
library("GMSE");
context("Summary functions");

test_that("Dimensions of gmse_summary list output are correct", {
    skip_on_cran();
    gmse_sim <- gmse(time_max = 3, plotting = FALSE);
    gmse_sum <- gmse_summary(gmse_sim);
    gmse_tab <- gmse_table(gmse_sim);
    expect_equal(length(gmse_sum), 4);
})

test_that("Dimensions of gmse_summary resource output are correct", {
    skip_on_cran();
    gmse_sim <- gmse(time_max = 3, plotting = FALSE);
    gmse_sum <- gmse_summary(gmse_sim);
    gmse_tab <- gmse_table(gmse_sim);
    expect_equal(dim(gmse_sum[["resources"]]), c(3,2));
})

test_that("Dimensions of simulation observation output are correct", {
    skip_on_cran();
    gmse_sim <- gmse(time_max = 3, plotting = FALSE);
    gmse_sum <- gmse_summary(gmse_sim);
    gmse_tab <- gmse_table(gmse_sim);
    expect_equal(dim(gmse_sum[["observations"]]), c(3,2));
})

test_that("Dimensions of simulation cost output are correct", {
    skip_on_cran();
    gmse_sim <- gmse(time_max = 3, plotting = FALSE);
    gmse_sum <- gmse_summary(gmse_sim);
    gmse_tab <- gmse_table(gmse_sim);
    expect_equal(dim(gmse_sum[["costs"]]), c(3,10));
})

test_that("Dimensions of simulation action output are correct", {
    skip_on_cran();
    gmse_sim <- gmse(time_max = 3, plotting = FALSE);
    gmse_sum <- gmse_summary(gmse_sim);
    gmse_tab <- gmse_table(gmse_sim);
    expect_equal(dim(gmse_sum[["actions"]]), c(12,13));
})

test_that("Dimensions of gmse_tab output are correct", {
    skip_on_cran();
    gmse_sim <- gmse(time_max = 3, plotting = FALSE);
    gmse_sum <- gmse_summary(gmse_sim);
    gmse_tab <- gmse_table(gmse_sim);
    expect_equal(dim(gmse_tab), c(3,8));
})

test_that("Length of gmse_apply_summary is correct", {
    skip_on_cran();
    sim_old <- gmse_apply(get_res = "Full", scaring = TRUE, culling = TRUE, 
                          land_ownership = TRUE);
    output  <- gmse_apply_summary(data = sim_old, 
                                  include = c("res","obs","culls","scares"));
    expect_equal(length(output), 4);
})


