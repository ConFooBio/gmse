library("testthat");
library("GMSE");
context("Simulated annealing algorithm");

test_that("Simulated annealing runs for managers", {
    sim_err <- try(gmse(time_max = 3, plotting = FALSE, mana_annealing = TRUE),
                   silent = TRUE);
    is_err  <- class(sim_err) == "try-error"
    expect_equal(is_err, FALSE);
})

test_that("Simulated annealing runs for users", {
    sim_err <- try(gmse(time_max = 3, plotting = FALSE, user_annealing = TRUE),
                   silent = TRUE);
    is_err  <- class(sim_err) == "try-error"
    expect_equal(is_err, FALSE);
})

test_that("Simulated annealing runs for managers and users", {
    sim_err <- try(gmse(time_max = 3, plotting = FALSE, mana_annealing = TRUE,
                        user_annealing = TRUE), silent = TRUE);
    is_err  <- class(sim_err) == "try-error"
    expect_equal(is_err, FALSE);
})