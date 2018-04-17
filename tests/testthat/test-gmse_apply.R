library(GMSE);
context("gmse_apply tests");

gmap      <-  gmse_apply();
gmap_full <-  gmse_apply(get_res = "full");
    
test_that("Default gmse_apply is returned with the right length and no error", {
    expect_equal(length(gmap), 4);
    expect_equal(length(gmap$resource_results), 1);
    expect_equal(length(gmap$observation_results), 1);
    expect_equal(length(gmap$manager_results), 6);
    expect_equal(dim(gmap$user_results), c(5, 8));
})

alt_res <- function(X = 1000, K = 2000, r = 1){
    X_1 <- X + r*X*(1 - X/K);
    return(X_1);
}

alt_obs <- function(resource_vector){
    X_obs <- resource_vector - 0.1 * resource_vector;
    return(X_obs);
}

alt_man <- function(observation_vector){
    policy <- observation_vector - 1000;
    if(policy < 0){
        policy <- 0;
    }
    return(policy);
}

alt_usr <- function(manager_vector){
    harvest <- manager_vector + manager_vector * 0.1;
    return(harvest);
}

galt <- gmse_apply(alt_res, alt_obs, alt_man, alt_usr);

test_that("Simple models produce the exact output", {
    expect_equal(galt$resource_results, 1500);
    expect_equal(galt$observation_results, 1350);
    expect_equal(galt$manager_results, 350);
    expect_equal(galt$user_results, 385);
})

comb_1 <- gmse_apply(alt_res, alt_obs, alt_man, user);

test_that("Combination model 1", {
    expect_equal(comb_1$resource_results, 1500);
    expect_equal(comb_1$observation_results, 1350);
    expect_equal(comb_1$manager_results[3], 350);
    expect_equal(dim(comb_1$user_results), c(5, 8));
})

comb_2 <- gmse_apply(alt_res, alt_obs, manager, user);

test_that("Combination model 2", {
    expect_equal(comb_2$resource_results, 1500);
    expect_equal(comb_2$observation_results, 1350);
    expect_equal(length(comb_2$manager_results), 6);
    expect_equal(dim(comb_2$user_results), c(5, 8));
})

comb_3 <- gmse_apply(alt_res, alt_obs, manager, alt_usr);

test_that("Combination model 3", {
    expect_equal(comb_3$resource_results, 1500);
    expect_equal(comb_3$observation_results, 1350);
    expect_equal(is.numeric(comb_3$manager_results), TRUE);
    expect_equal(is.numeric(comb_3$user_results), TRUE);
})


comb_4 <- gmse_apply(alt_res, observation, alt_man, alt_usr);

test_that("Combination model 4", {
    expect_equal(comb_4$resource_results, 1500);
    expect_equal(is.numeric(comb_4$observation_results), TRUE);
    expect_equal(is.numeric(comb_4$manager_results), TRUE);
    expect_equal(is.numeric(comb_4$user_results), TRUE);
})

comb_5 <- gmse_apply(alt_res, observation, alt_man, user);

test_that("Combination model 5", {
    expect_equal(comb_5$resource_results, 1500);
    expect_equal(is.numeric(comb_5$observation_results), TRUE);
    expect_equal(length(comb_5$manager_results), 6);
    expect_equal(dim(comb_5$user_results), c(5, 8));
})

comb_6 <- gmse_apply(alt_res, observation, manager, alt_usr);

test_that("Combination model 6", {
    expect_equal(comb_6$resource_results, 1500);
    expect_equal(is.numeric(comb_6$observation_results), TRUE);
    expect_equal(is.numeric(comb_6$manager_results), TRUE);
    expect_equal(is.numeric(comb_6$user_results), TRUE);
})

comb_7 <- gmse_apply(alt_res, observation, manager, user);

test_that("Combination model 7", {
    expect_equal(comb_7$resource_results, 1500);
    expect_equal(is.numeric(comb_7$observation_results), TRUE);
    expect_equal(length(comb_7$manager_results), 6);
    expect_equal(dim(comb_7$user_results), c(5, 8));
})

comb_8 <- gmse_apply(resource, alt_obs, alt_man, user);

test_that("Combination model 8", {
    expect_equal(is.numeric(comb_8$resource_results), TRUE);
    expect_equal(is.numeric(comb_8$observation_results), TRUE);
    expect_equal(length(comb_8$manager_results), 6);
    expect_equal(dim(comb_8$user_results), c(5, 8));
})

comb_9 <- gmse_apply(resource, alt_obs, manager, user);

test_that("Combination model 9", {
    expect_equal(is.numeric(comb_9$resource_results), TRUE);
    expect_equal(is.numeric(comb_9$observation_results), TRUE);
    expect_equal(length(comb_9$manager_results), 6);
    expect_equal(dim(comb_9$user_results), c(5, 8));
})

comb_10 <- gmse_apply(resource, alt_obs, manager, alt_usr);

test_that("Combination model 10", {
    expect_equal(is.numeric(comb_10$resource_results), TRUE);
    expect_equal(is.numeric(comb_10$observation_results), TRUE);
    expect_equal(is.numeric(comb_10$manager_results), TRUE);
    expect_equal(is.numeric(comb_10$user_results), TRUE);
})

comb_11 <- gmse_apply(resource, observation, alt_man, alt_usr);

test_that("Combination model 11", {
    expect_equal(is.numeric(comb_11$resource_results), TRUE);
    expect_equal(is.numeric(comb_11$observation_results), TRUE);
    expect_equal(is.numeric(comb_11$manager_results), TRUE);
    expect_equal(is.numeric(comb_11$user_results), TRUE);
})

comb_12 <- gmse_apply(resource, observation, alt_man, user);

test_that("Combination model 12", {
    expect_equal(is.numeric(comb_12$resource_results), TRUE);
    expect_equal(is.numeric(comb_12$observation_results), TRUE);
    expect_equal(length(comb_12$manager_results), 6);
    expect_equal(dim(comb_12$user_results), c(5, 8));
})

comb_13 <- gmse_apply(resource, observation, manager, alt_usr);

test_that("Combination model 13", {
    expect_equal(is.numeric(comb_13$resource_results), TRUE);
    expect_equal(is.numeric(comb_13$observation_results), TRUE);
    expect_equal(is.numeric(comb_13$manager_results), TRUE);
    expect_equal(is.numeric(comb_13$user_results), TRUE);
})

comb_14 <- gmse_apply(resource, observation, manager, user);

test_that("Combination model 14", {
    expect_equal(is.numeric(comb_14$resource_results), TRUE);
    expect_equal(is.numeric(comb_14$observation_results), TRUE);
    expect_equal(length(comb_14$manager_results), 6);
    expect_equal(dim(comb_14$user_results), c(5, 8));
})


