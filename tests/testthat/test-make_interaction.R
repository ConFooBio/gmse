library("testthat");
library("GMSE");
context("Interaction array and table initialisation");

test_that("Dimensions of interaction array are correct", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 1000, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    land_vec <- sample(x = 1:4, size = 48, replace = TRUE);
    land_arr <- array(data = land_vec, dim = c(4, 4, 3));
    
    int_array <- make_interaction_array(res, land_arr);
    int_table <- make_interaction_table(res, land_arr);
    
    expect_equal(dim(int_array), c(3, 3));
})

test_that("Dimensions of interaction table are correct", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 1000, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    land_vec <- sample(x = 1:4, size = 48, replace = TRUE);
    land_arr <- array(data = land_vec, dim = c(4, 4, 3));
    
    int_array <- make_interaction_array(res, land_arr);
    int_table <- make_interaction_table(res, land_arr);
    
    expect_equal(dim(int_table), c(3, 4));
})
