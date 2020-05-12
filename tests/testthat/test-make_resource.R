library("testthat");
library("GMSE");
context("Resource initialisation");

test_that("Dimensions of resource array are correct", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 1, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = 0.1,
                         max_age            = 5
    );
    
    expect_equal(dim(res), c(10, 22));
})

test_that("Resource IDs are correct", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 1, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = 0.1,
                         max_age            = 5
    );
    
    expect_equal(res[,1], 1:10);
})

test_that("Resource types are initialised correctly", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 1, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = 0.1,
                         max_age            = 5
    );
    
    expect_equal(min(res[,2]), 1);
})

test_that("Various trait values of resources are initialised correctly", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 1, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = 0.1,
                         max_age            = 5
    );
    
    expect_equal( sum(res[,8:11]), 0);
})

test_that("No markings initialised", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 1, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = 0.1,
                         max_age            = 5
    );
    
    expect_equal( sum(res[,13:14]), 0);
})

test_that("Resource consumption rates are initialised correctly", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 1, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = 0.1,
                         max_age            = 5
    );
    
    expect_equal( min(res[,15]), 0.1);
})

test_that("No initial adjustment to resource trait values", {
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 10, 
                         resource_types     = 1, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = 0.1,
                         max_age            = 5
    );
    
    expect_equal( sum(res[,16:20]), 0);
})
