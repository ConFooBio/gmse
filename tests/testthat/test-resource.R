library("testthat");
library("GMSE");
context("Resource model");

test_that("Dimensions of resource array are correct", {
    skip_on_cran();
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 100, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    land <- make_landscape(model        = "IBM", 
                           rows         = 10, 
                           cols         = 10, 
                           cell_types   = 1, 
                           cell_val_mn  = 1, 
                           cell_val_sd  = 0, 
                           cell_val_max = 1, 
                           cell_val_min = 1,
                           layers       = 3, 
                           ownership    = 1,  
                           owner_pr     = NULL
    );
    
    paras <- c(0, 1, 1, 2, 2, 100, 100, 0, 0, 1, 10, 20, 10, 10, 1, 1, 1, 1, 12,
               1, 0, 100, 10, 20, 20, 2, 0.1, 0.1, 0, 5, 7, 11, 100, 4, 5, 6, 3,
               9, 10, 18, 19, 20, 17, 8, 1, 1, 15, 14, 1, 4, 5, 6, 10, 12, 2, 
               17, 1, 2, 3, 13, 3, -1, -1, 1, 0, 2, 2, 8, 7, 13, 4, 7, 0, 16, 0,
               -0.1, -0.1, 0.1, 0.1, 0.5, 1, 2, 15, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
               1, 1, 1, 1, 10, 1000, 100, 100, 0, 0, 10, 0, 0, 0, 1, 0, 0, 0, 0,
               1, 16, 1000, 10);
    
    res_res <- resource( RESOURCES = res, 
                         LAND      = land, 
                         PARAS     = paras,
                         model     = "IBM"
    )
    
    expect_equal(length(res_res), 3);
    expect_equal(dim(res_res[[2]]), c(10, 10, 3));
    expect_equal(length(res_res[[3]]), 115);
})

test_that("Resource model doesn't alter parameters", {
    skip_on_cran();
    res <- make_resource(model              = "IBM", 
                         resource_quantity  = 100, 
                         resource_types     = 2, 
                         rows               = 10, 
                         cols               = 10, 
                         move               = 1, 
                         rm_pr              = 0,
                         lambda             = 0,
                         consumption_rate   = c(0.1, 0.2),
                         max_age            = 5
    );
    
    land <- make_landscape(model        = "IBM", 
                           rows         = 10, 
                           cols         = 10, 
                           cell_types   = 1, 
                           cell_val_mn  = 1, 
                           cell_val_sd  = 0, 
                           cell_val_max = 1, 
                           cell_val_min = 1,
                           layers       = 3, 
                           ownership    = 1,  
                           owner_pr     = NULL
    );
    
    paras <- c(0, 1, 1, 2, 2, 100, 100, 0, 0, 1, 10, 20, 10, 10, 1, 1, 1, 1, 12,
               1, 0, 100, 10, 20, 20, 2, 0.1, 0.1, 0, 5, 7, 11, 100, 4, 5, 6, 3,
               9, 10, 18, 19, 20, 17, 8, 1, 1, 15, 14, 1, 4, 5, 6, 10, 12, 2, 
               17, 1, 2, 3, 13, 3, -1, -1, 1, 0, 2, 2, 8, 7, 13, 4, 7, 0, 16, 0,
               -0.1, -0.1, 0.1, 0.1, 0.5, 1, 2, 15, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
               1, 1, 1, 1, 10, 1000, 100, 100, 0, 0, 10, 0, 0, 0, 1, 0, 0, 0, 0,
               1, 16, 1000, 10);
    
    res_res <- resource( RESOURCES = res, 
                         LAND      = land, 
                         PARAS     = paras,
                         model     = "IBM"
    )
    expect_equal(length(res_res), 3);
    expect_equal(dim(res_res[[2]]), c(10, 10, 3));
    expect_equal(length(res_res[[3]]), 115);
})
