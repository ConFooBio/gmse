library("testthat");
library("GMSE");
context("Landscape initialisation");

test_that("Landscape dimensions are initialised accurately", {
    land  <-  make_landscape(model        = "IBM", 
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
    expect_equal(dim(land), c(10, 10, 3));
})

test_that("Landscape values are initialised accurately", {
    land  <-  make_landscape(model        = "IBM", 
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
    expect_equal(max(land), 1);
    expect_equal(min(land), 1);
})

test_that("Landscape values are reset when needed", {
    land  <-  make_landscape(model        = "IBM", 
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
    expect_equal(age_land(land+1, land, 2)[,,2], land[,,2]);
})
