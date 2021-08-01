library("testthat");
library("GMSE");
context("Landscape initialisation");

test_that("Landscape dimensions are initialised accurately", {
    skip_on_cran();
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
                             owners       = 1,
                             public_land  = 0
    );
    expect_equal(dim(land), c(10, 10, 3));
})

test_that("Landscape values are initialised accurately", {
    skip_on_cran();
    land  <-  make_landscape(model        = "IBM", 
                             rows         = 10, 
                             cols         = 10, 
                             cell_types   = 1, 
                             cell_val_mn  = 1, 
                             cell_val_sd  = 0, 
                             cell_val_max = 1, 
                             cell_val_min = 1,
                             layers       = 3, 
                             ownership    = 0,
                             owners       = 1,
                             public_land  = 1
    );
    expect_equal(max(land), 1);
    expect_equal(min(land), 1);
})

test_that("Landscape values are reset when needed", {
    skip_on_cran();
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
                             owners       = 1,
                             public_land  = 0
    );
    expect_equal(age_land(land+1, land, 2)[,,2], land[,,2]);
})


test_that("All users are given land evenly distributed", {
    skip_on_cran();
    land  <-  make_landscape(model        = "IBM", 
                             rows         = 64, 
                             cols         = 64, 
                             cell_types   = 1, 
                             cell_val_mn  = 1, 
                             cell_val_sd  = 0, 
                             cell_val_max = 1, 
                             cell_val_min = 1,
                             layers       = 3, 
                             ownership    = 1,
                             owners       = 16,
                             public_land  = 0
    );
    expect_equal(unique(table(land[,,3])), (64*64/16) );
})

test_that("All users are given land when unequal", {
    skip_on_cran();
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
                             owners       = 7,
                             public_land  = 0
    );
    expect_equal(length(unique(as.vector(land[,,3]))), 7 );
})


test_that("Public land is added appropriately", {
    skip_on_cran();
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
                             owners       = 4,
                             public_land  = 0.5
    );
    expect_equal(sum(land[,,3] == 1) / (10 * 10), 0.5 );
    expect_equal(length(unique(as.vector(land[,,3]))), (4 + 1) );
})
