library(GMSE);
context("Resource initialisation");

set.seed(1);
res <- make_resource(model              = "IBM", 
                     resource_quantity  = 10, 
                     resource_types     = 2, 
                     rows               = 10, 
                     cols               = 10, 
                     move               = 1, 
                     rm_pr              = 0,
                     lambda             = 0,
                     consumption_rate   = c(0.1, 0.2),
                     max_age            = 5
);

test_that("Dimensions of resource array are correct", {
    expect_equal(dim(res), c(10, 20));
})

test_that("Resource IDs are correct", {
    expect_equal(res[,1], 1:10);
})

test_that("Resource types are initialised correctly", {
    expect_equal(unique(res[,2]), c(1,2));
})

test_that("Locations of resources are initialised correctly", {
    expect_equal( c(min(res[,5:6]), max(res[,5:6])), c(0,9) );
})

test_that("Various trait values of resources are initialised correctly", {
    expect_equal( sum(res[,8:11]), 0);
})

test_that("No markings initialised", {
    expect_equal( sum(res[,13:14]), 0);
})

test_that("Resource consumption rates are initialised correctly", {
    expect_equal( c(min(res[,15]), max(res[,15])), c(0.1, 0.2) );
})

test_that("Resource ages are initialised correctly", {
    expect_equal( c(min(res[,12]), max(res[,12])), c(1,5) );
})

test_that("No initial adjustment to resource trait values", {
    expect_equal( sum(res[,16:20]), 0);
})

set.seed(Sys.time())