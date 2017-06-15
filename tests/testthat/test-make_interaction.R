library(GMSE);
context("Interaction array and table initialisation");

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

land_vec <- sample(x = 1:4, size = 48, replace = TRUE);
land_arr <- array(data = land_vec, dim = c(4, 4, 3));

int_array <- make_interaction_array(res, land_arr);
int_table <- make_interaction_table(res, land_arr);

test_that("Dimensions of landscape array are correct", {
    expect_equal(dim(int_array), c(3, 3));
})

test_that("Values on landscape array are correct", {
    expect_equal(as.vector(diag(int_array)), c(1, 1, 1));
    expect_equal(sum(int_array), 3);
})

test_that("Dimensions of landscape table are correct", {
    expect_equal(dim(int_table), c(3, 4));
})

test_that("Values on landscape table are correct", {
    expect_equal(as.vector(int_table[,2]), c(1, 2, 1));
    expect_equal(as.vector(int_table[,1]), c(0, 0, 1));
    expect_equal(sum(int_table), 5);
})

set.seed(Sys.time())