library(GMSE);
context("Cost array initialisation");

set.seed(1);
agents  <-  make_agents(model        = "IBM",
                        agent_number = 2,
                        type_counts  = c(1,1),
                        move         = 0,
                        vision       = 20,
                        rows         = 100,
                        cols         = 100
)
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

res_opts  <- c(1, 1, 1, 1, 1);
lnd_opts  <- c(1, 1);
min_cost  <- 10;

set.seed(1);
cost <- make_costs(agents, res, res_opts, lnd_opts, min_cost);

test_that("Dimensions of cost array are correct", {
    expect_equal(dim(cost), c(7, 13, 2));
})

test_that("Values on the cost array are accurate", {
    expect_equal(c(min(cost), max(cost)), c(10, 100001));
})

set.seed(Sys.time())