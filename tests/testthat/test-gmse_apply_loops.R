library("testthat");
library("GMSE");
context("gmse_apply looping tests");

test_that("Looping gmse_apply time step", {
    skip_on_cran();
    
    sim_new  <- gmse_apply(get_res = "Full", 
                           old_list = gmse_apply(get_res = "Full"));

    new_ts <- as.integer(sim_new[["PARAS"]][1]);
    
    expect_equal(new_ts, 2)
})


test_that("Looping gmse_apply action_threshold", {
    skip_on_cran();
    
    sim_new <- gmse_apply(get_res = "Full", 
                          old_list = gmse_apply(get_res = "Full", 
                                old_list = gmse_apply(get_res = "Full", 
                                    old_list = gmse_apply(get_res = "Full",
                                                          scaring = FALSE, 
                                                          stakeholders = 3, 
                                                          action_thres = 10, 
                                                          user_budget = 10, 
                                                          budget_bonus = 123)
                                    )
                                )
                          );

    expect_equal(sim_new[["PARAS"]][107], 0);
    expect_equal(sim_new[["PARAS"]][108], 2);
    expect_equal(sim_new[["PARAS"]][111], 123);
})




