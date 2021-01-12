# GMSE VERSION 0.6.0.0

## MAJOR CHANGES

* The order of operations in the resource submodel has changed. Formerly, resources would move on the landscape, reproduce, die, and then potentially consume on the landscape. Now, resources feed on the landscape, reproduce, die, and then move.

* The default landscape ownership is now divided using a shortest-splitline algorithm, which generates more realistic rectangular blocks of land.

* The default perceived effects of different actions have been changed for both the manager and users in the fitness function of the genetic algorithm.

## NEW FEATURES

* Resource birth and death can now be caused by resource consumption on the landscape using the new arguments `consume_repr`, `consume_surv`, and `times_feeding` (note that movement to new cells occurs between times feeding).

* Managers can apply an action threshold, which stops them from updating policy in a time step if the observed resource density is sufficiently close to the target. If they do not update policy, managers can receive a budget bonus that can be applied to the next time step. Relevant arguments are `action_thres` and `budget_bonus`, and a new vignette explains their use.

* The perceived effects of different actions for users can now be manually set using the arguments, `perceive_scare`, `perceive_cull`, `perceive_cast`, `perceive_feed`, `perceive_help`, `perceive_tend` and `perceive_kill`. Hence, it is possible to make users perceive some actions to be more or less effective than they are likely to be.

* Land ownership can be allocated unevenly among users using the `ownership_var` argument.

* Manager and user budgets can now be incremented as a function of landscape yield using the `usr_yld_budget` and `man_yld_budget` arguments.


## BUG FIXES

* The `gmse_gui` function has been updated so that options available in `gmse` and `gmse_apply` also work in `gmse`.

* Differences between default arguments of `gmse` and `gmse_apply` have now been rectified. Now a run of `gmse` and looped `gmse_apply` with default subfunctions returns the same system dynamics.

* A crash in `gmse_apply` caused by setting an argument name to the argument itself (e.g., `stakeholders = stakeholders`) was noticed sometimes. An error message is now produced in `gmse_apply` to warn against having the same name for an argument and the argument's value.


