![](https://raw.githubusercontent.com/bradduthie/gmse/1727ea37f32f0f40df8ee6e8277d0d1723c88639/notebook/images/GMSE_logo_name.png)

[![](http://www.r-pkg.org/badges/version/GMSE?color=yellowgreen)](https://cran.r-project.org/package=GMSE) [![](http://cranlogs.r-pkg.org/badges/grand-total/GMSE?color=yellowgreen)](http://cranlogs.r-pkg.org/badges/grand-total/GMSE)
[![](http://cranlogs.r-pkg.org/badges/last-month/GMSE?color=yellowgreen)](http://cranlogs.r-pkg.org/badges/last-month/GMSE)


Generalised Management Strategy Evaluation
--------------------------------------------------------------------------------

**The GMSE package integrates game theory and ecological theory to construct social-ecological models that simulate the management of populations and stakeholder actions. These models build off of a previously developed management strategy evaluation (MSE) framework to simulate all aspects of management: population dynamics, manager observation of populations, manager decision making, and stakeholder responses to management decisions. The newly developed generalised management strategy evaluation (GMSE) framework uses genetic algorithms to mimic the decision-making process of managers and stakeholders under conditions of change, uncertainty, and conflict. Simulations can be run using gmse(), gmse_apply(), and gmse_gui() functions.**

--------------------------------------------------------------------------------

## Installation

**Install from CRAN**

To install [this package](https://CRAN.R-project.org/package=GMSE) from CRAN.

```
install.packages("GMSE")
```

**Install with GitHub**

To install this package from GitHub, make sure that the `devtools` library is installed.

```
install.packages("devtools")
library(devtools)
```

Use `install_github` to install using `devtools`.

```
install_github("bradduthie/GMSE")
```

## Running a simulation

To run a simulation, use the gmse() function.

```
sim <- gmse();
```

Optional arguments taken by gmse() are used to specify simulation parameter values. Simulation results will be plotted automatically given the default `plotting = TRUE`, but can be plotted again using the `plot_gmse_results` function.

```
plot_gmse_results(res = sim$resource, obs = sim$observation, land = sim$land, 
agents = sim$agents, paras = sim$paras, ACTION = sim$action, COST = sim$cost)
```

Simulations can also be run from a browser-based graphical user interface by running the `gmse_gui()` function.

```
gmse_gui();
```

This function is also available as a [standalone application](https://bradduthie.shinyapps.io/gmse_gui/) in shiny.

Finally, simulations can be run using `gmse_apply()`, a function for modellers that allows the integration of custom resource, observation, manager, and user subfunctions into the GMSE framework. The `gmse_apply()` function simulates a single time step and can incorporate both custom and standard `gmse()` arguments.

```
# Example alternative, custom-made, logistic growth resource function
alt_res <- function(X, K = 2000, rate = 1){
    X_1 <- X + rate*X*(1 - X/K);
    return(X_1);
}
# Incorproate into gmse_apply with standard gmse arguments
sim <- gmse_apply(res_mod = alt_res, X = 1000, stakeholders = 6); 
```

Problems or requests can be introduced on the GMSE GitHub [issues page](https://github.com/bradduthie/gmse/issues) or [Wiki](https://github.com/bradduthie/gmse/wiki), or emailed to [Brad Duthie](https://bradduthie.github.io/).
