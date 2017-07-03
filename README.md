# GMSE [![](http://www.r-pkg.org/badges/version/GMSE)](https://cran.r-project.org/package=GMSE) [![](http://cranlogs.r-pkg.org/badges/grand-total/GMSE)](http://cranlogs.r-pkg.org/badges/grand-total/GMSE)

Generalised Management Strategy Evaluation
--------------------------------------------------------------------------------

**The GMSE package integrates game theory and ecological theory to construct social-ecological models that simulate the management of populations and stakeholder actions. These models build off of a previously developed management strategy evaluation (MSE) framework to simulate all aspects of management: population dynamics, manager observation of populations, manager decision making, and stakeholder responses to management decisions. The newly developed generalised management strategy evaluation (GMSE) framework uses genetic algorithms to mimic the decision-making process of managers and stakeholders under conditions of change, uncertainty, and conflict. All simulations can be run using the `gmse` function.**

--------------------------------------------------------------------------------

## Installation

**Install from CRAN**

To install [this package](https://cran.r-project.org/web/packages/GMSE/) from CRAN.

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

To run a simulation, use the `gmse()` function.

```
sim <- gmse();
```

Optional arguments taken by `gmse` are used to specify simulation parameter values. Simulation results will be plotted automatically given the default `plotting = TRUE`, but can be plotted again using the `plot_gmse_results` function.

```
plot_gmse_results(res = sim$resource, obs = sim$observation, land = sim$land, 
agents = sim$agents, paras = sim$paras, ACTION = sim$action, COST = sim$cost)
```

Problems or requests can be introduced on the GMSE GitHub [issues page](https://github.com/bradduthie/gmse/issues) or [Wiki](https://github.com/bradduthie/gmse/wiki), or emailed to [Brad Duthie](https://bradduthie.github.io/).