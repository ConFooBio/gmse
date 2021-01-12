![](https://raw.githubusercontent.com/bradduthie/gmse/1727ea37f32f0f40df8ee6e8277d0d1723c88639/notebook/images/GMSE_logo_name.png)

[![](http://www.r-pkg.org/badges/version/GMSE?color=yellowgreen)](https://cran.r-project.org/package=GMSE) [![](http://cranlogs.r-pkg.org/badges/grand-total/GMSE?color=yellowgreen)](http://cranlogs.r-pkg.org/badges/grand-total/GMSE)
[![](http://cranlogs.r-pkg.org/badges/last-month/GMSE?color=yellowgreen)](http://cranlogs.r-pkg.org/badges/last-month/GMSE)


Generalised Management Strategy Evaluation
--------------------------------------------------------------------------------

**The GMSE package integrates game theory and ecological theory to construct social-ecological models that simulate the management of populations and stakeholder actions. These models build off of a previously developed management strategy evaluation (MSE) framework to simulate all aspects of management: population dynamics, manager observation of populations, manager decision making, and stakeholder responses to management decisions. The newly developed generalised management strategy evaluation (GMSE) framework uses genetic algorithms to mimic the decision-making process of managers and stakeholders under conditions of change, uncertainty, and conflict. Simulations can be run using gmse(), gmse_apply(), and gmse_gui() functions. For more, see [the GMSE website](https://confoobio.github.io/gmse/index.html).**

--------------------------------------------------------------------------------

*This project has received funding from the [European Union's Horizon 2020 research and innovation programme](http://ec.europa.eu/programmes/horizon2020/) under grant agreement No 679651 to [Nils Bunnefeld](https://sti-cs.org/nils-bunnefeld/). Package maintainer [Brad Duthie](https://bradduthie.github.io/) is currently funded by a [Leverhulme Trust](https://www.leverhulme.ac.uk/) [ECF](https://www.leverhulme.ac.uk/funding/grant-schemes/early-career-fellowships).*

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
install_github("ConFooBio/GMSE")
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

This function is also available as a [standalone application](https://bradduthie.shinyapps.io/gmse_gui) in shiny.

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

## Documentation

For additional help in getting started with GMSE, the following vignettes are available.

- **GMSE introduction**: A general overview of GMSE, including a short example simulation and how to interpret the output.
- **The genetic algorithm**: Further details on what the genetic algorithm is and how it works in GMSE.
- **Use of gmse_apply**: An extended introduction to the `gmse_apply` function, and how to use it to further customise simulations.
- **Example case study**: An example case study illustrating how `gmse` or `gmse_gui` could be used to simulate a scenario of conflict between farmers and waterfowl conservation.
- **Advanced options**: Example use of `gmse_apply` in the farmer and waterfowl case study demonstrating advanced customisation that is possible in GMSE.
- **Fisheries integration**: Introduction to how GMSE could be linked to other packages such as the [Fisheries Library in R](http://www.flr-project.org/) (FLR) using `gmse_apply`.
- **Replicate simulations**: Introduction to the use of replicate simulatoins for model inference, with an example of how this can be done in GMSE.
- **GMSE data structures**: Detailed explanation of key data structures used in GMSE.

Descriptions of individual GMSE functions are provided in the [GMSE documentation](https://cran.r-project.org/package=GMSE/GMSE.pdf) available on CRAN.

## Reference

Duthie, A. B., Cusack, J. J., Jones, I. L., Nilsen, E. B., Pozo, R. A., Rakotonarivo, O. S., Van Moorter, B, and Bunnefeld, N. (2018). GMSE: an R package for generalised management strategy evaluation. *Methods in Ecology and Evolution*. https://doi.org/10.1101/221432




