#' GMSE simulation
#' 
#' The gmse function is the the primary function to call to run a simulation.
#' It calls other functions that run resource, observation, management, and user
#' models in each time step. Hence while individual models can be used on their
#' own, gmse() is really all that is needed to run a simulation. 
#'
#' 
#'@param time_max This value sets the maximum number of time steps for a simulation. There are no constraints for length of time that a simulation can run. The default is 100 time steps.
#'@param land_dim_1 This value sets the number of cells on the x dimension of the landscape (i.e., the number of columns in the landscape array; this can also be thought of as the x-axis when the landscape image is plotted). There is no maximum, but the minimum dimension of a landscape is 2 cells. The default is 100 cells.
#'@param land_dim_2 This value sets the number of cells on the x dimension of the landscape (i.e., the number of columns in the landscape array; this can also be thought of as the x-axis when the landscape image is plotted). There is no maximum, but the minimum dimension of a landscape is 2 cells. The default is 100 cells.
#'@param res_movement This value determines how far resources move during a time step. Exact movement is probabilistic and partly affected by `res_move_type` settings. Under default settings, during each time step, resources move from zero to res_movement cells away from their starting cell in any direction. Hence res_movement is the maximum distance away from a resources starting cell that it can move in a time step; other types of resource movement, however, interpret res_movement differently to get the raw distance moved (see res_move_type). The default value is 4.
#'@param remove_pr This value is the density-independent and user-independent probability of a resource being removed (e.g., dying) during a time step in the resource model. Under default settings, this value is set to zero, with resource removal being determined entirely by carrying capcity on resource survival, and by user actions. 
#'@param lambda This value is the baseline population growth rate of resources. Each resource in the simulation produces Poisson(lambda) offspring in one time step within the resource model. The value of lambda might be increased or decreased by user actions, and juvenile survival can potentially be decreased by a carrying capacity placed on birth. The default value is 0.3, meaning that the average resource produces one offspring every three time steps.
#'@param agent_view This value determines how far agents (managers and stakeholders) can see on the landscape. At the moment, this affects only the sampling ability of managers in the observation model for density-based and transect-based estimates of resource abundance. In these types of estimates, when managers have a higher agent_view, they are capable of observing a larger area of landscape and therefore of getting a larger (in the case of density-based estimation) or more efficient (in the case of transect-based estimation) sample of resources from which to estimate total resource abundance. The default value of agent_view is 20, so agents can see 20 cells away from their current cell in any direction.
#'@param agent_move This value determines how far agents can move. At the moment, this does not affect much in the simulation because agent movement does not affect agent actions (interactions with resources can be limited to stakeholder's owned land, but do not currently depend on where an agent is on the landscape -- effectively assuming that agents are mobile enough to do what they want to do to resources). The one exception is for density-based estimation, which can be biased by low values of agent_move by causing the manager to sample the same (or nearby) landscape cells to estimate total resource abundance; if resources are spatially autocorrelated, then managers might over or under-estimate total abundance. Therefore, as a default, this value is set to 50 so that managers can move to any cell on a (torus) landscape in a time step, removing any bias for density sampling.
#'@param res_birth_K This value is the carrying capacity on new resources added per time step (e.g., birth). If more offspring are born in a time step than res_birth_K, then offspring are randomly removed from the population until offspring born equals res_birth_K. By default, carrying capacity is effectively applied to death instead of birth, so the default value of res_birth_K is set to 10000 (and hence not enacted because the number of births is never this high).
#'@param res_death_K This value is the carrying capacity on resources in the population. Carrying capacity is realised by an increase in mortality probability as resource abundance approaches res_death_K. In each time step, realised mortality probability equals the number of resources over carrying capacity divided by the number of resources (i.e., [resource count - carrying capacity] / resource count). Hence, as the resource abundance increases above carrying capcity, mortality probability also increases in proportion, generating some stochasticity in resource survival. Note that carrying capacity is independent of user actions; if a user culls a resource this culling is applied after mortality probability due to carrying capacity has already been calculated. The default value for res_death_K is 400.
#'@param edge_effect This determines what happens at the edge of the landscape. Currently there is only one option (value 1), which causes the landscape to wrap around as a torus (effectively removing the edge); resources that leave off of one side of the landscape will reappear on the other side of the landscape. 
#'@param res_move_type This determines the type of movement that resources do. There are four different movement options: (0) No movement -- resources are sessile, (1) Uniform movement in any direction up to `res_movement` cells away during a time step. Movement direction is random and the cell distance moved is randomly selected from zero to `res_movement`. (2) Poisson selected movement in the x and y dimensions where distance in each direction is determined by Poisson(res_movement) and direction (e.g., left versus right) is randomly selected for each dimension. This type of movement tends to look a bit odd with low `res_movement` values because it results in very little diagonal movement. It also is not especially biologically realistic, so should probably not be used without a good reason. (3) Uniform movement in any direction up to `res_movement` cells away during a a time step `res_movement` times. In other words, the `res_movement` variable of each resource is acting to determine the times that a resource moves in a time step and the maximum distance it travels each time it moves. This type of movement has been simulated in ecological models, particularly plant-pollinator systems. The default movement type is (1).
#'@param res_birth_type The type of resource addition (birth) that occurs. Currently, the only value allowed is 2, which causes all resources to produce Poisson(lambda) offspring each time step, where `lambda` is the population growth rate also set as an argument in gmse simulations.
#'@param res_death_type The type of resource removal (death) that occurs. A value of (1) causes death to be entirely density-independent and with a probability of `removal_pr` for each resource (which may be further affected by agent actions or interactions with landscape cells). A value of (2) causes death to be density-dependent (though potentially independently affected by agents and landscape), with mortality probability calculated based on the carrying capacity `res_death_K` set in as an argument in gmse simulations. The default `res_death_type` is (2), as values of (1) must be used carefully because it can result in exponential growth that leads to massive population sizes that slow down simulations.
#'@param observe_type The type of observation sampling of resources being done by managers in the observation model. There are currently four options for sampling. (0) Density-based sampling, in which managers sample all resources within some subset of the landscape; the size of this subset is all of the resources within a distance of `agent_view` from the cell of the manager. Managers sample `times_observe` subsets, where `times_observe` is a parameter value set in the gmse simulation. Managers then extrapolate the density of resources in the subset to estimate the total number of resources on a landscape. (1) Mark-recapture estimate of the popuation, in which managers randomly sample `times_observe` resources in the population without any spatial bias (if there are fewer than `times_observe` resources, managers sample all resources) `times_observe` times with replacement. The first `fixed_observe` times are interpreted as marks, while the remaining times are interpreted as recaptures (note that `fixed_observe` must be less than `times_observe`). Hence if a resource is observed at any time in `fixed_observe` independent observations, then it is considered marked; if it is observed again at any time in `times_observe - fixed_observe` independent observations, then it is considered recaptured. A Chapman estimate is used in the manager model to estimate population size from these observation data. (2) Transect-based sampling (linear), in which a manager samples an entire row of the landscape and counts the resources on the row, then moves onto the next row of the landscape until the entire landscape has been covered. The number of cells in each row (i.e., the height) equals `agent_view`, so fewer transects are needed if agents can see farther. If `res_move_obs == TRUE`, then resources can move on the landscape between each transect sampling, potentially causing observation error if some resources are double counted or not counted at all due to movement. If `res_move_obs == FALSE`, then this type of observation should produce no error, and resource estimation will be exact. (3) Transect-based sampling (block), in which a manager samples a block of the landscape and counts the resources in the block, then moves on to the next (equally sized) block until the entire landscape has been covered. Blocks are square, with the length of each side equaling `agent_view`, so fewer blocks are needed if agents can see farther. If `res_move_obs == TRUE`, then resources can move on the landscape between each block sampling, potentially causing observation error if some resources are double counted or not counted at all due to movement. If `res_move_obs == FALSE`, then this type of observation should produce no error, and resource estimation will be exact. The default observation type is 0 for density-based sampling.
#'@param fixed_observe This parameter affects mark-recapture observation (i.e., applies only when observe_type == 1). Its value defines how many observations within the observation model for a single time step are interpreted as marks instead of recaptures; hence, this value must be less than `times_observe`.
#'@param times_observe This parameter defines how many times a manager will make observations within the observation model; it applies only to density-based sampling (`observe_type = 0`) and mark-recapture sampling (`observe_type = 1`). In the former case, the value determines how many times the manager goes out to sample resources from a subset of the landscape. In the latter case, the value determines how many times the manager goes out to attempt to find new resources to mark or recapture (hence its value must be greater than `fixed_observe`).
#'@param obs_move_type This determines the type of movement that agents do. The four different movement types of agents are identical to those of resources: : (0) No movement -- agents are sessile, (1) Uniform movement in any direction up to `agent_move` cells away during a time step. Movement direction is random and the cell distance moved is randomly selected from zero to `agent_move`. (2) Poisson selected movement in the x and y dimensions where distance in each direction is determined by Poisson(agent_move) and direction (e.g., left versus right) is randomly selected for each dimension. This type of movement tends to look a bit odd with low `agent_move` values because it results in very little diagonal movement. It also is not especially realistic, so should probably not be used without a good reason. (3) Uniform movement in any direction up to `agent_move` cells away during a a time step `agent_move` times. In other words, the `agent_move` variable of each agent is acting to determine the times that an agent moves in a time step and the maximum distance it travels each time it moves. This type of movement has been simulated in ecological models, particularly plant-pollinator systems. The default movement type is (1).
#'@param res_min_age This value defines the minimum age at which resources are recorded and acted upon by agents; below this age, resources are ignored. The default value of this parameter is 1, which means that offspring just produced during a time step (age = 0) are not observed or acted upon by agents. Note that if this value is set to zero such that newly added resources are counted, then the population might appear to go over carrying capacity regularly because carrying capacity is not realised until the next resource model if it applies to the death of resource (this is not a problem for the simulation itself, it just needs to be noted). 
#'@param res_move_obs This is a TRUE or FALSE value that defines whether or not resources are to move between `times_observe` times being observed. The default value is TRUE, but if the option is set to FALSE then it shuts down all resource movement during sampling (making `observe_type = 2` and `observe_type = 3` error free). 
#'@param Euclidean_dist This is a TRUE or FALSE value that defines whether distance in the simulation should be judged as number of cells away or the actual Euclidean distance between points (e.g., if the landscape were interpreted as a map). The default is set to FALSE, and until GMSE is capable of reading in real-world maps, I don't think there is any good reason to set it to TRUE.
#'@param plotting This is a TRUE or FALSE value that determines whether or not the simulation results will be plotted. The default is TRUE. If plotted, then a function is called to show the dynamics of resources and agent actinos over time. The plotted function plots the dynamics of GMSE resource, observation, managemer, and user models in six separate sub-panels. (1) Upper left panel: Shows the locations of resources on the landscape (black dots); landscape terrain is also shown in brown, but at the moment, this is only cosmetic and does not reflect anything occurring in the model. (2) Upper right panel: Shows ownership of land by agents; land is divided proportional based on parameters set in gmse() and colours correspond with other subplots. If agent utilities and actions are restricted to land (`land_ownership` in the gmse() function), then this gives some idea of where actions are being performed and where resources are affecting the landscape. (3) Middle left panel: Shows the actual population abundance (black solid line) and the population abundance estimated by the manager (blue solid line) over time. The dotted red line shows the resource carrying capacity (death-based) and the dotted blue line shows the target for resource abundance as set in the gmse() function; the orange line shows the total percent yield of the landscape (i.e., 100 percent means that resources have not decreased yield at all, 0 percent means that resources have completely destroyed all yield). (4) Middle right panel: Shows the raw landscape yield for each stakeholder (can be ignored if `land_ownership` is FALSE) over time; colours correspond to land ownership shown in the upper right panel. (5) Lower left panel: The cost of stakeholders performing actions over time, as set by the manager. (6) Lower right panel: The total number of actions performed by all stakeholders over time.
#'@param hunt This is a TRUE or FALSE value that determines whether the simulation will be halted each time step after `start_hunting` time steps to ask the user how many resources they want to hunt (some management information is given to help make this choice). This feature will be expanded upon in later versions. Right now, the human is playing the role of agent number 2, the first stake-holder in the simulation. By default, this value is set to FALSE.
#'@param start_hunting The time step in which the human (*not* the simulated agent) is allowed to start hunting if `hunt = TRUE`. The default value is 95.
#'@param res_consume The fraction of remaining biomass (e.g. crop production) that a resource consumes while occupying a landscape cell. The default value is 0.5, so if one resource occupies the cell, then landscape production is halved, if two resources occupy the cell, then landscape production drops to 0.25; if three, then production drops to 0.125, etc.
#'@param ga_popsize The size of populations of agents in the genetic algorithm (not resources in the simulation). The actions of each agent in the simulation are duplicated `ga_popsize` times, and this population of individual agent actions undergoes a process of natural selection to find an adaptive strategy. Selection is naturally stronger in larger populations, but a default population size of 100 is more than sufficient to find adaptive strategies. 
#'@param ga_mingen The minimum number of generations in the genetic algorithms of the simulation (*not* the number of time steps in the simulation itself). The actions of each agent in the simulation are duplicated `ga_popsize` times, and this population of individual agent actions undergoes a process of natural selection at least `ga_mingen` times to find an adaptive strategy. If convergence criteria `converge_crit` is set to a default value of 100, then the genetic algorithm will almost always continue for exactly `ga_mingen` generations. The default value is 20, which is usually plenty for finding adaptive agent strategies -- the objective is not to find optimal strategies, but strategies that are strongly in line with agent interests.
#'@param ga_seedrep At the start of each genetic algorithm, `ga_popsize` replicate agents are produced; `ga_seedrep` of these replicates are *exact* replicates, while the rest have random actions to introduce variation into the population. Because adaptive agent strategies are not likely to change wildly from one generation to the next, it is highly recommended to use some value of `ga_seedrep` greater than zero; the default value is 20, which does a good job of finding adaptive strategies.
#'@param ga_sampleK In the genetic algorithm, fitnesses are assgined to different agent strategies and compete in a tournament to be selected into the next generation. The tournament samples `ga_sampleK` strategies at random and with replacement from the population of `ga_popsize` to be included in the torunament. The default value is 20.
#'@param ga_chooseK In the genetic algorithm, fitnesses are assgined to different agent strategies and compete in a tournament to be selected into the next generation. The tournament samples `ga_sampleK` strategies at random and with replacement from the population of `ga_popsize` to be included in the torunament, and from these randomly selected strategies, the top `ga_chooseK` strategies are selected. The default value is 2, so the top 10 percent of the random sample in a tournament makes it into the next generation (note that multiple tournaments are run until `ga_popsize` strategies are selected for the next generation). 
#'@param ga_mutation In the genetic algorithm, this is the mutation rate of any action within an agent's strategy. When a mutation occurs, the action is either increased or decreased by a value of 1. If the action drops below zero, then the value after mutation is multiplied by -1.
#'@param ga_crossover In the genetic algorithm, this is the crossover rate of any action within an agent's strategy with a randomly selected different strategy in the population of size `ga_popsize`.
#'@param move_agents This is a TRUE or FALSE value that defines whether or not agents should move at the end of each time step. The default value is TRUE.
#'@param max_ages This is the maximum age of resources. If resources reach this age, then they are removed in the resource model with a probability of 1. The default `max_ages` is 5.
#'@param minimum_cost This is the mimimum cost of any action in the manager and user models. Higher values allow managers to have greater precision when setting policy. For example, managers believe (typically correctly) that they will double culling number by setting the cost of culling at 1 instead of 2. If actions always cost at least some minium value, then some increment just above that value is always available to more precisely affect user actions. Hence it is generally better to simply give everyone a bigger budget and set a minimum cost, giving more precision to managers to fine tune policy. The default value of minimum_cost is therefore set to 10.
#'@param user_budget This is the total budget of each stakeholder for performing actions. The cost of performing an action is determined by the `miminimum_cost` of actions, and the policy set by the manager. The default `user_budget` is 1000. The maximum budget is 10000.
#'@param manager_budget This is the total budget for the manager when setting policy. Higher budgets make it easier to restrict the actions of stakeholders; lower budgets make it more difficult for managers to limit the actions of stakeholders by setting policy. The default `manager_budget` is 1000. The maximum budget is 10000.
#'@param manage_target This is the target resource abundance that the manager attempts to keep the population at; the default value is 200.
#'@param RESOURCE_ini This is the initial abundance of resources at the start of the simulation; the default is 200.
#'@param scaring This is a TRUE or FALSE value determining whether or not scaring is an option for managers and stakeholders. If so, then stakeholders that scare cause resources to be moved from their current landscape cell to a random cell on the landscape (note, it is possible that the resource could be scared back onto the stakeholder's own land again). The default value of this is FALSE.
#'@param culling This is a TRUE or FALSE value determining whether or not culling is an option for managers and stakeholders. If so, then stakeholders that cull cause the resource to be removed from the simulation permanently (i.e., killing the resource). The default value of this is TRUE.
#'@param castration This is a TRUE or FALSE value determining whether or not castration is an option for managers and stakeholders. If so, then stakeholders that castrate do not remove the resource from the simulation, but prohibit the resource from reproducing by setting its `lambda` value to zero. The default value of this is FALSE.
#'@param feeding This is a TRUE or FALSE value determining whether or not feeding is an option for managers and stakeholders. If so, then stakeholders that feed increase a resource's growth rate (lambda) for one time step by 100 percent. The default value of this is FALSE.
#'@param help_offspring This is a TRUE or FALSE value determining whether or not feeding is an option for managers and stakeholders. If so, then stakeholders that help_offspring increase a resource's offspring production for one time step by one (i.e., one more offspring is produced). The default value of this is FALSE.
#'@param tend_crops This is a TRUE or FALSE value determining whether or not tending crops on the landscape is allowed for stakehodlers. If so, then stakeholders can increase one cells yield by 50 percent for each action to `tend_crops`. Actions on the landscape cannot be regulated by managers, so the cost of this action is always `minimum_cost`. The default value of this is FALSE.
#'@param kill_crops This is a TRUE or FALSE value determining whether or not killing crops on the landscape is allowed for stakehodlers. If so, then stakeholders can remove the crop yield on a cell completely for each action to `tend_crops`. Actions on the landscape cannot be regulated by managers, so the cost of this action is always `minimum_cost`.
#'@param stakeholders This is the number of stakeholders in a simulation; there is always one manager, and any natural number of stakeholders.
#'@param manage_caution This value moderates the caution a manager has when changing policy by assuming that at least `manage_caution` of each possible action will always be performed by stakeholders. I manager will therefore not ignore policy for one action because no stakeholder is engaging in it; the default value of `manage_caution` is 1.
#'@param land_ownership This value defines whether stakeholders own land and their actions are restricted to land that they own. If FALSE, then stakeholders can act on any landscape cell; if TRUE, then agents can only act on their own cells. The default of this value is TRUE.
#'@param manage_freq This is the frequency with which policy is set by managers; a value of 1 means that policy is set in the manager model every time step; a value of 2 means that poilcy is set in the manager model every other time step, etc. The default value is 1.
#'@param converge_crit This is the convergence criteria for terminating a genetic algorithm. After continuing for the minimum number of generations, `ga_mingen`, the genetic algorithm will terminate if the convergence criteria is met. Usually making this criteria low doesn't do much to improve adaptive strategies, so the default value is 100, which in practice cases the genetic algorithm to simply terminate after `ga_mingen` generations.
#'@return A large list is returned that includes detailed simulation histories for the resource, observation, management, and user models. This list includes eight elements, most of which are themselves complex lists of arrays: (1) A list of length `time_max` in which each element is an array of resources as they exist at the end of each time step. Resource arrays include all resources and their attributes (e.g., locations, growth rates, offspring, how they are affected by stakeholders, etc.). (2) A list of length `time_max` in which each element is an array of resource observations from the observation model. Observation arrays are similar to resource arrays, except that they can have a smaller number of rows if not all resources are observed, and they have additional columns that show the history of each resource being observed over the course of `times_observe` observations in the observation model. (3) A 2D array showing parameter values at each time step (unique rows); most of these values are static but some (e.g., resource number) change over time steps. (4) A list of length `time_max` in which each element is an array of the landscape that identifies proportion of crop production per cell. This allows for looking at where crop production is increased or decreased over time steps as a consequence of resource and stakeholder actions. (5) The total time the simulation took to run (not counting plotting time). (6) A 2D array of agents and their traits. (7) A list of length `time_max` in which each element is a 3D array of the costs of performing each action for managers and stakeholders (each agent gets its own array layer with an identical number of rows and columns); the change in costs of particular actions can therefore be be examined over time. (8) A list of length `time_max` in which each element is a 3D array of the actions performed by managers and stakeholders (each agent gets its own array layer with an identical number of rows and columns); the change in actions of agents can therefore be examined over time. Because the above lists cannot possibly be interpreted by eye all at once in the simulation output, it is highly recommended that the contents of a simulation be stored and interprted individually if need be; alternativley, simulations can more easily be interpreted through plots when `plotting = TRUE`.
#'@examples
#'sim <- gmse(lambda = 0.3, stakeholders = 10, land_ownership = TRUE);
#'@useDynLib GMSE
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon
#'@importFrom stats rnorm rpois
#'@export
gmse <- function( time_max       = 100,   # Max number of time steps in sim
                  land_dim_1     = 100,   # x dimension of the landscape
                  land_dim_2     = 100,   # y dimension of the landscape
                  res_movement   = 4,     # How far do resources move
                  remove_pr      = 0.0,   # Density independent resource death
                  lambda         = 0.30,  # Resource growth rate
                  agent_view     = 20,    # Number cells agent view around them
                  agent_move     = 50,    # Number cells agent can move
                  res_birth_K    = 10000, # Carrying capacity applied to birth
                  res_death_K    = 400,   # Carrying capacity applied to death
                  edge_effect    = 1,     # What type of edge on the landscape
                  res_move_type  = 1,     # What type of movement for resources
                  res_birth_type = 2,     # What type of birth for resources
                  res_death_type = 2,     # What type of death for resources
                  observe_type   = 0,     # Type of observation used
                  fixed_observe  = 20,    # How many obs (if type = 1)
                  times_observe  = 40,    # How many times obs (if type = 0)
                  obs_move_type  = 1,     # Type of movement for agents
                  res_min_age    = 1,     # Minimum age recorded and observed
                  res_move_obs   = TRUE,  # Move resources while observing
                  Euclidean_dist = FALSE, # Use Euclidean distance in view
                  plotting       = TRUE,  # Plot the results
                  hunt           = FALSE, # Does the user hunt resources?
                  start_hunting  = 95,    # What generation hunting starts
                  res_consume    = 0.5,   # Pr. landscape cell consumed by res
                  ga_popsize     = 100,   # Pop size in genetic algorithm
                  ga_mingen      = 20,    # Minimum generations in a ga run
                  ga_seedrep     = 20,    # How many copies to seed a ga with
                  ga_sampleK     = 20,    # Random sample size in ga tournament
                  ga_chooseK     = 2,     # Select from sample in ga tournament
                  ga_mutation    = 0.1,   # Mutation rate in genetic algorithm
                  ga_crossover   = 0.1,   # Crossover rate in genetic algorithm
                  move_agents    = TRUE,  # Move agents once per time step
                  max_ages       = 5,     # Maximum age of any resource(s)
                  minimum_cost   = 10,    # Minimum cost value
                  user_budget    = 1000,  # What is the budget of a user
                  manager_budget = 1000,  # The budget of a manager
                  manage_target  = 200,   # The target resource abundance
                  RESOURCE_ini   = 200,   # Number of initial resources
                  scaring        = FALSE, # Scaring allowed in simulations
                  culling        = TRUE,  # Culling/hunting allowed
                  castration     = FALSE, # Castration allowed
                  feeding        = FALSE, # Feeding resources allowed
                  help_offspring = FALSE, # Helping offspring allowed
                  tend_crops     = FALSE, # Tending crops allowed
                  kill_crops     = FALSE, # Killing crops allowed
                  stakeholders   = 4,     # Number of stake-holders
                  manage_caution = 1,     # Caution rate of the manager
                  land_ownership = FALSE, # Do stake-holders act on their land?
                  manage_freq    = 1,     # Frequency that management enacted
                  converge_crit  = 100,   # Convergence criteria
                  manager_sense  = 0.1    # Manager sensitivity
){
    
    if(observe_type == 1 & times_observe < 2){
        stop("Need to observe at least twice for mark-recapture");   
    }
    if(user_budget > 10000 | manager_budget > 10000){
        stop("User and manager budgets cannot exceed 10000");
    }
    
    user_res_opts  <- c(scaring, culling, castration, feeding, help_offspring);
    user_lnd_opts  <- c(tend_crops, kill_crops);
    pop_model      <- "IBM";
    movement       <- res_movement;
    res_types_ini  <- 1;
    time           <- 0;
    
    proc_start       <- proc.time();
    proc_check_start <- proc_start;
    
    # Set the landscape
    land_alloc   <- c(0, rep(x = 1/stakeholders, times = stakeholders));
    LANDSCAPE_r  <- make_landscape( model       = pop_model, 
                                    rows        = land_dim_1, 
                                    cols        = land_dim_2, 
                                    cell_types  = 1,
                                    cell_val_mn = 1,
                                    cell_val_sd = 0,
                                    ownership   = 1:(stakeholders + 1),
                                    owner_pr    = land_alloc
    );
    # Set the starting conditions for one resource
    starting_resources <- make_resource( model              = pop_model, 
                                         resource_quantity  = RESOURCE_ini,
                                         resource_types     = res_types_ini,
                                         rows               = land_dim_1,
                                         cols               = land_dim_2,
                                         move               = movement,
                                         rm_pr              = remove_pr,
                                         lambda             = lambda,
                                         consumption_rate   = res_consume,
                                         max_age            = max_ages[1]
    );
    # This will obviously need to be changed -- new function in initialise.R
    AGENTS   <- make_agents( model        = pop_model,
                             agent_number = 1 + stakeholders,
                             type_counts  = c(1, stakeholders),
                             vision       = agent_view,
                             rows         = land_dim_1,
                             cols         = land_dim_2,
                             move         = agent_move
    ); 
    
    Jacobian <- make_interaction_array(RESOURCES = starting_resources,
                                       LAND      = LANDSCAPE_r
    );
    Jacobian[1,2] <- -1 * res_consume; # Temporary to fix consumption rate
    
    interaction_tabl <- make_interaction_table(starting_resources, LANDSCAPE_r);
    
    COST   <- make_costs( AGENTS = AGENTS, RESOURCES = starting_resources,
                          res_opts = user_res_opts, lnd_opts = user_lnd_opts,
                          min_cost = minimum_cost);
    ACTION <- make_utilities( AGENTS = AGENTS, RESOURCES = starting_resources);
    if(land_ownership == TRUE){
        ACTION[1,5:7,] <- 1;
        ACTION[2,5:7,] <- 1;
    }
    ACTION[3,5:7,1]  <- 0;
    ACTION[1,5,2:5]  <- 0;
    ACTION[1,5,1]    <- 100;
    ACTION[2,5,2:5]  <- 100;
    ACTION[2,5,3]    <- 100;
    ACTION[1,5,1]    <- manage_target;

    AGENTS[,17]     <- user_budget;
    AGENTS[1,17]    <- manager_budget;
    
    time       <- time + 1;  # Ready for the initial time step.
    cells      <- land_dim_1 * land_dim_2; # Number of cells in the landscape
    
    ldx <- land_dim_1;
    ldy <- land_dim_2;
    ldz <- dim(LANDSCAPE_r)[3];
    rbK <- res_birth_K;
    rdK <- res_death_K;
    edg <- edge_effect;
    r_m <- res_move_type;
    rbt <- res_birth_type;
    rdt <- res_death_type;
    obt <- observe_type;
    fxo <- fixed_observe;
    tmo <- times_observe;
    o_m <- obs_move_type;
    rma <- res_min_age;
    rmo <- res_move_obs;
    Euc <- Euclidean_dist;
    prc <- res_consume;
    gap <- ga_popsize;
    gam <- ga_mingen;
    gas <- ga_seedrep;
    gal <- ga_sampleK;
    gac <- ga_chooseK;
    gmu <- ga_mutation;
    gcr <- ga_crossover;
    mva <- move_agents;
    mxa <- max_ages;
    rsi <- dim(starting_resources)[1];
    ttr <- dim(starting_resources)[2];
    agn <- dim(AGENTS)[1];
    agt <- dim(AGENTS)[2];
    lkr <- dim(interaction_tabl)[1];
    lyr <- dim(ACTION)[3];
    roc <- dim(ACTION)[1];
    coc <- dim(ACTION)[2];
    aav <- user_res_opts;
    alv <- user_lnd_opts;
    mnc <- minimum_cost;
    usb <- user_budget;
    mac <- manage_caution;
    cnv <- converge_crit;
    mas <- manager_sense;

    paras <- c(time,    # 0. The dynamic time step for each function to use 
               edg,     # 1. The edge effect (0: nothing, 1: torus)
               r_m,     # 2. Res movement (0: none, 1: unif, 2: Poisson, ...)
               rbt,     # 3. Type of birth (0: none, 1: uniform, 2: Poisson)
               rdt,     # 4. Type of death (0: none, 1: uniform, 2: K-based)
               rbK,     # 5. Carrying capacity for birth (-1 = unregulated)
               rdK,     # 6. Carrying capacity for death (-1 = unregulated)
               0,       # 7. The type of AGENT doing the observations
               obt,     # 8. The type of observing done for estimating pop.
               1,       # 9. The type of resource observed (note: dynamic)
               fxo,     # 10. Fix mark? Do observers mark exactly n resources?
               tmo,     # 11. Times resources observed during one time step
               ldx,     # 12. Land dimension on the x axis
               ldy,     # 13. Land dimension on the y axis
               o_m,     # 14. Agent movement (option same as #2)
               1,       # 15. Type category for resource observation
               rma,     # 16. Minimum age of sampling (1 excludes juveniles)
               1,       # 17. Type category for agent observation (default = 1)
               12,      # 18. Column where res seen recorded in agent array
               rmo,     # 19. Move resources while observing (0/1 = N/Y)
               Euc,     # 20. Distance is Euclidean (1) or within-cell (0)
               gap,     # 21. Population size set in the genetic algorithm
               gam,     # 22. Minimum number of generations to run a ga
               gas,     # 23. Number of replicate agents to seed a ga with
               gal,     # 24. The number of sample agents for tournament in ga
               gac,     # 25. The number of selected agents in a ga tournament
               gmu,     # 26. The mutation rate of loci in the genetic algorithm
               gcr,     # 27. The crossover rate in the genetic algorithm
               mva,     # 28. Move agents once per time step
               mxa,     # 29. Maximum age of resources
               7,       # 30. The column of the time trait in the resource array
               11,      # 31. The column for storing age in the resource array
               rsi,     # 32. The number of resources in the model
               4,       # 33. The column for resource x location on landscape
               5,       # 34. The column for resource y location on landscape
               6,       # 35. The column for the movement parameter for resource
               ldz,     # 36. The number of layers (3D depth) in the landscape
               9,       # 37. The column in resource array for growth parameter
               10,      # 38. The column in the resource array for offspring
               18,      # 39. The column to adjust the growth rate helpem
               19,      # 40. The column to adjust the offspring feedem
               ttr,     # 41. Total columns in the resource array
               17,      # 42. The column to adjust the death resource column
               8,       # 43. The column in resource array affecting removal
               1,       # 44. A column to refer to a resource type as needed
               1,       # 45. A type of resource to do interacting with land
               15,      # 46. The column in a resource array affected by land
               14,      # 47. The column in resource array of land effect size
               1,       # 48. The landscape layer interacting with a resource
               4,       # 49. The column for the agent's x location on landscape
               5,       # 50. The column for the agent's y location on landscape
               6,       # 51. The column for the movement parameter for agents
               10,      # 52. The column in agent array where marks accumulate
               12,      # 53. The column in resource array where marks accrue
               agn,     # 54. The total number of agents in the model
               agt,     # 55. The total number of agent traits (cols)
               1,       # 56. The column of resource type 1
               2,       # 57. The column of resource type 2
               3,       # 58. The column of resource type 3
               13,      # 59. The tally column of the resource array
               lkr,     # 60. The number of rows in the lookup array
               rsi,     # 61. The number of rows in the observation array
               ttr+tmo, # 62. The number of cols int he observation array
               1,       # 63. The ID of the managing agent (usually 1)
               0,       # 64. The layer of ACTION and COST where manager located
               lyr,     # 65. The number of layers in ACTION and COST arrays
               lkr - 1, # 66. The number of rows for setting action costs
               8,       # 67. The view column of the agent array
               roc,     # 68. The number of rows in the COST and ACTION arrays
               coc,     # 69. The number of cols in the COST and ACTION arrays
               4,       # 70. Col actions vary for other individuals in ga
               7,       # 71. Col actions vary for self individuals in ga
               0,       # 72. Total actions in the action array
               16,      # 73. The column to adjust the castration of a resource
               0,       # 74. Manager's projected change if resource moved
               -1*mas,  # 75. Manager's projected change if resource killed
               -1*mas,  # 76. Manager's projected change if resource castrated
               1*mas,   # 77. Manager's projected change if resource growth +
               1*mas,   # 78. Manager's projected change if resource offspring +
               0.50,    # 79. User's improvement of land (proportion)
               1,       # 80. Landscape layer on which crop yield is located
               2,       # 81. Landscape layer on which ownership is defined
               15,      # 82. Column in agent array where cell yield recorded
               0,       # 83. Temporary element to hold action array row
               0,       # 84. Temporary element to hold action array col
               0,       # 85. Temporary element to hold action array layer
               0,       # 86. Effect of landscape cell on resource growth rate
               0,       # 87. Effect of landscape cell on resource death rate
               aav[1],  # 88. Is the movement option available?
               aav[2],  # 89. Is the killing option available?
               aav[3],  # 90. Is the castration option available?
               aav[4],  # 91. Is the feedem option available?
               aav[5],  # 92. Is the helpem option availabe?
               alv[1],  # 93. Is the kill crop production option available?
               alv[2],  # 94. Is the increase crop growth option available?
               mac,     # 95. How many actions should managers assume exist?
               mnc,     # 96. What is the minimum cost for any action?
               usb,     # 97. The user budget
               cnv,     # 98. The convergence criteria of the genetic algorithm
               rsi,     # 99. Estimate of res type 1 from the observation model
               0,       # 100. Upper CI for res type 1 estimate
               0        # 101. Lower CI for res type 1 estimate
    );
    RESOURCE_REC    <- NULL;
    RESOURCES       <- starting_resources;
    OBSERVATION_REC <- NULL;
    AGENT_REC       <- NULL;
    LANDSCAPE_INI   <- LANDSCAPE_r;
    LANDSCAPE_REC   <- NULL;
    COST_REC        <- NULL;
    ACTION_REC      <- NULL;
    PARAS_REC       <- matrix(data=0, ncol = length(paras), nrow = time_max-1);
    
    print("Initialising simulations ... ");
    
    while(time < time_max){
        
        if(move_agents == TRUE){
            AGENTS[1,5] <- sample(x = 1:ldx, size = 1); # Move manager randomly
            AGENTS[1,6] <- sample(x = 1:ldy, size = 1); 
        }
        
        RESOURCE_NEW      <- resource(RESOURCES  = RESOURCES,
                                      LAND       = LANDSCAPE_r,
                                      PARAS      = paras,
                                      model      = "IBM"
        ); 
        RESOURCES             <- RESOURCE_NEW[[1]];
        LANDSCAPE_r           <- RESOURCE_NEW[[2]];
        paras                 <- RESOURCE_NEW[[3]];
        
        OBSERVATION_NEW   <- observation(RESOURCES  = RESOURCES,
                                         LAND       = LANDSCAPE_r,
                                         PARAS      = paras,
                                         AGENTS     = AGENTS,
                                         inter_tabl = interaction_tabl,
                                         fix_mark   = fxo,
                                         times      = tmo,
                                         samp_age   = rma,
                                         agent_type = 0,
                                         type_cat   = 1,
                                         obs_method = obt,
                                         move_res   = rmo
        );
        OBSERVATION_r  <- OBSERVATION_NEW[[1]];
        paras          <- OBSERVATION_NEW[[3]];
        
        AGENTS_NEW        <- anecdotal(RESOURCES   = RESOURCES,
                                       LAND        = LANDSCAPE_r,
                                       PARAS       = paras,
                                       AGENTS      = AGENTS,
                                       res_type    = 1,
                                       samp_age    = rma,
                                       agent_type  = -1,
                                       type_cat    = 1,
                                       move_agents = mva
        );
        AGENTS <- AGENTS_NEW[[1]];

        if(time %% manage_freq == 0){
            MANAGER  <- manager(RESOURCES   = RESOURCES,
                                AGENTS      = AGENTS,
                                LAND        = LANDSCAPE_r, 
                                PARAS       = paras,
                                COST        = COST,
                                ACTION      = ACTION,
                                INTERACT    = Jacobian,
                                inter_tabl  = interaction_tabl,
                                OBSERVATION = OBSERVATION_r,
                                model       = "IBM"
            );
            ACTION <- MANAGER[[4]];
            COST   <- MANAGER[[5]];
        }
        
        USERS <- user(RESOURCES  = RESOURCES,
                      AGENTS     = AGENTS,
                      LAND       = LANDSCAPE_r, 
                      PARAS      = paras,
                      COST       = COST,
                      ACTION     = ACTION,
                      INTERACT   = Jacobian,
                      inter_tabl = interaction_tabl,
                      model      = "IBM"
        );
        RESOURCES    <- USERS[[1]];
        AGENTS       <- USERS[[2]];
        LANDSCAPE_r  <- USERS[[3]];
        ACTION       <- USERS[[4]];
        COST         <- USERS[[5]];
        
        RESOURCE_REC[[time]]     <- RESOURCES;
        OBSERVATION_REC[[time]]  <- OBSERVATION_NEW[[1]];
        AGENT_REC[[time]]        <- AGENTS;
        LANDSCAPE_REC[[time]]    <- LANDSCAPE_r[,,2];
        COST_REC[[time]]         <- COST;
        ACTION_REC[[time]]       <- ACTION;
        PARAS_REC[time,]         <- paras;
        
        LANDSCAPE_r <- age_land(LAND          = LANDSCAPE_r, 
                                landscape_ini = LANDSCAPE_INI, layer = 2);
        
        time              <- time + 1;
        paras[1]          <- time;
        if(dim(RESOURCES)[1] < 10){
            print("Extinction has occurred");
            break;
        }
        proc_check_end  <- proc.time();
        time_taken      <- proc_check_end - proc_check_start;
        if(time_taken[3] > 5){
            print(paste("Generation ", time, "of ", time_max));
            proc_check_start <- proc.time();
        }

        if(hunt == TRUE & time > start_hunting){
            HUNT_OUTCOME <- be_hunter(OBSERVATION_r, AGENTS, RESOURCES, 
                                      LANDSCAPE_r, paras, agent_view,
                                      times_observe);
            RESOURCES    <- HUNT_OUTCOME$RESOURCES;
            paras        <- HUNT_OUTCOME$PARAS;
        }
    }
    
    res_columns <- c("Resource_ID",
                     "Resource_type_1",
                     "Resource_type_2",
                     "Resource_type_3",
                     "Resource_loc_x",
                     "Resource_loc_y",
                     "Resource_move",
                     "Resource_time",
                     "Resource_rm_pr",
                     "Resource_growth",
                     "Resource_grown",
                     "Resource_age",
                     "Resource_marked",
                     "Resource_tally",
                     "Consumption_rate",
                     "Adjust_removal",
                     "Adjust_growth",
                     "Adjust_grown",
                     "temp_adj1",
                     "temp_adj2"
    );
    colnames(RESOURCES)    <- res_columns;
    
    proc_end   <- proc.time();
    total_time <- proc_end - proc_start;
       
    sim_results <- list(resource    = RESOURCE_REC,
                        observation = OBSERVATION_REC,
                        paras       = PARAS_REC,
                        land        = LANDSCAPE_REC,
                        time_taken  = total_time,
                        agents      = AGENTS,
                        cost        = COST_REC,
                        action      = ACTION_REC
                        );
    
    if(plotting == TRUE){
        if(obt == 0){
            case01plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       paras  = paras,
                       ACTION = ACTION_REC,
                       COST   = COST_REC,
                       view   = agent_view,
                       times  = times_observe);
        }
        if(obt == 1){
            case01plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1],
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       ACTION = ACTION_REC,
                       COST   = COST_REC,
                       paras  = paras);
        }
        if(obt == 2){
            case23plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       ACTION = ACTION_REC,
                       paras  = paras);
        }
        if(obt == 3){
            case23plot(res    = RESOURCE_REC, 
                       obs    = OBSERVATION_REC, 
                       land1  = LANDSCAPE_r[,,1], 
                       land2  = LANDSCAPE_REC,
                       land3  = LANDSCAPE_r[,,3],
                       agents = AGENT_REC,
                       ACTION = ACTION_REC,
                       paras  = paras);
        }
    }

    return(sim_results);
}
################################################################################
