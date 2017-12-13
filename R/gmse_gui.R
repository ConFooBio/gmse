#' GMSE GUI function
#' 
#' The gmse_gui function will call a browser-based graphical user interface 
#' (GUI) for the gmse function. The GUI will run simulations for a limited range
#' of parameter values and present results as plots.
#'
#'@return A browser should immediately open with the gmse graphical user
#'interface
#'@examples
#'\dontrun{
#'sim <- gmse_gui();
#'}
#'@useDynLib GMSE
#'@importFrom grDevices topo.colors
#'@importFrom graphics abline axis image mtext par plot points polygon
#'@importFrom stats rnorm rpois
#'@importFrom shiny div p icon headerPanel column hr sliderInput selectInput
#'@importFrom shiny radioButtons actionButton plotOutput eventReactive runApp
#'@importFrom shiny shinyApp renderPlot uiOutput renderTable tableOutput
#'@importFrom shiny fluidPage tags br
#'@importFrom shinydashboard dashboardSidebar sidebarMenu menuItem dashboardBody
#'@importFrom shinydashboard tabItems tabItem dashboardHeader dashboardPage
#'@importFrom shinyjs useShinyjs
#'@export
gmse_gui <- function(){ 

#-------------------------------------------------------------------------------
    skin <- Sys.getenv("DASHBOARD_SKIN")
    skin <- tolower(skin)
    if (skin == "") skin <- "green"
    
    menuIconClass <- "fa-lg" 
    # This makes the sidebar of options and settings (e.g., parameters)
    sidebar <-   dashboardSidebar(
        useShinyjs(),
        sidebarMenu(id = "tab",
                    div(style = "background: rgb(0, 100, 0); height: 50px",
                        p(icon("cogs"),"GMSE", 
                          style = "font-size: 200%; padding-left:50px;padding-top:5px")),
                    
                    menuItem("Global parameters", tabName = "global", 
                             icon = icon("globe", class=menuIconClass)),
                    
                    div(align="center"),
                    
                    menuItem("Resource model", tabName = "resource", 
                             icon = icon("tree", class=menuIconClass)),
                    
                    div(align="center"),
                    
                    menuItem("Observation model", tabName = "observation", 
                             icon = icon("binoculars", class=menuIconClass)),
                    
                    div(align="center"),
                    
                    menuItem("Manager model", tabName = "manager", 
                             icon = icon("balance-scale", class=menuIconClass)),
                    
                    div(align="center"),
                    
                    menuItem("User model", tabName = "user", 
                             icon = icon("users", class=menuIconClass)),
                    
                    div(align="center"),
                    
                    menuItem("Genetic algorithm", tabName = "ga", 
                             icon = icon("gamepad", class=menuIconClass)),
                    
                    div(align="center"),
                    
                    menuItem("Run simulation", tabName = "run", 
                             icon = icon("play", class=menuIconClass)),
                    
                    uiOutput("sim_output"),
                    
                    hr(style ="width: 70%; color: white; align: center"),
                    
                    menuItem("General results", icon = icon("line-chart"), 
                             tabName = "plotting"),
                    
                    menuItem("Confict results", icon = icon("meh-o"), 
                             tabName = "plot_effort"),
                    
                    menuItem("Resource table", icon = icon("leaf"), 
                             tabName = "resource_tab"),
                    
                    menuItem("Cost table", icon = icon("euro"), 
                             tabName = "cost_tab"),
                    
                    menuItem("Action table", icon = icon("wrench"), 
                             tabName = "action_tab"),
                    
                    div(align="center"),
                    
                    hr(style ="width: 70%; color: white; align: center"),
                    
                    menuItem("Getting started", tabName = "help", 
                             icon = icon("question-circle", class=menuIconClass)),
                    
                    menuItem("Source code for app", icon = icon("code"),
                             href = "https://github.com/bradduthie/gmse"
                    ),
                    
                    menuItem("ConFooBio project", icon = icon("circle"),
                             href = "https://sti-cs.org/confoobio/"
                    ),
                    
                    div(uiOutput("Export"), style = "text-align: center")
        )
    )
    
    
    body <- dashboardBody(
        fluidPage(
            tabItems(
                tabItem("global",
                        
                        headerPanel(title = "Global GMSE parameters"),
                        
                        column(width = 6, offset = 0, style='padding:0px;',
                               
                               hr(),
                               
                               tags$div(title="This value sets the maximum number of time steps for a simulation.",   
                                        sliderInput("time",
                                                    "Number of generations",
                                                    min = 10,
                                                    max = 200,
                                                    step  = 10,
                                                    value = 100)
                               ),
                               
                               tags$div(title="This value sets the number of cells on the x dimension of the landscape (i.e., the number of columns in the landscape array; this can also be thought of as the x-axis when the landscape image is plotted). ", 
                                        sliderInput("land_dim_1",
                                                    "X dimension length (in cells)",
                                                    min = 10,
                                                    max = 400,
                                                    step = 10,
                                                    value = 100)
                               ),
                               
                               tags$div(title="This value sets the number of cells on the y dimension of the landscape (i.e., the number of columns in the landscape array; this can also be thought of as the y-axis when the landscape image is plotted). ", 
                                        sliderInput("land_dim_2",
                                                    "Y dimension length (in cells)",
                                                    min = 10,
                                                    max = 400,
                                                    step = 10,
                                                    value = 100)
                               ),
                               
                               tags$div(title="The proportion of the landscape that will be public, and not owned by users The remaining proportion of the landscape will be evenly divided among users. Note that this option is only available when users own land.", 
                                        sliderInput("public_land",
                                                    "Proportion of land that is public",
                                                    min = 0,
                                                    max = 1,
                                                    step = 0.01,
                                                    value = 0)
                               )
                               
                        )
                        
                ),
                tabItem("resource",
                        
                        headerPanel(title = "Resource model parameters"),
                        
                        column(width = 6, offset = 0, style='padding:0px;',
                               
                               hr(),
                               
                               tags$div(title="This determines the type of movement that resources do. There are four different movement options: \n\t 1. Uniform movement in any direction during a time step. Movement direction is random and the cell distance moved is randomly selected up to a maximum movement value. \n\t 2. Poisson selected movement in the x and y dimensions where distance in each direction is determined by a Poisson function and direction (e.g., left versus right) is randomly selected for each dimension. This type of movement tends to look a bit odd with low resource movement values because it results in very little diagonal movement. It also is not especially biologically realistic, so should probably not be used without a good reason. \n\t 3. Uniform movement in any direction up to 'Resource movement' cells away during a a time step 'Resource movement' times. In other words, the 'Resource movement' variable (see below) determines the times that a resource moves in a time step and the maximum distance it travels each time it moves. This type of movement has been simulated in ecological models, particularly plant-pollinator systems. \n\t 4. No movement, in which resources are sessile.",   
                                        selectInput("res_move_type", "Resource movement type:",
                                                    c( "Uniform movement in any direction" = "1",
                                                       "Poisson movement in x and y" = "2",
                                                       "Uniform movement in any direct N times"= "3",
                                                       "No movement"        = "0")
                                        )
                               ),
                               
                               tags$div(title="The type of resource removal (death) that occurs. \n\t 1. Density-independent causes a fixed probability removal for each resource (which may be further affected by agent actions or interactions with landscape cells). \n\t 2. Density-dependent removal causes mortality probability to be calculated based on resource carrying capacity (though potentially independently affected by agents and landscape). \n\t Density-independent mortality must be used carefully because it can result in exponential growth that leads to massive population sizes that slow down simulations.",                 
                                        selectInput("res_death_type", "Resource death type:",
                                                    c("Density-dependent" = "2", 
                                                      "Density-independent"   = "1")
                                        )
                               ),
                               
                               tags$div(title="This value determines how far resources can move during a time step. Exact movement is probabilistic and partly affected by 'Resource movement type' settings. Typically, this is the maximum distance away from a resources starting cell that it can move in a time step; other types of resource movement, however, interpret res_movement differently to get the raw distance moved (see above).", 
                                        sliderInput("res_movement",
                                                    "Resource movement",
                                                    min   = 0,
                                                    max   = 50,
                                                    step  = 1,
                                                    value = 20)
                               ),
                               
                               tags$div(title="This value is the density-independent and user-independent probability of a resource being removed (e.g., dying) during a time step in the resource model. By default, this value is set to zero, with resource removal being determined entirely by carrying capcity on resource survival, and by user actions.",                     
                                        sliderInput("remove_pr",
                                                    "Density-independent resource death",
                                                    min   = 0,
                                                    max   = 1,
                                                    step  = 0.01,
                                                    value = 0)
                               ),
                               
                               tags$div(title="This value is the baseline population growth rate of resources. Each resource in the simulation produces offspring in one time step within the resource model, and this value is the rate parameter of a Poisson random function from which an offspring number is sampled. The value of lambda might be increased or decreased by user actions, and juvenile survival can potentially be decreased by a carrying capacity placed on birth.",
                                        sliderInput("lambda_gr",
                                                    "Resource growth rate",
                                                    min   = 0,
                                                    max   = 4,
                                                    step  = 0.01,
                                                    value = 0.30)
                               ),
                               
                               tags$div(title="This value is the carrying capacity on new resources added per time step (e.g., birth). If more offspring are born in a time step than this value, then offspring are randomly removed from the population until offspring born equals the birth carrying capacity.",
                                        sliderInput("res_birth_K",
                                                    "Carrying capacity applied to birth",
                                                    min   = 10,
                                                    max   = 10000,
                                                    step  = 10,
                                                    value = 10000)
                               ),
                               
                               tags$div(title="This value is the carrying capacity on resources in the population. Carrying capacity is realised by an increase in mortality probability as resource abundance approaches carrying capacity. In each time step, realised mortality probability equals the number of resources over carrying capacity divided by the number of resources (i.e., [resource count - carrying capacity] / resource count). Hence, as the resource abundance increases above carrying capcity, mortality probability also increases in proportion, generating some stochasticity in resource survival. Note that carrying capacity is independent of user actions; if a user culls a resource this culling is applied after mortality probability due to carrying capacity has already been calculated.",
                                        sliderInput("res_death_K",
                                                    "Carrying capacity applied to death",
                                                    min   = 10,
                                                    max   = 10000,
                                                    step  = 10,
                                                    value = 2000)
                               ),
                               
                               tags$div(title="The fraction of remaining biomass (e.g. crop production) that a resource consumes while occupying a landscape cell. The default value is 0.5, so if one resource occupies the cell, then landscape production is halved, if two resources occupy the cell, then landscape production drops to 0.25; if three, then production drops to 0.125, etc.",
                                        sliderInput("res_consume",
                                                    "Proportion of landscape cell consumed",
                                                    min   = 0,
                                                    max   = 1,
                                                    step  = 0.01,
                                                    value = 0.5)
                               ),
                               
                               tags$div(title="This is the initial abundance of resources at the start of the simulation.", 
                                        sliderInput("RESOURCE_ini",
                                                    "Initial population size",
                                                    min   = 10,
                                                    max   = 10000,
                                                    step  = 10,
                                                    value = 1000)
                               )
                               
                        )
                        
                ),
                tabItem("observation",
                        
                        headerPanel(title = "Observation model parameters"),
                        
                        column(width = 6, offset = 0, style='padding:0px;',
                               
                               hr(),
                               
                               tags$div(title="The type of observation sampling of resources being done by managers in the observation model. There are currently four options for sampling. \n\t 1. Density-based sampling, in which managers sample all resources within some subset of the landscape; the size of this subset is all of the resources within viewing distance of the cell of the manager. Managers sample a set number of landscape subsets (see below). Managers then extrapolate the density of resources in the subset to estimate the total number of resources on a landscape. \n\t 2. Mark-recapture estimate of the popuation, in which managers mark a fixed number of resources in the population without any spatial bias (if there are fewer than this fixed number of resources, managers sample all resources) a set number of times with replacement. The manager then recaptures a fixed number of resources, also without any spatial bias. A Chapman estimate is used in the manager model to estimate population size from these observation data. \n\t 3. Transect-based sampling (linear), in which a manager samples an entire row of the landscape and counts the resources on the row, then moves onto the next row of the landscape until the entire landscape has been covered. The number of cells in each row (i.e., the height) equals the manager's viewing distance, so fewer transects are needed if agents can see farther. Resources can potentially move on the landscape between each transect sampling, causing observation error if some resources are double counted or not counted at all due to movement. If resources are not allowed to move during sampling, then this type of observation should produce no error, and resource estimation will be exact. \n\t 4. Transect-based sampling (block), in which a manager samples a block of the landscape and counts the resources in the block, then moves on to the next (equally sized) block until the entire landscape has been covered. Blocks are square, with the length of each side equaling the manager's viewing distance, so fewer blocks are needed if agents can see farther. As with linear sampling, resources can potentially move on the landscape between each block sampling, causing observation error if some resources are double counted or not counted at all due to movement.",
                                        selectInput("observe_type", "Type of observation made",
                                                    c("Density-based observation"  = "0", 
                                                      "Mark-recapture sampling"    = "1",
                                                      "Sampling a linear transect" = "2",
                                                      "Sampling a block transect"  = "3")
                                        )
                               ),
                               
                               tags$div(title="This determines the type of movement that agents do. The four different movement types of agents are identical to those of resources: \n\t 1. Uniform movement in any direction up to some maximum cells away during a time step. Movement direction is random and the cell distance moved is randomly selected from zero to a set maximum (see below). \n\t 2. Poisson selected movement in the x and y dimensions where distance in each direction is determined by a Poisson function and direction (e.g., left versus right) is randomly selected for each dimension. This type of movement tends to look a bit odd with low agent movement values because it results in very little diagonal movement. It also is not especially realistic, so should probably not be used without a good reason. \n\t 3. Uniform movement in any direction up to a maximum number cells (see below) away during a time step that same maximum number of times. In other words, a single variable of each agent is acting to determine the times that an agent moves in a time step and the maximum distance it travels each time it moves. \n\t 4. No movement, in which agents stay in one place.", 
                                        selectInput("obs_move_type", 
                                                    "Agent movement while observing",
                                                    c("Uniform movement in any direction" = "1",
                                                      "Poisson movement in x and y" = "2",
                                                      "Uniform movement in any direct N times"= "3",
                                                      "No movement"        = "0")
                                        )
                               ),
                               
                               tags$div(title="Whether or not resources are to move between manager sampling times.", 
                                        radioButtons("res_move_obs", 
                                                     "Resources can move during observation",
                                                     c("True" = "1",
                                                       "False" = "0"))
                               ),
                               
                               tags$div(title="This value determines how far agents (managers and stakeholders) can see on the landscape. At the moment, this affects only the sampling ability of managers in the observation model for density-based and transect-based estimates of resource abundance. In these types of estimates, when managers have a higher agent_view, they are capable of observing a larger area of landscape and therefore of getting a larger (in the case of density-based estimation) or more efficient (in the case of transect-based estimation) sample of resources from which to estimate total resource abundance. The default value of agent_view is 20, so agents can see 20 cells away from their current cell in any direction.", 
                                        sliderInput("agent_view",
                                                    "Number of cells agents can view around them",
                                                    min   = 1,
                                                    max   = 50,
                                                    step  = 1,
                                                    value = 10)
                               ),
                               
                               tags$div(title="This value determines how far agents can move. At the moment, this does not affect much in the simulation because agent movement does not affect agent actions (interactions with resources can be limited to stakeholder's owned land, but do not currently depend on where an agent is on the landscape -- effectively assuming that agents are mobile enough to do what they want to do to resources). The one exception is for density-based estimation, which can be biased by low values of agent_move by causing the manager to sample the same (or nearby) landscape cells to estimate total resource abundance; if resources are spatially autocorrelated, then managers might over or under-estimate total abundance. Therefore, as a default, this value is set to 50 so that managers can move to any cell on a (torus) landscape in a time step, removing any bias for density sampling.",                 
                                        sliderInput("agent_move",
                                                    "Number of cells agents can move",
                                                    min   = 1,
                                                    max   = 50,
                                                    step  = 1,
                                                    value = 50)
                               ),
                               
                               tags$div(title="This parameter defines how many times a manager will make observations within the observation model; it applies only to density-based sampling.", 
                                        sliderInput("times_observe",
                                                    "Times observations made (if density-based)",
                                                    min   = 1,
                                                    max   = 20,
                                                    step  = 1,
                                                    value = 1)
                               ),
                               
                               tags$div(title="This parameter affects mark-recapture observation. Its value defines how many resources will be marked in each time step as part of a mark-recapture population size estimate.", 
                                        sliderInput("fixed_mark",
                                                    "Resources marked (if mark-recapture)",
                                                    min   = 1,
                                                    max   = 100,
                                                    step  = 1,
                                                    value = 50)
                               ),
                               
                               tags$div(title="This parameter affects mark-recapture observation. Its value defines how many resources will be (re)captured in each time step as part of a mark-recapture population size estimate.", 
                                        sliderInput("fixed_recapt",
                                                    "Resources recaptured (if mark-recapture)",
                                                    min   = 10,
                                                    max   = 200,
                                                    step  = 5,
                                                    value = 150)
                               ),
                               
                               tags$div(title="This value defines the minimum age at which resources are recorded and acted upon by agents; below this age, resources are ignored. The default value of this parameter is 1, which means that offspring just produced during a time step (age = 0) are not observed or acted upon by agents. Note that if this value is set to zero such that newly added resources are counted, then the population might appear to go over carrying capacity regularly because carrying capacity is not realised until the next resource model if it applies to the death of resource (this is not a problem for the simulation itself, it just needs to be noted).",  
                                        sliderInput("res_min_age",
                                                    "Minimum age of resource observation",
                                                    min   = 0,
                                                    max   = 5,
                                                    step  = 1,
                                                    value = 0)
                               ),
                               
                               tags$div(title="This is the maximum age of resources. If resources reach this age, then they are removed in the resource model with a probability of 1.", 
                                        sliderInput("max_ages",
                                                    "Maximum age of a resource",
                                                    min   = 1,
                                                    max   = 50,
                                                    step  = 1,
                                                    value = 5)
                               )
                        )
                ),
                tabItem("manager",
                        
                        headerPanel(title = "Manager model parameters"),
                        
                        column(width = 6, offset = 0, style='padding:0px;',
                               
                               hr(),
                               
                               tags$div(title="This is the total budget for the manager when setting policy. Higher budgets make it easier to restrict the actions of stakeholders; lower budgets make it more difficult for managers to limit the actions of stakeholders by setting policy.", 
                                        sliderInput("manager_budget",
                                                    "Total manager policy-setting budget",
                                                    min   = 100,
                                                    max   = 10000,
                                                    step  = 100,
                                                    value = 1000)
                               ),
                               
                               tags$div(title="This is the target resource abundance at which the manager attempts to keep the population.", 
                                        sliderInput("manage_target",
                                                    "Target size of the resource population",
                                                    min   = 10,
                                                    max   = 10000,
                                                    step  = 10,
                                                    value = 1000)
                               ),
                               
                               tags$div(title="This is the frequency with which policy is set by managers; a value of 1 means that policy is set in the manager model every time step; a value of 2 means that poilcy is set in the manager model every other time step, etc.", 
                                        sliderInput("manage_freq",
                                                    "Manage frequency (once per N time steps)",
                                                    min   = 1,
                                                    max   = 5,
                                                    step  = 1,
                                                    value = 1)
                               )
                        )
                ),
                tabItem("user",
                        
                        headerPanel(title = "User model parameters"),
                        
                        column(width = 6, offset = 0, style='padding:0px;',
                               
                               hr(),
                               
                               tags$div(title="This value defines whether stakeholders own land and their actions are restricted to land that they own. If false, then stakeholders can act on any landscape cell; if true, then agents can only act on their own cells.", 
                                        radioButtons("land_ownership", 
                                                     "Users own and act on their patch of land",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "0")
                               ),
                               
                               tags$div(title="Whether or not agents should move at the end of each time step", 
                                        radioButtons("move_agents", 
                                                     "Agents move in each time step",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "1")
                               ),
                               
                               tags$div(title="Whether or not scaring is an option for managers and stakeholders. If so, then stakeholders that scare cause resources to be moved from their current landscape cell to a random cell on the landscape (note, it is possible that the resource could be scared back onto the stakeholder's own land again).", 
                                        radioButtons("scaring", 
                                                     "Scaring is a possible action",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "0")
                               ),
                               
                               tags$div(title="Whether or not culling is an option for managers and stakeholders. If so, then stakeholders that cull cause the resource to be removed from the simulation permanently (i.e., killing the resource).", 
                                        radioButtons("culling", 
                                                     "Culling is a possible action",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "1")
                               ),
                               
                               tags$div(title="Whether or not castration is an option for managers and stakeholders. If so, then stakeholders that castrate do not remove the resource from the simulation, but prohibit the resource from reproducing by setting its 'lambda' value to zero.", 
                                        radioButtons("castration", 
                                                     "Castration is a possible action",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "0")
                               ),
                               
                               tags$div(title="Whether or not feeding is an option for managers and stakeholders. If so, then stakeholders that feed increase a resource's growth rate (lambda) for one time step by 100 percent.",
                                        radioButtons("feeding", 
                                                     "Feeding resources is a possible action",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "0")
                               ),
                               
                               tags$div(title="Whether or not feeding is an option for managers and stakeholders. If so, then stakeholders that help_offspring increase a resource's offspring production for one time step by one (i.e., one more offspring is produced).",
                                        radioButtons("help_offspring", 
                                                     "Helping offspring is a possible action",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "0")
                               ),
                               
                               tags$div(title="Whether or not killing crops on the landscape is allowed for stakeholders. If so, then stakeholders can remove the crop yield on a cell completely for each action to kill crops. Actions on the landscape cannot be regulated by managers, so the cost of this action is always fixed.",
                                        radioButtons("kill_crops", 
                                                     "Destroying crops is a possible action",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "0")
                               ),
                               
                               tags$div(title="Whether or not tending crops on the landscape is allowed for stakeholders. If so, then stakeholders can increase one cells yield by 50 percent for each action to tend crops. Actions on the landscape cannot be regulated by managers, so the cost of this action is always fixed.",
                                        radioButtons("tend_crops", 
                                                     "Tending crops is a possible action",
                                                     c("True" = "1",
                                                       "False" = "0"), selected = "0")
                               ),
                               
                               tags$div(title="The per landscape cell proportional increase in crop yield when stakeholders take one action to increase yield on their landscape. The default value is set to 0.5 (i.e., a 50 percent increase in yield on a cell).",
                                        sliderInput("tend_crop_yld",
                                                    "Pr. additional yield from tending crops",
                                                    min   = 0,
                                                    max   = 2,
                                                    step  = 0.01,
                                                    value = 0.2)
                               ),
                               
                               tags$div(title="This is the number of users in a simulation; there is always one manager, plus any natural number of users.",
                                        sliderInput("stakeholders",
                                                    "Total number of users",
                                                    min   = 1,
                                                    max   = 100,
                                                    step  = 1,
                                                    value = 4)
                               ),
                               
                               tags$div(title="This is the mimimum cost of any action in the manager and user models. Higher values allow managers to have greater precision when setting policy. For example, managers believe (typically correctly) that they will double culling number by setting the cost of culling at 1 instead of 2. If actions always cost at least some minium value, then some increment just above that value is always available to more precisely affect user actions. Hence it is generally better to simply give everyone a bigger budget and set a minimum cost, giving more precision to managers to fine tune policy. The default value of minimum_cost is therefore set to 10.",
                                        sliderInput("minimum_cost",
                                                    "Minimum cost of agent actions",
                                                    min   = 1,
                                                    max   = 50,
                                                    step  = 1,
                                                    value = 10)
                               ),
                               
                               tags$div(title="This is the total budget of each stakeholder for performing actions. The cost of performing an action is determined by the 'miminimum_cost' of actions, and the policy set by the manager.",
                                        sliderInput("user_budget",
                                                    "Total user action budget",
                                                    min   = 100,
                                                    max   = 10000,
                                                    step  = 100,
                                                    value = 1000)
                               )
                        )
                ),
                tabItem("ga",
                        
                        headerPanel(title = "Genetic algorithm parameters"),
                        
                        column(width = 6, offset = 0, style='padding:0px;',
                               
                               hr(),
                               
                               tags$div(title="The size of populations of agents in the genetic algorithm (not resources in the simulation). The actions of each agent in the simulation are duplicated to the size of the population, and this population of individual agent actions undergoes a process of natural selection to find an adaptive strategy. Selection is naturally stronger in larger populations, but a default population size of 100 is more than sufficient to find adaptive strategies.",
                                        sliderInput("ga_popsize",
                                                    "Population size",
                                                    min   = 10,
                                                    max   = 200,
                                                    step  = 5,
                                                    value = 100)
                               ),
                               
                               tags$div(title="The minimum number of generations in the genetic algorithms of the simulation (not the number of time steps in the simulation itself). The actions of each agent in the simulation are duplicated to the size of the population, and this population of individual agent actions undergoes a process of natural selection to find an adaptive strategy. If convergence criteria is set to a default value of 100, then the genetic algorithm will almost always continue for exactly this minimum number of generations. The default value is 20, which is usually plenty for finding adaptive agent strategies -- the objective is not to find optimal strategies, but strategies that are strongly in line with agent interests.",
                                        sliderInput("ga_mingen",
                                                    "Minimum generations per run",
                                                    min   = 10,
                                                    max   = 200,
                                                    step  = 5,
                                                    value = 40)
                               ),
                               
                               tags$div(title="The genetic algorithm will continue if the mean increase in figness from one generation to the next is greater than the convergence criteria. If convergence criteria is set to a default value of 1, then the genetic algorithm will continue as long as there is at least a one percent increase in fitness.",
                                        sliderInput("converge_crit",
                                                    "Convergence criteria before termination",
                                                    min   = 1,
                                                    max   = 200,
                                                    step  = 1,
                                                    value = 1)
                               ),
                               
                               tags$div(title="At the start of each genetic algorithm, a population of replicate agents are produced; this seed is the number of exact replicates, while the rest have random actions to introduce variation into the population. Because adaptive agent strategies are not likely to change wildly from one generation to the next, it is highly recommended to use some value greater than zero; the default value is 20, which does a good job of finding adaptive strategies.",
                                        sliderInput("ga_seedrep",
                                                    "Copies of agent used to seed population",
                                                    min   = 0,
                                                    max   = 100,
                                                    step  = 1,
                                                    value = 20)
                               ),
                               
                               tags$div(title="Fitnesses are assigned to different agent strategies and compete in a tournament to be selected into the next generation. The tournament samples a number strategies at random and with replacement from the population to be included in the torunament.",
                                        sliderInput("ga_sampleK",
                                                    "Random sample size in fitness tournament",
                                                    min   = 10,
                                                    max   = 100,
                                                    step  = 1,
                                                    value = 20)
                               ),
                               
                               tags$div(title="Fitnesses are assigned to different agent strategies and compete in a tournament to be selected into the next generation. The tournament samples a number of strategies at random and with replacement from the population to be included in the torunament, and from these randomly selected strategies, the top strategies are selected. The default value selected is 2, so the top 10 percent of the random sample in a tournament makes it into the next generation (note that multiple tournaments are run until a sufficient number of strategies are selected for the next generation).",
                                        sliderInput("ga_chooseK",
                                                    "Selections from sample in tournament",
                                                    min   = 1,
                                                    max   = 20,
                                                    step  = 1,
                                                    value = 2)
                               ),
                               
                               tags$div(title="The mutation rate of any action within an agent's strategy. When a mutation occurs, the action is either increased or decreased by a value of 1. If the action drops below zero, then the value after mutation is multiplied by -1.",
                                        sliderInput("ga_mutation",
                                                    "Mutation rate",
                                                    min   = 0,
                                                    max   = 0.5,
                                                    step  = 0.01,
                                                    value = 0.1)
                               ),
                               
                               tags$div(title="In the genetic algorithm, this is the crossover rate of any action within an agent's strategy with a randomly selected different strategy in the population.",
                                        sliderInput("ga_crossover",
                                                    "Crossover rate",
                                                    min   = 0,
                                                    max   = 0.5,
                                                    step  = 0.01,
                                                    value = 0.1)
                               )
                               
                        )
                ),
                
                tabItem("run",
                        
                        headerPanel(title = "Run a simulation"),
                        
                        column(width = 6, offset = 0, style='padding:0px;',
                               
                               hr(),
                               
                               actionButton("go", "Run Simulation Now")
                               
                        )
                ),
                
                tabItem("plotting",
                        
                        headerPanel(title = "Simulation output (please be patient)"),
                        
                        hr(),
                        
                        plotOutput("plot1", height = 900, width = 700)
                ),
                
                tabItem("plot_effort",
                        
                        headerPanel(title = "Conflict output (please be patient)"),
                        
                        hr(),
                        
                        plotOutput("plot2", height = 900, width = 700)
                ),
                
                tabItem("resource_tab",
                        
                        headerPanel(title = "Resource abundance and estimate"),
                        
                        hr(),
                        
                        tableOutput("table1")
                ),
                
                tabItem("cost_tab",
                        
                        headerPanel(title = "Costs set by manager"),
                        
                        hr(),
                        
                        tableOutput("table2")
                ),
                
                tabItem("action_tab",
                        
                        headerPanel(title = "Actions of users"),
                        
                        hr(),
                        
                        tableOutput("table3")
                ),
                
                tabItem("help",
                        
                        headerPanel(title = "Introduction to GMSE"),
                        
                        hr(),
                        
                        p("The GMSE application integrates game theory and ecological theory to construct social-ecological models that simulate the management of populations and stakeholder actions. These models build off of a previously developed management strategy evaluation (MSE) framework to simulate all aspects of management: population dynamics, manager observation of populations, manager decision making, and stakeholder responses to management decisions. The newly developed generalised management strategy evaluation (GMSE) framework uses genetic algorithms to mimic the decision-making process of managers and stakeholders under conditions of change, uncertainty, and conflict. All simulations can be run using the gmse() function."),
                        
                        hr(),
                        
                        br(),
                        
                        headerPanel(title = "How to run a simulation"),
                        
                        hr(),
                        
                        p("Model parameter values can be set by selecting tabs to the left. Detailed explanations of parameters will pop up by hovering over them with the cursor. To run a simulation, click on the 'Run simulation' tab followed by the 'Run Simulation Now' button. Simulation results will not appear until the 'General results' tab is selected."),
                        
                        hr(),
                        
                        br(),
                        
                        headerPanel(title = "How to interpret general results"),
                        
                        p("The 'General results' tab plots the dynamics of GMSE resource, observation, manager, and user models in six separate sub-panels. (1) Upper left panel: Shows the locations of resources on the landscape (black dots). (2) Upper right panel: Shows ownership of land by agents if 'Users own and act on their patch of land' is set to 'True' in the user model tab; land is divided proportional based on parameters set in gmse() and colours correspond with other subplots. This panel shows where actions are being performed by individual users, and where resources are affecting the landscape. (3) Middle left panel: Shows the actual population abundance (black solid line) and the population abundance estimated by the manager (blue solid line; where applicable, shading indicates 95 percent confidence intervals) over time. The dotted red line shows the resource carrying capacity (as applied to mortality) and the dotted blue line shows the target for resource abundance as set in the gmse() function; the orange line shows the total percent yield of the landscape (i.e., 100 percent means that resources have not decreased yield at all, 0 percent means that resources have completely destroyed all yield). (4) Middle right panel: Shows the raw landscape yield for each stakeholder (can be ignored if users do not own land) over time; colours correspond to land ownership shown in the upper right panel. (5) Lower left panel: The cost of stakeholders performing actions over time, as set by the manager. (6) Lower right panel: The total number of actions performed by all stakeholders over time."),
                        
                        hr(),
                        
                        br(),
                        
                        headerPanel(title = "How to interpret conflict results"),
                        
                        p("The 'Conflict results' tab plots the permissiveness that each manager exhibits for each user action (scaring, culling, etc.) and the effort that each individual user puts into each action over time. On the left axis, permissiveness is calculated as 100 minus the percent of the manager's budget put into increasing the cost of a particular action, so, e.g., if a manager puts all of their effort into increasing the cost of culling, then permissiveness of culling is 0; if they put none of their effort into increasing the cost of culling, then permissiveness of culling is 100. On the right axis, percentage of user action expended is the percent of a user's budget put into a particular action (note, these might not add up to 100 because users are not forced to use their entire budget). Coloured lines that are above black lines could potentially (cautiously) be interpreted as conflict between managers and users.")
                        
                )
                
            ))
    )
    
    # This function builds the fancy title at the top of the browser
    header <- dashboardHeader(
        title = list(icon("cogs"),"GMSE"), disable = TRUE, titleWidth = 260
    )
    
    #The below calls the user environment (I assume in the normal shiny way)
    ui <- dashboardPage(header, sidebar, body, skin = skin)
    
    #The server function itself contains everything else to get the program to run
    # server <- function(input, output, session){}
    server <- function(input, output){
        
        systime <- eventReactive(input$go, {
            as.integer(Sys.time());
        })
        
        run_gmse <- eventReactive(input$go, {
            gmse(time_max       = input$time,
                 land_dim_1     = input$land_dim_1,
                 land_dim_2     = input$land_dim_2,
                 res_movement   = input$res_movement,
                 remove_pr      = input$remove_pr,
                 lambda         = input$lambda_gr,
                 agent_view     = input$agent_view,
                 agent_move     = input$agent_move,
                 res_birth_K    = input$res_birth_K,
                 res_death_K    = input$res_death_K,
                 edge_effect    = 1,
                 res_move_type  = as.numeric(input$res_move_type),
                 res_birth_type = 2,
                 res_death_type = as.numeric(input$res_death_type),
                 observe_type   = as.numeric(input$observe_type),
                 fixed_mark     = input$fixed_mark,
                 fixed_recapt   = input$fixed_recapt,
                 times_observe  = input$times_observe,
                 obs_move_type  = as.numeric(input$obs_move_type),
                 res_min_age    = input$res_min_age,
                 res_move_obs   = as.numeric(input$res_move_obs),
                 Euclidean_dist = FALSE,
                 plotting       = FALSE,
                 hunt           = FALSE,
                 start_hunting  = 95,
                 res_consume    = input$res_consume,
                 ga_popsize     = input$ga_popsize,
                 ga_mingen      = input$ga_mingen,
                 ga_seedrep     = input$ga_seedrep,
                 ga_sampleK     = input$ga_sampleK,
                 ga_chooseK     = input$ga_chooseK,
                 ga_mutation    = input$ga_mutation,
                 ga_crossover   = input$ga_crossover,
                 move_agents    = as.numeric(input$move_agents),
                 max_ages       = input$max_ages,
                 minimum_cost   = input$minimum_cost,
                 user_budget    = input$user_budget,
                 manager_budget = input$manager_budget,
                 manage_target  = input$manage_target,
                 RESOURCE_ini   = input$RESOURCE_ini,
                 scaring        = as.numeric(input$scaring),
                 culling        = as.numeric(input$culling),
                 castration     = as.numeric(input$castration),
                 feeding        = as.numeric(input$feeding),
                 help_offspring = as.numeric(input$help_offspring),
                 tend_crops     = as.numeric(input$tend_crops),
                 tend_crop_yld  = input$tend_crop_yld,
                 kill_crops     = as.numeric(input$kill_crops),
                 stakeholders   = input$stakeholders,
                 manage_caution = 1,
                 land_ownership = as.numeric(input$land_ownership),
                 manage_freq    = input$manage_freq,
                 converge_crit  = input$converge_crit,
                 manager_sense  = 0.1,
                 public_land    = input$public_land
            );
        })
        
        output$plot1 <- renderPlot({
            set.seed(systime());
            sim <- run_gmse();
            plot_gmse_results(res = sim$resource, obs = sim$observation, 
                              land = sim$land, agents = sim$agents, 
                              paras = sim$paras, ACTION = sim$action, 
                              COST = sim$cost);
        })
        
        output$plot2 <- renderPlot({
            set.seed(systime());
            sim <- run_gmse();
            plot_gmse_effort(agents = sim$agents, paras = sim$paras, 
                             ACTION = sim$action,  COST = sim$cost);
        })
        
        output$table1 <- renderTable({
            set.seed(systime());
            sim    <- run_gmse();
            simsum <- gmse_summary(sim);
            simres <- simsum$resources;
            simest <- simsum$observations;
            showt  <- cbind(simres[,1:2], simest[,2]);
            colnames(showt) <- c("time_step", "population_size", 
                                 "estimate_of_pop_size");
            showt[,1:3];
        }, include.colnames = TRUE)
        
        output$table2 <- renderTable({
            set.seed(systime());
            sim    <- run_gmse();
            simsum <- gmse_summary(sim);
            simcos <- simsum$costs[,-2];
            includ <- as.numeric(which(!is.na(simcos[1,])));
            simcos[,includ];
        }, include.colnames = TRUE)
        
        output$table3 <- renderTable({
            set.seed(systime());
            sim    <- run_gmse();
            simsum <- gmse_summary(sim);
            simact <- simsum$actions[,-3];
            includ <- as.numeric(which(!is.na(simact[1,])));
            simact[,includ];
        }, include.colnames = TRUE)
        
    } 

# The app is called with the below
app <- shinyApp(ui, server)
    runApp(app, launch.browser = TRUE)

#-------------------------------------------------------------------------------
}


