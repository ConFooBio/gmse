library(shiny)
library(shinydashboard)
library(shinyjs)
library(GMSE)

# Calling the function below will call the gmse function in a browser

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
                
                menuItem("Results", icon = icon("line-chart"), 
                         tabName = "plotting"),

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
    tabItems(
        tabItem("global",
               
                headerPanel(title = "Global GMSE parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                
                    hr(),
                       
                    sliderInput("time",
                                "Number of generations",
                                min = 10,
                                max = 200,
                                step  = 10,
                                value = 100),
                    
                    sliderInput("land_dim_1",
                                "X dimension length (in cells)",
                                min = 10,
                                max = 400,
                                step = 10,
                                value = 100),
                    
                    sliderInput("land_dim_2",
                                "Y dimension length (in cells)",
                                min = 10,
                                max = 400,
                                step = 10,
                                value = 100),
                    
                    sliderInput("public_land",
                                "Proportion of land that is public",
                                min = 0,
                                max = 1,
                                step = 0.01,
                                value = 0)
                
                )
                
        ),
        tabItem("resource",
                
                headerPanel(title = "Resource model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                    hr(),
                      
                    selectInput("res_move_type", "Resource movement type:",
                               c( "Uniform movement in any direction" = "1",
                                  "Poisson movement in x and y" = "2",
                                  "Uniform movement in any direct N times"= "3",
                                  "No movement"        = "0")
                               ),
                
                    selectInput("res_death_type", "Resource death type:",
                                c("Density-dependent" = "1", 
                                  "Density-independent"   = "0")
                    ),
                    
                    sliderInput("res_movement",
                                "Resource movement",
                                min   = 0,
                                max   = 50,
                                step  = 1,
                                value = 20),
                
                    sliderInput("remove_pr",
                                "Density-independent resource death",
                                 min   = 0,
                                 max   = 1,
                                 step  = 0.01,
                                 value = 0),                
                
                    sliderInput("lambda_gr",
                                "Resource growth rate",
                                min   = 0,
                                max   = 4,
                                step  = 0.01,
                                value = 0.30),
                
                    sliderInput("res_birth_K",
                                "Carrying capacity applied to birth",
                                min   = 10,
                                max   = 10000,
                                step  = 10,
                                value = 0),
                
                    sliderInput("res_death_K",
                                "Carrying capacity applied to death",
                                min   = 10,
                                max   = 10000,
                                step  = 10,
                                value = 600),
                    
                    sliderInput("res_consume",
                                "Proportion of landscape cell consumed",
                                min   = 0,
                                max   = 1,
                                step  = 0.01,
                                value = 0.5)
                
                )
                
        ),
        tabItem("observation",
                
                headerPanel(title = "Observation model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                    hr(),
                
                    selectInput("observe_type", "Type of observation made",
                                c("Density-based observation"  = "0", 
                                  "Mark-recapture sampling"    = "1",
                                  "Sampling a linear transect" = "2",
                                  "Sampling a block transect"  = "3")
                    ),
                
                    selectInput("obs_move_type", 
                                "Agent movement while observing",
                                c("No movement"        = "0", 
                                  "Uniform movement in any direction" = "1",
                                  "Poisson movement in x and y" = "2",
                                  "Uniform movement in any direct N times"= "3")
                    ),
                    
                    radioButtons("res_move_obs", 
                                 "Resources can move during observation",
                                 c("True" = "TRUE",
                                   "False" = "FALSE")),
                    
                    sliderInput("agent_view",
                                "Number of cells agents can view around them",
                                min   = 1,
                                max   = 50,
                                step  = 1,
                                value = 10),
                
                    sliderInput("agent_move",
                                "Number of cells agents can move",
                                min   = 1,
                                max   = 50,
                                step  = 1,
                                value = 50),
                    
                    sliderInput("times_observe",
                                "Times observations made (if density-based)",
                                min   = 1,
                                max   = 20,
                                step  = 1,
                                value = 8),
                    
                    sliderInput("fixed_mark",
                                "Resources marked (if mark-recapture)",
                                min   = 1,
                                max   = 100,
                                step  = 1,
                                value = 50),
                    
                    sliderInput("fixed_recapt",
                                "Resources recaptured (if mark-recapture)",
                                min   = 10,
                                max   = 200,
                                step  = 5,
                                value = 150),
                    
                    sliderInput("res_min_age",
                                "Minimum age of resource observation",
                                min   = 0,
                                max   = 5,
                                step  = 1,
                                value = 0),
                    
                    sliderInput("max_ages",
                                "Maximum age of a resource",
                                min   = 1,
                                max   = 50,
                                step  = 1,
                                value = 5),
                    
                    sliderInput("RESOURCE_ini",
                                "Initial population size",
                                min   = 10,
                                max   = 10000,
                                step  = 10,
                                value = 300)
                    
                )
                
        ),
        tabItem("manager",
                
                headerPanel(title = "Manager model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                       hr(),
                       
                       sliderInput("manager_budget",
                                   "Total manager policy-setting budget",
                                   min   = 100,
                                   max   = 10000,
                                   step  = 100,
                                   value = 1000),
                       
                       sliderInput("manage_target",
                                   "Target size of the resource population",
                                   min   = 10,
                                   max   = 10000,
                                   step  = 10,
                                   value = 300),
                       
                       sliderInput("manage_freq",
                                   "Frequency management decisions are enacted",
                                   min   = 1,
                                   max   = 5,
                                   step  = 1,
                                   value = 1)
                       
                )
                
        ),
        tabItem("user",
                
                headerPanel(title = "User model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                       hr(),
                       
                       radioButtons("land_ownership", 
                                    "Users own and act on their patch of land",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("move_agents", 
                                    "Agents move in each time step",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("scaring", 
                                    "Scaring is a possible action",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("culling", 
                                    "Culling is a possible action",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("castration", 
                                    "Castration is a possible action",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("feeding", 
                                    "Feeding resources is a possible action",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("help_offspring", 
                                    "Helping offspring is a possible action",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("kill_crops", 
                                    "Destroying crops is a possible action",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       radioButtons("tend_crops", 
                                    "Tending crops is a possible action",
                                    c("True" = "TRUE",
                                      "False" = "FALSE")),
                       
                       sliderInput("tend_crop_yld",
                                   "Pr. additional yield from tending crops",
                                   min   = 0,
                                   max   = 2,
                                   step  = 0.01,
                                   value = 0.2),
                       
                       sliderInput("stakeholders",
                                   "Total number of users",
                                   min   = 1,
                                   max   = 100,
                                   step  = 1,
                                   value = 4),
                       
                       sliderInput("minimum_cost",
                                   "Minimum cost of agent actions",
                                   min   = 1,
                                   max   = 50,
                                   step  = 1,
                                   value = 10),
                       
                       sliderInput("user_budget",
                                   "Total user action budget",
                                   min   = 100,
                                   max   = 10000,
                                   step  = 100,
                                   value = 1000)
                       
                )
        ),
        tabItem("ga",
                
                headerPanel(title = "Genetic algorithm parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                       hr(),
                       
                       sliderInput("ga_popsize",
                                   "Population size",
                                   min   = 10,
                                   max   = 200,
                                   step  = 5,
                                   value = 100),
                       
                       sliderInput("ga_mingen",
                                   "Minimum generations per run",
                                   min   = 10,
                                   max   = 200,
                                   step  = 5,
                                   value = 40),
                       
                       sliderInput("converge_crit",
                                   "Convergence criteria before termination",
                                   min   = 1,
                                   max   = 200,
                                   step  = 1,
                                   value = 100),
                       
                       sliderInput("ga_seedrep",
                                   "Copies of agent used to seed population",
                                   min   = 0,
                                   max   = 100,
                                   step  = 1,
                                   value = 20),
                       
                       sliderInput("ga_sampleK",
                                   "Random sample size in fitness tournament",
                                   min   = 10,
                                   max   = 100,
                                   step  = 1,
                                   value = 20),
                       
                       sliderInput("ga_chooseK",
                                   "Selections from sample in tournament",
                                   min   = 1,
                                   max   = 20,
                                   step  = 1,
                                   value = 2),
                       
                       sliderInput("ga_mutation",
                                   "Mutation rate",
                                   min   = 0,
                                   max   = 0.5,
                                   step  = 0.01,
                                   value = 0.1),
                       
                       sliderInput("ga_crossover",
                                   "Crossover rate",
                                   min   = 0,
                                   max   = 0.5,
                                   step  = 0.01,
                                   value = 0.1)
                       
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
        )
        
    )
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

    run_gmse <- eventReactive(input$go, {
        gmse(time_max       = input$time,
             land_dim_1     = input$land_dim_1,
             land_dim_2     = input$land_dim_2,
             res_movement   = input$res_movement,
             remove_pr      = input$remove_pr,
             lambda         = input$lambda_gr,
             agent_view     = 10,
             agent_move     = 50,
             res_birth_K    = 10000,
             res_death_K    = 600,
             edge_effect    = 1,
             res_move_type  = 1,
             res_birth_type = 2,
             res_death_type = 2,
             observe_type   = 0,
             fixed_mark     = 50,
             fixed_recapt   = 150,
             times_observe  = 8,
             obs_move_type  = 1,
             res_min_age    = 0,
             res_move_obs   = 1,
             Euclidean_dist = FALSE,
             plotting       = FALSE,
             hunt           = FALSE,
             start_hunting  = 95,
             res_consume    = 0.5,
             ga_popsize     = 100,
             ga_mingen      = 40,
             ga_seedrep     = 20,
             ga_sampleK     = 20,
             ga_chooseK     = 2,
             ga_mutation    = 0.1,
             ga_crossover   = 0.1,
             move_agents    = 1,
             max_ages       = 5,
             minimum_cost   = 10,
             user_budget    = 1000,
             manager_budget = 1000,
             manage_target  = 300,
             RESOURCE_ini   = 300,
             scaring        = 1,
             culling        = 1,
             castration     = 1,
             feeding        = 1,
             help_offspring = 1,
             tend_crops     = 1,
             tend_crop_yld  = 1,
             kill_crops     = 1,
             stakeholders   = 4,
             manage_caution = 1,
             land_ownership = 1,
             manage_freq    = 1,
             converge_crit  = 100,
             manager_sense  = 0.1,
             public_land    = 0
        )

    })
    
    output$plot1 <- renderPlot({
        sim <- run_gmse();
        plot_gmse_results(res = sim$resource, obs = sim$observation, 
                          land = sim$land, agents = sim$agents, 
                          paras = sim$paras, ACTION = sim$action, 
                          COST = sim$cost);
    })
    
}    


# The app is called with the below
app <- shinyApp(ui, server)
    runApp(app, launch.browser = TRUE)

#-------------------------------------------------------------------------------
}




