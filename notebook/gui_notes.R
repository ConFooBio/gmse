# Relevant functions from elementR that might help
# These go *within* the main shiny function




run_gmse <- function(){ # nocov start



#skyn
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
                
                menuItem("Run simulation", tabName = "run", 
                         icon = icon("play", class=menuIconClass)),
                
                uiOutput("sim_output"),
                
                hr(style ="width: 70%; color: white; align: center"),
                
                menuItem("Plotting", icon = icon("line-chart"), tabName = "plotting"),

                menuItem("Source code for app", icon = icon("code"),
                         href = "https://github.com/bradduthie/gmse"
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
                                value = 100)
                
                )
                
        ),
        tabItem("resource",
                
                headerPanel(title = "Resource model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                    hr(),
                      
                    selectInput("res_move_type", "Resource movement type:",
                               c("No movement"        = 0, 
                                  "Uniform movement in any direction" = 1,
                                  "Poisson movement in x and y" = 2,
                                  "Uniform movement in any direct N times" = 3)
                               ),
                
                    sliderInput("res_movement",
                                "Resource movement",
                                min   = 10,
                                max   = 200,
                                step  = 10,
                                value = 100),
                
                    sliderInput("remove_pr",
                                "Density-independent resource death",
                                 min   = 0,
                                 max   = 1,
                                 step  = 0.01,
                                 value = 0),                
                
                    sliderInput("lambda",
                                "Resource growth rate",
                                min   = 0,
                                max   = 4,
                                step  = 0.01,
                                value = 0),
                
                    sliderInput("res_birth_K",
                                "Carrying capacity applied to birth",
                                min   = 0,
                                max   = 1,
                                step  = 0.01,
                                value = 0),
                
                    sliderInput("res_death_K",
                                "Carrying capacity applied to death",
                                min   = 0,
                                max   = 1,
                                step  = 0.01,
                                value = 0)
                
                )
                
        ),
        tabItem("observation",
                
                headerPanel(title = "Observation model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                    hr(),
                
                    selectInput("variable", "Model type:",
                                c("Numerical Model"        = "Numer", 
                                  "Individual-Based Model" = "IBM")),
                
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
                                value = 50)
                )
                
        ),
        tabItem("manager",
                
                headerPanel(title = "Manager model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                       hr()
                       
                )
                
        ),
        tabItem("user",
                
                headerPanel(title = "Manager model parameters"),
                
                column(width = 6, offset = 0, style='padding:0px;',
                       
                       hr()
                       
                )
        ),
        tabItem("run",
                actionButton("go", "Run"),
                
                actionButton("reset", "Reset")
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
server <- function(input){

    # observe({
    #     if(input$tab == "global"){
    #         # LEFT OFF HERE ------------------------------------
    #         output$start1 <-  renderUI({
    #             fluidRow(
    #                 box(
    #                     background = "light-blue",
    #                     height = 85,
    #                     width = 12,
    #                     column(9,
    #                            div(h3(icon("cogs"),"Set the global parameters here"), style = "display: inline-block;")
    #                     )
    #                 )
    #             )
    #         })
    #         
    #     }
    # })
    
}    


# The app is called with the below
######################
######## CALL shinyApp
######################
app <- shinyApp(ui, server)
    runApp(app, launch.browser = TRUE)
} #
























