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

                sliderInput("time",
                            "Number of generations",
                            min = 10,
                            max = 200,
                            step  = 10,
                            value = 100)
                
        ),
        tabItem("resource",
                
                selectInput("variable", "Model type:",
                            c("Numerical Model"        = "Numer", 
                              "Individual-Based Model" = "IBM")),
                
                sliderInput("time",
                            "Number of generations",
                            min = 10,
                            max = 200,
                            step  = 10,
                            value = 100),
                
                sliderInput("N_initial",
                            "Initial prey density",
                            min   = 0,
                            max   = 1,
                            step  = 0.01,
                            value = 0.5)
        ),
        tabItem("observation",
                "Sub-item 1 tab content"
        ),
        tabItem("manager",
                "Sub-item 2 tab content"
        ),
        tabItem("user",
                "Sub-item 2 tab content"
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

    observe({
        if(input$tab == "global"){
            # LEFT OFF HERE ------------------------------------
            output$start1 <-  renderUI({
                fluidRow(
                    box(
                        background = "light-blue",
                        height = 85,
                        width = 12,
                        column(9,
                               div(h3(icon("cogs"),"Set the global parameters here"), style = "display: inline-block;")
                        )
                    )
                )
            })
            
        }
    })
    
}    


# The app is called with the below
######################
######## CALL shinyApp
######################
app <- shinyApp(ui, server)
    runApp(app, launch.browser = TRUE)
} #
























