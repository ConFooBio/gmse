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
                
                menuItem(" Plotting", icon = icon("line-chart"), tabName = "Config"),

                menuItem("Source code for app", icon = icon("code"),
                         href = "https://github.com/bradduthie/gmse"
                ),
                div(uiOutput("Export"), style = "text-align: center")
    )
)


# This makes the main body of the gui -- where the plot will probably go for GMSE
body <- dashboardBody(
    
    includeCSS(system.file("www/elementR.css", package="elementR")),
    
    div(style = "min-height:100vh; min-width: (100vw - 230); display:flex",
        div(style = "background-color: #726a5e; width: 31px;",
            div(style = "background-color: #726a5e; width: 30px;position:fixed",
                div(style = "background: rgb(0, 100, 0); height: 50px"),
                uiOutput('ValidFlag1'),
                div(style = "background-color: #726a5e; height: 30px; width: 30px"),
                uiOutput('ValidFlag2'),
                div(style = "background-color: #726a5e; height: 30px; width: 30px"),
                uiOutput('ValidFlag3'),
                div(style = "background-color: #726a5e; height: 30px; width: 30px"),
                uiOutput('ValidFlag4'),
                div(style = "background-color: #726a5e; height: 30px; width: 30px"),
                uiOutput('ValidFlag5')
            )
        ),
        div(style = "width: 100%; margin-top:10px; margin-left:10px;margin-bottom:10px;margin-right:0px;",
            uiOutput("TopBar"),
            tabItems(
                
                tabItem("start",
                        uiOutput("start1"),
                        
                        fluidRow(
                            uiOutput("start2")
                        )
                        
                        
                        
                ), #eo tab start
                
                
                tabItem("Standards", style = "padding-right: 0px; padding-left: 0px",
                        uiOutput("Standards1"),
                        uiOutput("Standards2")
                        
                        
                        
                ), #eo tab Standards
                tabItem("MachDrift", style = "padding-right: 0px; padding-left: 0px",
                        uiOutput("MachDrift1"),
                        uiOutput("MachDrift2"),
                        uiOutput('MachDrift3')
                        
                ), #eo tab MachDrift
                
                tabItem("Samples", style = "padding-right: 0px; padding-left: 0px",
                        box(width = 12,background = "aqua", style = "background-color: #726a5e;margin-bottom:10px",
                            column(5,
                                   uiOutput("sample1")
                            ),
                            column(2,
                                   uiOutput("sample2")
                            ),
                            column(3,
                                   uiOutput("sample3")
                            ),
                            column(1, class = "class2",
                                   br(),
                                   uiOutput("sample4")
                            )
                            
                        ),
                        column(12,
                               uiOutput("Sample5"))
                        
                ), #eo tab Samples
                
                tabItem("realign",
                        uiOutput("realign1"),
                        uiOutput("realign2"),
                        uiOutput("realign8"),
                        uiOutput("realign10"),
                        fluidRow(
                            column(3, uiOutput("realign3")),
                            column(9, uiOutput("realign5"))
                        )
                        
                        
                        
                        
                ), #eo tab realign
                tabItem("Config",
                        uiOutput("config0"),
                        uiOutput("config4"),
                        uiOutput("config2"),
                        uiOutput("config3"),
                        uiOutput("config1")
                        
                ), #eo tab Config
                tabItem("SessionConfig",
                        uiOutput("Precision1"),
                        uiOutput("Precision4")
                ) #eo tab Config
                
            )
        )
    )
)#dashboardBody


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
                               div(h3(icon("cogs"),"Step 1. Create a new project or load an existing one"), style = "display: inline-block;")
                        )
                    )
                )
            })
            
            output$start2 <- renderUI({
                
                div(
                    box(
                        title = list(icon("folder-o"),"New Project"),
                        width = 6,
                        status="primary",
                        solidHeader = TRUE,
                        p("1. Choose the project folder"),
                        actionButton("createProjButton", "Create your project !"),
                        actionButton("runExampleNew", "Run Example")
                    ),
                    box(
                        title = list(icon("folder"),"Load Project"),
                        width = 6,
                        solidHeader = TRUE,
                        status="primary",
                        p("1. Choose a project to load"),
                        actionButton("loadProjButton","Load your Project"),
                        actionButton("runExampleLoad", "Load Example")
                        
                    )
                    
                )
                
                
                
            })
        }
    
    
}    


# The app is called with the below
######################
######## CALL shinyApp
######################
app <- shinyApp(ui, server)
    runApp(app, launch.browser = TRUE)


} #
























