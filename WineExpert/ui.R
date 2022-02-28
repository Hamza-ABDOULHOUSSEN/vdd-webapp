library(shiny)
library(shinydashboard)

dashboardPage(
  
  ## HEADER
  dashboardHeader(title = "Basic dashboard"),
  
  ## SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("bar-chart-o")),
      menuItem("TD", icon = icon("th"), tabName = "TD",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  
  
  ## BODY
  dashboardBody(
    tabItems(
      ## HOME PAGE
      tabItem(tabName = "home",
              h2("Home")
      ),
      
      ## HISTOGRAMME
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content"),
              plotOutput("histo")
      ),
      
      ## DATA
      tabItem(tabName = "data",
              h2("Data"),
              box(
                title = "Data", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("data")
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons(
                  "sexe",
                  "Sexe",
                  choices =  c("les deux" = "both",
                               "homme" = "M",
                               "femme" = "F"),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                )
                
              )
              
      ),
      
      ## TD PAGE
      tabItem(tabName = "TD",
              h2("TD"),
              
              box(
                title = "Gauss", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("gaussienne")
              ),
              box(
                title = "Sinus", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("sinus")
              ),
              box(
                title = "Inputs", background = "black",
                numericInput(
                  "n",
                  "n",
                  value=25,
                  min = 0,
                  max = NA,
                  step = 1,
                  width = NULL
                ),
                checkboxInput("check", "relier les points", value = FALSE, width = NULL),
                radioButtons(
                  "button",
                  "couleurs",
                  choices =  c("rouge" = "red",
                               "vert" = "green",
                               "bleu" = "blue"),
                  selected = NULL,
                  inline = FALSE,
                  width = NULL
                ),
                sliderInput("epaisseur", "Decimal:",
                            min = 1, max = 5,
                            value = 0.5, step = 0.1
                ),
                
                # Button
                downloadButton("downloadImage", "Download")
              )
              
      )
    )
  )
  
  
)