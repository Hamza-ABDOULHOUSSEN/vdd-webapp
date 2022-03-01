library(shiny)
library(shinydashboard)
library(markdown)

dashboardPage(
  
  ## HEADER
  dashboardHeader(title = "Basic dashboard"),
  
  ## SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Age", tabName = "age", icon = icon("bar-chart-o")),
      menuItem("Adress", tabName = "adress", icon = icon("bar-chart-o")),
      menuItem("TD", icon = icon("th"), tabName = "TD",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  
  
  ## BODY
  dashboardBody(
    tabItems(
      ## HOME PAGE
      tabItem(tabName = "home",
              includeMarkdown("markdown/home.md")
      ),
      
      ## HISTOGRAMME
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content"),
              plotOutput("histo")
      ),
      
      ## DATA
      tabItem(tabName = "age",
              h2("Age"),
              box(
                title = "Age", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("age")
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
      
      ## ADRESSE
      tabItem(tabName = "adress",
              h2("Adress Rural or Urban"),
              box(
                title = "adress", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("adress")
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons(
                  "adress",
                  "adress",
                  choices =  c("les deux" = "both",
                               "Urban" = "U",
                               "Rural" = "R"),
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