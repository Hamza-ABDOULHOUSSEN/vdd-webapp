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
      menuItem("Sexe", tabName = "sexe", icon = icon("bar-chart-o")),
      menuItem("Adresse", tabName = "adresse", icon = icon("bar-chart-o")),
      menuItem("Analyse_bivariee", tabName = "Analyse_bivariee", icon = icon("bar-chart-o")),
      menuItem("TD", icon = icon("th"), tabName = "TD",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  
  
  ## BODY
  dashboardBody(
    tabItems(
      ## HOME PAGE
      tabItem(tabName = "home",
              includeMarkdown("markdown/home.md"),
              htmlOutput("home_info")
      ),
      
      ## HISTOGRAMME
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content"),
              plotOutput("histo")
      ),
      
      ## AGE
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
                  "age",
                  "age",
                  choices =  c("les deux" = "both",
                               "15" = 15,
                               "16" = 16,
                               "17" = 17),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                )
                
              )
              
      ),
      
      ## SEXE
      tabItem(tabName = "sexe",
              h2("Sexe homme ou femme"),
              tabBox(
                tabPanel("Histogramme", plotOutput("sexe")),
                tabPanel("Boîte à moustache", plotOutput("sexe_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons(
                  "sexe",
                  "sexe",
                  choices =  c("les deux" = "both",
                               "Homme" = "M",
                               "Femme" = "F"),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("sexe_info")
              ),
              
      ),
      
      ## ADRESSE
      tabItem(tabName = "adresse",
              h2("Adresse Rural ou Urbain"),
              box(
                title = "adresse", status = "primary", solidHeader = TRUE,
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
      
      ## Analyse bivariee
      tabItem(tabName = "Analyse_bivariee",
              h2("Correlation avec la moyenne"),
              box(
                title = "correlation", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("correlation")
              ),
              
              box(
                title = "Inputs", background = "black",
                radioButtons(
                  "variables",
                  "variables",
                  choices =  c(" age" = " age",
                               "familly relation" = "famrel",
                               "absences" = "absences",
                               "notes"="G3"),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                ),
                
                radioButtons(
                  "types",
                  "types",
                  choices =  c("Shapiro-Wilk" = " sw",
                               "chi 2" = "ch"),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                )
                
              ),
              
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("analyse_info")
              ),
              
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