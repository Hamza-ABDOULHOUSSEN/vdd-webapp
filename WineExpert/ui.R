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
      menuItem("Age", tabName = "age", icon = icon("bar-chart-o")),
      menuItem("Sexe", tabName = "sexe", icon = icon("bar-chart-o")),
      menuItem("Milieu d'habitation", tabName = "adress", icon = icon("bar-chart-o")),
      menuItem("Cohabitation des parents", tabName = "pstatus", icon = icon("bar-chart-o")),
      menuItem("Education de la mère", tabName = "medu", icon = icon("bar-chart-o")),
      menuItem("Education du père", tabName = "fedu", icon = icon("bar-chart-o"))
    )
  ),
  
  
  ## BODY
  dashboardBody(
    tabItems(
      ## HOME PAGE
      tabItem(tabName = "home",
              includeMarkdown("markdown/home.md"),
              fileInput(
                "file",
                "charger un fichier",
                multiple = FALSE,
                accept = NULL,
                width = NULL,
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              ),
              htmlOutput("home_info")
      ),
      
      ## AGE
      tabItem(tabName = "age",
              h2("Age"),
              tabBox(
                tabPanel("Histogramme", plotOutput("age")),
                tabPanel("Boîte à moustache", plotOutput("age_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons(
                  "age",
                  "Age",
                  choices =  c("Total" = "both",
                               "15" = 15,
                               "16" = 16,
                               "17" = 17),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("age_info")
              ),
              
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
                  choices =  c("Total" = "both",
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
      tabItem(tabName = "adress",
              h2("Milieu d'habitation Rural ou Urbain"),
              tabBox(
                tabPanel("Histogramme", plotOutput("adress")),
                tabPanel("Boîte à moustache", plotOutput("adress_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons(
                  "adress",
                  "Milieu d'habitation",
                  choices =  c("Total" = "both",
                               "Rural" = "R",
                               "Urbain" = "U"),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("adress_info")
              ),
              
      ),
      
      ## COHABITATION DES PARENTS
      tabItem(tabName = "pstatus",
              h2("Cohabitation des parents Ensemble ou Séparés"),
              tabBox(
                tabPanel("Histogramme", plotOutput("pstatus")),
                tabPanel("Boîte à moustache", plotOutput("pstatus_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons(
                  "pstatus",
                  "Cohabitation des parents",
                  choices =  c("Total" = "both",
                               "Ensemble" = "T",
                               "Séparés" = "A"),
                  selected = NULL,
                  inline = FALSE,
                  width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("pstatus_info")
              ),
              
      ),
      
      ## MOTHER EDUCATION
      tabItem(tabName = "medu",
              h2("Education de la mère"),
              tabBox(
                tabPanel("Histogramme", plotOutput("medu")),
                tabPanel("Boîte à moustache", plotOutput("medu_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("medu", "niveau d'education (0 faible)",
                            min = 0, max = 4,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("medu_info")
              ),
              
      ),
      
      ## FATHER EDUCATION
      tabItem(tabName = "fedu",
              h2("Education du père"),
              tabBox(
                tabPanel("Histogramme", plotOutput("fedu")),
                tabPanel("Boîte à moustache", plotOutput("fedu_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("fedu", "niveau d'education (0 faible)",
                            min = 0, max = 4,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("fedu_info")
              ),
              
      )
      
    )
  )
  
  
)