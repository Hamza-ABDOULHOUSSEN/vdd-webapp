library(shiny)
library(shinydashboard)
library(markdown)

dashboardPage(
  
  ## HEADER
  dashboardHeader(title = "StudentExpert"),
  
  ## SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Age", tabName = "age", icon = icon("bar-chart-o")),
      menuItem("Sexe", tabName = "sexe", icon = icon("bar-chart-o")),
      menuItem("Milieu d'habitation", tabName = "adress", icon = icon("bar-chart-o")),
      menuItem("Cohabitation des parents", tabName = "pstatus", icon = icon("bar-chart-o")),
      menuItem("Education de la mère", tabName = "medu", icon = icon("bar-chart-o")),
      menuItem("Education du père", tabName = "fedu", icon = icon("bar-chart-o")),
      menuItem("Qualité des relations familiales", tabName = "famrel", icon = icon("bar-chart-o")),
      menuItem("Temps libre", tabName = "freetime", icon = icon("bar-chart-o")),
      menuItem("Sortie", tabName = "goout", icon = icon("bar-chart-o")),
      menuItem("Consommation d'alcool en semaine", tabName = "Dalc", icon = icon("bar-chart-o")),
      menuItem("Consommation d'alcool en week-end", tabName = "Walc", icon = icon("bar-chart-o")),
      menuItem("Santé", tabName = "health", icon = icon("bar-chart-o")),
      menuItem("Soutien extérieur", tabName = "schoolsup", icon = icon("bar-chart-o")),
      menuItem("Support familiale", tabName = "famsup", icon = icon("bar-chart-o")),
      menuItem("Cours supplémentaires", tabName = "paid", icon = icon("bar-chart-o")),
      menuItem("Activités extra scolaire", tabName = "activities", icon = icon("bar-chart-o")),
      menuItem("Est allé en garderie", tabName = "nursery", icon = icon("bar-chart-o")),
      menuItem("Volonté de faire des études supérieures", tabName = "higher", icon = icon("bar-chart-o")),
      menuItem("Accés à internet", tabName = "internet", icon = icon("bar-chart-o")),
      menuItem("Relation amoureuse", tabName = "romantic", icon = icon("bar-chart-o")),
      menuItem("Analyse_bivariee", tabName = "Analyse_bivariee", icon = icon("bar-chart-o")),
      menuItem("unidimentional", tabName = "unidimentional", icon = icon("bar-chart-o"))
    )
  ),
  
  
  ## BODY
  dashboardBody(
    includeCSS("css/style.css"),
    tabItems(
      ## HOME PAGE
      tabItem(tabName = "home",
              h1("Page d'accueil"),
              h2("Chargement données"),
              fileInput(
                "file",
                "Charger un fichier",
                multiple = FALSE,
                accept = NULL,
                width = NULL,
                buttonLabel = "Recherche...",
                placeholder = "Pas de fichier"
              ),
              htmlOutput("home_alert"),
              includeMarkdown("markdown/home.md"),
              htmlOutput("home_info"),
              htmlOutput("home_info_footer")
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
                sliderInput("age", "Age",
                            min = 15, max = 22,
                            value = 0),
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
                tabPanel("Boîte à moustache", plotOutput("sexe_bam")),
                tabPanel("Progression", plotOutput("sexe_pro"))
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
                ),
                radioButtons(
                  "reg",
                  "Regression",
                  choices =  c("oui" = "yes",
                               "non" = "no"),
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
              
      ),
      
      ## FAMREL
      tabItem(tabName = "famrel",
              h2("Qualité des relations familiales"),
              tabBox(
                tabPanel("Histogramme", plotOutput("famrel")),
                tabPanel("Boîte à moustache", plotOutput("famrel_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("famrel", "qualité de la relation familiale (1 faible)",
                            min = 1, max = 5,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("famrel_info")
              ),
              
      ),
      
      ## FREETIME
      tabItem(tabName = "freetime",
              h2("Temps libre après les cours"),
              tabBox(
                tabPanel("Histogramme", plotOutput("freetime")),
                tabPanel("Boîte à moustache", plotOutput("freetime_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("freetime", "Temps libre après les cours (1 faible)",
                            min = 1, max = 5,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("freetime_info")
              ),
              
      ),
      
      ## SORTIE
      tabItem(tabName = "goout",
              h2("Sortie"),
              tabBox(
                tabPanel("Histogramme", plotOutput("goout")),
                tabPanel("Boîte à moustache", plotOutput("goout_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("goout", "Sortie (1 faible)",
                            min = 1, max = 5,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("goout_info")
              ),
              
      ),
      
      ## CONSO ALCOOL SEMAINE
      tabItem(tabName = "Dalc",
              h2("Consommation d'alcool en semaine"),
              tabBox(
                tabPanel("Histogramme", plotOutput("Dalc")),
                tabPanel("Boîte à moustache", plotOutput("Dalc_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("Dalc", "Consommation d'alcool en semaine (1 faible)",
                            min = 1, max = 5,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("Dalc_info")
              ),
              
      ),
      
      ## CONSO ALCOOL WEEK END
      tabItem(tabName = "Walc",
              h2("Consommation d'alcool en week-end"),
              tabBox(
                tabPanel("Histogramme", plotOutput("Walc")),
                tabPanel("Boîte à moustache", plotOutput("Walc_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("Walc", "Consommation d'alcool en week-end (1 faible)",
                            min = 1, max = 5,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("Walc_info")
              ),
              
      ),
      
      ## SANTE
      tabItem(tabName = "health",
              h2("Santé"),
              tabBox(
                tabPanel("Histogramme", plotOutput("health")),
                tabPanel("Boîte à moustache", plotOutput("health_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                sliderInput("health", "Santé (1 faible)",
                            min = 1, max = 5,
                            value = 0),
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("health_info")
              ),
              
      ),
      
      ## COURS SUPPL
      tabItem(tabName = "schoolsup",
              h2("Soutien extérieur"),
              tabBox(
                tabPanel("Histogramme", plotOutput("schoolsup")),
                tabPanel("Boîte à moustache", plotOutput("schoolsup_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("schoolsup", "Soutien extérieur",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("schoolsup_info")
              ),
              
      ),
      
      ## SUPPORT FAMILIALE
      tabItem(tabName = "famsup",
              h2("Support familiale"),
              tabBox(
                tabPanel("Histogramme", plotOutput("famsup")),
                tabPanel("Boîte à moustache", plotOutput("famsup_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("famsup", "Support familiale",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("famsup_info")
              ),
              
      ),
      
      ## COURS SUPP
      tabItem(tabName = "paid",
              h2("Cours supplémentaires"),
              tabBox(
                tabPanel("Histogramme", plotOutput("paid")),
                tabPanel("Boîte à moustache", plotOutput("paid_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("paid", "Cours supplémentaires",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("paid_info")
              ),
              
      ),
      
      ## Activités extra scolaire
      tabItem(tabName = "activities",
              h2("Activités extra scolaire"),
              tabBox(
                tabPanel("Histogramme", plotOutput("activities")),
                tabPanel("Boîte à moustache", plotOutput("activities_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("activities", "Activités extra scolaire",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("activities_info")
              ),
              
      ),
      
      ## Est allé en garderie
      tabItem(tabName = "nursery",
              h2("Est allé en garderie"),
              tabBox(
                tabPanel("Histogramme", plotOutput("nursery")),
                tabPanel("Boîte à moustache", plotOutput("nursery_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("nursery", "Est allé en garderie",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("nursery_info")
              ),
              
      ),
      
      ## Volonté de faire des études supérieures
      tabItem(tabName = "higher",
              h2("Volonté de faire des études supérieures"),
              tabBox(
                tabPanel("Histogramme", plotOutput("higher")),
                tabPanel("Boîte à moustache", plotOutput("higher_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("higher", "Volonté de faire des études supérieures",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("higher_info")
              ),
              
      ),
      
      ## Accés à internet
      tabItem(tabName = "internet",
              h2("Accés à internet"),
              tabBox(
                tabPanel("Histogramme", plotOutput("internet")),
                tabPanel("Boîte à moustache", plotOutput("internet_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("internet", "Accés à internet",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("internet_info")
              ),
              
      ),
      
      ## Relation amoureuse
      tabItem(tabName = "romantic",
              h2("Relation amoureuse"),
              tabBox(
                tabPanel("Histogramme", plotOutput("romantic")),
                tabPanel("Boîte à moustache", plotOutput("romantic_bam"))
              ),
              box(
                title = "Inputs", background = "black",
                radioButtons("romantic", "Relation amoureuse",
                             choices =  c("Total" = "both",
                                          "oui" = "yes",
                                          "non" = "no"),
                             selected = NULL,
                             inline = FALSE,
                             width = '800px'
                )
              ),
              box(
                title = "info", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("romantic_info")
              ),
              
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
                  choices =  c("age" = "age",
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
                  choices =  c("Shapiro-Wilk" = "sw",
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
      
      ## analyse unidimentionnelle
      tabItem(tabName = "unidimentional",
              h2("Analyse unidimentionelle"),
              box(
                title = "unidimentional", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("unidimentional")
              ),
              box(
                title = "info on final grade", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("univ_info")
              ),
              
              
      )
      
      
    )
  )
  
  
)