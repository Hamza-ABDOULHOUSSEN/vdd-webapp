library(shiny)
library(shinydashboard)

dashboardPage(
  
  ## HEADER
  dashboardHeader(title = "Basic dashboard"),
  
  ## SIDEBAR
  dashboardSidebar(
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
  ),
  
  
  ## BODY
  dashboardBody(
    plotOutput("gaussienne"),
    plotOutput("sinus")
  )
)