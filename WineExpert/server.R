library ( shiny )
library(ggplot2)

shinyServer ( function (input , output ) {
  
  # DATA IMPORT IN 'student'
  mat=read.table("../data/student/student-mat.csv",sep=";",header=TRUE)
  por=read.table("../data/student/student-por.csv",sep=";",header=TRUE)
  
  student=merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
  
  output$histo <- renderPlot({
    age = student$age
    hist(age, main="Histogramme de l'age des élèves", col="darkblue")
  })
  
  # data
  output$data <- renderPlot({
    
    if (input$sexe == "both") {
      ggplot(student, aes(G3.x, color=sex)) + scale_color_manual(values = c("F" = "red", "M"="blue")) + geom_histogram() + ggtitle("Histogramme") + geom_vline(xintercept = c(mean(student[student$sex == "M", ]$G3.x), mean(student[student$sex == "F", ]$G3.x)), colour = c("blue", "red"), linetype = c("longdash","longdash"))
    }
    else {
      sub = subset(student, sex == input$sexe)
      
      if (input$sexe == "M") {
        couleur = "blue"
      }
      else {
        couleur = "red"
      }
      
      data = sub$G3.x
      ggplot(sub, aes(G3.x)) + geom_histogram(col=couleur) + ggtitle("Histogramme") + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash")
    }
  })
  
  # gaussienne
  output$gaussienne <- renderPlot({
    
    x <- seq(-10, 10, 20 / (input$n - 1))
    y <- dnorm(x)
    
    if (input$check) {
      plot(x,y, type="l", col=input$button, lwd=input$epaisseur)
    }
    else {
      plot(x,y, col=input$button, lwd=input$epaisseur)
    }
  })
  
  # sinus
  output$sinus <- renderPlot({
    
    x <- seq(-10, 10, 20 / (input$n - 1))
    y <- sin(2*pi*x)
    
    if (input$check) {
      plot(x,y, type="l", col=input$button, lwd=input$epaisseur)
    }
    else {
      plot(x,y, col=input$button, lwd=input$epaisseur)
    }
  })
  
})
