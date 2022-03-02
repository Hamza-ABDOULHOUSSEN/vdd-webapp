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
  
  # age
  output$age <- renderPlot({
    
    variable = input$age
    
    if (variable == "both") {
      ggplot(student, aes(G3.x, fill=age, color=age)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, age == variable)
      data = sub$G3.x
      ggplot(student, aes(G3.x)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  # sexe
  output$sexe <- renderPlot({
    
    variable = input$sexe
    
    if (variable == "both") {
      ggplot(student, aes(G3.x, fill=sex, color=sex)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, sex == variable)
      data = sub$G3.x
      ggplot(student, aes(G3.x)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$sexe_bam <- renderPlot({
    
    variable = input$sexe
    
    if (variable == "both") {
      ggplot(student, aes(G3.x, fill=sex, color=sex)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, sex == variable)
      data = sub$G3.x
      ggplot(student, aes(G3.x)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  # adresse
  output$adress <- renderPlot({
    
    variable = input$adress
    
    if (variable == "both") {
      ggplot(student, aes(G3.x, fill=address, color=address)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "R") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, address == variable)
      data = sub$G3.x
      ggplot(student, aes(G3.x)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
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
