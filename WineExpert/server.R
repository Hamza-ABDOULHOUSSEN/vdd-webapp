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
  
  output$home_info <- renderText({ 
    
    var1 = "données"
    L = attributes(mat)
    
    HTML(
      paste(paste("nombre de colonne : ", length(mat)),
            paste("nombre d'étudiant (ligne) : ", length(mat$age)),
            "",
            "<details> <summary> <b> liste des colonnes <b/> </summary>",
            sep="<br/>",
            paste0(L$names,
                   sep="<br/>",
                   collapse= ""
            ),
            "</details>"
            )
      )
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
      ggplot(sub, aes(G3.x)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
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
  
  output$sexe_info <- renderText({ 
    
    var1 = "homme"
    var2 = "femme"
    sub1 = subset(student, sex == "M")
    data1 = sub1$G3.x
    sub2 = subset(student, sex == "F")
    data2 = sub2$G3.x
    
    HTML(
      paste(paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
    })
  
  # adresse
  output$adress <- renderPlot({
    
    variable = input$adress
    
    if (variable == "both") {
      ggplot(student, aes(G3.x, fill=address, color=address)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "U") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, address == variable)
      data = sub$G3.x
      ggplot(sub, aes(G3.x)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$adress_bam <- renderPlot({
    
    variable = input$adress
    
    if (variable == "both") {
      ggplot(student, aes(G3.x, fill=address, color=address)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, address == variable)
      data = sub$G3.x
      ggplot(student, aes(G3.x)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$adress_info <- renderText({ 
    
    var1 = "milieu rural"
    var2 = "milieu urbain"
    sub1 = subset(student, address == "R")
    data1 = sub1$G3.x
    sub2 = subset(student, address == "U")
    data2 = sub2$G3.x
    
    HTML(
      paste(paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
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
