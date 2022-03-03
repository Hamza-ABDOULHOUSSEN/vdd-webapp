library ( shiny )
library(ggplot2)

shinyServer ( function (input , output ) {
  
  # DATA IMPORT IN 'student'
  #mat=read.table("../data/student/student-mat.csv",sep=";",header=TRUE)
  #por=read.table("../data/student/student-por.csv",sep=";",header=TRUE)
  
  #student=merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
  
  output$home_info <- renderText({ 
    
    
    if (!is.null(input$file)) {
      
      student <- read.table(input$file$datapath, sep=";",header=TRUE)
      
      L = attributes(student)
      
      HTML(
        paste(
        paste("nombre de colonne : ", length(student)),
        paste("nombre d'étudiant (ligne) : ", length(student$age)),
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
    }
    
  })
  
  # AGE
  output$age <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$age
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=age, color=age)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, age == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$age_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$age
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=age, color=age)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, age == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$age_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "élève de 15 ans "
    var2 = "élève de 16 ans "
    sub1 = subset(student, age == 15)
    data1 = sub1$G3
    sub2 = subset(student, age == 16)
    data2 = sub2$G3
    
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
  
  # sexe
  output$sexe <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$sexe
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=sex, color=sex)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, sex == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$sexe_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$sexe
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=sex, color=sex)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, sex == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$sexe_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "homme"
    var2 = "femme"
    sub1 = subset(student, sex == "M")
    data1 = sub1$G3
    sub2 = subset(student, sex == "F")
    data2 = sub2$G3
    
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
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$adress
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=address, color=address)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "U") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, address == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$adress_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$adress
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=address, color=address)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "F") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, address == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$adress_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "milieu rural"
    var2 = "milieu urbain"
    sub1 = subset(student, address == "R")
    data1 = sub1$G3
    sub2 = subset(student, address == "U")
    data2 = sub2$G3
    
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
  
  # cohab parents
  output$pstatus <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$pstatus
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=Pstatus, color=Pstatus)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "T") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, Pstatus == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$pstatus_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$pstatus
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=Pstatus, color=Pstatus)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "T") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, Pstatus == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$pstatus_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec parents ensemble"
    var2 = "avec parents séparés"
    sub1 = subset(student, Pstatus == "T")
    data1 = sub1$G3
    sub2 = subset(student, Pstatus == "A")
    data2 = sub2$G3
    
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
  
  # MEDU
  output$medu <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$medu
    
    couleur = "#F8766D"

    sub = subset(student, Medu == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    })
  
  output$medu_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$pstatus
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=Pstatus, color=Pstatus)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "T") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, Pstatus == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$medu_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    titre = paste("Niveau d'education ", input$medu)
    var1 = ""
    sub1 = subset(student, Medu == input$medu)
    data1 = sub1$G3
    
    HTML(
      paste(paste("nombre ",var1," : ", length(data1)),
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2),
            paste("min ",var1," : ", min(data1)),
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)),
            paste("mediane ",var1," : ", median(data1)),
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)),
            paste("max ",var1," : ", max(data1)),
            sep="<br/>"))
  })
  
})
