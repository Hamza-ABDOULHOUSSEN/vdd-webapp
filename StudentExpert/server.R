library ( shiny )
library(ggplot2)

shinyServer ( function (input , output ) {
  
  # DATA IMPORT IN 'student'
  #mat=read.table("../data/student/student-mat.csv",sep=";",header=TRUE)
  #por=read.table("../data/student/student-por.csv",sep=";",header=TRUE)
  
  #student=merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
  
  output$home_alert <- renderText({
    if (!is.null(input$file)) {return(NULL)}
    HTML(paste(
      "<div class='alert'> <span class='closebtn' onclick=",
      '"',"this.parentElement.style.display='none';",'"',">&times;</span> Attention, il faut charger des données pour afficher les visualisations </div>")
    )
      
  })
  
  output$home_info <- renderText({ 
    
    
    if (!is.null(input$file)) {
      
      student <- read.table(input$file$datapath, sep=";",header=TRUE)
      
      L = attributes(student)
      
      HTML(
        paste(
        "<div class='square'> <h3> Informations </h3> ",
        paste("nombre de colonne : ", length(student)),
        paste("nombre d'étudiant (ligne) : ", length(student$age)),
        "",
        "<details> <summary> <b> liste des colonnes <b/> </summary>",
        sep="<br/>",
        paste0(L$names,
               sep="<br/>",
               collapse= ""
        ),
        "</details> </div>"
        )
      )
    }
    
  })
  
  output$home_info_footer <- renderText({ 
      
    
      HTML("<footer class='footer text-faded text-center py-5'> <div class='container'><p class='m-0 small'>Copyright © StudentExpert <br>Page réalisée par Killian CRESSANT et Hamza ABDOULHOUSSEN</p></div> </footer>")
    
  })
  
  # AGE
  
  output$age <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$age
    
    couleur = "#F8766D"
    
    sub = subset(student, age == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$age_bam <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$age
    
    couleur = "#F8766D"
    
    sub = subset(student, age == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0, 20, 1))
  })
  
  output$age_info <- renderText({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    titre = paste("Age ", input$age)
    var1 = ""
    sub1 = subset(student, age == input$age)
    data1 = sub1$G3
    correl = cor(student$age, student$failures)
    
    HTML(
      paste(titre,
            paste("nombre ", var1, " : ", length(data1)),
            paste("moyenne ", var1, " : ", round(mean(data1)), digits=2),
            paste("min ", var1, " : ", min(data1)),
            paste("1er quartile  ", var1, " : ", quantile(data1, 0.25)),
            paste("mediane ", var1, " : ", median(data1)),
            paste("3eme quartile  ", var1, " : ", quantile(data1, 0.75)),
            paste("max ", var1, " : ", max(data1)), "",
            paste("correlation : ", round(correl,2)),
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
  
  output$sexe_pro <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$sexe
    
    if (variable == "both") {
      if (input$reg == "yes") {
        ggplot(student, aes(x=G1, y=G2, color=sex)) + geom_point() + xlim(0,20) + ylim(0,20) + geom_smooth(method=lm)
      }
      else {
        ggplot(student, aes(x=G1, y=G2, color=sex)) + geom_point() + xlim(0,20) + ylim(0,20)
      }
      
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
      if (input$reg == "yes") {
        ggplot(sub, aes(x=G1, y=G2, color=sex)) + geom_point() + xlim(0,20) + ylim(0,20) + geom_smooth(method=lm)
      }
      else {
        ggplot(sub, aes(x=G1, y=G2, color=sex)) + geom_point() + xlim(0,20) + ylim(0,20)
      }
      
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
    variable = input$medu
      
    couleur = "#F8766D" 
    
    sub = subset(student, Medu == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
  })
  
  output$medu_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    titre = paste("Niveau d'education ", input$medu)
    var1 = ""
    sub1 = subset(student, Medu == input$medu)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ",var1," : ", length(data1)),
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2),
            paste("min ",var1," : ", min(data1)),
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)),
            paste("mediane ",var1," : ", median(data1)),
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)),
            paste("max ",var1," : ", max(data1)),
            sep="<br/>"))
  })
  
  # FEDU
  output$fedu <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$fedu
    
    couleur = "#F8766D"
    
    sub = subset(student, Fedu == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$fedu_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$fedu
    
    couleur = "#F8766D" 
    
    sub = subset(student, Fedu == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
  })
  
  output$fedu_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    titre = paste("Niveau d'education ", input$fedu)
    var1 = ""
    sub1 = subset(student, Fedu == input$fedu)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ",var1," : ", length(data1)),
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2),
            paste("min ",var1," : ", min(data1)),
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)),
            paste("mediane ",var1," : ", median(data1)),
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)),
            paste("max ",var1," : ", max(data1)),
            sep="<br/>"))
  })
  
  # FAMREL
  output$famrel <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$famrel
    
    couleur = "#F8766D"
    
    sub = subset(student, famrel == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$famrel_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$famrel
    
    couleur = "#F8766D" 
    
    sub = subset(student, famrel == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
  })
  
  output$famrel_info <- renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    titre = paste("Niveau d'education ", input$famrel)
    var1 = ""
    sub1 = subset(student, famrel == input$famrel)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ",var1," : ", length(data1)),
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2),
            paste("min ",var1," : ", min(data1)),
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)),
            paste("mediane ",var1," : ", median(data1)),
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)),
            paste("max ",var1," : ", max(data1)),
            sep="<br/>"))
  })
  
  # FREETIME
  
  output$freetime <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$freetime
    
    couleur = "#F8766D"
    
    sub = subset(student, freetime == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$freetime_bam <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$freetime
    
    couleur = "#F8766D"
    
    sub = subset(student, freetime == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0, 20, 1))
  })
  
  output$freetime_info <- renderText({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    titre = paste("Temps libre après les cours ", input$freetime)
    var1 = ""
    sub1 = subset(student, freetime == input$freetime)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ", var1, " : ", length(data1)),
            paste("moyenne ", var1, " : ", round(mean(data1)), digits=2),
            paste("min ", var1, " : ", min(data1)),
            paste("1er quartile  ", var1, " : ", quantile(data1, 0.25)),
            paste("mediane ", var1, " : ", median(data1)),
            paste("3eme quartile  ", var1, " : ", quantile(data1, 0.75)),
            paste("max ", var1, " : ", max(data1)),
            sep="<br/>"))
  })
  
  # GOOUT
  
  output$goout <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$goout
    
    couleur = "#F8766D"
    
    sub = subset(student, goout == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$goout_bam <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$goout
    
    couleur = "#F8766D"
    
    sub = subset(student, goout == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0, 20, 1))
  })
  
  output$goout_info <- renderText({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    titre = paste("Sortie ", input$goout)
    var1 = ""
    sub1 = subset(student, goout == input$goout)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ", var1, " : ", length(data1)),
            paste("moyenne ", var1, " : ", round(mean(data1)), digits=2),
            paste("min ", var1, " : ", min(data1)),
            paste("1er quartile  ", var1, " : ", quantile(data1, 0.25)),
            paste("mediane ", var1, " : ", median(data1)),
            paste("3eme quartile  ", var1, " : ", quantile(data1, 0.75)),
            paste("max ", var1, " : ", max(data1)),
            sep="<br/>"))
  })
  
  # DALC
  
  output$Dalc <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$Dalc
    
    couleur = "#F8766D"
    
    sub = subset(student, Dalc == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$Dalc_bam <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$Dalc
    
    couleur = "#F8766D"
    
    sub = subset(student, Dalc == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0, 20, 1))
  })
  
  output$Dalc_info <- renderText({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    titre = paste("Consommation d'alcool en semaine ", input$Dalc)
    var1 = ""
    sub1 = subset(student, Dalc == input$Dalc)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ", var1, " : ", length(data1)),
            paste("moyenne ", var1, " : ", round(mean(data1)), digits=2),
            paste("min ", var1, " : ", min(data1)),
            paste("1er quartile  ", var1, " : ", quantile(data1, 0.25)),
            paste("mediane ", var1, " : ", median(data1)),
            paste("3eme quartile  ", var1, " : ", quantile(data1, 0.75)),
            paste("max ", var1, " : ", max(data1)),
            sep="<br/>"))
  })
  
  # WALC
  
  output$Walc <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$Walc
    
    couleur = "#F8766D"
    
    sub = subset(student, Walc == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$Walc_bam <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$Walc
    
    couleur = "#F8766D"
    
    sub = subset(student, Walc == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0, 20, 1))
  })
  
  output$Walc_info <- renderText({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    titre = paste("Consommation d'alcool en week-end ", input$Walc)
    var1 = ""
    sub1 = subset(student, Walc == input$Walc)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ", var1, " : ", length(data1)),
            paste("moyenne ", var1, " : ", round(mean(data1)), digits=2),
            paste("min ", var1, " : ", min(data1)),
            paste("1er quartile  ", var1, " : ", quantile(data1, 0.25)),
            paste("mediane ", var1, " : ", median(data1)),
            paste("3eme quartile  ", var1, " : ", quantile(data1, 0.75)),
            paste("max ", var1, " : ", max(data1)),
            sep="<br/>"))
  })
  
  ## HEALTH
  
  output$health <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$health
    
    couleur = "#F8766D"
    
    sub = subset(student, health == variable)
    data = sub$G3
    ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
  })
  
  output$health_bam <- renderPlot({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    variable = input$health
    
    couleur = "#F8766D"
    
    sub = subset(student, health == variable)
    data = sub$G3
    ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0, 20, 1))
  })
  
  output$health_info <- renderText({
    if (is.null(input$file))
    {
      return (NULL)}
    
    student <- read.table(input$file$datapath, sep = ";", header = TRUE)
    titre = paste("Santé ", input$health)
    var1 = ""
    sub1 = subset(student, health == input$health)
    data1 = sub1$G3
    
    HTML(
      paste(titre,
            paste("nombre ", var1, " : ", length(data1)),
            paste("moyenne ", var1, " : ", round(mean(data1)), digits=2),
            paste("min ", var1, " : ", min(data1)),
            paste("1er quartile  ", var1, " : ", quantile(data1, 0.25)),
            paste("mediane ", var1, " : ", median(data1)),
            paste("3eme quartile  ", var1, " : ", quantile(data1, 0.75)),
            paste("max ", var1, " : ", max(data1)),
            sep="<br/>"))
  })
  
  ## SCHOOLSUP
  
  output$schoolsup <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$schoolsup
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=schoolsup, color=schoolsup)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, schoolsup == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$schoolsup_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$schoolsup
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=schoolsup, color=schoolsup)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, schoolsup == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$schoolsup_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, schoolsup == "yes")
    data1 = sub1$G3
    sub2 = subset(student, schoolsup == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Soutien extérieur",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
  ## FAMSUP
  output$famsup <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$famsup
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=famsup, color=famsup)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, famsup == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$famsup_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$famsup
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=famsup, color=famsup)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, famsup == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$famsup_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, famsup == "yes")
    data1 = sub1$G3
    sub2 = subset(student, famsup == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Support familiale",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
## PAID
  output$paid <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$paid
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=paid, color=paid)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, paid == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$paid_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$paid
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=paid, color=paid)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, paid == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$paid_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, paid == "yes")
    data1 = sub1$G3
    sub2 = subset(student, paid == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Cours supplémentaires",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
  ## ACTIVITIES
  output$activities <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$activities
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=activities, color=activities)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, activities == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$activities_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$activities
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=activities, color=activities)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, activities == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$activities_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, activities == "yes")
    data1 = sub1$G3
    sub2 = subset(student, activities == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Activités extra scolaire",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
  ## NURSERY
  output$nursery <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$nursery
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=nursery, color=nursery)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, nursery == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$nursery_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$nursery
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=nursery, color=nursery)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, nursery == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$nursery_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, nursery == "yes")
    data1 = sub1$G3
    sub2 = subset(student, nursery == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Est allé en garderie",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
  ## HIGHER
  output$higher <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$higher
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=higher, color=higher)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, higher == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$higher_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$higher
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=higher, color=higher)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, higher == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$higher_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, higher == "yes")
    data1 = sub1$G3
    sub2 = subset(student, higher == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Volonté de faire des études supérieures",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
  ## INTERNET
  output$internet <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$internet
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=internet, color=internet)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, internet == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$internet_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$internet
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=internet, color=internet)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, internet == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$internet_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, internet == "yes")
    data1 = sub1$G3
    sub2 = subset(student, internet == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Accés à internet",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
  ## ROMANTIC
  output$romantic <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$romantic
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=romantic, color=romantic)) + geom_histogram(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, romantic == variable)
      data = sub$G3
      ggplot(sub, aes(G3)) + geom_histogram(data=sub, fill=couleur) + geom_vline(xintercept = mean(data), colour = couleur, linetype = "longdash") + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1)) + scale_y_continuous(name="Nombre d'élève")
    }
  })
  
  output$romantic_bam <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    variable = input$romantic
    
    if (variable == "both") {
      ggplot(student, aes(G3, fill=romantic, color=romantic)) + geom_boxplot(alpha=0.8) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
    else {
      
      if (variable == "no") {
        couleur = "#F8766D"
      }
      else {
        couleur = "#00BFC4"
      }
      
      sub = subset(student, romantic == variable)
      data = sub$G3
      ggplot(student, aes(G3)) + geom_boxplot(data=sub, fill=couleur) + scale_x_continuous(name="Résultat final", breaks=seq(0,20,1))
    }
  })
  
  output$romantic_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    var1 = "avec"
    var2 = "sans"
    sub1 = subset(student, romantic == "yes")
    data1 = sub1$G3
    sub2 = subset(student, romantic == "no")
    data2 = sub2$G3
    
    HTML(
      paste("Relation amoureuse",
            paste("nombre ",var1," : ", length(data1)), paste("nombre ",var2," : ", length(data2)), "",
            paste("moyenne ",var1," : ", round(mean(data1)), digits=2), paste("moyenne ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1er quartile  ",var1," : ", quantile(data1,0.25)), paste("1er quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("mediane ",var1," : ", median(data1)), paste("mediane ",var2," : ", median(data2)), "",
            paste("3eme quartile  ",var1," : ", quantile(data1,0.75)), paste("3eme quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            sep="<br/>"))
  })
  
  #analyse bivariee
  output$correlation <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    
    x<-student$G3
    alco<-student$Dalc
    alc2<-student$Walc
    stt<-student$studytime
    trav<-student$traveltime
    fam<-student$famrel
    age<-student$age
    med<-student$Medu
    fef<-student$Fedu
    abs<-student$absences
    
    a=cor(x,alco)
    b=cor(x,alc2)
    c=cor(x,stt)
    d=cor(x,trav)
    e=cor(x,fam)
    f=cor(x,age)
    g=cor(x,med)
    h=cor(x,fef)
    i=cor(x,abs)
    
    
    
    corr<-c(a,b,c,d,e,f,g,h,i)
    x<-1:length(corr)
    info<-c("alcool_work","alcool_week","studytime","travel","family_relation","age","mother_ed","father_ed","abs")
    plot(x,corr, xaxt="n", pch=21,bg='red',col='blue',cex=3)
    axis(1, at=1:9, labels=info)
    
  })
  
  output$analyse_info <- renderText({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    
    variable=input$variables
    type=input$types
    
    if(variable=="age"){
      if(type=="sw"){
        y<-shapiro.test(student$age)
        arg = "age"
      }
    }
    else if(variable=="famrel"){
      if(type=="sw"){
        y<-shapiro.test(student$famrel)
        arg = "family relation"
      }
    }
    else if(variable=="absences"){
      if(type=="sw"){
        y<-shapiro.test(student$absences)
        arg = "absences"
      }
    }
    else {
      if(type=="sw"){
        y<-shapiro.test(student$G3)
        arg = "notes"
      }
    }
    
    HTML(
      paste(paste("test of normality distribution for the ", arg ," : ", y$p.value ),"",
            sep="<br/>"))
    
  })
  
  #analyse univariable
  
  output$unidimentional <- renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    
    sub1 = subset(student, romantic == "yes")
    data1 = sub1$G3
    couleur='#dea024'
    ggplot(student,aes(G3, fill= romantic, color=romantic))+ geom_density(data=student, fill=couleur, alpha=0.3)  + scale_x_continuous(name="Final result", breaks=seq(0,20,1)) + scale_y_continuous(name="Number of student")
    
  })
  
  output$univ_info<-renderText({ 
    if (is.null(input$file)) {return(NULL)}
    
    student <- read.table(input$file$datapath, sep=";",header=TRUE)
    
    var1 = "in relationship"
    var2 = "single"
    sub1 = subset(student, romantic == "yes")
    data1 = sub1$G3
    sub2 = subset(student, romantic == "no")
    data2 = sub2$G3
    
    
    
    HTML(
      paste(paste("Position study :"),"",
            paste("number ",var1," : ", length(data1)), paste("number ",var2," : ", length(data2)), "",
            paste("mean ",var1," : ", round(mean(data1)), digits=2), paste("mean ",var2," : ", round(mean(data2), digits=2)), "",
            paste("min ",var1," : ", min(data1)), paste("min ",var2," : ", min(data2)), "",
            paste("1st quartile  ",var1," : ", quantile(data1,0.25)), paste("1st quartile  ",var2," : ", quantile(data2,0.25)), "",
            paste("median ",var1," : ", median(data1)), paste("median ",var2," : ", median(data2)), "",
            paste("3rd quartile  ",var1," : ", quantile(data1,0.75)), paste("3rd quartile  ",var2," : ", quantile(data2,0.75)), "",
            paste("max ",var1," : ", max(data1)), paste("max ",var2," : ", max(data2)), "",
            paste("scattering study :"),"",
            paste("variance  ",var1," : ", round(var(data1),digits = 2)), paste("variance ",var2," : ", round(var(data2),digits = 2)), "",
            paste("coefficient of variation ",var1," : ", round(sqrt(var(data1))/mean(data1),digits=2)), paste("coefficient of variation ",var2," : ", round(sqrt(var(data2))/mean(data2),digits=2)), "",
            paste("interquartil  ",var1," : ", quantile(data1,0.75)-quantile(data1,0.25)), paste("interquartil ",var2," : ", quantile(data2,0.75)-quantile(data2,0.25)), "",
            sep="<br/>"))
    
    
    
    
    
    
  })
  
  
})
