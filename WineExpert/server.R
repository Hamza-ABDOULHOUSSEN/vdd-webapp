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
    L = attributes(student)
    
    HTML(
      paste(paste("nombre de colonne : ", length(student)),
            paste(L$names),
            paste("nombre d'étudiant (ligne) : ", length(student$age)),
            sep="<br/>"))
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
  
  
  #analyse bivariee
  output$correlation <- renderPlot({
    
    x<-mat$G3
    alco<-mat$Dalc
    alc2<-mat$Walc
    stt<-mat$studytime
    trav<-mat$traveltime
    fam<-mat$famrel
    age<-mat$age
    med<-mat$Medu
    fef<-mat$Fedu
    abs<-mat$absences
    
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
    
    
    variable=input$variables
    type=input$types
    if(variable=="age"){
      if(type=="sw"){
        y<-shapiro.test(mat$age)
        HTML(
          paste(paste("test of normality distribution for the age : ",y$p.value),"",
                sep="<br/>")) 
      }
    }
    if(variable=="famrel"){
      if(type=="sw"){
        y<-shapiro.test(mat$famrel)
        HTML(
          paste(paste("test of normality distribution for the family relation : ",y$p.value ),"",
                sep="<br/>"))
      }
    }
    if(variable=="absences"){
      if(type=="sw"){
        y<-shapiro.test(mat$absences)
        HTML(
          paste(paste("test of normality distribution for the absences : ",y$p.value ),"",
                sep="<br/>"))
        
      }
    }
    if(variable=="G3"){
      if(type=="sw"){
        y<-shapiro.test(mat$G3)
        HTML(
          paste(paste("test of normality distribution for the notes : ",y$p.value ),"",
                sep="<br/>"))
      }
    }
    
  })
  
  
  #analyse univariable
  
  output$unidimentional <- renderPlot({
    sub1 = subset(student, romantic.x == "yes")
    data1 = sub1$G3.x
    couleur='#dea024'
    ggplot(student,aes(G3.x, fill= romantic.x, color=romantic.x))+ geom_density(data=student, fill=couleur, alpha=0.3)  + scale_x_continuous(name="Final result", breaks=seq(0,20,1)) + scale_y_continuous(name="Number of student")

  })
  
  
  
  output$univ_info<-renderText({ 
    
    var1 = "in relationship"
    var2 = "single"
    sub1 = subset(student, romantic.x == "yes")
    data1 = sub1$G3.x
    sub2 = subset(student, romantic.x == "no")
    data2 = sub2$G3.x
    
    
    
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
