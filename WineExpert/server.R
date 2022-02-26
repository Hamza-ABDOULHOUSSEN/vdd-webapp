library ( shiny )


shinyServer ( function (input , output ) {
  
  # DATA IMPORT IN 'student'
  mat=read.table("../data/student/student-mat.csv",sep=";",header=TRUE)
  por=read.table("../data/student/student-por.csv",sep=";",header=TRUE)
  
  student=merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
  
  output$histo <- renderPlot({
    age = student$age
    hist(age, main="Histogramme de l'age des élèves", col="darkblue")
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
