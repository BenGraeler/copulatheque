## copulas small

library(shiny)
library(spcopula)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # generate conditional slider
  output$paramA <- renderUI({
    paramB <- input$b
    limA <- spcopula:::limA(paramB)
    
    # check whether slider a has once been acivated
    if(is.null(input$a))
      a <- 0
    else 
      a <- input$a
    
    sliderInput("a", "parameter a:",  
                min = limA, max = 1, value = max(limA, a), step=0.01)
  })
    
  checkFrank <- function(param) {
    if(param==0)
      return(indepCopula(2))
    else
      return(frankCopula(param))
  }
  
  paramA <- reactive({
    if(is.null(input$a))
      return(0)
    else
      return(input$a)
  })
                     
  paramB <- reactive({
   if(is.null(input$b))
     return(0)
   else
     return(input$b)
  })
                                        
  cop <- reactive(switch(input$family,
                         asCopula = asCopula(c(paramA(),paramB())),
                         claytonCopula = claytonCopula(input$paramClayton),
                         cqsCopula = cqsCopula(c(input$a,input$b)),
                         frankCopula = checkFrank(input$paramFrank),
                         normalCopula = normalCopula(input$paramEllip),
                         gumbelCopula = gumbelCopula(input$paramGumbel),
                         tCopula = tCopula(input$paramEllip, df=input$df)))
  
  output$caption <- renderText(describeCop(cop(), "very short"))
  
  fun <- reactive(switch(input$fun,
                         dCopula = dCopula,
                         pCopula = pCopula))
  
  titleFun <- reactive(switch(input$fun,
                              dCopula = "PDF: strength of dependence",
                              pCopula = "CDF"))
  
#   zlabFun <- reactive(switch(input$fun,
#                               dCopula = "PDF",
#                               pCopula = "CDF"))

  output$kendallsTau <- renderText(paste("Kendall's tau:", round(tau(cop()),2)))
  
#   output$copulaSurface <- renderPlot(persp(cop(), fun(), xlab="u", ylab="v",
#                                            zlab="", main=titleFun(),
#                                            ticktype="detailed"))

#   output$copulaSample <- renderPlot({plot(rCopula(500,cop()),asp=1)})
  
  output$copulaPlots <- renderPlot({
    par(mfrow=c(1,2))
    persp(cop(), fun(),xlab="u", ylab="v",
          zlab="", main=titleFun(),
          ticktype="detailed")
    plot(rCopula(input$sampleSize, cop()), asp=1, 
         main=paste("sample of size", input$sampleSize),
         xlab="u", ylab="v")
  })
  
})