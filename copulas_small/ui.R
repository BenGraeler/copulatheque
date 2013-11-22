# copulas small

library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Different Copula Families"),
  
  sidebarPanel("This small shiny app illustrates a couple of copula families implemented in the", 
               a("copula", href="http://cran.r-project.org/web/packages/copula/index.html", target="_blank"), 
               "and",
               a("spcopula", href="http://r-forge.r-project.org/projects/spcopula/", target="_blank"), 
               "R packages. Please select the copula family and corresponding parameters below.",
               selectInput("family", "Copula family", 
                           list("Asymmetric Copula" =  "asCopula",
                                "Clayton Copula" = "claytonCopula",
                                "CQS copula" = "cqsCopula",
                                "Frank Copula" = "frankCopula",
                                "Gaussian Copula" = "normalCopula",
                                "Gumbel Copula" = "gumbelCopula",
                                "Student t Copula" = "tCopula")),
               
               selectInput("fun", "function",
                           list("PDF" = "dCopula",
                                "CDF" = "pCopula")),
               
               # asCopula and cqsCopula
               conditionalPanel(condition = "input.family == 'cqsCopula' || input.family == 'asCopula'",
                                sliderInput("b", "parameter b:",
                                            min = -1, max = 1, value = 0, step=0.01),
                                uiOutput("paramA")),
               
               # gumbel Copula
               conditionalPanel(condition = "input.family == 'gumbelCopula'",
                                sliderInput("paramGumbel", "parameter:",
                                            min=1, max=10, value=1, step=0.01)),
               
               # Gaussian and Student t Copulas
               conditionalPanel(condition = "input.family == 'normalCopula' || input.family == 'tCopula'",
                                sliderInput("paramEllip", "parameter:",
                                            min=-1, max=1, value=0, step=0.01)),
               
               # Frank Copula
               conditionalPanel(condition = "input.family == 'frankCopula'",
                                sliderInput("paramFrank", "parameter:",
                                            min=-10, max=10, value=0, step=0.01)),

               # Student t Copulas
               conditionalPanel(condition = "input.family == 'tCopula'",
                                sliderInput("df", "degrees of freedom:",
                                            min=0, max=20, value=2, step=0.01)),
               
               # Clayton Copula
               conditionalPanel(condition = "input.family == 'claytonCopula'",
                                sliderInput("paramClayton", "parameter:",
                                            min=-1, max=10, value=0, step=0.01)),
               
               # sample length
               sliderInput("sampleSize", "sample size:",
                           min=10, max=1000, value=250, step=1)),
  
  mainPanel(
    h3(textOutput("caption")),
#     plotOutput("copulaSurface"),
    plotOutput("copulaPlots"),
    textOutput("kendallsTau"))
  
))