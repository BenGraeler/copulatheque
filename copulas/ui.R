# shiny-app copulas

library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Different Copula Families"),
  
  sidebarPanel("This small shiny app illustrates a couple of copula families implemented in the", 
               a("copula", href="http://cran.r-project.org/web/packages/copula/index.html", target="_blank"), 
               ", ",
               a("VineCopula", href="http://cran.r-project.org/web/packages/VineCopula/index.html", target="_blank"), 
               "and",
               a("spcopula", href="http://r-forge.r-project.org/projects/spcopula/", target="_blank"), 
               "R packages. This and additional scripts can be found in my",
               a("GitHub repository.", href="http://github.com/BenGraeler/shiny-apps/", target="_blank"),
               "Please select the copula family and corresponding parameters below.",               
               selectInput("family", "Copula family", 
                           list("Asymmetric Copula" =  "asCopula",
                                "BB1: Clayton-Gumbel" = "BB1Copula",
                                "BB6: Joe-Gumbel" = "BB6Copula",
                                "BB7: Joe-Clayton" = "BB7Copula",
                                "BB8: Joe-Frank" = "BB8Copula",
                                "Clayton Copula" = "claytonCopula",
                                "CQS copula" = "cqsCopula",
                                "Frank Copula" = "frankCopula",
                                "Gaussian Copula" = "normalCopula",
                                "Gumbel Copula" = "gumbelCopula",
                                "Joe Copula" = "joeBiCopula",
                                "Student t Copula" = "tCopula",
                                "Tawn type 1 Copula" = "tawnT1Copula",
                                "Tawn type 2 Copula" = "tawnT2Copula",
                                "biv. Spatial Copula" = "spCopula")),
               
               selectInput("margin", "Marignal distribution function",
                           list("uniform" = "unif",
                                "Guassian" = "norm")),
               
               # asCopula and cqsCopula
               conditionalPanel(condition = "input.family == 'cqsCopula' || input.family == 'asCopula'",
                                sliderInput("b", "parameter b:",
                                            min = -1, max = 1, value = 0, step=0.01),
                                uiOutput("paramA")),
               
               # rotated versions?
               conditionalPanel(condition = "input.family != 'cqsCopula' && input.family != 'asCopula' && input.family != 'frankCopula' && input.family != 'normalCopula' && input.family != 'tCopula' && input.family != 'spCopula'",
                                selectInput("rot", "rotated version:",
                                            list("none"= "none",
                                                 "90 degrees" = "r90",
                                                 "survival" = "sur",
                                                 "270 degrees" = "r270"))),
               ## BB1 Copula
               # none and survival
               conditionalPanel(condition = "input.family == 'BB1Copula' && (input.rot == 'none' || input.rot == 'sur')",
                                sliderInput("param1BB1pos", "first parameter:",
                                            min=0.01, max=10, value=1, step=0.01),
                                sliderInput("param2BB1pos", "second parameter:",
                                            min=1, max=10, value=1, step=0.01)),
               # r90 and r270
               conditionalPanel(condition = "input.family == 'BB1Copula' && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("param1BB1neg", "first parameter:",
                                            min=-10, max=-0.01, value=-1, step=0.01),
                                sliderInput("param2BB1neg", "second parameter:",
                                            min=-10, max=-1, value=-1, step=0.01)),
               
               ## BB6 Copula
               # none and survival
               conditionalPanel(condition = "input.family == 'BB6Copula' && (input.rot == 'none' || input.rot == 'sur')",
                                sliderInput("param1BB6pos", "first parameter:",
                                            min=1, max=10, value=1, step=0.01),
                                sliderInput("param2BB6pos", "second parameter:",
                                            min=1, max=10, value=1, step=0.01)),
               # r90 and r270
               conditionalPanel(condition = "input.family == 'BB6Copula' && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("param1BB6neg", "first parameter:",
                                            min=-10, max=-1, value=-1, step=0.01),
                                sliderInput("param2BB6neg", "second parameter:",
                                            min=-10, max=-1, value=-1, step=0.01)),
               
               ## BB7 Copula
               # none and survival
               conditionalPanel(condition = "input.family == 'BB7Copula' && (input.rot == 'none' || input.rot == 'sur')",
                                sliderInput("param1BB7pos", "first parameter:",
                                            min=1, max=10, value=1, step=0.01),
                                sliderInput("param2BB7pos", "second parameter:",
                                            min=0.01, max=10, value=1, step=0.01)),
               # r90 and r270
               conditionalPanel(condition = "input.family == 'BB7Copula' && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("param1BB7neg", "first parameter:",
                                            min=-10, max=-1, value=-1, step=0.01),
                                sliderInput("param2BB7neg", "second parameter:",
                                            min=-10, max=-0.01, value=-1, step=0.01)),
               
               ## BB8 Copula
               # none and survival
               conditionalPanel(condition = "input.family == 'BB8Copula' && (input.rot == 'none' || input.rot == 'sur')",
                                sliderInput("param1BB8pos", "first parameter:",
                                            min=1, max=10, value=1, step=0.01),
                                sliderInput("param2BB8pos", "second parameter:",
                                            min=0.01, max=1, value=1, step=0.01)),
               # r90 and r270
               conditionalPanel(condition = "input.family == 'BB8Copula' && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("param1BB8neg", "first parameter:",
                                            min=-10, max=-1, value=-1, step=0.01),
                                sliderInput("param2BB8neg", "second parameter:",
                                            min=-1, max=-0.01, value=-1, step=0.01)),
               ## Clayton Copula
               # none
               conditionalPanel(condition = "input.family == 'claytonCopula' && input.rot == 'none'",
                                sliderInput("paramClaytonNone", "parameter:",
                                            min=-1, max=10, value=0, step=0.01)),
               
               # survival
               conditionalPanel(condition = "input.family == 'claytonCopula' && input.rot == 'sur'",
                                sliderInput("paramClaytonSur", "parameter:",
                                            min=0.01, max=10, value=1, step=0.01)),
               
               # r90 and r270
               conditionalPanel(condition = "input.family == 'claytonCopula' && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("paramClaytonNeg", "parameter:",
                                            min=-10, max=-0.01, value=-1, step=0.01)),
               
               ## Frank Copula
               conditionalPanel(condition = "input.family == 'frankCopula'",
                                sliderInput("paramFrank", "parameter:",
                                            min=-10, max=10, value=0, step=0.01)),
               
               ## Gaussian and Student t Copulas
               conditionalPanel(condition = "input.family == 'normalCopula' || input.family == 'tCopula'",
                                sliderInput("paramEllip", "parameter:",
                                            min=-1, max=1, value=0, step=0.01)),
               
               ## Gumbel Copula
               # none and sur
               conditionalPanel(condition = "input.family == 'gumbelCopula' && (input.rot == 'none' || input.rot == 'sur')",
                                sliderInput("paramGumbelPos", "parameter:",
                                            min=1, max=10, value=1, step=0.01)),
               # r90 and r270
               conditionalPanel(condition = "input.family == 'gumbelCopula' && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("paramGumbelNeg", "parameter:",
                                            min=-10, max=-1, value=-1, step=0.01)),

               ## Joe Copula
               # none and sur
               conditionalPanel(condition = "input.family == 'joeBiCopula' && (input.rot == 'none' || input.rot == 'sur')",
                                sliderInput("paramJoePos", "parameter:",
                                            min=1.01, max=10, value=2, step=0.01)),
               # r90 and r270
               conditionalPanel(condition = "input.family == 'joeBiCopula' && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("paramJoeNeg", "parameter:",
                                            min=-10, max=-1.01, value=-2, step=0.01)),
               
               ## Student t Copulas
               conditionalPanel(condition = "input.family == 'tCopula'",
                                sliderInput("df", "degrees of freedom:",
                                            min=0, max=20, value=2, step=0.01)),
               
               ## Tawn Copula
               # none and survival
               conditionalPanel(condition = "(input.family == 'tawnT1Copula' || input.family == 'tawnT2Copula') && (input.rot == 'none' || input.rot == 'sur')",
                                sliderInput("param1TawnPos", "first parameter:",
                                            min=1, max=10, value=1, step=0.01),
                                sliderInput("param2TawnPos", "second parameter:",
                                            min=0, max=1, value=0.5, step=0.01)),
               # r90 and r270
               conditionalPanel(condition = "(input.family == 'tawnT1Copula' || input.family == 'tawnT2Copula') && (input.rot == 'r90' || input.rot == 'r270')",
                                sliderInput("param1TawnNeg", "first parameter:",
                                            min=-10, max=-1, value=-1, step=0.01),
                                sliderInput("param2TawnNeg", "second parameter:",
                                            min=0, max=1, value=0.5, step=0.01)),
               
               ## Bivariate Spatial Copula
               conditionalPanel(condition = "input.family == 'spCopula'",
                                sliderInput("spatialDistance", "spatial distance:",
                                            min=0, max=800, value=250, step=10,
                                            animate=animationOptions(interval=500, loop=T)),
                                plotOutput("corFunPlot", height=300)),
               
               # sample length
               conditionalPanel(condition = "input.family != 'spCopula'",
                                sliderInput("sampleSize", "sample size (set to minimum for a contour plot):",
                                            min=10, max=1000, value=250, step=1)
                                )),
  
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("copulaPlots"),
    textOutput("kendallsTau"),
    textOutput("tailIndex"))  
))
