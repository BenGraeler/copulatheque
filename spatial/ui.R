# spatial

library(shiny)
library(shinyRGL)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Different Spatio-Temporal Variogram Families"),
  
  sidebarPanel("This small shiny app illustrates a couple of different spatio-temporal variogram families as defined in", 
               a("gstat", href="http://cran.r-project.org/web/packages/gstat/index.html", target="_blank"),
               HTML("The sample variogram is calculated from daily mean PM<sub>10</sub>"),
               "concentrations across Germany during the year 2005 (see 'demo(stkrige)' in gstat).",br(),br(),
               selectInput("family", "Variogram family:", 
                           list("separable cov. model" =  "separable",
                                "product-sum cov. model" = "productSum",
                                "sum-metric cov. model" = "sumMetric",
                                "simple sum-metric cov. model" = "simpleSumMetric",
                                "metric cov. model" = "metric")),
               selectInput("type", "Select plot type:",
                           list("levelplot" = "levelplot",
                                "wireframe" = "wireframe",
                                "3D wireframe" = "openGL"))
               ),
  
  mainPanel(h3(textOutput("caption")),
            conditionalPanel(condition= "input.type != 'openGL'",
                             plotOutput("vgmPlot")),
            conditionalPanel(condition= "input.type == 'openGL'",
                             webGLOutput("rgl",height="600px")),
            textOutput("rmse"),
            verbatimTextOutput("model"))  
))