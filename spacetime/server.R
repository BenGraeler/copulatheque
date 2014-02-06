## spatial
# setwd("spatial/")

library(shiny)
library(gstat)


load("fittedSTvariogramModels.RData")
data(vv)

## gstat fix
library(lattice)
library(sp)
plotStVariogram = function(x, model=NULL, ..., col = bpy.colors(), xlab, ylab,
                            map = TRUE, convertMonths = FALSE, as.table=T,
                            wireframe = FALSE, both = FALSE, all=FALSE) {
  lst = list(...)
  if (!is.null(lst$col.regions))
    col = lst$col.regions
  if (is(x$timelag, "yearmon")) {
    if (convertMonths) {
      x$timelag = as.numeric(x$timelag) * 12
      attr(x$timelag, "units") = "months"
    } else
      attr(x$timelag, "units") = "years"
  }
  if (missing(xlab)) {
    xlab = "distance"
    u =  attr(x$spacelag, "units")
    if (!is.null(u))
      xlab = paste(xlab, " (", u, ")", sep="")
  }
  if (missing(ylab)) {
    ylab = "time lag"
    u = attr(x$timelag, "units")
    if (!is.null(u))
      ylab = paste(ylab, " (", u, ")", sep="")
  }
  if(!is.null(model)) {
    if (is(model,"StVariogramModel"))
      model <- list(model)
    for (mod in model) {
      x[[mod$stModel]] <- variogramSurface(mod, x[,c("spacelag","timelag")])$model
    }
  }
  x0 = x # needed by wireframe()
  if (!is.null(model)) {
    modelNames  <- sapply(model, function(x) x$stModel)
    v0 <- x[,c("dist","id","spacelag","timelag")]
    for (i in modelNames) {
      v0 <- rbind(v0, x[,c("dist","id","spacelag","timelag")])
    }
    v0$what = factor(c(rep("sample", nrow(x)), rep(modelNames, each=nrow(x))),
                     levels=c("sample", modelNames),ordered=T)
    v0$gamma = c(x$gamma, unlist(x[,modelNames]))
    x = v0
  }
  if (wireframe) { 
    if (!is.null(model)) {
      if (both) { # plot sample and first model in one wireframe plot
        if (length(model) > 1)
          warning("Only the first of the provided variogram models will be used.")
        wireframe(as.formula(paste(model[[1]]$stModel,"+gamma ~ spacelag*timelag")),
                  x0, drape = TRUE, col.regions = col, 
                  xlab = xlab, ylab = ylab, ...)
      } else {
        if (all) { # plot sample and all models in separate wireframes
          wireframe(gamma ~ spacelag*timelag | what, 
                    x, drape = TRUE, col.regions = col, 
                    xlab = xlab, ylab = ylab, as.table=as.table, ...)
        } else { # plot all theoretical models in separate wireframes, the default
          if (length(model) > 1)
            wireframe(gamma ~ spacelag*timelag | what, 
                      x[-(1:nrow(x0)),], drape = TRUE, col.regions = col, 
                      xlab = xlab, ylab = ylab, as.table=as.table, ...)
          else 
            wireframe(as.formula(paste(model[[1]]$stModel,"~ spacelag*timelag")), 
                      x0, drape = TRUE, col.regions = col, 
                      xlab = xlab, ylab = ylab, as.table=as.table, ...)
        }
      }
    } else # without a model, plot only the sample variogram as a wireframe
      wireframe(gamma ~ spacelag * timelag, x0, drape = TRUE, col = col,
                xlab = xlab, ylab = ylab, ...)
  } else if (map) {
    if (!is.null(model))
      f = gamma ~ spacelag + timelag | what
    else
      f = gamma ~ spacelag + timelag
    levelplot(f, x, xlab = xlab, ylab = ylab, col.regions = col, as.table=as.table, ...)
  } else { # not map, not wireplot
    if (!is.null(model))
      f = gamma ~ dist | what
    else
      f = gamma ~ dist
    x$id = factor(x$id, levels=unique(x$id))
    bp = bpy.colors(length(levels(x$id)))
    ps = list(superpose.line=list(col=bp), superpose.symbol=list(col=bp))
    ylim = c(0, max(x$gamma) * 1.04)
    xlim = c(0, max(x$dist) * 1.04)
    xyplot(f, x, groups = x$id, type='b', ylim = ylim, xlim = xlim,
           auto.key = list(space = "right"), xlab = xlab, 
           par.settings = ps, ...)
  }
}

# print StVariogramModel
printStVariogramModel <- function(x) {
  possComp <- c("space", "time", "joint")
  for(comp in possComp[possComp %in% names(x)]) {
    rownames(x[[comp]]) <- 1:nrow(x[[comp]])
    cat(paste(comp,"component: \n"))
    print(x[[comp]])
  }
  
  possAddPar <- c("sill", "nugget", "stAni")
  for(addPar in possAddPar[possAddPar %in% names(x)]) {
    cat(paste(addPar, ": ",x[[addPar]],"\n", sep=""))
  }
}


# Define server logic
shinyServer(function(input, output) {
  output$caption <- renderText(switch(input$family,
                                      separable = "separable covariance model",
                                      productSum = "product-sum covariance model",
                                      sumMetric = "sum-metric covariance model",
                                      simpleSumMetric = "simple sum-metric covariance model",
                                      metric = "metric covariance model"))
  
  acModel <-reactive(switch(input$family,
                            separable = vgmST("separable", 
                                              space=vgm(input$sep.space.sill,
                                                        input$sep.space.fam,
                                                        input$sep.space.range,
                                                        1-input$sep.space.sill),
                                              time=vgm(input$sep.time.sill,
                                                       input$sep.time.fam,
                                                       input$sep.time.range,
                                                       1-input$sep.time.sill),
                                              sill=input$sep.sill),
                            productSum = vgmST("productSum", 
                                               space=vgm(input$ps.space.sill,
                                                         input$ps.space.fam,
                                                         input$ps.space.range),
                                               time=vgm(input$ps.time.sill,
                                                        input$ps.time.fam,
                                                        input$ps.time.range),
                                               sill=input$ps.sill,
                                               nugget=input$ps.nugget),
                            sumMetric = vgmST("sumMetric", 
                                              space=vgm(input$sumMetric.space.sill,
                                                        input$sumMetric.space.fam,
                                                        input$sumMetric.space.range,
                                                        input$sumMetric.space.nugget),
                                              time=vgm(input$sumMetric.time.sill,
                                                        input$sumMetric.time.fam,
                                                        input$sumMetric.time.range,
                                                        input$sumMetric.time.nugget),
                                              joint=vgm(input$sumMetric.joint.sill,
                                                        input$sumMetric.joint.fam,
                                                        input$sumMetric.joint.range,
                                                        input$sumMetric.joint.nugget),
                                              stAni=input$sumMetric.stAni),
                            simpleSumMetric = vgmST("simpleSumMetric", 
                                                    space=vgm(input$simpleSumMetric.space.sill,
                                                              input$simpleSumMetric.space.fam,
                                                              input$simpleSumMetric.space.range),
                                                    time=vgm(input$simpleSumMetric.time.sill,
                                                             input$simpleSumMetric.time.fam,
                                                             input$simpleSumMetric.time.range),
                                                    joint=vgm(input$simpleSumMetric.joint.sill,
                                                              input$simpleSumMetric.joint.fam,
                                                              input$simpleSumMetric.joint.range),
                                                    nugget=input$simpleSumMetric.nugget,
                                                    stAni=input$simpleSumMetric.stAni),
                            metric = vgmST("metric", 
                                           joint=vgm(input$metric.sill,
                                                     input$metric.fam,
                                                     input$metric.range,
                                                     input$metric.nugget),
                                           stAni=input$metric.stAni)))
  
  output$vgmPlot <- renderPlot({
    switch(input$type,
           levelplot = print(plot(vv, acModel())),
           wireframe = print(plotStVariogram(vv, acModel(), wireframe=TRUE, all=TRUE, scales=list(arrows=FALSE))))
    }, width=800, height=400)
  
  validPS <- function(model) {
    if(model$stModel != "productSum")
      return(NULL)
    k <- (sum(model$space$psill) + sum(model$time$psill) - model$sill)/(sum(model$space$psill) * sum(model$time$psill))
    if (k <= 0 | k > 1/max(model$space$psill[model$space$model != "Nug"],
                           model$time$psill[model$time$model != "Nug"])) 
      return("Invalid model: try different sill values.")
    return(NULL)
  }
  
  output$valid <- renderText(validPS(acModel()))
#   output$rmse <- renderText(paste("The root-mean-squared-error of the following model is:",
#                                   round(attr(acModel(),"optim.output")$value,2)))
  output$model <- renderPrint(printStVariogramModel(acModel()))
})
