## spatial
# setwd("spatial/")

library(shiny)
options(rgl.useNULL=TRUE)
library(shinyRGL)
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

## simplified lollipop3d function
library(rgl)
surf.n=50

data.x <- vv$spacelag
data.y <- vv$timelag
data.z <- vv$gamma

xlim <- range(data.x)
ylim <- range(data.y)
zlim <- range(data.z)

asp <- c(y=1,z=1)

x.ticks <- pretty(xlim)
x.ticks <- x.ticks[x.ticks>=min(xlim) & x.ticks<=max(xlim)]
x.ticklabs <- as.character(x.ticks)
y.ticks <- pretty(ylim)
y.ticks <- y.ticks[y.ticks>=min(ylim) & y.ticks<=max(ylim)]
y.ticklabs <- as.character(y.ticks)
z.ticks <- pretty(zlim)
z.ticks <- z.ticks[z.ticks>=min(zlim) & z.ticks<=max(zlim)]
z.ticklabs <- as.character(z.ticks)

surf.x <- seq(xlim[1],xlim[2],length=surf.n)
surf.y <- seq(ylim[1],ylim[2],length=surf.n)

lollipop3d <- function(surf.fun,
                       col.surf=fg,col.stem=c(fg,fg), col.pt="gray",
                       type.surf="line", ptsize,
                       lwd.stem=2,bg="white",fg="black",col.axes=fg,col.axlabs=fg,
                       axis.arrow=TRUE,axis.labels=TRUE,box.col=bg, axes=c("lines","box")) {
  axes <- match.arg(axes)
  col.stem <- rep(col.stem,length=2)

  surf.z <- outer(surf.x,surf.y,surf.fun)  ## requires surf.fun be vectorized
  z.interc <- surf.fun(data.x,data.y)
  zdiff <- diff(range(c(surf.z,data.z)))
  
  xdiff <- diff(xlim)
  ydiff <- diff(ylim)
  y.adj <- if (asp[1]<=0) 1 else asp[1]*xdiff/ydiff
  data.y <- y.adj*data.y
  y.ticks <- y.adj*y.ticks
  ylim <- ylim*y.adj
  ydiff <- diff(ylim)
  z.adj <- if (asp[2]<=0) 1 else asp[2]*xdiff/zdiff
  data.z <- z.adj*data.z
  surf.y <- y.adj*surf.y
  surf.z <- z.adj*surf.z
  z.interc <- z.adj*z.interc
  z.ticks <- z.adj*z.ticks
  zlim <- z.adj*zlim
  
  clear3d("all")
  light3d()
  bg3d(color=c(bg,fg))

  surface3d(surf.x,surf.y,surf.z,alpha=0.4,
            front=type.surf,back=type.surf, col=col.surf,lit=TRUE)
  
  if (missing(ptsize)) 
    ptsize <- 0.02*xdiff
 
  ## draw points
  spheres3d(data.x,data.y,data.z,r=ptsize,lit=TRUE,color=col.pt)
  bbox <- par3d("bbox")
  if (axes=="box") {
    bbox3d(xat=x.ticks, xlab=x.ticklabs, yat=y.ticks, ylab=y.ticklabs,
           zat=z.ticks, zlab=z.ticklabs, lit=TRUE)
  } else if (axes=="lines") { ## set up axis lines
    bbox <- par3d("bbox")
    axis3d(edge="x",at=x.ticks,labels=x.ticklabs,col=col.axes,arrow=axis.arrow)
    axis3d(edge="y",at=y.ticks,labels=y.ticklabs,col=col.axes,arrow=axis.arrow)
    axis3d(edge="z",at=z.ticks,labels=z.ticklabs,col=col.axes,arrow=axis.arrow)
    box3d(col=col.axes)
  }
  decorate3d(xlab="space [km]", ylab="time [days]", zlab="", box=FALSE, axes=FALSE,
             col=col.axlabs,main="")
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
                            separable = fitSepModel,
                            productSum = fitProdSumModel,
                            sumMetric = fitSumMetricModel,
                            simpleSumMetric = fitSimpleSumMetricModel,
                            metric = fitMetricModel))
  
  output$vgmPlot <- renderPlot({
    switch(input$type,
           levelplot = print(plot(vv, acModel())),
           wireframe = print(plotStVariogram(vv, acModel(), wireframe=TRUE, all=TRUE)))
    }, width=800, height=400)
  
  output$rgl <- renderWebGL({
    if(input$type=="openGL")
      openGL = lollipop3d(function(x,y) variogramSurface(acModel(),data.frame(spacelag=x,timelag=y))[,3],
                          ptsize=sqrt(vv$np/2000))
    else
      NULL
    }, width=600, height=600)
  
  output$rmse <- renderText(paste("The root-mean-squared-error of the following model is:",
                                  round(attr(acModel(),"optim.output")$value,2)))
  output$model <- renderPrint(printStVariogramModel(acModel()))
})
