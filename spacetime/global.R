# spacetime: definitions shared by ui.R and server.R

library(shiny)
library(bslib)
library(gstat)
library(ggplot2)

# Sample variogram of daily mean PM10 concentrations at rural background
# stations in Germany 2005 (spatial lags in km, temporal lags in days) and the
# spatio-temporal variogram models fitted to it. data/variograms.rds is a copy
# of spatial/data/variograms.rds, created by spatial/data-raw/prepare_data.R.
variograms <- readRDS(file.path("data", "variograms.rds"))
pm10_sample <- variograms$sample
fitted_models <- variograms$models

model_labels <- c(
  separable = "separable",
  productSum = "product-sum",
  sumMetric = "sum-metric",
  simpleSumMetric = "simple sum-metric",
  metric = "metric"
)

# variogram families offered for the components
vgm_families <- local({
  families <- vgm()
  families <- families[!families$short %in% c("Nug", "Pow", "Err", "Int"), ]
  setNames(as.character(families$short), as.character(families$long))
})

# families with a shape parameter kappa
kappa_families <- c("Exc", "Mat", "Ste")

# Parameters of the spatio-temporal models in gstat's notation (see ?vgmST and
# vignette("spatio-temporal-kriging", package = "gstat")): the variograms
# space, time and joint each have a family, a partial sill, a range, a shape
# parameter kappa (used by kappa_families only) and, depending on the model,
# a nugget. In the separable model, space and time are standardised
# variograms with a sill of 1: the partial sill is a share of 1, the remainder
# the nugget. The scalar arguments of vgmST() are the joint sill (separable),
# the weight k of the product (product-sum), the joint nugget (simple
# sum-metric) and the spatio-temporal anisotropy stAni (metric models).
model_specs <- list(
  separable = list(components = c("space", "time"), nugget = FALSE,
                   standardised = TRUE, globals = "sill"),
  productSum = list(components = c("space", "time"), nugget = TRUE,
                    globals = "k"),
  sumMetric = list(components = c("space", "time", "joint"), nugget = TRUE,
                   globals = "stAni"),
  simpleSumMetric = list(components = c("space", "time", "joint"),
                         nugget = FALSE, globals = c("nugget", "stAni")),
  metric = list(components = "joint", nugget = TRUE, globals = "stAni")
)

component_params <- function(spec) {
  c("model", "psill", "range", "kappa", if (isTRUE(spec$nugget)) "nugget")
}

# input id(s) of a parameter, vectorised; a missing component (NA) is dropped
param_id <- function(family, ...) {
  parts <- list(family, ...)
  missing_part <- vapply(parts, function(p) length(p) == 1 && is.na(p),
                         logical(1))
  do.call(paste, c(parts[!missing_part], sep = "_"))
}

# all input ids of a family with the parameter they hold
family_params <- function(family) {
  spec <- model_specs[[family]]
  comps <- lapply(spec$components, function(comp) {
    params <- component_params(spec)
    data.frame(id = param_id(family, comp, params), component = comp,
               param = params)
  })
  globals <- data.frame(id = param_id(family, spec$globals), component = NA,
                        param = spec$globals)
  rbind(globals, do.call(rbind, comps))
}

# parameter values of a fitted model, named by input id
model_params <- function(model, family) {
  params <- family_params(family)
  values <- lapply(seq_len(nrow(params)), function(i) {
    comp <- params$component[i]
    param <- params$param[i]
    if (is.na(comp)) {
      return(unname(model[[param]]))
    }
    vgm_comp <- model[[comp]]
    nug <- vgm_comp$model == "Nug"
    switch(param,
           model = as.character(vgm_comp$model[!nug][1]),
           psill = vgm_comp$psill[!nug][1],
           range = vgm_comp$range[!nug][1],
           kappa = vgm_comp$kappa[!nug][1],
           nugget = sum(vgm_comp$psill[nug]))
  })
  setNames(values, params$id)
}

# spatio-temporal model from the parameter values, named by input id
build_model <- function(family, values, temporal_unit) {
  spec <- model_specs[[family]]
  value <- function(...) values[[param_id(family, ...)]]
  comps <- lapply(spec$components, function(comp) {
    psill <- value(comp, "psill")
    args <- list(psill, value(comp, "model"), value(comp, "range"),
                 kappa = value(comp, "kappa"))
    if (isTRUE(spec$standardised)) {
      args$nugget <- 1 - psill
    } else if (isTRUE(spec$nugget)) {
      args$nugget <- value(comp, "nugget")
    }
    do.call(vgm, args)
  })
  names(comps) <- spec$components
  globals <- lapply(spec$globals, value)
  names(globals) <- spec$globals
  do.call(vgmST, c(list(family), comps, globals,
                   list(temporalUnit = temporal_unit)))
}

# Overall spatio-temporal sill of a model, the plateau of the variogram for
# large spatial and temporal lags (for families with a sill)
model_sill <- function(model) {
  comp_sill <- function(comp) sum(model[[comp]]$psill)
  sill <- switch(
    strsplit(model$stModel, "_")[[1]][1],
    separable = model$sill,
    productSum = model$k * comp_sill("space") * comp_sill("time") +
      comp_sill("space") + comp_sill("time"),
    sumMetric = comp_sill("space") + comp_sill("time") + comp_sill("joint"),
    simpleSumMetric = comp_sill("space") + comp_sill("time") +
      comp_sill("joint") + model$nugget,
    metric = comp_sill("joint")
  )
  unname(sill)
}

# sample variograms ------------------------------------------------------------

lag_units <- function(sample) {
  c(space = attr(sample$spacelag, "units") %||% "spatial units",
    time = attr(sample$timelag, "units") %||% "temporal units")
}

# Reads a sample variogram (class StVariogram, see gstat::variogramST) from
# an .rds file or from an .RData file holding a single such object.
read_sample <- function(path, name) {
  if (grepl("\\.rds$", name, ignore.case = TRUE)) {
    sample <- readRDS(path)
  } else {
    env <- new.env()
    objects <- load(path, envir = env)
    is_sample <- vapply(objects,
                        function(o) inherits(env[[o]], "StVariogram"),
                        logical(1))
    if (sum(is_sample) != 1) {
      stop("the file needs to contain exactly one StVariogram object")
    }
    sample <- env[[objects[is_sample]]]
  }
  if (!inherits(sample, "StVariogram")) {
    stop("the object is not a sample variogram of class StVariogram")
  }
  missing_cols <- setdiff(c("np", "dist", "gamma", "timelag", "spacelag"),
                          names(sample))
  if (length(missing_cols) > 0) {
    stop("columns missing: ", paste(missing_cols, collapse = ", "))
  }
  units <- attr(sample$timelag, "units")
  sample$timelag <- as.numeric(sample$timelag)
  attr(sample$timelag, "units") <- units
  sample
}

# Ranges of the parameter sliders, derived from the sample variogram: a
# parameter's slider covers at least the given multiple of the largest lag or
# semivariance.
sample_limits <- function(sample) {
  max_space <- max(sample$spacelag, na.rm = TRUE)
  max_time <- max(sample$timelag, na.rm = TRUE)
  max_gamma <- max(sample$gamma, na.rm = TRUE)
  nice <- function(x) max(pretty(c(0, x)))
  list(
    space_range = nice(3 * max_space),
    time_range = nice(3 * max_time),
    joint_range = nice(6 * max_space),
    psill = nice(1.5 * max_gamma),
    nugget = nice(0.5 * max_gamma),
    sill = nice(1.5 * max_gamma),
    stAni = nice(4 * max_space / max_time),
    k = 5
  )
}

slider_max <- function(limits, comp, param) {
  if (param == "range") limits[[paste0(comp, "_range")]] else limits[[param]]
}

param_label <- function(family, comp, param, units) {
  spec <- model_specs[[family]]
  comp_label <- c(space = "spatial", time = "temporal", joint = "joint")
  # global parameters are labelled with their vgmST() argument
  if (is.na(comp)) {
    return(switch(
      param,
      sill = "sill: joint spatio-temporal sill",
      nugget = "nugget: joint spatio-temporal nugget",
      k = "k: weight of the product (k > 0)",
      stAni = sprintf("stAni: spatio-temporal anisotropy (%s per %s)",
                      units[["space"]], units[["time"]])
    ))
  }
  switch(
    param,
    model = paste(comp_label[[comp]], "variogram family"),
    psill = if (isTRUE(spec$standardised)) {
      "partial sill (standardised, nugget = 1 - partial sill)"
    } else {
      "partial sill"
    },
    range = sprintf("range (%s)",
                    units[[if (comp == "time") "time" else "space"]]),
    kappa = "shape parameter kappa",
    nugget = "nugget"
  )
}

# settings of the slider of a numeric parameter; the slider always includes
# the current value
slider_settings <- function(family, comp, param, value, limits) {
  if (param == "kappa") {
    return(list(min = 0.05, max = 5, step = 0.05,
                value = min(max(value, 0.05), 5)))
  }
  standardised <- isTRUE(model_specs[[family]]$standardised) &&
    param == "psill"
  upper <- if (standardised) 1 else slider_max(limits, comp, param)
  if (!standardised && value > upper) {
    upper <- max(pretty(c(0, 1.5 * value)))
  }
  # ranges, the joint sill, k and stAni need to be positive
  positive <- param %in% c("range", "sill", "k", "stAni")
  lower <- if (positive) upper / 1000 else 0
  list(min = lower, max = upper, step = signif(upper / 1000, 1),
       value = min(max(value, lower), upper))
}

# Rescales parameter values from one sample variogram to another, keeping
# the shape of the model relative to the extent of the lags and the
# semivariance: ranges follow the largest spatial or temporal lag, sills and
# nuggets the largest semivariance.
rescale_params <- function(family, values, from, to) {
  extent <- function(sample) {
    c(space = max(sample$spacelag, na.rm = TRUE),
      time = max(sample$timelag, na.rm = TRUE),
      gamma = max(sample$gamma, na.rm = TRUE))
  }
  ratio <- extent(to) / extent(from)
  standardised <- isTRUE(model_specs[[family]]$standardised)
  params <- family_params(family)
  for (i in seq_len(nrow(params))) {
    id <- params$id[i]
    comp <- params$component[i]
    factor <- switch(
      params$param[i],
      range = ratio[[if (comp == "time") "time" else "space"]],
      psill = if (standardised) 1 else ratio[["gamma"]],
      nugget = ,
      sill = ratio[["gamma"]],
      k = 1 / ratio[["gamma"]],
      stAni = ratio[["space"]] / ratio[["time"]],
      1
    )
    if (is.numeric(values[[id]])) {
      values[[id]] <- values[[id]] * factor
    }
  }
  values
}

model_rmse <- function(sample, model) {
  fit <- fit.StVariogram(sample, model, fit.method = 0)
  sqrt(attr(fit, "MSE"))
}

# Fits the model to the sample with the current parameters as starting values
# and gstat's default bounds, weighting by the number of pairs and the
# spatio-temporal distance (fit.method 7).
fit_model <- function(sample, model) {
  spacelags <- sample$spacelag[sample$spacelag > 0]
  st_ani <- model$stAni %||% tryCatch(
    estiStAni(sample, range(spacelags) * c(1, 0.5)),
    error = function(e) NA
  )
  fit.StVariogram(sample, model, fit.method = 7, stAni = st_ani,
                  method = "L-BFGS-B")
}

# plots ------------------------------------------------------------------------

model_surface <- function(model, spacelag, timelag) {
  variogramSurface(model, data.frame(spacelag = spacelag,
                                     timelag = timelag))$gamma
}

sample_points <- function(sample) {
  points <- as.data.frame(sample)[, c("np", "dist", "gamma", "spacelag",
                                      "timelag")]
  points$timelag <- as.numeric(points$timelag)
  points[!is.na(points$gamma), ]
}

# Boundaries of the distance bins; derived from the spatial lags if the
# sample does not provide them
lag_boundaries <- function(sample) {
  bounds <- attr(sample, "boundaries")
  if (is.null(bounds)) {
    lags <- sort(unique(sample$spacelag[sample$spacelag > 0]))
    mids <- (head(lags, -1) + tail(lags, -1)) / 2
    first_width <- if (length(mids) > 0) mids[1] - lags[1] else lags[1]
    bounds <- c(max(lags[1] - first_width, 0), mids,
                tail(lags, 1) + first_width)
  }
  bounds
}

# sample and model variogram at the sample lags; each spatial lag is drawn
# across its distance bin, the zero lag (pairs of the same location) to the
# left of the origin
lag_cells <- function(sample, model) {
  points <- sample_points(sample)
  bounds <- lag_boundaries(sample)
  bin <- pmax(findInterval(points$spacelag, bounds, rightmost.closed = TRUE),
              1)
  zero <- points$spacelag == 0
  width <- diff(bounds)[1]
  data.frame(
    spacelag = points$spacelag,
    timelag = points$timelag,
    xmin = ifelse(zero, -width / 2, bounds[bin]),
    xmax = ifelse(zero, 0, bounds[bin + 1]),
    sample = points$gamma,
    model = model_surface(model, points$spacelag, points$timelag)
  )
}

axis_labels <- function(sample) {
  units <- lag_units(sample)
  list(space = sprintf("spatial lag (%s)", units[["space"]]),
       time = sprintf("temporal lag (%s)", units[["time"]]),
       gamma = "semivariance")
}

plot_heat_map <- function(sample, model) {
  cells <- lag_cells(sample, model)
  labels <- axis_labels(sample)
  long <- rbind(
    transform(cells, what = "sample", gamma = sample),
    transform(cells, what = "model", gamma = model)
  )
  long$what <- factor(long$what, levels = c("sample", "model"))

  ggplot(long, aes(xmin = xmin, xmax = xmax,
                   ymin = timelag - 0.5, ymax = timelag + 0.5,
                   fill = gamma)) +
    geom_rect() +
    facet_wrap(~what) +
    scale_fill_viridis_c(labels$gamma) +
    scale_x_continuous(labels$space, expand = c(0, 0)) +
    scale_y_continuous(labels$time, breaks = unique(cells$timelag),
                       expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank(), panel.spacing = unit(2, "lines"))
}

plot_difference <- function(sample, model) {
  cells <- lag_cells(sample, model)
  labels <- axis_labels(sample)
  limit <- max(abs(cells$model - cells$sample))

  ggplot(cells, aes(xmin = xmin, xmax = xmax,
                    ymin = timelag - 0.5, ymax = timelag + 0.5,
                    fill = model - sample)) +
    geom_rect() +
    scale_fill_distiller("model - sample", palette = "RdBu",
                         limits = c(-limit, limit)) +
    scale_x_continuous(labels$space, expand = c(0, 0)) +
    scale_y_continuous(labels$time, breaks = unique(cells$timelag),
                       expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank())
}

# sample variogram as points, the model as lines, one colour per temporal lag
plot_profiles <- function(sample, model) {
  points <- sample_points(sample)
  labels <- axis_labels(sample)
  space_grid <- seq(0, max(lag_boundaries(sample)), length.out = 200)
  curves <- expand.grid(spacelag = space_grid,
                        timelag = sort(unique(points$timelag)))
  curves$gamma <- model_surface(model, curves$spacelag, curves$timelag)

  ggplot(mapping = aes(colour = factor(timelag))) +
    geom_line(aes(spacelag, gamma), curves, linewidth = 0.8) +
    geom_point(aes(dist, gamma, size = np), points) +
    scale_colour_viridis_d(labels$time, end = 0.9) +
    scale_size_area("number of pairs", max_size = 4,
                    labels = scales::label_comma()) +
    labs(x = labels$space, y = labels$gamma) +
    expand_limits(y = 0) +
    theme_minimal(base_size = 14)
}

# model surface with the sample variogram as markers
plot_3d <- function(sample, model) {
  points <- sample_points(sample)
  labels <- axis_labels(sample)
  space_grid <- seq(0, max(lag_boundaries(sample)), length.out = 60)
  time_grid <- seq(0, max(points$timelag), length.out = 30)
  grid <- expand.grid(spacelag = space_grid, timelag = time_grid)
  z <- matrix(model_surface(model, grid$spacelag, grid$timelag),
              nrow = length(time_grid), byrow = TRUE)

  plotly::plot_ly() |>
    plotly::add_surface(x = space_grid, y = time_grid, z = z,
                        colorscale = "Viridis", opacity = 0.8,
                        name = "model",
                        colorbar = list(title = labels$gamma)) |>
    plotly::add_markers(x = points$dist, y = points$timelag, z = points$gamma,
                        marker = list(size = 4, color = "black"),
                        name = "sample") |>
    plotly::layout(scene = list(
      xaxis = list(title = labels$space),
      yaxis = list(title = labels$time),
      zaxis = list(title = labels$gamma)
    ))
}

# R code creating the model, shown to the user
model_code <- function(family, values, temporal_unit) {
  spec <- model_specs[[family]]
  value <- function(...) values[[param_id(family, ...)]]
  num <- function(x) format(signif(x, 4))
  comps <- vapply(spec$components, function(comp) {
    vgm_model <- value(comp, "model")
    psill <- value(comp, "psill")
    args <- c(num(psill), sprintf('"%s"', vgm_model),
              num(value(comp, "range")))
    if (isTRUE(spec$standardised)) {
      args <- c(args, num(1 - psill))
    } else if (isTRUE(spec$nugget)) {
      args <- c(args, num(value(comp, "nugget")))
    }
    if (vgm_model %in% kappa_families) {
      args <- c(args, paste("kappa =", num(value(comp, "kappa"))))
    }
    sprintf("%s = vgm(%s)", comp, paste(args, collapse = ", "))
  }, character(1))
  globals <- vapply(spec$globals, function(g) paste(g, "=", num(value(g))),
                    character(1))
  globals <- c(globals, sprintf('temporalUnit = "%s"', temporal_unit))
  sprintf('vgmST("%s",\n      %s)', family,
          paste(c(comps, globals), collapse = ",\n      "))
}
