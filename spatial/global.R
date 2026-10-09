# spatial: definitions shared by ui.R and server.R

library(shiny)
library(bslib)
library(gstat)
library(sf)
library(ggplot2)

# Sample variogram of daily mean PM10 concentrations at rural background
# stations in Germany 2005 (spatial lags in km, temporal lags in days) and the
# spatio-temporal variogram models fitted to it, see data-raw/prepare_data.R
variograms <- readRDS(file.path("data", "variograms.rds"))
fitted_models <- variograms$models
vv_boundaries <- attr(variograms$sample, "boundaries")
vv <- as.data.frame(variograms$sample)
vv <- vv[!is.na(vv$gamma), ]

model_labels <- c(
  separable = "separable",
  productSum = "product-sum",
  sumMetric = "sum-metric",
  simpleSumMetric = "simple sum-metric",
  metric = "metric"
)

# rural background PM10 stations and German federal states, prepared with
# data-raw/prepare_data.R
pm10_stations <- st_read(file.path("data", "pm10_stations.gpkg"),
                         layer = "stations", quiet = TRUE)
de_states <- st_read(file.path("data", "pm10_stations.gpkg"),
                     layer = "states", quiet = TRUE)

space_label <- "spatial lag (km)"
time_label <- "temporal lag (days)"
gamma_label <- "semivariance"

fit_rmse <- function(model) sqrt(attr(model, "MSE"))

fit_n_par <- function(model) length(attr(model, "optim.output")$par)

model_surface <- function(model, spacelag, timelag) {
  variogramSurface(model, data.frame(spacelag = spacelag,
                                     timelag = timelag))$gamma
}

# sample and model variogram at the sample lags; each spatial lag is drawn
# across its distance bin, the zero lag (pairs of the same station) to the
# left of the origin
lag_cells <- function(model) {
  bounds <- vv_boundaries
  bin <- findInterval(vv$spacelag, bounds, rightmost.closed = TRUE)
  zero <- vv$spacelag == 0
  width <- diff(bounds)[1]
  data.frame(
    spacelag = vv$spacelag,
    timelag = vv$timelag,
    xmin = ifelse(zero, -width / 2, bounds[pmax(bin, 1)]),
    xmax = ifelse(zero, 0, bounds[pmax(bin, 1) + 1]),
    sample = vv$gamma,
    model = model_surface(model, vv$spacelag, vv$timelag)
  )
}

plot_heat_map <- function(model) {
  cells <- lag_cells(model)
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
    scale_fill_viridis_c(gamma_label) +
    scale_x_continuous(space_label, expand = c(0, 0)) +
    scale_y_continuous(time_label, breaks = unique(cells$timelag),
                       expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank(), panel.spacing = unit(2, "lines"))
}

plot_difference <- function(model) {
  cells <- lag_cells(model)
  limit <- max(abs(cells$model - cells$sample))

  ggplot(cells, aes(xmin = xmin, xmax = xmax,
                    ymin = timelag - 0.5, ymax = timelag + 0.5,
                    fill = model - sample)) +
    geom_rect() +
    scale_fill_distiller("model - sample", palette = "RdBu",
                         limits = c(-limit, limit)) +
    scale_x_continuous(space_label, expand = c(0, 0)) +
    scale_y_continuous(time_label, breaks = unique(cells$timelag),
                       expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank())
}

# sample variogram as points, the model as lines, one colour per temporal lag
plot_profiles <- function(model) {
  space_grid <- seq(0, max(vv_boundaries), length.out = 200)
  time_lags <- sort(unique(vv$timelag))
  curves <- expand.grid(spacelag = space_grid, timelag = time_lags)
  curves$gamma <- model_surface(model, curves$spacelag, curves$timelag)

  ggplot(mapping = aes(colour = factor(timelag))) +
    geom_line(aes(spacelag, gamma), curves, linewidth = 0.8) +
    geom_point(aes(avgDist, gamma, size = np), vv) +
    scale_colour_viridis_d(time_label, end = 0.9) +
    scale_size_area("number of pairs", max_size = 4,
                    labels = scales::label_comma()) +
    labs(x = space_label, y = gamma_label) +
    expand_limits(y = 0) +
    theme_minimal(base_size = 14)
}

# model surface with the sample variogram as markers
plot_3d <- function(model) {
  space_grid <- seq(0, max(vv_boundaries), length.out = 60)
  time_grid <- seq(0, max(vv$timelag), length.out = 30)
  grid <- expand.grid(spacelag = space_grid, timelag = time_grid)
  z <- matrix(model_surface(model, grid$spacelag, grid$timelag),
              nrow = length(time_grid), byrow = TRUE)

  plotly::plot_ly() |>
    plotly::add_surface(x = space_grid, y = time_grid, z = z,
                        colorscale = "Viridis", opacity = 0.8,
                        name = "model",
                        colorbar = list(title = gamma_label)) |>
    plotly::add_markers(x = vv$avgDist, y = vv$timelag, z = vv$gamma,
                        marker = list(size = 4, color = "black"),
                        name = "sample") |>
    plotly::layout(scene = list(
      xaxis = list(title = space_label),
      yaxis = list(title = time_label),
      zaxis = list(title = gamma_label)
    ))
}

plot_stations <- function() {
  ggplot() +
    geom_sf(data = de_states, fill = "grey95", colour = "grey60") +
    geom_sf(aes(colour = mean_pm10), pm10_stations, size = 2.5) +
    scale_colour_viridis_c(expression("mean PM"[10] ~ "(µg/m³)")) +
    theme_void(base_size = 14)
}

# components (space, time, joint) and global parameters of a model as table
model_table <- function(model) {
  components <- intersect(c("space", "time", "joint"), names(model))
  rows <- lapply(components, function(comp) {
    vgm <- model[[comp]]
    data.frame(
      component = comp,
      model = as.character(vgm$model),
      `partial sill` = vgm$psill,
      range = vgm$range,
      check.names = FALSE
    )
  })
  do.call(rbind, rows)
}

model_globals <- function(model) {
  globals <- intersect(c("sill", "nugget", "k", "stAni"), names(model))
  labels <- c(sill = "sill", nugget = "nugget", k = "k",
              stAni = "spatio-temporal anisotropy (km/day)")
  if (length(globals) == 0) {
    return(NULL)
  }
  paste(paste0(labels[globals], ": ",
               vapply(globals, function(g) format(round(model[[g]], 2)),
                      character(1))),
        collapse = ", ")
}
