# copulas small: definitions shared by ui.R and server.R

library(shiny)
library(bslib)
library(copula)
library(shinyWidgets)
library(VC2copula)
library(ggplot2)

# Constructors of the rotated versions (VC2copula). The 90 and 270 degree
# rotations take negated parameters, following the VineCopula convention.
rotations <- function(r90, sur, r270) list(r90 = r90, sur = sur, r270 = r270)

rotation_labels <- c(
  "none" = "none",
  "90 degrees" = "r90",
  "survival (180 degrees)" = "sur",
  "270 degrees" = "r270"
)

# Bivariate copula families. Each entry defines the constructor, the range of
# the (first) dependence parameter and, where applicable, a second parameter
# and the rotated versions. The slider bounds stay strictly inside the
# admissible parameter space where the boundary itself is not supported and
# respect the bounds enforced by VineCopula.
copula_families <- list(
  clayton = list(
    label = "Clayton", fun = claytonCopula,
    min = -0.99, max = 10, value = 1,
    rotated = rotations(r90ClaytonCopula, surClaytonCopula, r270ClaytonCopula),
    # the rotated versions (VineCopula) only support positive dependence
    rotated_range = c(min = 0.01, max = 10)
  ),
  frank = list(
    label = "Frank", fun = frankCopula,
    min = -10, max = 10, value = 3
  ),
  normal = list(
    label = "Gaussian", fun = normalCopula,
    min = -0.99, max = 0.99, value = 0.5
  ),
  gumbel = list(
    label = "Gumbel", fun = gumbelCopula,
    min = 1, max = 10, value = 2,
    rotated = rotations(r90GumbelCopula, surGumbelCopula, r270GumbelCopula)
  ),
  joe = list(
    label = "Joe", fun = joeCopula,
    min = 1.01, max = 10, value = 2,
    rotated = rotations(r90JoeBiCopula, surJoeBiCopula, r270JoeBiCopula)
  ),
  amh = list(
    label = "Ali-Mikhail-Haq", fun = amhCopula,
    min = -1, max = 0.99, value = 0.5
  ),
  fgm = list(
    label = "Farlie-Gumbel-Morgenstern", fun = fgmCopula,
    min = -1, max = 1, value = 0.5
  ),
  plackett = list(
    label = "Plackett", fun = plackettCopula,
    min = 0.01, max = 20, value = 3
  ),
  galambos = list(
    label = "Galambos", fun = galambosCopula,
    min = 0.01, max = 10, value = 1
  ),
  huslerReiss = list(
    label = "Hüsler-Reiss", fun = huslerReissCopula,
    min = 0.01, max = 10, value = 1
  ),
  t = list(
    label = "Student t", fun = tCopula,
    min = -0.99, max = 0.99, value = 0.5
  ),
  # additional families from VineCopula, wrapped by VC2copula
  BB1 = list(
    label = "BB1 (Clayton-Gumbel)", fun = BB1Copula,
    min = 0.01, max = 7, value = 0.5,
    param2 = list(min = 1, max = 7, value = 1.5, negate = TRUE),
    rotated = rotations(r90BB1Copula, surBB1Copula, r270BB1Copula)
  ),
  BB6 = list(
    label = "BB6 (Joe-Gumbel)", fun = BB6Copula,
    min = 1, max = 6, value = 2,
    param2 = list(min = 1, max = 8, value = 2, negate = TRUE),
    rotated = rotations(r90BB6Copula, surBB6Copula, r270BB6Copula)
  ),
  BB7 = list(
    label = "BB7 (Joe-Clayton)", fun = BB7Copula,
    min = 1, max = 6, value = 2,
    param2 = list(min = 0.01, max = 20, value = 2, negate = TRUE),
    rotated = rotations(r90BB7Copula, surBB7Copula, r270BB7Copula)
  ),
  BB8 = list(
    label = "BB8 (Joe-Frank)", fun = BB8Copula,
    min = 1, max = 8, value = 3,
    param2 = list(min = 0.01, max = 1, value = 0.5, negate = TRUE),
    rotated = rotations(r90BB8Copula, surBB8Copula, r270BB8Copula)
  ),
  tawnT1 = list(
    label = "Tawn type 1", fun = tawnT1Copula,
    min = 1, max = 10, value = 2,
    param2 = list(min = 0.01, max = 1, value = 0.5, negate = FALSE),
    rotated = rotations(r90TawnT1Copula, surTawnT1Copula, r270TawnT1Copula)
  ),
  tawnT2 = list(
    label = "Tawn type 2", fun = tawnT2Copula,
    min = 1, max = 10, value = 2,
    param2 = list(min = 0.01, max = 1, value = 0.5, negate = FALSE),
    rotated = rotations(r90TawnT2Copula, surTawnT2Copula, r270TawnT2Copula)
  )
)

param_input_id <- function(family) paste0("param_", family)

# range of the (first) parameter, which may differ for the rotated versions
param_range <- function(spec, rotation = "none") {
  if (rotation != "none" && !is.null(spec$rotated_range)) {
    spec$rotated_range
  } else {
    c(min = spec$min, max = spec$max)
  }
}
param2_input_id <- function(family) paste0("param2_", family)

families_with <- function(field) {
  names(Filter(function(spec) !is.null(spec[[field]]), copula_families))
}

# Build the copula object for the selected family. Parameters at the
# independence boundary make the constructors return indepCopula() with a
# message, which is expected here and therefore silenced.
build_copula <- function(family, param, param2 = NULL, rotation = "none",
                         df = NULL) {
  spec <- copula_families[[family]]
  if (family == "t") {
    return(tCopula(param, df = df))
  }
  if (is.null(spec$rotated)) {
    rotation <- "none"
  }

  params <- param
  if (!is.null(spec$param2)) {
    params <- c(param, param2)
  }
  if (rotation %in% c("r90", "r270")) {
    params[1] <- -params[1]
    if (isTRUE(spec$param2$negate)) {
      params[2] <- -params[2]
    }
  }

  fun <- if (rotation == "none") spec$fun else spec$rotated[[rotation]]
  suppressMessages(fun(params))
}

# Degrees of freedom of the t copula: non-integer below 2, integer above
t_df_choices <- c(seq(0.1, 1.9, by = 0.1), 2:30)

# pCopula() only supports integer degrees of freedom for the t copula. In
# general, the CDF is obtained by numerically integrating the closed-form
# conditional distribution of the bivariate t distribution:
# C(u, v) = int_0^u P(V <= v | U = s) ds
t_copula_cdf <- function(u, copula) {
  u <- matrix(u, ncol = 2)
  rho <- copula@getRho(copula)
  df <- copula@parameters[[2]]
  if (df == round(df)) {
    return(pCopula(u, copula))
  }

  cond_cdf <- function(s, x2) {
    x1 <- qt(s, df)
    pt((x2 - rho * x1) / sqrt((df + x1^2) * (1 - rho^2) / (df + 1)), df + 1)
  }
  cdf <- function(u1, u2) {
    if (u1 == 0 || u2 == 0) return(0)
    if (u1 == 1) return(u2)
    if (u2 == 1) return(u1)
    # the t copula is exchangeable: integrating over the shorter interval is
    # numerically more stable for strong negative dependence
    integrate(cond_cdf, 0, min(u1, u2), x2 = qt(max(u1, u2), df),
              subdivisions = 1000L)$value
  }
  mapply(cdf, u[, 1], u[, 2])
}

# lambda() is not implemented for all families (e.g. FGM)
tail_dependence <- function(cop) {
  tryCatch(lambda(cop), error = function(e) NULL)
}

# Plotting -------------------------------------------------------------------

margin_settings <- list(
  uniform = list(limits = c(0, 1), axes = c("u", "v")),
  gaussian = list(limits = c(-3, 3), axes = c("x", "y"))
)

function_titles <- list(
  pdf = c(uniform = "PDF: strength of dependence",
          gaussian = "Bivariate density with Gaussian margins"),
  cdf = c(uniform = "CDF",
          gaussian = "Bivariate CDF with Gaussian margins")
)

copula_cdf <- function(copula) {
  if (is(copula, "tCopula")) t_copula_cdf else pCopula
}

# Evaluate the PDF or CDF on a regular grid. Uniform margins use cell
# midpoints, avoiding the boundary of the unit square where densities may be
# infinite. Gaussian margins transform the copula via qnorm(), i.e. the
# density becomes c(pnorm(x), pnorm(y)) * dnorm(x) * dnorm(y).
copula_grid <- function(copula, type = "pdf", margins = "uniform", n = 71) {
  if (margins == "uniform") {
    x <- (seq_len(n) - 0.5) / n
    u <- x
  } else {
    x <- seq(-3, 3, length.out = n)
    u <- pnorm(x)
  }

  grid <- expand.grid(x = x, y = x, KEEP.OUT.ATTRS = FALSE)
  uv <- as.matrix(expand.grid(u, u, KEEP.OUT.ATTRS = FALSE))
  if (type == "pdf") {
    grid$z <- dCopula(uv, copula)
    if (margins == "gaussian") {
      grid$z <- grid$z * dnorm(grid$x) * dnorm(grid$y)
    }
  } else {
    grid$z <- copula_cdf(copula)(uv, copula)
  }
  grid
}

plot_copula_function <- function(grid, type = "pdf", margins = "uniform") {
  if (type == "pdf" && margins == "uniform") {
    # copula densities span several orders of magnitude, hence the log scale
    grid$z <- pmax(grid$z, 1e-3)
    contours <- contour_lines(log10(grid$z), bins = 12)
    fill_scale <- scale_fill_viridis_c(
      "density", transform = "log10",
      labels = scales::label_number(drop0trailing = TRUE)
    )
  } else if (type == "pdf") {
    contours <- contour_lines(grid$z, bins = 10)
    fill_scale <- scale_fill_viridis_c("density")
  } else {
    contours <- contour_lines(grid$z, breaks = seq(0.1, 0.9, by = 0.1))
    fill_scale <- scale_fill_viridis_c("CDF", limits = c(0, 1))
  }

  ggplot(grid, aes(x, y)) +
    geom_raster(aes(fill = z), interpolate = TRUE) +
    contours +
    fill_scale +
    copula_plot_theme(function_titles[[type]][[margins]], margins) +
    # legend below the plot keeps the plot area as large as the sample plot
    theme(legend.position = "bottom",
          legend.key.width = unit(2.5, "cm"),
          legend.key.height = unit(0.3, "cm"))
}

plot_copula_sample <- function(sample, margins = "uniform") {
  if (margins == "gaussian") {
    sample <- qnorm(sample)
  }
  ggplot(data.frame(x = sample[, 1], y = sample[, 2]), aes(x, y)) +
    geom_point(colour = "#2c7fb8", alpha = 0.6, size = 1.8) +
    copula_plot_theme(paste("Sample of size", nrow(sample)), margins)
}

# interactive 3D perspective plot of the PDF or CDF
plot_copula_3d <- function(grid, type = "pdf", margins = "uniform") {
  axes <- margin_settings[[margins]]$axes
  x <- unique(grid$x)
  y <- unique(grid$y)
  # plotly expects z[i, j] to belong to y[i] and x[j]
  z <- t(matrix(grid$z, nrow = length(x)))
  z_title <- if (type == "pdf") "density" else "CDF"

  # as in the 2D view, copula densities are shown on a log scale
  log_scale <- type == "pdf" && margins == "uniform"
  if (log_scale) {
    z <- pmax(z, 1e-3)
    ticks <- 10^seq(floor(log10(min(z))), ceiling(log10(max(z))))
    surface_colour <- log10(z)
    colorbar <- list(title = z_title, tickvals = log10(ticks),
                     ticktext = format(ticks, drop0trailing = TRUE, trim = TRUE,
                                       scientific = FALSE))
  } else {
    surface_colour <- z
    colorbar <- list(title = z_title)
  }

  # horizontal colour bar below the plot, as in the 2D view
  colorbar <- c(colorbar, list(orientation = "h", y = 0, yanchor = "top",
                               len = 0.6, thickness = 12))

  plotly::plot_ly(x = x, y = y, z = z, surfacecolor = surface_colour,
                  type = "surface", colorscale = "Viridis",
                  colorbar = colorbar) |>
    plotly::layout(
      margin = list(l = 0, r = 0, b = 0, t = 0),
      scene = list(
        xaxis = list(title = axes[1]),
        yaxis = list(title = axes[2]),
        zaxis = list(title = z_title,
                     type = if (log_scale) "log" else "linear",
                     dtick = if (log_scale) 1),
        aspectratio = list(x = 1, y = 1, z = 0.7),
        camera = list(eye = list(x = 1.3, y = -1.3, z = 0.8))
      )
    ) |>
    plotly::config(displaylogo = FALSE)
}

# no contour lines for flat surfaces, e.g. the independence copula
contour_lines <- function(z, ...) {
  if (diff(range(z)) < 1e-8) {
    return(NULL)
  }
  geom_contour(aes(z = .env$z), colour = "white", alpha = 0.6,
               linewidth = 0.3, ...)
}

copula_plot_theme <- function(title, margins = "uniform") {
  settings <- margin_settings[[margins]]
  list(
    coord_equal(xlim = settings$limits, ylim = settings$limits,
                expand = FALSE),
    labs(title = title, x = settings$axes[1], y = settings$axes[2]),
    theme_minimal(base_size = 14),
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = NA, colour = "grey70"),
          plot.margin = margin(5.5, 18, 5.5, 5.5))
  )
}
