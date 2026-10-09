# Builds the data used by the app from the daily mean PM10 concentrations at
# rural background stations in Germany 2005 (DE_RB_2005, gstat):
# - data/pm10_stations.gpkg: stations and German federal states for the map
# - data/variograms.rds: the sample variogram and the fitted spatio-temporal
#   variogram models, following demo(stkrige) in gstat
#
# DE_RB_2005 and DE_NUTS1 (spacetime) are distributed as sp/spacetime
# objects; they are converted to sf once here, the sample variogram is
# computed from a stars vector data cube. The app itself works with sf only.
# Run from the app directory: Rscript data-raw/prepare_data.R

library(sf)
library(stars)
library(gstat)

set.seed(123)

data("DE_RB_2005", package = "gstat")
data("air", package = "spacetime")

# the stored CRS uses the deprecated "+init=epsg:" syntax
utm32 <- st_crs(32632)

stations <- st_as_sf(DE_RB_2005@sp) |>
  st_set_crs(NA) |>
  st_set_crs(utm32)

# daily means as a station x day matrix, NA where nothing was reported
days <- zoo::index(DE_RB_2005@time)
pm10 <- matrix(NA_real_, nrow(stations), length(days))
pm10[DE_RB_2005@index] <- DE_RB_2005@data$PM10

# vector data cube (station x day) for the sample variogram
pm10_cube <- st_as_stars(
  list(PM10 = pm10),
  dimensions = st_dimensions(station = st_geometry(stations), time = days)
)

stations <- data.frame(
  code = stations$station_european_code,
  altitude = stations$station_altitude,
  days_reported = rowSums(!is.na(pm10)),
  mean_pm10 = rowMeans(pm10, na.rm = TRUE),
  geometry = st_geometry(stations)
) |>
  st_as_sf()

states <- st_as_sf(DE_NUTS1)[, "NAME_1"] |>
  setNames(c("name", "geometry")) |>
  st_set_geometry("geometry") |>
  st_transform(utm32) |>
  # 1 km tolerance is plenty for an overview map and keeps the file small
  st_simplify(preserveTopology = TRUE, dTolerance = 1000)

out <- file.path("data", "pm10_stations.gpkg")
st_write(stations, out, layer = "stations", delete_dsn = TRUE, quiet = TRUE)
st_write(states, out, layer = "states", quiet = TRUE)

# sample variogram -------------------------------------------------------------

emp_vgm <- variogramST(PM10 ~ 1, pm10_cube, tlags = 0:6, progress = FALSE)

# spatio-temporal anisotropy as starting value for the fits
lin_st_ani <- estiStAni(emp_vgm, c(50000, 200000))

# fit in km
emp_vgm$dist <- emp_vgm$dist / 1000
emp_vgm$avgDist <- emp_vgm$avgDist / 1000
emp_vgm$spacelag <- emp_vgm$spacelag / 1000
attr(emp_vgm, "boundaries") <- attr(emp_vgm, "boundaries") / 1000
attr(emp_vgm$spacelag, "units") <- "km"
lin_st_ani <- lin_st_ani / 1000

# fitted models ----------------------------------------------------------------

fit <- function(model, ...) {
  fit.StVariogram(emp_vgm, model, fit.method = 7, stAni = lin_st_ani,
                  method = "L-BFGS-B", ...)
}

models <- list(
  separable = fit(
    vgmST("separable",
          space = vgm(0.9, "Exp", 200, 0.1),
          time = vgm(0.9, "Sph", 3.5, 0.1),
          sill = 120),
    control = list(parscale = c(100, 1, 10, 1, 100)),
    lower = c(10, 0, 0.1, 0, 0.1),
    upper = c(2000, 1, 12, 1, 200)
  ),
  productSum = fit(
    vgmST("productSum",
          space = vgm(10, "Exp", 200, 1),
          time = vgm(10, "Sph", 2, 1),
          k = 2),
    control = list(parscale = c(1, 10, 1, 1, 0.1, 1, 10)),
    lower = rep(0.0001, 7)
  ),
  sumMetric = fit(
    vgmST("sumMetric",
          space = vgm(20, "Sph", 150, 1),
          time = vgm(10, "Exp", 2, 0.5),
          joint = vgm(80, "Sph", 1500, 2.5),
          stAni = 120),
    lower = c(sill.s = 0, range.s = 10, nugget.s = 0,
              sill.t = 0, range.t = 0.1, nugget.t = 0,
              sill.st = 0, range.st = 10, nugget.st = 0,
              anis = 40),
    upper = c(sill.s = 200, range.s = 1e3, nugget.s = 20,
              sill.t = 200, range.t = 75, nugget.t = 20,
              sill.st = 200, range.st = 5e3, nugget.st = 20,
              anis = 500),
    control = list(parscale = c(1, 100, 1, 1, 0.5, 1, 1, 100, 1, 100),
                   maxit = 1e4)
  ),
  simpleSumMetric = fit(
    vgmST("simpleSumMetric",
          space = vgm(120, "Sph", 150),
          time = vgm(120, "Exp", 10),
          joint = vgm(120, "Sph", 150),
          nugget = 10, stAni = 150),
    lower = c(sill.s = 0, range.s = 10,
              sill.t = 0, range.t = 0.1,
              sill.st = 0, range.st = 10,
              nugget = 0, anis = 40),
    upper = c(sill.s = 200, range.s = 500,
              sill.t = 200, range.t = 20,
              sill.st = 200, range.st = 5000,
              nugget = 100, anis = 1000),
    control = list(parscale = c(1, 10, 1, 1, 1, 100, 1, 10))
  ),
  metric = fit(
    vgmST("metric",
          joint = vgm(60, "Mat", 150, 10, kappa = 0.6),
          stAni = 60),
    control = list(parscale = c(10, 20, 5, 10)),
    lower = c(80, 50, 5, 50),
    upper = c(200, 1500, 60, 300)
  )
)

# root-mean-squared errors of the fits
print(sapply(models, function(m) sqrt(attr(m, "MSE"))))

# plain numbers for plotting
emp_vgm$timelag <- as.numeric(emp_vgm$timelag, units = "days")
attr(emp_vgm$timelag, "units") <- "days"

saveRDS(list(sample = emp_vgm, models = models),
        file.path("data", "variograms.rds"))
