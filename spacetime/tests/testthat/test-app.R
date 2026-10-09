# End-to-end tests of the variogram families, plot types, model fitting and
# the upload of a sample variogram.
# Requires Chrome; set NOT_CRAN=true, otherwise AppDriver skips the tests.

visible_errors <- function(app) {
  app$get_js(
    "$('.shiny-output-error:visible').map((i, e) => e.id).get()"
  )
}

plot_visible <- function(app, id) {
  app$get_js(sprintf("$('#%s').is(':visible')", id))
}

# whether the conditional panel around an input is shown, independent of the
# state of the accordion holding it
condition_met <- function(app, id) {
  app$get_js(sprintf(
    "$('#%s').closest('.shiny-panel-conditional').css('display') !== 'none'",
    id
  ))
}

surface_rendered <- function(app) {
  app$get_js(
    "(() => {
      const el = document.getElementById('vgm_plot_3d');
      return !!(el && el.data && el.data.length && el.data[0].type === 'surface');
    })()"
  )
}

rmse <- function(app) as.numeric(app$get_value(output = "rmse"))

new_app <- function(name) {
  AppDriver$new(app_dir = "../../", name = name,
                load_timeout = 60000, timeout = 30000)
}

test_that("the sliders start at the fitted models", {
  app <- new_app("families")
  on.exit(app$stop())

  for (family in names(model_labels)) {
    app$set_inputs(family = family)
    # sliders round the fitted values
    expect_equal(rmse(app), sqrt(attr(fitted_models[[family]], "MSE")),
                 tolerance = 0.05)
    for (view in c("heatmap", "difference", "profiles")) {
      app$set_inputs(view = view)
      expect_length(visible_errors(app), 0)
    }
    expect_match(app$get_value(output = "model_code"),
                 sprintf('^vgmST\\("%s"', family))
    # new notation: temporal unit of the sample, overall sill from the model
    expect_match(app$get_value(output = "model_code"),
                 'temporalUnit = "days"\\)$')
    expect_equal(as.numeric(app$get_value(output = "st_sill")),
                 model_sill(fitted_models[[family]]), tolerance = 0.05)
  }
})

test_that("changing a parameter changes the model", {
  app <- new_app("parameters")
  on.exit(app$stop())

  app$set_inputs(family = "metric")
  before <- rmse(app)
  app$set_inputs(metric_joint_model = "Gau")
  expect_false(isTRUE(all.equal(rmse(app), before)))
  expect_match(app$get_value(output = "model_code"), '"Gau"')
  # kappa is only offered for families with a shape parameter
  expect_false(condition_met(app, "metric_joint_kappa"))
  app$set_inputs(metric_joint_model = "Mat")
  expect_true(condition_met(app, "metric_joint_kappa"))
})

test_that("fitting improves a mis-specified model", {
  app <- new_app("fit")
  on.exit(app$stop())

  app$set_inputs(family = "separable", separable_sill = 60)
  before <- rmse(app)
  app$click("fit")
  app$wait_for_idle()
  expect_lt(rmse(app), before)
  expect_length(visible_errors(app), 0)
})

test_that("the 3D view replaces the 2D plot by a plotly surface", {
  app <- new_app("view-3d")
  on.exit(app$stop())

  app$set_inputs(view = "3d")
  app$wait_for_js(
    "document.getElementById('vgm_plot_3d').data !== undefined"
  )
  expect_false(plot_visible(app, "vgm_plot"))
  expect_true(plot_visible(app, "vgm_plot_3d"))
  for (family in names(model_labels)) {
    app$set_inputs(family = family)
    expect_true(surface_rendered(app))
  }
})

test_that("an uploaded sample variogram replaces the PM10 sample", {
  app <- new_app("upload")
  on.exit(app$stop())

  # the PM10 sample in metres instead of km
  sample_m <- pm10_sample
  sample_m$dist <- sample_m$dist * 1000
  sample_m$spacelag <- sample_m$spacelag * 1000
  attr(sample_m$spacelag, "units") <- "m"
  attr(sample_m, "boundaries") <- attr(sample_m, "boundaries") * 1000
  file <- tempfile(fileext = ".RData")
  save(sample_m, file = file)

  app$upload_file(sample_file = file)
  expect_equal(app$get_value(output = "sample_name"), basename(file))
  expect_length(visible_errors(app), 0)
  expect_match(app$get_js("$('label[for=metric_joint_range]').text()"),
               "\\(m\\)")

  # the parameters follow the new spatial unit, the fit stays as good
  app$set_inputs(family = "metric")
  expect_equal(app$get_value(input = "metric_joint_range"),
               1000 * model_params(fitted_models$metric,
                                   "metric")$metric_joint_range,
               tolerance = 0.01)
  expect_equal(rmse(app), sqrt(attr(fitted_models$metric, "MSE")),
               tolerance = 0.05)
  app$click("fit")
  app$wait_for_idle()
  expect_lt(rmse(app), 7)

  app$click("reset_sample")
  expect_equal(app$get_value(output = "sample_name"), "PM10 Germany 2005")
})
