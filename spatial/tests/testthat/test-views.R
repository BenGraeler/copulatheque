# End-to-end tests of the variogram families and plot types.
# Requires Chrome; set NOT_CRAN=true, otherwise AppDriver skips the tests.

visible_errors <- function(app) {
  app$get_js(
    "$('.shiny-output-error:visible').map((i, e) => e.id).get()"
  )
}

plot_visible <- function(app, id) {
  app$get_js(sprintf("$('#%s').is(':visible')", id))
}

surface_rendered <- function(app) {
  app$get_js(
    "(() => {
      const el = document.getElementById('vgm_plot_3d');
      return !!(el && el.data && el.data.length && el.data[0].type === 'surface');
    })()"
  )
}

test_that("all families render in all 2D plot types", {
  app <- AppDriver$new(app_dir = "../../", name = "views",
                       load_timeout = 60000, timeout = 30000)
  on.exit(app$stop())

  for (family in names(model_labels)) {
    app$set_inputs(family = family)
    for (view in c("heatmap", "difference", "profiles")) {
      app$set_inputs(view = view)
      expect_length(visible_errors(app), 0)
    }
    expect_equal(app$get_value(output = "rmse"),
                 format(round(fit_rmse(fitted_models[[family]]), 2),
                        nsmall = 2))
  }
})

test_that("the 3D view replaces the 2D plot by a plotly surface", {
  app <- AppDriver$new(app_dir = "../../", name = "view-3d",
                       load_timeout = 60000, timeout = 30000)
  on.exit(app$stop())

  expect_true(plot_visible(app, "vgm_plot"))
  expect_false(plot_visible(app, "vgm_plot_3d"))

  app$set_inputs(view = "3d")
  app$wait_for_js(
    "document.getElementById('vgm_plot_3d').data !== undefined"
  )
  expect_false(plot_visible(app, "vgm_plot"))
  expect_true(plot_visible(app, "vgm_plot_3d"))

  for (family in names(model_labels)) {
    app$set_inputs(family = family)
    expect_true(surface_rendered(app))
    expect_length(visible_errors(app), 0)
  }
})

test_that("the station map is drawn", {
  app <- AppDriver$new(app_dir = "../../", name = "map",
                       load_timeout = 60000, timeout = 30000)
  on.exit(app$stop())

  expect_match(app$get_value(output = "station_map")$src, "^data:image/png")
})
