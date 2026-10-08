# End-to-end tests of the margin selection and the interactive 3D view.
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
      const el = document.getElementById('function_plot_3d');
      return !!(el && el.data && el.data.length && el.data[0].type === 'surface');
    })()"
  )
}

test_that("PDF and CDF render with uniform and Gaussian margins", {
  app <- AppDriver$new(app_dir = "../../", name = "margins",
                       load_timeout = 60000, timeout = 30000)
  on.exit(app$stop())

  for (family in c("clayton", "t", "BB7")) {
    app$set_inputs(family = family)
    for (margins in c("gaussian", "uniform")) {
      for (fun in c("cdf", "pdf")) {
        app$set_inputs(margins = margins, fun = fun)
        expect_length(visible_errors(app), 0)
      }
    }
  }
})

test_that("the 3D switch replaces the heat map by a plotly surface", {
  app <- AppDriver$new(app_dir = "../../", name = "view-3d",
                       load_timeout = 60000, timeout = 30000)
  on.exit(app$stop())

  expect_true(plot_visible(app, "function_plot"))
  expect_false(plot_visible(app, "function_plot_3d"))

  app$set_inputs(view_3d = TRUE)
  app$wait_for_js(
    "document.getElementById('function_plot_3d').data !== undefined"
  )
  expect_false(plot_visible(app, "function_plot"))
  expect_true(plot_visible(app, "function_plot_3d"))
  expect_true(surface_rendered(app))

  # the surface follows the inputs
  for (settings in list(list(margins = "gaussian"), list(fun = "cdf"),
                        list(family = "gumbel"), list(rotation = "r90"))) {
    do.call(app$set_inputs, settings)
    expect_true(surface_rendered(app))
    expect_length(visible_errors(app), 0)
  }

  app$set_inputs(view_3d = FALSE)
  expect_true(plot_visible(app, "function_plot"))
  expect_false(plot_visible(app, "function_plot_3d"))
})
