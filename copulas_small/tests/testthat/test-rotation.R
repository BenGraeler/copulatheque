# End-to-end tests of the parameter slider updates for rotated copulas.
# Requires Chrome; set NOT_CRAN=true, otherwise AppDriver skips the tests.

slider_state <- function(app, id) {
  app$get_js(sprintf(
    "(() => {
      const s = $('#%s').data('ionRangeSlider');
      return {min: s.options.min, max: s.options.max, value: s.result.from};
    })()",
    id
  ))
}

visible_errors <- function(app) {
  app$get_js(
    "$('.shiny-output-error:visible').map((i, e) => e.id).get()"
  )
}

expect_no_output_errors <- function(app) {
  expect_length(visible_errors(app), 0)
  values <- app$get_values(output = c("kendalls_tau", "tail_dependence"))
  expect_false(any(vapply(values$output, inherits, logical(1), "error")))
}

test_that("Clayton slider range follows the selected rotation", {
  app <- AppDriver$new(app_dir = "../../", name = "rotation",
                       load_timeout = 60000, timeout = 30000)
  on.exit(app$stop())

  expect_equal(app$get_value(input = "family"), "clayton")
  app$set_inputs(param_clayton = -0.5)
  expect_equal(slider_state(app, "param_clayton"),
               list(min = -0.99, max = 10, value = -0.5))
  expect_no_output_errors(app)

  # rotated versions only support positive parameters: range and value adapt
  for (rotation in c("r90", "sur", "r270")) {
    app$set_inputs(rotation = rotation)
    expect_equal(slider_state(app, "param_clayton"),
                 list(min = 0.01, max = 10, value = 0.01))
    expect_no_output_errors(app)
  }

  app$set_inputs(param_clayton = 5)
  expect_equal(app$get_value(output = "kendalls_tau"), "-0.71")

  # back to the unrotated copula: full range, value kept
  app$set_inputs(rotation = "none")
  expect_equal(slider_state(app, "param_clayton"),
               list(min = -0.99, max = 10, value = 5))
  expect_equal(app$get_value(output = "kendalls_tau"), "0.71")

  # the same holds for the CDF view
  app$set_inputs(param_clayton = -0.7)
  app$set_inputs(fun = "cdf", rotation = "sur")
  expect_equal(slider_state(app, "param_clayton"),
               list(min = 0.01, max = 10, value = 0.01))
  expect_no_output_errors(app)
})

test_that("all rotatable families render without errors in every rotation", {
  app <- AppDriver$new(app_dir = "../../", name = "rotation-families",
                       load_timeout = 60000, timeout = 30000)
  on.exit(app$stop())

  for (family in setdiff(families_with("rotated"), "clayton")) {
    app$set_inputs(family = family)
    for (rotation in c("none", "r90", "sur", "r270")) {
      app$set_inputs(rotation = rotation)
      expect_no_output_errors(app)
    }
  }
})
