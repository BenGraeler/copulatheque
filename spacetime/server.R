# spacetime

function(input, output, session) {

  sample <- reactiveVal(pm10_sample)
  sample_name <- reactiveVal("PM10 Germany 2005")

  # adapt ranges, labels and values of a family's inputs to the sample
  update_inputs <- function(family, values, sample) {
    limits <- sample_limits(sample)
    units <- lag_units(sample)
    params <- family_params(family)
    for (i in seq_len(nrow(params))) {
      id <- params$id[i]
      label <- param_label(family, params$component[i], params$param[i], units)
      if (params$param[i] == "model") {
        updateSelectInput(session, id, label = label, selected = values[[id]])
      } else {
        settings <- slider_settings(family, params$component[i],
                                    params$param[i], values[[id]], limits)
        updateSliderInput(session, id, label = label, min = settings$min,
                          max = settings$max, value = settings$value,
                          step = settings$step)
      }
    }
  }

  current_values <- function(family) {
    ids <- family_params(family)$id
    setNames(lapply(ids, function(id) input[[id]]), ids)
  }

  set_sample <- function(new_sample, name) {
    old_sample <- sample()
    sample(new_sample)
    sample_name(name)
    for (family in names(model_specs)) {
      values <- rescale_params(family, current_values(family), old_sample,
                               new_sample)
      update_inputs(family, values, new_sample)
    }
  }

  observeEvent(input$sample_file, {
    file <- input$sample_file
    new_sample <- tryCatch(
      read_sample(file$datapath, file$name),
      error = function(e) {
        showNotification(paste("The sample variogram could not be read:",
                               conditionMessage(e)), type = "error")
        NULL
      }
    )
    req(new_sample)
    set_sample(new_sample, file$name)
  })

  observeEvent(input$reset_sample, set_sample(pm10_sample, "PM10 Germany 2005"))

  temporal_unit <- reactive(lag_units(sample())[["time"]])

  values <- reactive({
    values <- current_values(input$family)
    req(!any(vapply(values, is.null, logical(1))))
    values
  })

  model <- reactive({
    tryCatch(
      build_model(input$family, values(), temporal_unit()),
      error = function(e) {
        validate(paste("The selected parameters do not define a valid model:",
                       conditionMessage(e)))
      }
    )
  })

  observeEvent(input$fit, {
    fitted <- withProgress(
      message = "Fitting the model ...",
      tryCatch(fit_model(sample(), model()), error = function(e) {
        showNotification(paste("The fit failed:", conditionMessage(e)),
                         type = "error")
        NULL
      })
    )
    req(fitted)
    update_inputs(input$family, model_params(fitted, input$family), sample())
  })

  output$caption <- renderText(
    paste(model_labels[[input$family]], "covariance model")
  )

  output$rmse <- renderText({
    current <- model()
    format(round(model_rmse(sample(), current), 2), nsmall = 2)
  })

  output$sample_name <- renderText(sample_name())

  output$sample_size <- renderText({
    points <- sample_points(sample())
    sprintf("%d spatial and %d temporal lags",
            length(unique(points$spacelag)), length(unique(points$timelag)))
  })

  output$vgm_plot <- renderPlot({
    req(input$view != "3d")
    current <- model()
    switch(input$view,
           heatmap = plot_heat_map(sample(), current),
           difference = plot_difference(sample(), current),
           profiles = plot_profiles(sample(), current))
  })

  output$vgm_plot_3d <- plotly::renderPlotly({
    current <- model()
    plot_3d(sample(), current)
  })

  output$st_sill <- renderText({
    current <- model()
    format(round(model_sill(current), 2), nsmall = 2)
  })

  output$model_code <- renderText(
    model_code(input$family, values(), temporal_unit())
  )
}
