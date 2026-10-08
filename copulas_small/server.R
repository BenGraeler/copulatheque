# copulas small

function(input, output, session) {

  spec <- reactive(copula_families[[input$family]])

  rotation <- reactive({
    if (is.null(spec()$rotated)) "none" else input$rotation
  })

  # adapt the parameter slider to the range of the selected rotation
  observeEvent(list(input$family, rotation()), {
    req(!is.null(spec()$rotated_range))
    id <- param_input_id(input$family)
    range <- param_range(spec(), rotation())
    value <- min(max(input[[id]], range[["min"]]), range[["max"]])

    # keep cop() from using the out-of-range value until the slider is updated
    if (value != input[[id]]) {
      freezeReactiveValue(input, id)
    }
    updateSliderInput(session, id, min = range[["min"]], max = range[["max"]],
                      value = value)
  })

  cop <- reactive({
    param <- input[[param_input_id(input$family)]]
    param2 <- input[[param2_input_id(input$family)]]
    req(param, is.null(spec()$param2) || !is.null(param2))

    tryCatch(
      build_copula(input$family, param, param2, rotation(),
                   df = as.numeric(input$df)),
      error = function(e) {
        validate(paste("The selected parameters are not admissible for this",
                       "copula:", conditionMessage(e)))
      }
    )
  })

  output$caption <- renderText({
    caption <- paste(spec()$label, "copula")
    if (rotation() != "none") {
      rotation_label <- names(rotation_labels)[rotation_labels == rotation()]
      caption <- paste0(caption, ", rotated: ", rotation_label)
    }
    caption
  })

  output$kendalls_tau <- renderText({
    # evaluate cop() first: inside S4 dispatch, validation messages would be
    # turned into regular errors
    copula <- cop()
    format(round(tau(copula), 2), nsmall = 2)
  })

  output$tail_dependence <- renderText({
    copula <- cop()
    lambda <- tail_dependence(copula)
    if (is.null(lambda)) {
      "not available"
    } else {
      paste(format(round(lambda, 2), nsmall = 2), collapse = ", ")
    }
  })

  # shared by the 2D and the 3D view
  function_grid <- reactive({
    copula <- cop()
    copula_grid(copula, input$fun, input$margins)
  })

  output$function_plot <- renderPlot({
    plot_copula_function(function_grid(), input$fun, input$margins)
  })

  output$function_plot_3d <- plotly::renderPlotly({
    plot_copula_3d(function_grid(), input$fun, input$margins)
  })

  output$sample_plot <- renderPlot({
    copula <- cop()
    plot_copula_sample(rCopula(input$sample_size, copula), input$margins)
  })
}
