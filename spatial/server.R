# spatial

function(input, output, session) {

  model <- reactive(fitted_models[[input$family]])

  output$caption <- renderText(
    paste(model_labels[[input$family]], "covariance model")
  )

  output$rmse <- renderText(format(round(fit_rmse(model()), 2), nsmall = 2))

  output$n_par <- renderText(fit_n_par(model()))

  output$vgm_plot <- renderPlot({
    req(input$view != "3d")
    switch(input$view,
           heatmap = plot_heat_map(model()),
           difference = plot_difference(model()),
           profiles = plot_profiles(model()))
  })

  output$vgm_plot_3d <- plotly::renderPlotly(plot_3d(model()))

  output$station_map <- renderPlot(plot_stations())

  output$model_table <- renderTable(model_table(model()), digits = 2)

  output$model_globals <- renderText(model_globals(model()))
}
