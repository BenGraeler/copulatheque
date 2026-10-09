# spatial

page_sidebar(
  title = "Different Spatio-Temporal Variogram Families",
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 320,
    p(
      "This small shiny app illustrates a couple of different",
      "spatio-temporal variogram families as defined in the",
      a("gstat", href = "https://cran.r-project.org/package=gstat",
        target = "_blank"),
      "R package. The sample variogram is calculated from daily mean",
      HTML("PM<sub>10</sub>"),
      "concentrations across Germany during the year 2005 (see",
      code("demo(stkrige)"), "in gstat)."
    ),
    selectInput("family", "Variogram family",
                choices = setNames(names(model_labels),
                                   paste(model_labels, "covariance model"))),
    radioButtons(
      "view", "Plot type",
      choices = c("heat map" = "heatmap",
                  "difference to sample" = "difference",
                  "spatial profiles" = "profiles",
                  "interactive 3D" = "3d")
    ),
    p(
      "Written by",
      a("Benedikt Gräler", href = "http://ben.graeler.org", target = "_blank",
        .noWS = "after"),
      ", updated Oct 2026."
    )
  ),

  layout_columns(
    fill = FALSE,
    value_box(
      title = "Root-mean-squared error of the fit",
      value = textOutput("rmse")
    ),
    value_box(
      title = "Number of fitted parameters",
      value = textOutput("n_par")
    )
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(
      full_screen = TRUE,
      card_header(textOutput("caption", inline = TRUE)),
      as_fill_carrier(
        conditionalPanel("input.view != '3d'", plotOutput("vgm_plot"))
      ),
      as_fill_carrier(
        conditionalPanel("input.view == '3d'",
                         plotly::plotlyOutput("vgm_plot_3d"))
      )
    ),
    layout_columns(
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header(HTML("Rural background PM<sub>10</sub> stations")),
        plotOutput("station_map")
      ),
      card(
        card_header("Model parameters"),
        tableOutput("model_table"),
        textOutput("model_globals")
      )
    )
  )
)
