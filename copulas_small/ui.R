# copulas small

family_labels <- vapply(copula_families, `[[`, character(1), "label")
family_choices <- setNames(names(copula_families), family_labels)[order(family_labels)]

js_family_in <- function(families) {
  sprintf("[%s].includes(input.family)",
          paste0("'", families, "'", collapse = ", "))
}

# parameter sliders per family, shown only for the selected family
param_panels <- lapply(names(copula_families), function(family) {
  spec <- copula_families[[family]]
  two_params <- !is.null(spec$param2)
  conditionalPanel(
    condition = sprintf("input.family == '%s'", family),
    sliderInput(
      param_input_id(family),
      if (two_params) "First parameter" else "Parameter",
      min = spec$min, max = spec$max, value = spec$value, step = 0.01
    ),
    if (two_params) {
      sliderInput(
        param2_input_id(family), "Second parameter",
        min = spec$param2$min, max = spec$param2$max,
        value = spec$param2$value, step = 0.01
      )
    }
  )
})

page_sidebar(
  title = "Different Copula Families",
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 320,
    p(
      "This small shiny app illustrates a couple of copula families",
      "implemented in the",
      a("copula", href = "https://cran.r-project.org/package=copula",
        target = "_blank"),
      "and",
      a("VineCopula", href = "https://cran.r-project.org/package=VineCopula",
        target = "_blank"),
      "R packages, the latter made accessible through",
      a("VC2copula", href = "https://cran.r-project.org/package=VC2copula",
        target = "_blank", .noWS = "after"),
      ". Please select the copula family and corresponding parameters below."
    ),
    selectInput("family", "Copula family", choices = family_choices,
                selected = "clayton"),
    radioButtons(
      "fun", "Function",
      choices = c("PDF" = "pdf", "CDF" = "cdf"),
      inline = TRUE
    ),
    radioButtons(
      "margins", "Margins",
      choices = c("uniform" = "uniform", "Gaussian" = "gaussian"),
      inline = TRUE
    ),
    input_switch("view_3d", "Interactive 3D perspective"),
    conditionalPanel(
      condition = js_family_in(families_with("rotated")),
      selectInput("rotation", "Rotation", choices = rotation_labels)
    ),
    param_panels,
    conditionalPanel(
      condition = "input.family == 't'",
      sliderTextInput("df", "Degrees of freedom",
                      choices = t_df_choices, selected = 2, grid = TRUE)
    ),
    sliderInput("sample_size", "Sample size",
                min = 10, max = 1000, value = 250, step = 10),
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
      title = "Kendall's tau",
      value = textOutput("kendalls_tau")
    ),
    value_box(
      title = "Tail dependence (lower, upper)",
      value = textOutput("tail_dependence")
    )
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header(textOutput("caption", inline = TRUE)),
      # fill carriers let the plots grow with the card, like the sample plot
      as_fill_carrier(
        conditionalPanel("!input.view_3d", plotOutput("function_plot"))
      ),
      as_fill_carrier(
        conditionalPanel("input.view_3d",
                         plotly::plotlyOutput("function_plot_3d"))
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Random sample"),
      plotOutput("sample_plot")
    )
  )
)
