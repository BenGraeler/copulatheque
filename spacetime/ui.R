# spacetime

# initial slider settings follow the PM10 sample and the fitted models
initial_limits <- sample_limits(pm10_sample)
initial_units <- lag_units(pm10_sample)

param_input <- function(family, comp, param, value) {
  id <- param_id(family, comp, param)
  label <- param_label(family, comp, param, initial_units)
  if (param == "model") {
    return(selectInput(id, label, choices = vgm_families, selected = value))
  }
  settings <- slider_settings(family, comp, param, value, initial_limits)
  input <- sliderInput(id, label, min = settings$min, max = settings$max,
                       value = settings$value, step = settings$step)
  if (param == "kappa") {
    # only shown for families with a shape parameter
    model_id <- param_id(family, comp, "model")
    input <- conditionalPanel(
      sprintf("[%s].includes(input.%s)",
              paste0("'", kappa_families, "'", collapse = ", "), model_id),
      input
    )
  }
  input
}

param_panels <- lapply(names(model_specs), function(family) {
  spec <- model_specs[[family]]
  values <- model_params(fitted_models[[family]], family)
  params <- family_params(family)
  inputs <- function(rows) {
    lapply(rows, function(i) {
      param_input(family, params$component[i], params$param[i],
                  values[[params$id[i]]])
    })
  }
  comp_titles <- c(space = "space: spatial variogram",
                   time = "time: temporal variogram",
                   joint = "joint: joint spatio-temporal variogram")
  conditionalPanel(
    condition = sprintf("input.family == '%s'", family),
    inputs(which(is.na(params$component))),
    accordion(
      open = FALSE,
      !!!lapply(spec$components, function(comp) {
        accordion_panel(comp_titles[[comp]],
                        inputs(which(params$component %in% comp)))
      })
    )
  )
})

page_sidebar(
  title = "Spatio-Temporal Variogram Families",
  theme = bs_theme(version = 5),

  sidebar = sidebar(
    width = 360,
    p(
      "This small shiny app lets you explore the spatio-temporal variogram",
      "families as defined in the",
      a("gstat", href = "https://cran.r-project.org/package=gstat",
        target = "_blank"),
      "R package. By default, the sample variogram is calculated from daily",
      "mean", HTML("PM<sub>10</sub>"), "concentrations across Germany during",
      "the year 2005 (see", code("demo(stkrige)"), "in gstat)."
    ),
    fileInput(
      "sample_file", "Your own sample variogram (optional)",
      accept = c(".rds", ".RData", ".rda"),
      placeholder = ".rds or .RData file"
    ),
    helpText(
      "The file needs to hold a single sample variogram as returned by",
      code("gstat::variogramST()", .noWS = "after"), ".",
      actionLink("reset_sample", "Back to the PM10 sample.")
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
    param_panels,
    actionButton("fit", "Fit model to sample",
                 icon = icon("wand-magic-sparkles"), class = "btn-primary"),
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
      title = "Root-mean-squared error of the model",
      value = textOutput("rmse")
    ),
    value_box(
      title = "Overall spatio-temporal sill",
      value = textOutput("st_sill")
    ),
    value_box(
      title = "Sample variogram",
      value = textOutput("sample_name"),
      p(textOutput("sample_size", inline = TRUE))
    )
  ),
  card(
    full_screen = TRUE,
    min_height = 450,
    card_header(textOutput("caption", inline = TRUE)),
    as_fill_carrier(
      conditionalPanel("input.view != '3d'", plotOutput("vgm_plot"))
    ),
    as_fill_carrier(
      conditionalPanel("input.view == '3d'",
                       plotly::plotlyOutput("vgm_plot_3d"))
    )
  ),
  card(
    fill = FALSE,
    card_header("R code of the model"),
    verbatimTextOutput("model_code")
  )
)
