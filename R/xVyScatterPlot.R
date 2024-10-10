#' Create UI components to select the variables to plot against each other
#'
#' `xVyScatterplotInput()` produces the select inputs for selecting the
#' variables to plot against each other
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [xVyScatterplotServer()] function.
#' @param x_label character Label for X variable select input
#' @param y_label character Label for Y variable select input
#'
#' @returns a [htmltools::tagList()] containing two [shiny::selectizeInput()]
#' controls and one [shiny::checkboxInput()]
#'
#' @export
#'
#' @examples
#'
#' xVyScatterplotInput("countData")
#'
xVyScatterplotInput <-
  function(id, x_label = "X variable", y_label = "Y variable") {
    tagList(
      selectizeInput(NS(id, "xVar"), x_label, choices = NULL),
      selectizeInput(NS(id, "yVar"), y_label, choices = NULL),
      checkboxInput(NS(id, "lm"), 'Add regression line', value = FALSE, width = NULL)
    )
}

#' Create Output to display the Scatterplot
#'
#' `xVyScatterplotOutput()` produces a PlotOutput element for the scatterplot
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [xVyScatterplotServer()] function.
#'
#' @returns a [htmltools::tagList()] containing three [shiny::plotOutput()]s
#'
#' @export
#'
#' @examples
#'
#' xVyScatterplotOutput("countData")
#'
xVyScatterplotOutput <- function(id) {
  tagList(
    plotOutput(NS(id, "xy_scatter"))
  )
}

#' Server function to produce an xy scatter plot
#'
#' `xVyScatterplotServer()` takes a data frame and produces a scatterplot
#' based on the variables selected
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [xVyScatterplotInput()] function.
#' @param data data.frame for plotting. Expected in long format with a column
#' named `variable` and one named `value`
#'
#' @returns a list invisibly containing one [shiny::reactive()] object
#' * plot_data a data.frame of plotted data
#'
#' @export
#'
#' @examples
#'
#' xVyScatterplotServer("rnaseqData")
#'
xVyScatterplotServer <- function(id, data = NULL, debug = FALSE) {
  stopifnot(is.reactive(data))

  moduleServer(id, function(input, output, session) {
    observe({
      var_choices = unique(data()$variable)
      updateSelectizeInput(
        session,
        inputId = "xVar",
        choices = var_choices,
        selected = var_choices[1]
      )
      updateSelectizeInput(
        session,
        inputId = "yVar",
        choices = var_choices,
        selected = var_choices[2]
      )
    }) |>
      bindEvent(data())

    plot_data <- reactive({
      data <- req(data())
      x_var <- req(input$xVar)
      y_var <- req(input$yVar)
      data |>
        dplyr::filter(variable %in% c(x_var, y_var)) |>
        tidyr::pivot_wider(names_from = variable, values_from = value)
    })

    plot <- reactive({
      plot_data <- req(plot_data())
      x_var <- req(input$xVar)
      y_var <- req(input$yVar)
      p <- ggplot2::ggplot(
        data = plot_data,
        aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))
      ) + ggplot2::geom_point() +
        ggplot2::theme_minimal()
      if (input$lm) {
        p <- p +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x, fill = "orange")
      }
      return(p)
    })

    output$xy_scatter <- renderPlot(plot())

    return(list(plot_data = plot_data))
  })
}

#' A test shiny app for the xVyScatterplot module
#'
#' `xVyScatterplotApp()` creates a small test app for testing the [xVyScatterplotInput()] and
#' [xVyScatterplotServer()] functions. A subset of the returned sample and count data.frames
#' are displayed in two [shiny::tableOutput()]s
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' xVyScatterplotApp()
xVyScatterplotApp <- function(debug = FALSE) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        xVyScatterplotInput("countData", x_label = "X Var", y_label = "Y Var")
      ),
      mainPanel(
        h3("Scatterplot:"),
        xVyScatterplotOutput("countData"),
      )
    )
  )
  server <- function(input, output, session) {
    set.seed(56)
    test_data = tibble::tibble(
      sample = paste0("sample-", 1:100),
      x1 = rnorm(100),
      y1 = x1 + runif(100, min = -2, max = 2),
      y2 = -x1 + runif(100, min = -3, max = 3)
    ) |>
      tidyr::pivot_longer(cols = -sample, names_to = "variable")
    xVyScatterplotServer("countData", data = reactive({test_data}), debug = debug)
  }
  shinyApp(ui, server)
}

