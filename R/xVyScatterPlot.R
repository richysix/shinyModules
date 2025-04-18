#' Create UI components to select the variables to plot against each other
#'
#' `xVyScatterplotInput()` produces the select inputs for selecting the
#' variables to plot against each other
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [xVyScatterplotServer()] function.
#' @param x_label character Label for X variable select input
#' @param y_label character Label for Y variable select input
#' @param hide_x logical. Whether to hide the X input select element
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
  function(id, x_label = "X variable", y_label = "Y variable",
           hide_x = FALSE) {
    if (hide_x) {
      xInput <- shinyjs::hidden(selectizeInput(NS(id, "xVar"), x_label, choices = NULL))
    } else {
      xInput <- selectizeInput(NS(id, "xVar"), x_label, choices = NULL)
    }
    tagList(
      shinyjs::useShinyjs(),
      xInput,
      selectizeInput(NS(id, "yVar"), y_label, choices = NULL),
      checkboxInput(NS(id, "lm"), 'Add regression line', value = FALSE, width = NULL),
      checkboxInput(NS(id, "cor"), 'Show correlation coefficient', value = FALSE, width = NULL),
      radioButtons(
        NS(id, "cor_method"),
        label = "Correlation method",
        choices = c("spearman", "pearson", "cosine"),
      )
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
xVyScatterplotServer <- function(id, data = NULL, xSelected = NULL, debug = FALSE) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(xSelected))

  moduleServer(id, function(input, output, session) {
    observe(
      {
        var_choices <- unique(data()$variable)
        updateSelectizeInput(
          session,
          inputId = "xVar",
          choices = var_choices,
          selected = var_choices[1],
          server = TRUE
        )
        updateSelectizeInput(
          session,
          inputId = "yVar",
          choices = var_choices,
          selected = var_choices[2],
          server = TRUE
        )
      },
      priority = 1
    ) |> bindEvent(data())

    observe(
      {
        req(xSelected())
        req(data())
        var_choices <- unique(data()$variable)
        updateSelectizeInput(
          session,
          inputId = "xVar",
          choices = var_choices,
          selected = xSelected(),
          server = TRUE
        )
      },
      priority = -1
    ) |> bindEvent(xSelected())

    observe(
      {
        req(input$xVar)
        req(input$yVar)
        req(data())
        var_choices <- setdiff(unique(data()$variable), input$xVar)
        if (input$yVar %in% var_choices) {
          selected <- input$yVar
        } else {
          selected <- var_choices[1]
        }
        if (debug) {
          print(var_choices)
        }
        updateSelectizeInput(
          session,
          inputId = "yVar",
          choices = var_choices,
          selected = selected,
          server = TRUE
        )
      },
      priority = 0
    ) |> bindEvent(input$xVar)

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
        ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))
      ) + ggplot2::geom_point() +
        ggplot2::theme_minimal()
      if (input$lm) {
        p <- p +
          ggplot2::geom_smooth(method = "lm", formula = y ~ x, fill = "orange")
      }
      if (input$cor) {
        # calculate correlation coefficient
        if ((input$cor_method) == "cosine") {
          cor_val <- coop::cosine(
            plot_data[[x_var]],
            plot_data[[y_var]]
          ) |> round(digits = 3)
        } else {
          cor_val <- cor(
            plot_data[[x_var]],
            plot_data[[y_var]],
            method = input$cor_method
          ) |> round(digits = 3)
        }

        # gets plot limits to position text
        build_obj <- ggplot2::ggplot_build(p)
        x_lims <- build_obj$layout$panel_params[[1]]$x.range
        x_range <- x_lims[2] - x_lims[1]
        text_x_pos <- x_lims[1] + 0.05*x_range
        y_lims <- build_obj$layout$panel_params[[1]]$y.range
        y_range <- y_lims[2] - y_lims[1]
        text_y_pos <- y_lims[2] - 0.1*y_range
        p <- p +
          ggplot2::annotate(
            "label",
            x = text_x_pos,
            y = text_y_pos,
            label = glue::glue("{input$cor_method} == {cor_val}"),
            parse = TRUE,
            size.unit = "pt",
            size = 16,
            colour = "firebrick4",
            hjust = "left"
          )
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
xVyScatterplotApp <- function(debug = TRUE, hide_x = FALSE) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        xVyScatterplotInput("countData", x_label = "X Var", y_label = "Y Var",
                            hide_x = hide_x),
        checkboxInput("change_default", "Change default gene")
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

    gene <- reactive({
      req(input$change_default)
      if (input$change_default) {
        return("y1")
      } else {
        return(NULL)
      }
    })
    xVyScatterplotServer("countData", data = reactive({test_data}),
                         xSelected = gene, debug = debug)
  }
  shinyApp(ui, server)
}

