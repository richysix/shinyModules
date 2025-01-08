#' Create UI components to display a count plot for a selected gene
#'
#' `countPlotInput()` produces a [shiny::selectInput()] control for choosing a gene
#' to display as a count plot.
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [countPlotInput()] and [countPlotOutput()] functions.
#'
#' @returns a [htmltools::tagList()] containing a [shiny::selectInput()] object
#'
#' @export
#'
#' @examples
#'
#' countPlotOutput("rnaseqData")
#'
countPlotInput <- function(id) {
  tagList(
    selectInput(NS(id, "count_plot_gene_select"),
                label = "Select a gene to display",
                choices = NULL),
    # X variable
    selectInput(
      NS(id, "x_var"),
      label = h6("X-axis variable"),
      choices = c("None")
    ),
    # Fill Input
    selectInput(
      NS(id, "fill_var"),
      label = h6("Colour variable"),
      choices = c("None")
    ),
    # Shape Input
    selectInput(
      NS(id, "shape_var"),
      label = h6("Shape variable"),
      choices = c("None")
    ),
  )
}

#' Create UI components to display a count plot for a selected gene
#'
#' `countPlotOutput()` produces a plotOutput space for a plot
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [countPlotServer()] function.
#'
#' @returns a [htmltools::tagList()] containing a [shiny::plotOutput] object
#'
#' @export
#'
#' @examples
#'
#' countPlotOutput("rnaseqData")
#'
countPlotOutput <- function(id) {
  tagList(
    uiOutput(NS(id, "shapeLevelsWarning")),
    plotOutput(NS(id, "count_plot"))
  )
}

#' Server function to create a count plot for a given gene
#'
#' `countPlotServer()` creates a count plot for a given gene from the counts and
#' sample information supplied. The plot is a [ggplot2] plot object made using
#' [biovisr::scatterplot_with_fill_and_shape()]. The plot is rendered to the
#' count_plot plotOutput from the `countPlotOutput()` function.
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [countPlotInput()] function.
#' @param counts a reactive counts object. Should contain only numeric columns
#' @param sample_info a reactive object. Represents the samples and associated
#' metadata
#' @param gene_metadata a reactive object. Contains the metadata for the genes
#' present in the counts object.
#' @param debug Turn on debugging message statements
#'
#' @returns a [shiny::reactive()] object which is the count plot
#'
#' @export
#'
#' @examples
#'
#' countPlotServer("rnaseq", counts = reactive(rnaseqVis::counts[1:10, 1:20]),
#' sample_info = reactive(rnaseqVis::sampleInfo[1:20,]),
#' gene_metadata = reactive(rnaseqVis::gene_metadata[1:10,]))
#'
countPlotServer <- function(id, counts = NULL, sample_info = NULL,
                          gene_metadata = NULL, debug = FALSE) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(sample_info))
  stopifnot(is.reactive(gene_metadata))

  moduleServer(id, function(input, output, session) {

    # update select input with genes in current set
    observe({
      req(gene_metadata())
      gene_ids <- gene_metadata()$GeneID
      updateSelectizeInput(
        session,
        inputId = "count_plot_gene_select",
        choices = gene_ids,
        server = TRUE
      )
    }) |> bindEvent(gene_metadata())

    # update fill variable with categorical column
    observe({
      req(sample_info())
      cat_columns <-
        sapply(colnames(sample_info()),
               function(x){
                 class(sample_info()[[x]]) == "character" |
                   class(sample_info()[[x]]) == "factor"
      })
      categorical_columns <- colnames(sample_info())[ cat_columns ]
      if (debug) {
        message("Update x, fill and shape variable controls")
        print(sample_info())
        message(paste("Categorical columns are", paste0(categorical_columns, collapse = ", ")))
      }
      if ("condition" %in% categorical_columns) {
        selected <- "condition"
      } else {
        selected <- NULL
      }
      updateSelectInput(session, inputId = "x_var",
                        choices = categorical_columns, selected = selected)
      updateSelectInput(session, inputId = "fill_var",
                        choices = categorical_columns, selected = selected)

      # for shape only use columns with fewer than 6 levels
      fct_nlevels <- sapply(categorical_columns, function(col){ nlevels(factor(sample_info()[[col]])) })
      if (any(fct_nlevels > 6)) {
        non_shape_columns <- names(fct_nlevels)[ fct_nlevels > 6 ]
        # generate alert
        output$shapeLevelsWarning <- renderUI(
          shinyWidgets::alert(
            tags$h4("Too many shape levels"),
            "Some of the shape variables have too many levels and ",
            "will not be available for selecting as shape.",
            tags$br(),
            tags$strong("Variables removed: "),
            paste(non_shape_columns, collapse = ", "),
            status = "warning",
            dismissible = TRUE
          )
        )
      }
      columns_to_use <- names(fct_nlevels)[ fct_nlevels <= 6 ]
      updateSelectInput(session, inputId = "shape_var",
                        choices = c("None", columns_to_use))
    }) |> bindEvent(sample_info())

    plot <- reactive({
      req(counts(), sample_info(), gene_metadata(),
          input$x_var, input$fill_var, input$shape_var)

      if (debug) {
        message("Input variables are:")
        message("X: ", input$x_var)
        message("Fill: ", input$fill_var)
        message("Shape: ", input$shape_var)
      }

      if (input$x_var != "None" & input$fill_var != "None") {
        # make x_var, fill_var and shape_var factors
        samples <- sample_info()
        if (input$shape_var == "None") {
          shape_variable <- NULL
          columns <- unique(c(input$x_var, input$fill_var))
        } else {
          shape_variable <- input$shape_var
          columns <- unique(c(input$x_var, input$fill_var, shape_variable))
        }
        if (debug) {
          message("Categorical columns: ", paste(columns, collapse = ", "))
          message("Sample data: ")
          print(samples)
        }
        for (col in columns) {
          samples[[col]] <- factor(samples[[col]],
                                   levels = unique(samples[[col]]))
        }
        gene_idx <- which(gene_metadata()$GeneID == input$count_plot_gene_select)
        if (debug) {
          message("Gene index: ", gene_idx)
        }
        counts_vec <- counts()[gene_idx,]
        counts_for_gene <- tibble::tibble(
          sample = colnames(counts_vec),
          count = as.numeric(counts_vec)
        ) |> dplyr::inner_join(samples, by = c("sample"))

        fill_colours <- create_fill_palette(samples[[input$fill_var]])
        if (!is.null(shape_variable)) {
          shapes <- create_shape_palette(samples[[shape_variable]])
        } else {
          shapes <- c(21:24)
        }

        if (debug) {
          message("counts for gene:")
          print(counts_for_gene)
          message("Colours: ", paste0(fill_colours, collapse = ", "))
          message("Shapes: ", paste0(shapes, collapse = ", "))
        }
        create_count_plot(counts_for_gene, x_var = input$x_var,
                          colour_var = input$fill_var, colour_palette = fill_colours,
                          shape_var = shape_variable, shape_palette = shapes)
      }
    })

    output$count_plot <- renderPlot(plot())

    return(list(gene1 = reactive({ input$count_plot_gene_select }) ))
  })
}

#' Make scatterplot of normalised counts
#'
#' @param count_data [tibble::tibble()] of count data joined to sample info
#' @param x_var character column name to use as the X variable
#' @param colour_var character column name to use as the colour variable
#' @param colour_palette character named vector of colours
#' @param shape_var character column name to use as the shape variable.
#' It can be NULL in which case the points are all circles
#' @param shape_palette numeric vector of numbers for the shapes
#' @param debug logical print debugging info
#'
#' @return a [ggplot2::ggplot()] object
#' @export
#'
#' @examples
#'
#' create_count_plot(counts_for_gene, x_var = 'condition', colour_var = 'treatment',
#' colour_palette = c('yellow', 'blue', 'red'), shape_var = 'timepoint')
#'
create_count_plot <- function(count_data, x_var, colour_var, colour_palette,
                              shape_var = NULL, shape_palette = c(21:24),
                              debug = FALSE) {
  # create basic plot
  if (debug) {
    message("Basic plot")
    message("Count data: ")
    print(count_data)
    message("X variable: ", x_var)
    message("Colour variable: ", colour_var)
    message("Colour palette: ", colour_palette)
    message("Shape variable: ", shape_var)
    message("Shape palette: ", shape_palette)
  }
  plot <- ggplot2::ggplot(data = count_data, ggplot2::aes(x = .data[[x_var]], y = count))
  print(plot)

  pos <- ggplot2::position_jitter(width = 0.3, height = 0)
  # add points with the correct shapes
  if (debug) { message("Add points") }
  if (is.null(shape_var)) {
    if (debug) {
      message("No shape var")
      message("Colour variable: ", colour_var)
      message("Colour palette: ", paste0(colour_palette, collapse = ", "))
    }
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(fill = .data[[colour_var]]),
                 size = 3, shape = 21, colour = 'black',
                 position = pos ) +
      ggplot2::scale_fill_manual(values = colour_palette)
  } else {
    if (debug) { message("Shape palette is filled") }
    if (shape_palette[1] == 16) {
      plot <- plot +
        ggplot2::geom_point(ggplot2::aes(colour = .data[[colour_var]],
                       shape = .data[[shape_var]]), size = 3,
                   position = pos) +
        ggplot2::scale_colour_manual(values = colour_palette,
                            guide = ggplot2::guide_legend(override.aes =
                                                   list(shape = shape_palette[1]),
                                                 order = 1)) +
        ggplot2::scale_shape_manual(values = shape_palette,
                           guide = ggplot2::guide_legend(order = 2))
    } else {
      if (debug) { message("Shape palette is open") }
      plot <- plot +
        ggplot2::geom_point(ggplot2::aes(fill = .data[[colour_var]],
                       shape = .data[[shape_var]]), size = 3,
                   position = pos ) +
        ggplot2::scale_fill_manual(values = colour_palette,
                          guide = ggplot2::guide_legend(override.aes =
                                                 list(shape = shape_palette[1]),
                                               order = 1)) +
        ggplot2::scale_shape_manual(values = shape_palette,
                           guide = ggplot2::guide_legend(order = 2))
    }
  }

  if (debug) { message("Add axis title") }
  plot +
    ggplot2::labs(y = "Normalised Counts") +
    ggplot2::theme_minimal()
}

#' Make a vector of colours based on a character vector
#'
#' @param fill_vec factor the column to based the colour palette on
#'
#' @return character named vector of colours for each level of the input vector
#' @export
#'
#' @examples
#'
#' create_fill_palette(counts_for_gene$treatment)
#'
create_fill_palette <- function(fill_vec) {
  # check number of levels
  num_colours <- nlevels(fill_vec)
  if (num_colours <= 10) {
    return(biovisr::cbf_palette(fill_vec, named = TRUE))
  } else {
    ord1 <- seq(1,num_colours,2)
    ord2 <- seq(2,num_colours,2)
    colour_palette <- scales::hue_pal()(num_colours)[ order(c(ord1,ord2)) ]
    names(colour_palette) <- levels(fill_vec)
    return(colour_palette)
  }
}

#' Make a vector of numbers representing shapes based on a character vector
#'
#' @param shape_vec factor the column to based the shape palette on
#'
#' @return integer vector of numbers that represent plotting shapes
#' see \link{https://ggplot2.tidyverse.org/reference/scale_shape.html}
#' @export
#'
#' @examples
#'
#' create_shape_palette(counts_for_gene$timepoint)
#'
#' create_shape_palette(factor(letters[1:6]))
#'
create_shape_palette <- function(shape_vec) {
  num_shapes <- nlevels(shape_vec)
  if (num_shapes > 6) {
    rlang::abort(message = "The supplied factor has too many levels",
                 class = "Too many shape levels!")
  } else if (num_shapes <= 4) {
    shapes <- c(21:24)
  } else {
    shapes <- c(16,15,17,18,4,8)
  }
  return(shapes[seq_len(num_shapes)])
}

#' A test shiny app for the countPlot module
#'
#' `countPlotApp()` creates a small test app for testing the [countPlotOutput()] and
#' [countPlotServer()] functions. It uses a subset of the package datasets `counts`,
#' `sampleInfo` and `gene_metadata` and create a [ggplot2::ggplot()] countPlot object.
#' It also has transform radio buttons to test using that information to change
#' the colour palette.
#'
#' @return a [shiny::shinyApp()] object.
#'
#' @examples
#'
#' countPlotApp()
#'
countPlotApp <- function(debug = TRUE) {
  # library(shinyBS)
  ui <- fluidPage(
    theme = bslib::bs_theme(version = 5),
    sidebarLayout(
      sidebarPanel(
        countPlotInput('rnaseq'),
        checkboxInput("load_data", "Load RNA-seq data"),
        width = 4
      ),
      mainPanel(
        fluidRow(
          countPlotOutput('rnaseq'),
          h4("Selected Gene"),
          textOutput("selected_gene")
        ),
        width = 8
      )
    )
  )

  server <- function(input, output, session) {
    data_list <- reactive({
      req(input$load_data)
      return(list(
        counts = rnaseqVis::counts[1:10, 1:20],
        sample_info = rnaseqVis::sampleInfo[1:20,],
        gene_metadata = rnaseqVis::gene_metadata[1:10,]
      ))
    })
    gene_selected <- countPlotServer(
      "rnaseq",
      counts = reactive({ data_list()$counts }),
      sample_info = reactive({ data_list()$sample_info }),
      gene_metadata = reactive({ data_list()$gene_metadata }),
      debug = debug
    )

    output$selected_gene <- renderText(gene_selected$gene1())
  }
  shinyApp(ui, server)
}
