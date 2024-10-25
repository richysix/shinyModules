#' Create UI components to select two genes to plot against each other
#'
#' `countXYScatterPlotInput()` produces the select inputs for selecting the
#' variables to plot against each other
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [countXYScatterPlotServer()] function.
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
#' countXYScatterPlotInput("countData")
#'
countXYScatterPlotInput <-
  function(id, x_label = "Gene1 X:", y_label = "Gene2 Y:") {
    xVyScatterplotInput(id = NS(id, "XYScatter"), x_label = x_label, y_label = y_label)
}

#' Create Output to display the Scatterplot
#'
#' `countXYScatterPlotOutput()` produces a PlotOutput element for the scatterplot
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [countXYScatterPlotServer()] function.
#'
#' @returns a [htmltools::tagList()] containing three [shiny::plotOutput()]s
#'
#' @export
#'
#' @examples
#'
#' countXYScatterPlotOutput("countData")
#'
countXYScatterPlotOutput <- function(id) {
  xVyScatterplotOutput(id = NS(id, "XYScatter"))
}

#' Server function to produce an xy scatter plot
#'
#' `countXYScatterPlotServer()` takes a data frame of count data and produces
#' a scatterplot based on the genes selected
#'
#' @param id namespace id for the UI components. Must match the id provided to the
#' [countXYScatterPlotInput()] function.
#' @param counts a reactive counts object. Should contain only numeric columns
#' @param sample_info a reactive object. Represents the samples and associated
#' metadata
#' @param gene_metadata a reactive object. Contains the metadata for the genes
#' present in the counts object.
#'
#' @returns a list containing two [shiny::reactive()] objects
#' * sampleInfo a data.frame of sample metadata
#' * counts a data.frame of RNAseq count data
#'
#' @export
#'
#' @examples
#'
#' countXYScatterPlotServer("rnaseqData")
#'
countXYScatterPlotServer <-
  function(id, counts = NULL, sample_info = NULL, gene_metadata = NULL,
           gene1 = NULL, debug = FALSE) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(sample_info))
  stopifnot(is.reactive(gene_metadata))
  stopifnot(is.reactive(gene1))

  moduleServer(id, function(input, output, session) {
    counts_long <- reactive({
      counts <- req(counts())
      metadata <- req(gene_metadata())
      if (debug) print(head(counts))
      if (debug) print(head(metadata))

      counts_long <- dplyr::select(metadata, GeneID) |>
        dplyr::bind_cols(counts) |>
        tidyr::pivot_longer(-GeneID, names_to = "sample") |>
        dplyr::rename(variable = GeneID)
      if (debug) print(head(counts_long))
      return(counts_long)
    })

    plot_data_list <- xVyScatterplotServer("XYScatter", data = counts_long,
                                           xSelected = gene1)
    return(plot_data_list)
  })
}

#' A test shiny app for the countXYScatterPlot module
#'
#' `countXYScatterPlotApp()` creates a small test app for testing the [countXYScatterPlotInput()] and
#' [countXYScatterPlotServer()] functions. A subset of the returned sample and count data.frames
#' are displayed in two [shiny::tableOutput()]s
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' countXYScatterPlotApp()
countXYScatterPlotApp <- function(debug = TRUE) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        countXYScatterPlotInput("countData", x_label = "Gene 1", y_label = "Gene 2"),
        checkboxInput("change_default", "Change default gene")
      ),
      mainPanel(
        h3("Scatterplot:"),
        countXYScatterPlotOutput("countData"),
      )
    )
  )
  server <- function(input, output, session) {
    set.seed(56)
    metadata = tibble::tibble(
      GeneID = paste0("ENSTEST", 1:100),
      Name = paste0("name-", 1:100),
    )
    test_data <- purrr::map(1:24, function(x){
      col_name <- paste0("sample-", x)
      tibble::tibble(!!col_name := sample(10:30, 100, replace = TRUE))
    }) |> purrr::list_cbind() |>
      cbind(metadata) |> tibble::as_tibble()
    samples <- tibble::tibble(
      sample = paste0("sample-", 1:24),
      condition = rep(c("wt", "hom"), each = 12)
    )

    gene <- reactive({
      req(input$change_default)
      if (input$change_default) {
        return("ENSTEST99")
      } else {
        return(NULL)
      }
    })
    countXYScatterPlotServer(
      "countData",
      counts = reactive({
        dplyr::select(test_data, dplyr::starts_with("sample"))
      }),
      sample_info = reactive({ samples }),
      gene_metadata = reactive({ metadata }),
      gene1 = gene,
      debug = debug
    )
  }
  shinyApp(ui, server)
}

