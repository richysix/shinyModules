library(shiny)
library(rnaseqVis)

#' A heatmap controls UI module
#'
#' \code{heatmap_controls_ui} Controls for a heatmap plot
#'
#' @param id                      id for the UI element. Must be matched when calling
#' the heatmap_controls_server function from callModule. [character]
#' @param label                   label for the UI control. [character]
#' @param transform_choice_names      vector of names for the Transform radio buttons
#' @param transform_choices           vector of choices for the Transform radio buttons
#' @param cluster_choice_names    vector of names for the Cluster checkbox group
#' @param cluster_choices         vector of choices for the Cluster checkbox group
#'
#' @return a tagList of UI elements
#'
#' @examples
#' ui <- fluidPage(
#'   fluidRow(
#'       wellPanel(heatmap_controls_ui("expr_heatmap_controls", label = "Expression Heatmap"))
#'   )
#' )
#'
#' @export
heatmap_controls_ui <- function(id, label = "Expression Heatmap",
                      transform_choice_names = c("Raw", "Mean Centred and Scaled", "log10", "Max Scaled"),
                      transform_choices = c("raw", "mcs", "log10", "max"),
                      cluster_choice_names = c("By Genes", "By Samples"),
                      cluster_choices = c("rows", "columns")) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    h4(label),
    radioButtons(
      ns("transform"),
      label = h4("Transformation"),
      choiceNames = transform_choice_names,
      choiceValues = transform_choices,
      selected = transform_choices[1]
    ),
    hr(),
    checkboxGroupInput(
      ns("clusterCheckGroup"),
      label = h4("Clustering"),
      choiceNames = cluster_choice_names,
      choiceValues = cluster_choices,
      selected = NULL
    ),
    hr(),
  )
}

#' A heatmap controls server function
#'
#' \code{heatmap_controls_server} returns a list of selections
#' made in the heatmap control UI module. This function should
#' be used with callModule.
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{transform_choice}{reactive character indicating transform selection}
#'   \item{cluster_choices}{reactive list indicating cluster checkbox group selections}
#' }
#'
#' server <- function(input, output, session) {
#'   heatmap_selections <- callModule(heatmap_controls_server, "expr_heatmap_controls")
#' }
#'
#' @export
heatmap_controls_server <- function(input, output, session) {
  return(
    list(
      transform_choice = reactive({ input$transform }),
      cluster_choices = reactive({ input$clusterCheckGroup })
    )
  )
}

#' A heatmap UI module
#'
#' \code{heatmap_ui} The output of a heatmap plot
#'
#' @param id                      id for the UI element. Must be matched when calling
#' the heatmap_server function from callModule. [character]
#'
#' @return a plotOutput object
#'
#' @examples
#' ui <- fluidPage(
#'   fluidRow(
#'       heatmap_ui("expr_heatmap"))
#'   )
#' )
#'
#' @export
heatmap_ui <- function(id) {
  ns <- NS(id)

  plotOutput(ns("heatmap_plot"))
}

#' A heatmap server module
#'
#' \code{heatmap_server} The server side code to produce a heatmap plot.
#' It produces the heatmap and returns the data used to create it.
#'
#' @param data                 The data used to create the heatmap
#' @param heatmap_selections   The selections made by the user in the controls UI section
#'
#' @return a matrix, transformed and clustered
#'
#' @examples
#'
#' server <- function(input, output, session) {
#'   heatmap_data <- callModule(heatmap_server, "expr_heatmap", heatmap_selections)
#' }

heatmap_server <- function(input, output, session, data, heatmap_selections) {
  trans_scaled_data <- reactive({
    plot_data <- data()
    if (heatmap_selections$transform_choice() == "mcs") {
      # scale operates on the column so need to transpose, scale and then transpose back
      plot_data <- t(scale(t(plot_data)))
    } else if (heatmap_selections$transform_choice() == "log10") {
      plot_data <- log10(plot_data + 1)
    } else if (heatmap_selections$transform_choice() == "max") {
      geneMaxCounts <- apply(plot_data, 1, max)
      # scale operates on the column so need to transpose, scale and then transpose back
      plot_data <-
        t(scale(t(plot_data), scale = geneMaxCounts, center = FALSE))
      # genes with a max of zero get converted to NAs
      # reset to zeros
      plot_data[ geneMaxCounts == 0, ] <- matrix( rep(0, sum(geneMaxCounts == 0)*ncol(plot_data) ), ncol = ncol(plot_data) )
    }
    # cluster
    data <-
      rnaseqVis::clusterMatrix(
        plot_data,
        byRow = any(heatmap_selections$cluster_choices() == "rows"),
        byCol = any(heatmap_selections$cluster_choices() == "columns")
      )
    return(data)
  })

  output$heatmap_plot <- renderPlot({
    rnaseqVis::expr_heatmap(trans_scaled_data())
  })

  return(trans_scaled_data)
}

