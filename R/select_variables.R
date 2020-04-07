library(shiny)

#' A UI module for selecting variables (pull-down)
#'
#' \code{select_vars_pulldown_ui} Produces a UI component for selecting variables
#' from a list. The default option is always the first item in the list
#'
#' @param id                      id for the UI element. Must be matched when calling
#' the server function from callModule. [character]
#' @param label                   label for the UI control. [character]
#' @param choice_names      vector of names for the Transform radio buttons
#' @param choices           vector of choices for the Transform radio buttons
#'
#' @return a tagList of UI elements
#'
#' @examples
#' ui <- fluidPage(
#'   fluidRow(
#'       wellPanel(select_vars_pulldown_ui("expr_heatmap_controls", label = "Expression Heatmap"))
#'   )
#' )
#'
#' @export
select_vars_pulldown_ui <- function(id, label = "X variable",
                                choices = NULL) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  req(choices)

  selectInput(ns('selected'), label, choices, selected = NULL, multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL)
}

#' A server function to get the value selected
#'
#' \code{select_vars_server} returns a list of selections
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
#'   heatmap_selections <- callModule(select_vars_server, "expr_heatmap_controls")
#' }
#'
#' @export
select_vars_pulldown_server <- function(input, output, session) {
  return(
    list(
      selected = reactive({ input$selected })
    )
  )
}
