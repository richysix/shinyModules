library(shiny)

#' A UI module for selecting variables (pull-down)
#'
#' \code{select_vars_pulldown_ui} Produces a UI component for selecting variables
#' from a list. The default option is always the first item in the list
#'
#' @param id         id for the UI element. Must be matched when calling
#' the server function from callModule. [character]
#' @param label      label for the UI control. [character]
#' @param choices    vector of choices for the pulldown list
#'
#' @return a selectInput UI element
#'
#' @examples
#' ui <- fluidPage(
#'   fluidRow(
#'       wellPanel(select_vars_pulldown_ui("xvar", label = "X Variable",
#'           choices = c('class', 'cyl', 'cty'))
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
#' \code{select_vars_pulldown_server} returns a list of selections
#' made in the select_vars UI module. This function should
#' be used with callModule.
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{selected}{reactive character indicating choice selected}
#' }
#'
#' server <- function(input, output, session) {
#'   xvar <- callModule(select_vars_pulldown_server, "xvar")
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
