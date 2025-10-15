library(shiny)
library(forcats)

#' A UI module for choosing how variables are ordered
#'
#' \code{order_variable_rb_ui} Produces a UI component for selecting how
#' to order a variable
#'
#' @param id                id for the UI element. Must be matched when calling
#' the server function from callModule. [character]
#' @param label             label for the UI control. [character]
#' @param choice_names      vector of names for the radio buttons
#' @param choices           vector of choices for the radio buttons
#'
#' @return a RadioButtons UI element
#'
#' @examples
#' ui <- fluidPage(
#'   fluidRow(
#'       wellPanel(order_variable_rb_ui("order_x", label = "Order X Variable",
#'           choice_names = c('Alphabetical', 'Numeric', 'Order of Appearance', 'By Frequency'),
#'           choices= c('alpha', 'numeric', 'appearance', 'by_freq')))
#'   )
#' )
#'
#' @export
order_variable_rb_ui <-
  function(id, label = "X variable",
          choice_names = c('Alphabetical', 'Numeric', 'Order of Appearance', 'By Frequency'),
          choices= c('alpha', 'numeric', 'appearance', 'by_freq')) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  radioButtons(ns('order_by'), label, selected = choices[1],
               inline = FALSE, width = NULL,
               choiceNames = choice_names,
               choiceValues = choices)
}

#' A server function to get the value selected
#'
#' \code{order_variable_rb_server} returns a list of selections
#' made in the heatmap control UI module. This function should
#' be used with callModule.
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{order_by}{reactive character indicating selection}
#' }
#'
#' server <- function(input, output, session) {
#'   x_ordering <- callModule(order_variable_rb_server, "order_x")
#' }
#'
#' @export
order_variable_rb_server <- function(input, output, session) {
  return(
    list(
      order_by = reactive({ input$order_by })
    )
  )
}

#' A server function to order a factor
#'
#' \code{order_variable_server} returns a list of selections
#' made in the heatmap control UI module. This function should
#' be used with callModule.
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param data reactive data source
#' @param xvar character
#' @param ordering reactive list containing order_by
#'
#' @return The data with the reordered factor
#'
#' server <- function(input, output, session) {
#'   x_ordering <- callModule(order_variable_server, "order_x")
#' }
#'
#' @export
order_variable_server <- function(input, output, session, fct_to_order, ordering) {
  reordered_factor <- reactive({
    order_by <- ordering$order_by()
    cat('obs_ordering: ', order_by, '\n')
    cat('obs_ordering: ', levels(fct_to_order), '\n')
    if (order_by == 'alpha') {
      fct_to_order <- factor(fct_to_order)
    } else if (order_by == 'numeric') {
      fct_to_order <- fct_inseq(fct_to_order)
    } else if (order_by == 'appearance') {
      fct_to_order <- fct_inorder(fct_to_order)
    } else if (order_by == 'by_freq') {
      fct_to_order <- fct_infreq(fct_to_order)
    }
    cat('obs_ordering: ', levels(fct_to_order), '\n')
  })
  return(reordered_factor)
}
