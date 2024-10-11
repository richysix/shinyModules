library(shiny)

#' A UI module for selecting variables (pull-down)
#'
#' \code{SelectVarPulldownUI} Produces a UI component for selecting variables
#' from a list. The default option is always the first item in the list
#'
#' @param id         id for the UI element. Must be matched when calling
#' the server function. [character]
#' @param label      label for the UI control. [character]
#' @param data       data.frame to take the variables list from
#'
#' @return a varSelectizeInput UI element
#'
#' @examples
#' ui <- fluidPage(
#'   fluidRow(
#'       wellPanel(SelectVarPulldownUI("xvar", label = "X Variable",
#'           choices = c('class', 'cyl', 'cty'))
#'     )
#'   )
#' )
#'
#' @export
SelectVarPulldownUI <- function(id, label = "X variable", data = NULL) {
  varSelectizeInput(
    inputId = NS(id, "var_select"),
    label = label,
    data = data,
    selected = NULL,
    multiple = FALSE,
    width = NULL,
    size = NULL
  )
}

#' A server function to get the value selected
#'
#' \code{SelectVarPulldownServer} returns a list of selections
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
#' selected_x <- SelectVarPulldownServer("xvar")
#'
#' @export
#'
SelectVarPulldownServer <- function(id, data = NULL, debug = FALSE) {
  stopifnot(is.reactive(data))

  moduleServer(id, function(input, output, session) {
    observe({
      df = req(data())
      if (debug) {
        print("Observer running...")
        print(head(df))
      }
      updateVarSelectizeInput(session, inputId = "var_select", data = df,
                              selected = "", server = TRUE)
    }) |> bindEvent(data())

    return(
      list(
        selected = reactive({ input$var_select })
      )
    )
  })
}

#' A test shiny app for the select_vars module
#'
#' `SelectVarPulldownApp()` creates a small test app for testing the
#' [SelectVarPulldownUI()] and [SelectVarPulldownServer()] functions.
#'
#' @return a [shiny::shinyApp()] object
#'
#' @examples
#' SelectVarPulldownApp()
#'
SelectVarPulldownApp <- function(debug = FALSE) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        SelectVarPulldownUI("xvar", label = "Choice 1"),
        SelectVarPulldownUI("yvar", label = "Choice 2"),
      ),
      mainPanel(
        h3("Selected variables:"),
        textOutput("xvar"),
        textOutput("yvar"),
        plotOutput("xy")
      )
    )
  )
  server <- function(input, output, session) {
    selected_x <- SelectVarPulldownServer(
      id = "xvar",
      data = reactive({ ggplot2::mpg }),
      debug = debug
    )
    output$xvar <- renderText(selected_x$selected())
    selected_y <- SelectVarPulldownServer(
      id = "yvar",
      data = reactive({ ggplot2::mpg }),
      debug = debug
    )
    output$yvar <- renderText(selected_y$selected())

    output$xy <- renderPlot({
      req(selected_x$selected())
      req(selected_y$selected())
      ggplot2::ggplot(data = ggplot2::mpg) +
        ggplot2::geom_point(ggplot2::aes(x = !!selected_x$selected(),
                                         y = !!selected_y$selected()))
    })
  }
  shinyApp(ui, server)
}
