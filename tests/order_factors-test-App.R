# test App for select variables modules
library(shiny)
library(ggplot2)
library(rprojroot)

root_path <- find_root(is_rstudio_project)
source(file.path(root_path, 'R', 'order_factors.R'))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      order_variable_rb_ui("order_numeric", label = "Order Numeric Variable"),
      order_variable_rb_ui("order_alpha", label = "Order Alpha Variable"),
      order_variable_rb_ui("order_mixed", label = "Order Mixed Variable"),
      order_variable_rb_ui("order_freq", label = "Order Freq Variable")
    ),
    mainPanel(
      tableOutput('dataframe'),
      textOutput('numeric_ordered'),
      textOutput('alpha_ordered'),
      textOutput('mixed_ordered'),
      textOutput('freq_ordered')
    )
  )
)

server <- function(input, output) {
  numeric_ordering <- callModule(order_variable_rb_server, "order_numeric")
  alpha_ordering <- callModule(order_variable_rb_server, "order_alpha")
  mixed_ordering <- callModule(order_variable_rb_server, "order_mixed")
  freq_ordering <- callModule(order_variable_rb_server, "order_freq")

  set.seed(433)
  in_data <- data.frame(
      numeric = factor(sample(1:10)),
      alpha = sample(letters[1:10]),
      mixed = c(sample(letters[1:5]), sample(1:3), c(10,20)),
      freq = sample(1:3, 10, replace = TRUE)
    )
  #in_data$alpha <- callModule(order_variable_server, "numeric_ordered", in_data$alpha, alpha_ordering)
  data <- reactive({
    alpha <- callModule(order_variable_server, "numeric_ordered", in_data$alpha, alpha_ordering)
    print(alpha)
    cat('alpha: ', alpha(), '\n')
    in_data$alpha <- alpha()
    cat('reactive_data: ', levels(in_data$alpha), '\n')
    #   alpha = callModule(order_variable_server, "numeric_ordered", in_data()$alpha, alpha_ordering),
    #   mixed = callModule(order_variable_server, "numeric_ordered", in_data()$mixed, mixed_ordering),
    #   freq = callModule(order_variable_server, "numeric_ordered", in_data()$freq, freq_ordering)
    # )
    return(in_data)
  })

  output$dataframe <- renderTable(data())
  # output$numeric_ordered <- renderText({
  #   return(levels(in_data[['numeric']]))
  # })
  output$alpha_ordered <- renderText({
    return(levels(data()[['alpha']]))
  })
  # output$mixed_ordered <- renderText({
  #   return(levels(data()[['mixed']]))
  # })
  # output$freq_ordered <- renderText({
  #   return(levels(data()[['freq']]))
  # })
}

shinyApp(ui, server)
