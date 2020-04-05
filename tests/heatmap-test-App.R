library('shiny')
# Test App for heatmap modules

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      heatmap_controls_ui("expr_heatmap_controls", label = "Expression Heatmap")
    ),
    mainPanel(
      heatmap_ui("expr_heatmap")
    )
  )
)

server <- function(input, output) {
  heatmap_selections <- callModule(heatmap_controls_server, "expr_heatmap_controls")
  data <- reactive({
    matrix(data = sample(1:10, 60, replace = TRUE),
         nrow = 10,
         dimnames = list(paste0('gene', 1:10),
                         paste0('sample', 1:6)))
  })
  heatmap_data <- callModule(heatmap_server, "expr_heatmap", data, heatmap_selections)
}

shinyApp(ui, server)
