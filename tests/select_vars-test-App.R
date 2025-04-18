# test App for select variables modules
library("shiny")
library("ggplot2")
library("rprojroot")

root_path <- find_root(is_rstudio_project)
source(file.path(root_path, 'R', 'select_variables.R'))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      select_vars_pulldown_ui("xvar", label = "X Variable",
                              choices = c('class', 'cyl', 'cty')),
      select_vars_pulldown_ui("yvar", label = "Y Variable",
                              choices = c('cty', 'hwy'))
    ),
    mainPanel(
      plotOutput('scatterplot')
    )
  )
)

server <- function(input, output) {
  xvar <- callModule(select_vars_pulldown_server, "xvar")
  yvar <- callModule(select_vars_pulldown_server, "yvar")
  print(xvar)
  print(yvar)

  output$scatterplot <- renderPlot({
    xvar <- xvar$selected()
    yvar <- yvar$selected()
    print(xvar)

    x <- rlang::sym(xvar)
    print(x)
    y <- rlang::sym(yvar)
    print(y)
    p <- ggplot(data = mpg) +
         geom_point(aes(x = !!x, y = !!y))
    return(p)
  })
}

shinyApp(ui, server)
