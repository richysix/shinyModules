% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\name{heatmap_controls_server}
\alias{heatmap_controls_server}
\title{A heatmap controls server function}
\usage{
heatmap_controls_server(input, output, session)
}
\arguments{
\item{input, output, session}{standard \code{shiny} boilerplate}
}
\value{
list with following components
\describe{
  \item{transform_choice}{reactive character indicating transform selection}
  \item{cluster_choices}{reactive list indicating cluster checkbox group selections}
}

server <- function(input, output, session) {
  heatmap_selections <- callModule(heatmap_controls_server, "expr_heatmap_controls")
}
}
\description{
\code{heatmap_controls_server} returns a list of selections
made in the heatmap control UI module. This function should
be used with callModule.
}
