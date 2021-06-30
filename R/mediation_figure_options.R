#' Options for mediation figure display
#'
#' @description 
#' Construct option list for mediation figure display. All arguments have
#' defaults.
#' 
#' @param height 
#' @param width 
#' @param graph_label 
#' @param node_text_size 
#' @param edge_text_size 
#' @param color 
#' @param ranksep 
#' @param minlen 
#'
#' @return
#' @export

mediaiton_figure_options <- function(height = .75,
                                     width = 2,
                                     graph_label = NA, 
                                     node_text_size = 12,
                                     edge_text_size = 12,
                                     color = "black",
                                     ranksep = .2,
                                     minlen = 3) {

  figure_options <- tibble::lst(
    height,
    width,
    graph_label,
    node_text_size,
    edge_text_size,
    color,
    ranksep,
    minlen
  )

  class(figure_options) <- "mediation_figure_options"

  figure_options

}

mediaiton_figure_options()
