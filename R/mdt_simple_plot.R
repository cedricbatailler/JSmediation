#' Plot method for simple_mediation object
#'
#' @param x A mediation model created with \code{mdt_simple}. 
#' @param coef_rounding 
#' @param coef_show_stars 
#' @param figure_options
#' @param ...
#'
#' @return
#' @export

plot.simple_mediation <- function(
  x,
  coef_rounding = 2,
  coef_show_stars = TRUE,
  figure_options = mediaiton_figure_options(),
  ...
){

  rlang::check_installed("DiagrammeR")

  mediation_figure <-
    tibble::tibble(
      lab_x   = purrr::chuck(x, "params", "IV"),
      lab_m   = purrr::chuck(x, "params", "M"),
      lab_y   = purrr::chuck(x, "params", "DV"),
      path_a_coef      = purrr::chuck(x, "paths", "a", "point_estimate"),
      path_b_coef      = purrr::chuck(x, "paths", "b", "point_estimate"),
      path_cprime_coef = purrr::chuck(x, "paths", "c'", "point_estimate")
    ) %>% 
    dplyr::mutate(dplyr::across(dplyr::ends_with("coef"),
                                ~round(., coef_rounding))) 
  
  if (coef_show_stars) {
    mediation_figure <-
      mediation_figure %>% 
      dplyr::mutate(
        path_a_sig      = x %>% get_APA_for("a")  %>% pvalue_from_APA(),  
        path_b_sig      = x %>% get_APA_for("b")  %>% pvalue_from_APA(),
        path_cprime_sig = x %>% get_APA_for("c'") %>% pvalue_from_APA()
      ) %>% 
      dplyr::mutate(dplyr::across(dplyr::ends_with("sig"), pvalue_to_stars)) %>% 
      dplyr::mutate(
        path_a_coef      = glue::glue("{path_a_coef}{path_a_sig}"),
        path_b_coef      = glue::glue("{path_b_coef}{path_b_sig}"),
        path_cprime_coef = glue::glue("{path_cprime_coef}{path_cprime_sig}")
      )
  }
  
  mediation_figure$height  <- purrr::chuck(figure_options, "height")
  mediation_figure$width   <- purrr::chuck(figure_options, "width")
  mediation_figure$color   <- purrr::chuck(figure_options, "color")
  mediation_figure$ranksep <- purrr::chuck(figure_options, "ranksep")
  mediation_figure$minlen  <- purrr::chuck(figure_options, "minlen")
  
  mediation_figure$node_text_size  <- purrr::chuck(figure_options, "node_text_size")
  mediation_figure$edge_text_size  <- purrr::chuck(figure_options, "edge_text_size")
  
  mediation_figure$graph_label <- ifelse(is.na(purrr::chuck(figure_options, "graph_label")), "", paste0("label = '", purrr::chuck(figure_options, "graph_label"), "'"))
  
  
  diagram_out <- glue::glue_data(mediation_figure,
                                 "digraph flowchart {
      fontname = Helvetica
      <<graph_label>>
      graph [ranksep = <<ranksep>>]

      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fixedsize = TRUE, width = <<width>>, height = <<height>>, fontsize = <<node_text_size>>, color = <<color>>]        
        mm [label = '<<lab_m>>']
        xx [label = '<<lab_x>>']
        yy [label = '<<lab_y>>']

      # edge definitions with the node IDs
      edge [minlen = <<minlen>>, fontname = Helvetica, fontsize = <<edge_text_size>>, color = <<color>>]
        mm -> yy [label = '<<path_a_coef>>'];
        xx -> mm [label = '<<path_b_coef>>'];
        xx -> yy [label = '<<path_cprime_coef>>'];
      
      { rank = same; mm }
      { rank = same; xx; yy }
      
      }
      ", .open = "<<", .close = ">>")  
  
  DiagrammeR::grViz(diagram_out)  
}

get_APA_for <- function(mediation_model, path) {
  purrr::chuck(mediation_model, "paths", path, "APA")
}

pvalue_to_stars<- function(p_value) {
  stats::symnum(p_value,
                corr = FALSE, na = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                symbols = c("***", "**", "*", ".", " ")) 
}