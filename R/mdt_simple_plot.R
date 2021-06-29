#' Plot method for simple_mediation object
#'
#' @param mediation_model 
#' @param coef_rounding 
#' @param coef_show_stars 
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

plot.simple_mediation <- function(mediation_model,
                                  coef_rounding = 2,
                                  coef_show_stars = TRUE,
                                  height = .75, 
                                  width = 2, 
                                  graph_label = NA, 
                                  node_text_size = 12, 
                                  edge_text_size = 12, 
                                  color = "black",
                                  ranksep = .2, 
                                  minlen = 3){
  
  rlang::check_installed("DiagrammeR")

  mediation_figure <-   
    tibble::tibble(
      lab_x   = purrr::chuck(mediation_model, "params", "IV"),
      lab_m   = purrr::chuck(mediation_model, "params", "M"),
      lab_y   = purrr::chuck(mediation_model, "params", "DV"),
      path_a_coef      = purrr::chuck(mediation_model, "paths", "a", "point_estimate"),
      path_b_coef      = purrr::chuck(mediation_model, "paths", "b", "point_estimate"),
      path_cprime_coef = purrr::chuck(mediation_model, "paths", "c'", "point_estimate")
    ) %>% 
    dplyr::mutate(dplyr::across(ends_with("coef"),
                                ~round(., coef_rounding))) 
  
  if (coef_show_stars) {
    mediation_figure <-
      mediation_figure %>% 
      dplyr::mutate(
        path_a_sig      = mediation %>% get_APA_for("a")  %>% pvalue_from_APA(),  
        path_b_sig      = mediation %>% get_APA_for("b")  %>% pvalue_from_APA(),
        path_cprime_sig = mediation %>% get_APA_for("c'") %>% pvalue_from_APA()
      ) %>% 
      dplyr::mutate(across(ends_with("sig"), pvalue_to_stars)) %>% 
      dplyr::mutate(
        path_a_coef      = glue::glue("{path_a_coef}{path_a_sig}"),
        path_b_coef      = glue::glue("{path_b_coef}{path_b_sig}"),
        path_cprime_coef = glue::glue("{path_cprime_coef}{path_cprime_sig}")
      )
  }
  
  mediation_figure$height  <- height   # node height
  mediation_figure$width   <- width    # node width
  mediation_figure$color   <- color    # node + edge border color
  mediation_figure$ranksep <- ranksep  # separation btwn mediator row and x->y row
  mediation_figure$minlen  <- minlen   # minimum edge length
  
  mediation_figure$node_text_size  <- node_text_size
  mediation_figure$edge_text_size  <- edge_text_size
  
  mediation_figure$graph_label <- ifelse(is.na(graph_label), "", paste0("label = '", graph_label, "'"))
  
  
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