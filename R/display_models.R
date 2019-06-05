#' @title Displays models from a mediation object
#'
#' @description When conducting a joint-significance test, different models are
#' fitted to the data. This function helps you see a summary of the models that
#' have been used in an object of class \code{mediation_model}.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#'
#' @return A list of \code{summary.lm} objects.
#'
#' @examples
#' data(ho_et_al)
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' my_model <- 
#'   mdt_simple(data = ho_et_al,
#'              IV = condition_c,
#'              DV = hypodescent,
#'              M = linkedfate)
#'            
#' display_models(my_model)
#'
#' @export

display_models <- function(mediation_model) {
  UseMethod("display_models")
}

#' @export

display_models.mediation_model <- function(mediation_model) {

  purrr::pluck(mediation_model, "js_models") %>%
    purrr::map(~summary(.x))

}
