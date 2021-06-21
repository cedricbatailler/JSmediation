#' @title Test assumptions for models underlying the mediation 
#'
#' @description When conducting a joint-significant test, different models are
#'   fitted to the data. This function tests the normality and
#'   heteroscedasticity assumptions for these models using \code{performance}.
#'   \code{check_assumptions} uses \code{\link[performance]{check_normality}}
#'   and \code{\link[performance]{check_heteroscedasticity}} to test the
#'   assumptions. 
#'   
#'   Note that \code{check_assumptions} returns a
#'   \code{mediation_model} object.
#'
#'
#' @param mediation_model An object of class \code{mediation_model}.
#'
#' @return An object of class \code{mediation_model}.
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
#' check_assumptions(my_model)
#'
#' @export
check_assumptions <- function(mediation_model) {
  UseMethod("check_assumptions")
}

#' @export
check_assumptions.mediation_model <- function(mediation_model) {
  
  rlang::check_installed("performance")
  
  purrr::pluck(mediation_model, "js_models") %>% 
    purrr::map2(names(.), ~ check_model(.y, .x))
  
  mediation_model
}

# Checks assumptions in a model
# Args:
#   model_name: a carachter to be printed
#   model: a lm object

check_model <- function(model_name, model) {
  cat(model_name, sep = "\n")
  performance::check_normality(model)
  performance::check_heteroscedasticity(model)
}