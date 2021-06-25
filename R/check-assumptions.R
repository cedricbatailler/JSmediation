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
#' @return Invisibly return an object of class \code{mediation_model}.
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
check_assumptions <- function(mediation_model, 
                              tests = c("normality", "heteroscedasticity")) {
  UseMethod("check_assumptions")
}

#' @export
check_assumptions.mediation_model <- 
  function(mediation_model, 
           tests = c("normality", "heteroscedasticity")) {
    
    rlang::check_installed("performance")
    
    supported_tests <- c("normality", "heteroscedasticity", "outliers")
    
    if(length(tests) < 1L) {
      rlang::abort(c("`tests` argument must contains at least one element."))
    }
    
    if(! is.character(tests)) {
      rlang::abort(c("`tests` argument must be a character vector."))
    }
    
    if(sum(! tests %in% supported_tests) >= 1) {
      rlang::warn(message = 
                    c("`tests` argument in `check_assumptions` contains unsupported checks.",
                      i  = "Supported checks are:",
                      "- normality",
                      "- heteroscedasticity",
                      "- outliers")
      )
    }

    purrr::pluck(mediation_model, "js_models") %>% 
      purrr::imap(~ check_model(.y, .x, tests))
    
    invisible(mediation_model)
  }

# Checks assumptions in a model
# Args:
#   model_name: a carachter to be printed
#   model: a lm object
#   tests: a character vector with the test to run

check_model <- function(model_name, model, tests) {
  cat(model_name, sep = "\n")
  
  if("normality" %in% tests)          { performance::check_normality(model) }
  if("heteroscedasticity" %in% tests) { performance::check_heteroscedasticity(model) }
  if("outliers" %in% tests)           { performance::check_outliers(model) }
  
}