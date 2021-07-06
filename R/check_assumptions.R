#' Test assumptions for models underlying the mediation
#'
#' @description When conducting a joint-significant test, different models are
#'   fitted to the data. This function tests assumptions regarding these models
#'   using the \code{performance} package.
#'
#'   The assumptions test are performed using
#'   \code{\link[performance]{check_normality}},
#'   \code{\link[performance]{check_heteroscedasticity}}, and
#'   \code{\link[performance]{check_outliers}}.
#'
#'   Note that \code{check_assumptions} returns a \code{mediation_model} object.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#' @param tests A character vector indicating which test to run. Supported test
#'   includes \code{"normality"}, \code{"heteroscedasticity"}, and
#'   \code{"outliers"}
#'
#' @return Invisibly returns an object of class \code{mediation_model}.
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
#' @family assumption checks
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

    if (length(tests) < 1L) {
      rlang::abort(c("`tests` argument must contains at least one element."))
    }

    if (! is.character(tests)) {
      rlang::abort(c("`tests` argument must be a character vector."))
    }

    if (sum(! tests %in% supported_tests) >= 1) {

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
  if ("normality" %in% tests) {
    performance::check_normality(model)
    }
  if ("heteroscedasticity" %in% tests) {
    performance::check_heteroscedasticity(model)
    }
  if ("outliers" %in% tests) {
    performance::check_outliers(model)
    }
}

#' Returns diagnostic plots for the linear model used in a mediation
#'
#' @description When conducting a joint-significant test, different models are
#'   fitted to the data. This function returns diagnostic plots for each of the
#'   model used in the mediation model. `check_assumptions_plot` uses the
#'   `performance` and `see` packages behind the scenes to provide the different
#'   plots.
#'
#'   This function is best used in an interactive context.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#'
#' @return Invisibly returns an object of class \code{mediation_model}.
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
#' check_assumptions_plot(my_model)
#'
#' @family assumption checks
#'
#' @export
check_assumptions_plot <- function(mediation_model) {
  UseMethod("check_assumptions_plot")
}

#' @export
check_assumptions_plot.mediation_model <- function(mediation_model) {
  purrr::pluck(mediation_model, "js_models") %>%
    purrr::iwalk(~ check_model_plot(model = .x, title = .y) %>%
                   print())
  
  invisible(mediation_model)
}

#' Returns an assumption plot built with performance::check_model
#'
#' @param model A linear model built with `lm`.
#' @param title A title to be printed on top of the assumption plot.
#'
#' @return A `patchwork` object.
#' 
#' @keywords internal
#' @noRd
check_model_plot <- function(model, title) {
  # packages
  rlang::is_installed(c("patchwork", "see", "withr"))

  withr::local_package("see")

  # get text for the annotation of the model
  model_formula <- purrr::chuck(model, "terms") %>% format()

  # plot the assumptions
  assumption_plot <-
    plot(performance::check_model(model))

  assumption_plot +
    patchwork::plot_annotation(title = title,
                               subtitle =
                                 glue::glue("Model formula: {model_formula}"),
    )
}
