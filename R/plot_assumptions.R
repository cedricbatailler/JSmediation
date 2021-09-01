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
#' plot_assumptions(my_model)
#'
#' @family assumption checks
#'
#' @export
plot_assumptions <-
  function(mediation_model,
           tests = c("normality", "heteroscedasticity", "outliers")) {
    UseMethod("plot_assumptions")
  }

#' @export
plot_assumptions.mediation_model <-
  function(mediation_model,
           tests = c("normality", "heteroscedasticity", "outliers")) {


    # check tests arg ---------------------------------------------------------
    supported_tests <- c("normality", "heteroscedasticity", "outliers")

    if (length(tests) < 1L) {
      rlang::abort("`tests` argument must contains at least one element.")
    }

    if (! is.character(tests)) {
      rlang::abort("`tests` argument must be a character vector.")
    }

    if (sum(tests %in% supported_tests) == 0) {

      rlang::abort(message =
                     c("`tests` argument in `check_assumptions` contains unsupported checks.",
                       i  = "Supported checks are:",
                       "- normality",
                       "- heteroscedasticity",
                       "- outliers")
      )
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

    # Build test list ---------------------------------------------------------
    tests_to_perform <- vector(mode = "character")

    if ("normality" %in% tests) {
      tests_to_perform <- append(tests_to_perform, c("qq", "normality"))
    }
    if ("heteroscedasticity" %in% tests) {
      tests_to_perform <- append(tests_to_perform, "homogeneity")
    }
    if ("outliers" %in% tests) {
      tests_to_perform <- append(tests_to_perform, "outliers")
    }

    # performance::check_model() ----------------------------------------------
    purrr::pluck(mediation_model, "js_models") %>%
      purrr::iwalk(~ check_model_plot(model = .x,
                                      title = .y,
                                      tests_to_perform = tests_to_perform) %>%
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
check_model_plot <- function(model, title, tests_to_perform) {
  # packages
  rlang::is_installed(c("patchwork", "see", "withr"))

  withr::local_package("see")

  # get text for the annotation of the model
  model_formula <-
    purrr::chuck(model, "terms") %>%
    format()

  # plot the assumptions
  assumption_plot <-
    plot(performance::check_model(model, check = tests_to_perform))

  assumption_plot +
    patchwork::plot_annotation(title = title,
                               subtitle =
                                 glue::glue("Model formula: {model_formula}"),
    )
}
