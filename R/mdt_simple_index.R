#' @title add_index method for simple mediation
#'
#' @description Adds confidence interval for the index of  mediation to a model
#'   fitted with \code{\link{mdt_simple}}.
#'
#' @param mediation_model A mediation model of class \code{"simple_mediation"}.
#' @param times Number of simulations to use to compute the Monte Carlo indirect
#'   effect confidence interval.
#' @param level Alpha threshold to use for the confidence interval.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details Indirect effect index for simple mediation uses \eqn{a} and \eqn{b}
#'   estimates and their standard errors to compute the \eqn{ab} product
#'   distribution using Monte Carlo methods (see MacKinnon, Lockwood, &
#'   Williams, 2004).
#'
#' @references MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004).
#'   Confidence Limits for the Indirect Effect: Distribution of the Product and
#'   Resampling Methods. \emph{Multivariate Behavioral Research}, \emph{39}(1),
#'   99-128. doi: 10.1207/s15327906mbr3901_4
#'
#' @examples
#' ## getting an indirect effect index
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' simple_model <- mdt_simple(data = ho_et_al,
#'                            IV = condition_c,
#'                            DV = hypodescent,
#'                            M = linkedfate)
#' add_index(simple_model)
#'
#' @export
add_index.simple_mediation <- function(mediation_model, times = 5000, level = .05, ...) {

  a   <- purrr::pluck(mediation_model, "paths", "a", "point_estimate")
  sea <- purrr::pluck(mediation_model, "paths", "a", "se")
  b   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
  seb <- purrr::pluck(mediation_model, "paths", "b", "se")

  ab_sampling <-
    MASS::mvrnorm(n  = times,
                  mu = c(a, b),
                  Sigma =
                    matrix(
                      c(sea^2, 0,
                        0, seb^2),
                      nrow = 2
                    ))

  indirect_sampling <- ab_sampling[ , 1] * ab_sampling[ , 2]
  CI <- stats::quantile(indirect_sampling, c(level / 2, 1 - level / 2))
  contains_zero <- (CI[[1]] < 0 & CI[[2]] > 0)

  indirect_index_infos <-
    indirect_effect(type       = "Indirect effect",
                    estimate   = a * b,
                    level      = level,
                    times      = times,
                    sampling   = indirect_sampling)

  mediation_model$indirect_index <- TRUE
  mediation_model$indirect_index_infos <- indirect_index_infos

  mediation_model
}
