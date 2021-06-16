#' @title add_index method for moderated mediation
#'
#' @description Adds the confidence interval for the index of moderated
#'   mediation to a model fitted with \code{\link{mdt_moderated}}.
#'
#' @param mediation_model A mediation model of class
#'   \code{"moderated_mediation"}.
#' @param times Number of simulations to use to compute the Monte Carlo indirect
#'   effect confidence interval.
#' @param level Alpha threshold to use for the confidence interval.
#' @param stage Moderated indirect effect's stage for which to compute the
#'   confidence interval. Can be either \code{1} (or \code{"first"}) or \code{2}
#'   (or \code{"second"}). To compute total indirect effect moderation index,
#'   use \code{"total"}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details Indirect effect moderation index for moderated mediation uses
#'   \eqn{a}, \eqn{a \times Mod}{a * Mod}, \eqn{b}, and \eqn{b \times Mod}{b *
#'   Mod} estimates and their standard errors to compute the appropriate index
#'   product distribution using Monte Carlo methods (see Muller, Judd, &
#'   Yzerbyt, 2005).
#'
#'   \pkg{JSmediation} supports different types of mediated indirect effect
#'   index: 
#'   \itemize{ 
#'     \item{Stage 1: }{computes the product between \eqn{a \times Mod}{a
#'     * Mod} and \eqn{b}.}
#'     \item{Stage 2: }{ computes the product between \eqn{a} and \eqn{b \times
#'     Mod}{b * Mod}.}
#'     \item{Total: }{ computes the sum of Stage 1 and Stage 2 distribution.}
#'   }
#'
#' @examples
#' ## getting a stage 1 moderated indirect effect index
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' ho_et_al$linkedfate_c <- scale(ho_et_al$linkedfate, scale = FALSE)
#' ho_et_al$sdo_c <- scale(ho_et_al$sdo, scale = FALSE)
#' moderated_model <- mdt_moderated(data = ho_et_al,
#'                                  IV = condition_c,
#'                                  DV = hypodescent,
#'                                  M = linkedfate_c,
#'                                  Mod = sdo_c)
#' add_index(moderated_model, stage = 1)
#'
#' @references Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
#'   is mediated and mediation is moderated. \emph{Journal of Personality and
#'   Social Psychology}, 89(6), 852-863. doi: 10.1037/0022-3514.89.6.852
#'
#' @export
add_index.moderated_mediation <- function(mediation_model, times = 5000, level = .05, stage = NULL, ...) {

  if(is.null(stage))
    stop(
      "Warning:\n You have to explicite the stage on which you want to compute the moderated indirect effect index with the stage argument."
    )

  stage <- as.character(stage)


  if(stage %in% c("1", "first", "2", "second")) {
    if(stage %in% c("1", "first")) {
      a   <- purrr::pluck(mediation_model, "paths", "a * Mod", "point_estimate")
      sea <- purrr::pluck(mediation_model, "paths", "a * Mod", "se")
      b   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
      seb <- purrr::pluck(mediation_model, "paths", "b", "se")

      type <- "Mediated moderation index (First stage)"
    } else if(stage %in% c("2", "second")) {
      a   <- purrr::pluck(mediation_model, "paths", "a", "point_estimate")
      sea <- purrr::pluck(mediation_model,  "paths", "a", "se")
      b   <- purrr::pluck(mediation_model,  "paths", "b * Mod", "point_estimate")
      seb <- purrr::pluck(mediation_model,  "paths", "b * Mod", "se")

      type <- "Mediated moderation index (Second stage)"
    }

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
      indirect_effect(type       = type,
                      estimate   = a * b,
                      level      = level,
                      times      = times,
                      sampling   = indirect_sampling)
  }
  
  else if(stage %in% c("total")) {

    a1   <- purrr::pluck(mediation_model, "paths", "a * Mod", "point_estimate")
    sea1 <- purrr::pluck(mediation_model, "paths", "a * Mod", "se")
    b1   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
    seb1 <- purrr::pluck(mediation_model, "paths", "b", "se")

    a2   <- purrr::pluck(mediation_model, "paths", "a", "point_estimate")
    sea2 <- purrr::pluck(mediation_model, "paths", "a", "se")
    b2   <- purrr::pluck(mediation_model, "paths", "b * Mod", "point_estimate")
    seb2 <- purrr::pluck(mediation_model, "paths", "b * Mod", "se")

    type <- "Indirect effect moderation index (total)"

    ab_sampling <-
      MASS::mvrnorm(n  = times,
                    mu = c(a1, b1, a2, b2),
                    Sigma =
                      matrix(
                        c(sea1^2,      0,      0,      0,
                          0,      seb1^2,      0,      0,
                          0,           0, sea2^2,      0,
                          0,           0,      0, seb2^2),
                        nrow = 4
                      ))

    indirect_sampling <- ab_sampling[ , 1] * ab_sampling[ , 2] +
      ab_sampling[ , 3] * ab_sampling[ , 4]
    
    indirect_index_infos <-
      indirect_effect(
        type          = type,
        estimate      = a1 * b1 + a2 * b2,
        level         = level,
        times         = times,
        sampling      = indirect_sampling
      )
  }



  mediation_model$indirect_index <- TRUE
  mediation_model$indirect_index_infos <-
    indirect_index_infos

  mediation_model
}
