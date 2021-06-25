#' Compute the indirect effect index for a specific value of the moderator
#'
#'  @param Mod The moderator value for which to compute the indirect effect. 
#'
#' Note that the function relies on a model specification as described in Muller
#' et al. (2005).
#' 
#' @export
compute_indirect_effect_for <- function(mediation_model, 
                                        Mod = 0, 
                                        times = 5000, 
                                        level = .05) {
  UseMethod("compute_indirect_effect_for")
}

#' @export
compute_indirect_effect_for.moderated_mediation <- 
  function(mediation_model, 
           Mod = 0, 
           times = 5000, 
           level = .05) {

  if (!is.numeric(M)) {
    rlang::abort("`Mod` argument must be numeric.")
  }

    if (length(M) != 1) {
      rlang::abort("`Mod` argument must be a single numeric value.")
    }

    a1   <- purrr::pluck(mediation_model, "paths", "a * Mod", "point_estimate")
    sea1 <- purrr::pluck(mediation_model, "paths", "a * Mod", "se")
    b1   <- purrr::pluck(mediation_model, "paths", "b", "point_estimate")
    seb1 <- purrr::pluck(mediation_model, "paths", "b", "se")
    
    a2   <- purrr::pluck(mediation_model, "paths", "a", "point_estimate")
    sea2 <- purrr::pluck(mediation_model, "paths", "a", "se")
    b2   <- purrr::pluck(mediation_model, "paths", "b * Mod", "point_estimate")
    seb2 <- purrr::pluck(mediation_model, "paths", "b * Mod", "se")
    
    param_sampling <-
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

    indirect_sampling <- (ab_sampling[ , 1] + ab_sampling[ , 2] * Mod) * 
      (ab_sampling[ , 3] + ab_sampling[ , 4] * Mod)
    
    indirect_effect(
      type          = glue::glue("Conditional simple mediation (Mod = {Mod})"),
      estimate      = a1 * b1 + a2 * b2,
      level         = level,
      times         = times,
      sampling      = indirect_sampling)
    
  }
