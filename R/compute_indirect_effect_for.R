#' Compute the indirect effect index for a specific value of the moderator
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param Mod The moderator value for which to compute the indirect effect.
#' @param times Number of simulations to use to compute the Monte Carlo indirect
#'   effect confidence interval.
#' @param level Alpha threshold to use for the confidence interval.
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

    # checks
    if (!is.numeric(Mod)) {
      rlang::abort("`Mod` argument must be numeric.")
    }

    if (length(Mod) != 1) {
      rlang::abort("`Mod` argument must be a single numeric value.")
    }

    mediation_dataset <- purrr::chuck(mediation_model, "data")
    moderator         <- purrr::chuck(mediation_model, "params", "Mod")

    # adjust the moderator coding so that 0 is the value we want to look at
    mediation_dataset <- 
      mediation_dataset %>% 
      dplyr::mutate(across(.data[[moderator]], ~ .x - Mod))

    # run a new moderated mediation model
    mediation_model_at_Mod <-
      mdt_moderated(data = mediation_dataset,
                    IV  = !! sym(purrr::chuck(mediation_model, "params", "IV")),
                    M   = !! sym(purrr::chuck(mediation_model, "params", "M")),
                    DV  = !! sym(purrr::chuck(mediation_model, "params", "DV")),
                    Mod = !! sym(purrr::chuck(mediation_model, "params", "Mod")))

    # extract effects of IV, which now reads as the effects of IV when Mod = Mod

    a_estimate   <- purrr::pluck(mediation_model_at_Mod, "paths", "a", "point_estimate")
    a_se         <- purrr::pluck(mediation_model_at_Mod, "paths", "a", "se")

    b_estimate   <- purrr::pluck(mediation_model_at_Mod, "paths", "b", "point_estimate")
    b_se         <- purrr::pluck(mediation_model_at_Mod, "paths", "b", "se")

    # sample the params
    param_sampling <-
      MASS::mvrnorm(n  = times,
                    mu = c(a_estimate, b_estimate),
                    Sigma =
                      matrix(
                        c(a_se^2,      0,
                          0, b_se^2),
                        nrow = 2
                      )
      )

    indirect_sampling <- param_sampling[ , 1] * param_sampling[ , 2] 

    indirect_effect(
      type          = glue::glue("Conditional simple mediation index (Mod = {Mod})"),
      estimate      = a_estimate  * b_estimate,
      level         = level,
      times         = times,
      sampling      = indirect_sampling)

  }
