#' Compute the indirect effect index for a specific value of the moderator
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description When computing a moderated mediation, one is assessing whether
#'   an indirect effect depends on a moderator value (Muller et al., 2005).
#'   [`mdt_moderated`] make it easy to assess moderated mediation, but it does
#'   not make accessong the actual indirect effect for a specific moderator
#'   values easy. `compute_indirect_effect_for` fills this gap.
#'
#' @param mediation_model A mediation 
#' @param Mod The moderator value for which to compute the indirect effect. Must
#'   be a numeric value, defaults to `0`.
#' @param times Number of simulations to use to compute the Monte `Carlo` indirect
#'   effect confidence interval. Must be numeric, defaults to 5000.
#' @param level Alpha threshold to use for the indirect effect's confidence
#'   interval.

#' @examples
#' # compute an indirect effect index for a specific value in a moderated 
#' # mediation.
#' data(ho_et_al)
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' ho_et_al$linkedfate <- as.numeric(scale(ho_et_al$linkedfate))
#' ho_et_al$sdo        <- as.numeric(scale(ho_et_al$sdo))
#' moderated_mediation_model <- mdt_moderated(data = ho_et_al,
#'                                            DV = hypodescent,
#'                                            IV = condition_c,
#'                                            M = linkedfate,
#'                                            Mod = sdo) 
#' compute_indirect_effect_for(moderated_mediation_model, Mod = 0)
#'
#' @references Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
#'   is mediated and mediation is moderated. *Journal of Personality and
#'   Social Psychology*, 89(6), 852-863. doi: 10.1037/0022-3514.89.6.852
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
      dplyr::mutate(dplyr::across(.data[[moderator]], ~ .x - Mod))

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
