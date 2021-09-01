#' Compute the indirect effect index for a specific value of the moderator
#'
#' @description
#'   When computing a moderated mediation, one assesses whether an indirect
#'   effect changes according a moderator value (Muller et al., 2005).
#'   [`mdt_moderated`] makes it easy to assess moderated mediation, but it does
#'   not allow accessing the indirect effect for a specific moderator values.
#'   `compute_indirect_effect_for` fills this gap.
#'
#' @param mediation_model A moderated mediation model fitted with
#'   `mdt_moderated`.
#' @param Mod The moderator value for which to compute the indirect effect. Must
#'   be a numeric value, defaults to `0`.
#' @param times Number of simulations to use to compute the Monte Carlo indirect
#'   effect confidence interval. Must be numeric, defaults to `5000`.
#' @param level Alpha threshold to use for the indirect effect's confidence
#'   interval. Defaults to `.05`.
#'
#' @details
#'   The approach used by `compute_indirect_effect_for` is similar to the
#'   approach used for simple slope analyses. Specifically, it will fit a new
#'   moderated mediation model, but with a data set with a different variable
#'   coding. Behind the scenes, `compute_indirect_effect_for` adjusts the
#'   moderator variable coding, so that the value we want to compute the
#'   indirect effect for is now `0`.
#'
#'   Once done, a new moderated mediation model is applied using the new data
#'   set. Because of the new coding, and because of how one interprets
#'   coefficients in a linear regression, \eqn{a \times b}{a * b} is now the
#'   indirect effect we wanted to compute (see the Models section).
#'
#'   Thanks to the returned values of \eqn{a}{a} and {b}{b} (\eqn{b_51}{b_51}
#'   and \eqn{b_64}{b_64}, see the Models section), it is now easy to compute
#'   \eqn{a \times b}{a * b}. `compute_indirect_effect_for` uses the same
#'   approach than the [`add_index`] function. A Monte Carlo simulation is used
#'   to compute the indirect effect index (MacKinnon et al., 2004).
#'
#' @section Models: In a moderated mediation model, three models are used.
#'   `compute_indirect_effect_for` uses the same model specification as
#'   [`mdt_moderated`]:
#'
#'   - \eqn{Y_i = b_{40} + \mathbf{b_{41}} X_i + b_{42} Mo_i + \mathbf{b_{43}}
#'   XMo_i }{Yi = b_41 + b41*Xi + b42*Moi + + b43*XMoi}
#'   - \eqn{M_i = b_{50} + \mathbf{b_{51}} X_i + b_{52} Mo_i + \mathbf{b_{53}
#'   XMo_i}}{Mi = b_50 + b_51*Xi + b_52 Moi + b53 XMoi}
#'   - \eqn{Y_i = b_{60} + \mathbf{c'_{61}} X_i + b_{62} Mo_i + \mathbf{b_{63}
#'   Xmo_i} + \mathbf{b_{64} Me_i} + \mathbf{b_{65} MeMo_i}}{Yi = b_60 + b61*Xi
#'   + b_62*Moi + b63 XMoi + b64 Mei + b65 MeMoi}
#'
#'   with \eqn{Y_i}{Yi}, the outcome value for the *i*th observation,
#'   \eqn{X_i}{Xi}, the predictor value for the *i*th observation,
#'   \eqn{Mo_i}{Xi}, the moderator value for the *i*th observation, and
#'   \eqn{M_i}{Mi}, the mediator value for the *i*th observation.
#'
#'   Coefficients associated with \eqn{a}, \eqn{a \times Mod}{a * Mod}, \eqn{b},
#'   \eqn{b \times Mod}{b * Mod}, \eqn{c}, \eqn{c \times Mod}{c * Mod},
#'   \eqn{c'}, and \eqn{c' \times Mod}{c' * Mod}, paths are respectively
#'   \eqn{b_{51}}{b_51}, \eqn{b_{53}}{b_53}, \eqn{b_{64}}{b_64},
#'   \eqn{b_{65}}{b_65}, \eqn{b_{41}}{b_41}, \eqn{b_{43}}{b_43},
#'   \eqn{b_{61}}{b_61}, and \eqn{b_{63}}{c63} (see Muller et al., 2005).
#'
#' @examples
#' # compute an indirect effect index for a specific value in a moderated
#' # mediation.
#' data(ho_et_al)
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' ho_et_al <- standardize_variable(ho_et_al, c(linkedfate, sdo))
#' moderated_mediation_model <- mdt_moderated(data = ho_et_al,
#'                                            DV = hypodescent,
#'                                            IV = condition_c,
#'                                            M = linkedfate,
#'                                            Mod = sdo)
#' compute_indirect_effect_for(moderated_mediation_model, Mod = 0)
#'
#' @references
#'   MacKinnon, D. P., Lockwood, C. M., & Williams, J. (2004). Confidence Limits
#'   for the Indirect Effect: Distribution of the Product and Resampling
#'   Methods. *Multivariate Behavioral Research*, *39*(1), 99-128. doi:
#'   10.1207/s15327906mbr3901_4
#'
#'   Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
#'   is mediated and mediation is moderated. *Journal of Personality and
#'   Social Psychology*, *89*(6), 852-863. doi: 10.1037/0022-3514.89.6.852
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
                    IV  = !!sym(purrr::chuck(mediation_model, "params", "IV")),
                    M   = !!sym(purrr::chuck(mediation_model, "params", "M")),
                    DV  = !!sym(purrr::chuck(mediation_model, "params", "DV")),
                    Mod = !!sym(purrr::chuck(mediation_model, "params", "Mod"))
                    )

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

    indirect_sampling <- param_sampling[, 1] * param_sampling[, 2]

    indirect_effect(
      type          = glue("Conditional simple mediation index (Mod = {Mod})"),
      estimate      = a_estimate  * b_estimate,
      level         = level,
      times         = times,
      sampling      = indirect_sampling)

  }
