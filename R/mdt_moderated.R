#' @title Fits a moderated mediation model
#'
#' @description Given a data frame, a predictor (`IV`), an outcome (`DV`), a
#'   mediator (`M`), and a moderator (`Mod`) conducts a joint-significant test
#'   for moderated mediation (see Yzerbyt, Muller, Batailler, & Judd, 2018).You
#'   can learn about moderated mediation in `vignette("moderated-mediation")`
#'
#'   [`add_index.moderated_mediation`] computes the moderated mediation index.
#'   [`compute_indirect_effect_for`] is used to compute the indirect effect
#'   index for a specific value of the moderator.
#'
#' @param data A data frame containing the variables in the model.
#' @param IV An unquoted variable in the data frame which will be used as
#'   the independent variable.
#' @param DV An unquoted variable in the data frame which will be used as
#'   the dependent variable.
#' @param M An unquoted variable in the data frame which will be used as
#'   the mediator.
#' @param Mod An unquoted variable in the data frame which will be used as
#'   the moderator.
#'
#' @template mediation_model
#'
#' @family mediation models
#'
#' @details With moderated mediation analysis, one tests whether the
#'   indirect effect of \eqn{X} on \eqn{Y} through \eqn{M} is moderated by
#'   \eqn{Mod}. The hypothesis behind this test is that \eqn{X} has an effect on
#'   \eqn{M} (\eqn{a}) which has an effect on \eqn{Y} (\eqn{b}), meaning that
#'   \eqn{X} has an indirect effect on \eqn{Y} through \eqn{M}.
#'
#'   Total moderation of the indirect effect of \eqn{X} on \eqn{Y} can be
#'   described as follows:
#'
#'   \eqn{c * Mod = c' * Mod + (a * Mod) * b + a * (b * Mod)}
#'
#'
#'   with \eqn{c * Mod} the total moderation of the indirect effect, \eqn{c' *
#'   Mod} the moderation of the direct effect, \eqn{(a * Mod) * b}, the
#'   moderation of the indirect effect passing by the moderation of \eqn{a}, and
#'   \eqn{a * (b * Mod)}, the moderation of the indirect effect passing by the
#'   moderation of \eqn{b} (see Models section; Muller et al., 2005).
#'
#'   Either both \eqn{a * Mod} and \eqn{b} or both \eqn{a} and \eqn{b * Mod}
#'   need to be simultaneously significant for a moderation of the indirect
#'   effect to be claimed (Muller et al., 2005).
#'
#' @section Models: In a moderated mediation model, three models will be used:
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
#'
#'   Coefficients associated with \eqn{a}, \eqn{a \times Mod}{a * Mod}, \eqn{b},
#'   \eqn{b \times Mod}{b * Mod}, \eqn{c}, \eqn{c \times Mod}{c * Mod},
#'   \eqn{c'}, and \eqn{c' \times Mod}{c' * Mod}, paths are respectively
#'   \eqn{b_{51}}{b_51}, \eqn{b_{53}}{b_53}, \eqn{b_{64}}{b_64},
#'   \eqn{b_{65}}{b_65}, \eqn{b_{41}}{b_41}, \eqn{b_{43}}{b_43},
#'   \eqn{b_{61}}{b_61}, and \eqn{b_{63}}{c63} (see Muller et al., 2005).
#'
#' @section Variable coding: Because joint-significance tests use linear models
#'   behind the scenes, variables involved in the model have to be numeric.
#'   `mdt_simple` will give an error if non-numeric variables are
#'   specified in the model.
#'
#'   If you need to convert a dichotomous categorical variable to a numeric one,
#'   please refer to the [`build_contrast`] function.
#'
#'   Note that variable coding is especially important in models with multiple
#'   predictors as is the case in the model used to conduct a joint-significance
#'   test of moderated mediation. Muller et al. (2005) recommend using variables
#'   that are either contrast-coded or centered. Using `mdt_moderated` with
#'   a DV, a mediator, or a moderator that is neither contrast-coded nor
#'   centered will give a warning message.
#'
#'
#' @references Muller, D., Judd, C. M., & Yzerbyt, V. Y. (2005). When moderation
#'   is mediated and mediation is moderated. *Journal of Personality and
#'   Social Psychology*, 89(6), 852-863. doi: 10.1037/0022-3514.89.6.852
#'
#'   Yzerbyt, V., Muller, D., Batailler, C., & Judd, C. M. (2018). New
#'   recommendations for testing indirect effects in mediational models: The
#'   need to report and test component paths. *Journal of Personality and
#'   Social Psychology*, *115*(6), 929–943. doi: 10.1037/pspa0000132
#'
#' @export

mdt_moderated <- function(data, IV, DV, M, Mod) {
  UseMethod("mdt_moderated")
}

#' @export
mdt_moderated.data.frame <- function(data, IV, DV, M, Mod) {

  # nse -----------------------------------------------------------------------
  IV_var  <- enquo(IV)
  DV_var  <- enquo(DV)
  M_var   <- enquo(M)
  Mod_var <- enquo(Mod)

  IV_name    <- rlang::quo_name(IV_var)
  DV_name    <- rlang::quo_name(DV_var)
  M_name     <- rlang::quo_name(M_var)
  Mod_name   <- rlang::quo_name(Mod_var)
  IVMod_name <- glue("{IV_name}:{Mod_name}")
  MMod_name  <- glue("{M_name}:{Mod_name}")

  IV_data  <- data %>% dplyr::pull(!!IV_var)
  M_data   <- data %>% dplyr::pull(!!M_var)
  DV_data  <- data %>% dplyr::pull(!!DV_var)
  Mod_data <- data %>% dplyr::pull(!!Mod_var)

  # type check ----------------------------------------------------------------
  if (!is.numeric(IV_data)) {
    stop(glue("Warning:
               IV ({IV_name}) must be numeric (see build_contrast() to
               convert a character vector to a contrast code)."))
  }

  if(!is.numeric(M_data)) {
    stop(glue("Warning:
               Mediator ({M_name}) must be numeric."))
  }

  if(!is.numeric(DV_data)) {
    stop(glue("Warning:
               DV ({DV_name}) must be numeric."))
}

  if(!is.numeric(Mod_data)) {
    stop(glue("Warning:
              Moderator ({DV_name}) must be numeric."))
  }

  # building models -----------------------------------------------------------
  model1 <-
    stats::as.formula(glue("{DV} ~ {IV} * {Mod}",
                           IV  = IV_name,
                           DV  = DV_name,
                           Mod = Mod_name))

  model2 <-
    stats::as.formula(glue("{M} ~ {IV} * {Mod}",
                           IV  = IV_name,
                           M   = M_name,
                           Mod = Mod_name))

  model3 <-
    stats::as.formula(glue("{DV} ~ ({IV} + {M}) * {Mod}",
                           DV  = DV_name,
                           IV  = IV_name,
                           M   = M_name,
                           Mod = Mod_name))

  # model fitting and cleaning ------------------------------------------------
  js_models <-
    list("X * Mod -> Y"       = model1,
         "X * Mod -> M"       = model2,
         "(X + M) * Mod -> Y" = model3) %>%
    purrr::map(~lm(.x, data))

  # paths ---------------------------------------------------------------------
  paths <-
    list("a"        = create_path(js_models, "X * Mod -> M", IV_name),
         "a * Mod"  = create_path(js_models, "X * Mod -> M", IVMod_name),
         "b"        = create_path(js_models, "(X + M) * Mod -> Y", M_name),
         "b * Mod"  = create_path(js_models, "(X + M) * Mod -> Y", MMod_name),
         "c"        = create_path(js_models, "X * Mod -> Y", IV_name),
         "c * Mod"  = create_path(js_models, "X * Mod -> Y", IVMod_name),
         "c'"       = create_path(js_models, "(X + M) * Mod -> Y", IV_name),
         "c' * Mod" = create_path(js_models, "(X + M) * Mod -> Y", IVMod_name))


  # bulding mediation model object --------------------------------------------
  mediation_model(
    type      = "moderated mediation",
    params    = list("IV"  = IV_name,
                     "DV"  = DV_name,
                     "M"   = M_name,
                     "Mod" = Mod_name),
    paths     = paths,
    js_models = js_models,
    data      = data,
    subclass  = "moderated_mediation"
  )
}
