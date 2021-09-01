#' @title Joint-significance test for simple mediation (wide-format input)
#'
#' @description Given a data frame, a predictor (`IV`), an outcome
#'   (`DV`), a mediator (`M`), and a grouping variable (`group`)
#'   conducts a joint-significant test for within-participant mediation (see
#'   Yzerbyt, Muller, Batailler, & Judd, 2018).
#'
#' @param data a data frame containing the variables in the model.
#' @param DV_A an unquoted numeric variable in the data frame which will be used
#'   as the dependent variable value for the "A" independent variable condition.
#' @param DV_B an unquoted numeric variable in the data frame which will be used
#'   as the dependent variable value for the "B" independent variable condition.
#' @param M_A an unquoted numeric variable in the data frame which will be used
#'   as the mediatior variable value for the "A" independent variable condition.
#' @param M_B an unquoted numeric variable in the data frame which will be used
#'   as the mediatior variable value for the "b" independent variable condition.
#'
#' @section Data formatting: To be consistent with other `mdt_*` family
#'   functions, [`mdt_within`] takes a long-format data frame as `data`
#'   argument. With this kind of format, each sampled unit has two rows, one for
#'   the first within-participant condition and one for the second
#'   within-participant condition. In addition, each row has one observation for
#'   the outcome and one observation for the mediator (see
#'   [`dohle_siegrist`] for an example.
#'
#'   Because such formatting is not the most common among social scientists
#'   interested in within-participant mediation, JSmediation contains the
#'   [`mdt_within_wide`] function which handles wide-formatted data
#'   input (but is syntax-inconsistent with other `mdt_*` family
#'   functions).
#'
#' @section Variable coding: Models underlying within-participant mediation use
#'   difference scores as DV (see Models section).  `mdt_within_wide` uses
#'   `M_A` \eqn{-} `M_B` and `DV_A` \eqn{-} `DV_B` in these
#'   models.
#'
#' @template mediation_model
#' @template within_details
#' @template within_models
#'
#' @references Judd, C. M., Kenny, D. A., & McClelland, G. H. (2001). Estimating
#'   and testing mediation and moderation in within-subject designs.
#'   *Psychological Methods*, *6*(2), 115-134. doi:
#'   10.1037//1082-989X.6.2.115
#'
#'   Montoya, A. K., & Hayes, A. F. (2017). Two-condition within-participant
#'   statistical mediation analysis: A path-analytic framework.
#'   *Psychological Methods*, *22*(1), 6-27. doi: 10.1037/met0000086
#'
#'   Yzerbyt, V., Muller, D., Batailler, C., & Judd, C. M. (2018). New
#'   recommendations for testing indirect effects in mediational models: The
#'   need to report and test component paths. *Journal of Personality and
#'   Social Psychology*, *115*(6), 929–943. doi: 10.1037/pspa0000132
#'
#' @export
mdt_within_wide <- function(data, DV_A, DV_B, M_A, M_B) {
  UseMethod("mdt_within_wide")
}

#' @export
mdt_within_wide.data.frame <- function(data, DV_A, DV_B, M_A, M_B) {

  # nse -----------------------------------------------------------------------
  DV_A_var       <- enquo(DV_A)
  DV_B_var       <- enquo(DV_B)
  M_A_var        <- enquo(M_A)
  M_B_var        <- enquo(M_B)


  DV_A_name       <- rlang::quo_name(DV_A_var)
  DV_B_name       <- rlang::quo_name(DV_B_var)
  M_A_name        <- rlang::quo_name(M_A_var)
  M_B_name        <- rlang::quo_name(M_B_var)

  DV_A_data       <- data %>% dplyr::pull(!!DV_A_var)
  DV_B_data       <- data %>% dplyr::pull(!!DV_B_var)
  M_A_data        <- data %>% dplyr::pull(!!M_A_var)
  M_B_data        <- data %>% dplyr::pull(!!M_B_var)

  # type check ---------------------------------------------------------------

  Var_n <-
    list(
      DV_A_name,
      DV_B_name,
      M_A_name,
      M_B_name
    )

  Var_check <-
    list(
      DV_A_data,
      DV_B_data,
      M_A_data,
      M_B_data
    ) %>%
    purrr::map_lgl(~ is.numeric(.x))

  Var_n_check <-
    Var_n[!Var_check]

  if (length(Var_n_check) != 0) {
    message <- "It appears that the following variables are not numeric:"

    for (var in Var_n_check) {
      message <- paste0(message, "\n* ", var)
    }

    stop(call. = FALSE, message)
  }

  # data wrangling ------------------------------------------------------------
  dataset <-
    data %>%
    dplyr::mutate(M_diff  = !!M_A_var - !!M_B_var,
                  M_mean  = (!!M_A_var + !!M_B_var) / 2,
                  M_mean_center =
                    scale((!!M_A_var + !!M_B_var) / 2,
                          scale = FALSE),
                  DV_diff = !!DV_A_var - !!DV_B_var)

  # bulding models ------------------------------------------------------------
  model1 <-
    stats::as.formula(DV_diff ~ 1)

  model2 <-
    stats::as.formula(M_diff ~ 1)

  model3 <-
    stats::as.formula(DV_diff ~ 1 + M_diff + M_mean_center)

  # model fitting and cleaning ------------------------------------------------
  js_models <-
    list("1 -> DV_diff"                   = model1,
         "1 -> M_diff"                    = model2,
         "1 + M_diff + M_mean -> DV_diff" = model3) %>%
    purrr::map(~lm(.x, dataset))

  # paths ---------------------------------------------------------------------
  paths <-
    list(
      "a"  = create_path(js_models, "1 -> M_diff", "(Intercept)"),
      "b"  = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", "M_diff"),
      "c"  = create_path(js_models, "1 -> DV_diff", "(Intercept)"),
      "c'" = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", "(Intercept)")
    )

  # bulding mediation model object --------------------------------------------
  mediation_model(
    type      = "within-participant mediation",
    params    = list("DV difference" = glue("{DV_A_name} - {DV_B_name}"),
                     "M difference"  = glue("{M_A_name} - {M_B_name}")),
    paths     = paths,
    js_models = js_models,
    data      = data,
    subclass  = "within_participant_mediation"
  )
}
