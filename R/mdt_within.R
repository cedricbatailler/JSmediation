#' @title Joint-significance test for within-participant mediation
#'
#' @description Given a data frame, a predictor (`IV`), an outcome
#'   (`DV`), a mediator (`M`), and a grouping variable (`group`)
#'   conducts a joint-significant test for within-participant mediation (see
#'   Yzerbyt, Muller, Batailler, & Judd, 2018).
#'
#' @param data a data frame containing the variables in the model.
#' @param IV an unquoted variable in the data frame which will be used as
#'   the independent variable.
#' @param M an unquoted variable in the data frame which will be used as
#'   the mediator.
#' @param DV an unquoted variable in the data frame which will be used as
#'   the dependent variable.
#' @param grouping an unquoted variable in the data frame which will be used as
#'   the grouping variable.
#' @param default_coding should the variable coding be the default? Defaults to
#'   `TRUE`.
#'
#' @template mediation_model
#' @template within_details
#' @template within_models
#'
#' @section Data formatting: To be consistent with other `mdt_*` family
#'   functions, `mdt_within` takes a long-format data frame as `data`
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
#'   difference scores as DV (see Models section). Because the function input
#'   does not allow the user to specify how the difference scores should be
#'   computed, [`mdt_within`] has a default coding.
#'
#'   `mdt_within`'s default behavior is to compute the difference score so
#'   the total effect (the effect of \eqn{X} on \eqn{Y}) will be positive and
#'   compute the other difference scores accordingly. That is, if
#'   `mdt_within` has to use \eqn{Y_{2i} - Y_{1i}} (instead of \eqn{Y_{1i}
#'   - Y_{2i}}) so that \eqn{c_{11}} is positive, it will use \eqn{M_{2i} -
#'   M_{1i}} (instead of \eqn{M_{1i} - M_{2i}} in the other models.
#'
#'   User can choose to have a negative total effect by using the
#'   `default_coding` argument.
#'
#'   Note that `DV` and `M` have to be numeric.
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
#' @family mediation models
#'
#' @export
mdt_within <- function(data, IV, DV, M, grouping, default_coding = TRUE) {
  UseMethod("mdt_within")
}

#' @export
mdt_within.data.frame <- function(data, IV, DV, M, grouping, default_coding = TRUE) {

  # nse -----------------------------------------------------------------------
  IV_var       <- enquo(IV)
  DV_var       <- enquo(DV)
  M_var        <- enquo(M)
  grouping_var <- enquo(grouping)

  IV_name       <- rlang::quo_name(IV_var)
  DV_name       <- rlang::quo_name(DV_var)
  M_name        <- rlang::quo_name(M_var)
  grouping_name <- rlang::quo_name(grouping_var)

  IV_data <- data %>% dplyr::pull(!!IV_var)
  DV_data <- data %>% dplyr::pull(!!DV_var)
  M_data  <- data %>% dplyr::pull(!!M_var)

  # type check ----------------------------------------------------------------
  if (!is.character(IV_data)) {
    stop(glue("Warning:
               IV ({IV_name}) must be character."))
  }
  if (!is.numeric(DV_data)) {
    stop(glue("Warning:
              DV ({DV_name}) must be numeric."))
  }

  if (!is.numeric(M_data)) {
    stop(glue("Warning:
              Mediator ({M_name}) must be numeric."))
  }

  # data wrangling ------------------------------------------------------------
  # naming
  IV_cond <- data %>%
    dplyr::pull(!!IV_var) %>%
    unique()

  M_cond_1_name <-
    as.character(glue("{M_name}_mean_{IV_cond[[1]]}"))
  M_cond_2_name <-
    as.character(glue("{M_name}_mean_{IV_cond[[2]]}"))

  M_mean_name <-
    as.character(glue("{M_name}_mean"))

  DV_cond_1_name <-
    as.character(glue("{DV_name}_mean_{IV_cond[[1]]}"))
  DV_cond_2_name <-
    as.character(glue("{DV_name}_mean_{IV_cond[[2]]}"))

  # wrangling
  wrangling_formula <-
    glue("{grouping} ~ {IV}",
         grouping = grouping_name,
         IV       = IV_name) %>%
    stats::as.formula()

  data_long <-
    data.table::dcast(formula = wrangling_formula,
                      data = data.table::as.data.table(data),
                      fun.aggregate = list(mean, mean),
                      value.var = list(DV_name, M_name)) %>%
    tibble::as_tibble()

  DV_A_sup_B <- rlang::is_true(
    data_long %>% dplyr::pull(DV_cond_1_name) %>%  mean() >
      data_long %>% dplyr::pull(DV_cond_2_name) %>%  mean()
  )

  # little bit hacky:
  # if A > B and default_coding is true, set A - B, if
  # B < A and defaults coding is false, set A - B,
  # else, set B - A.
  if (DV_A_sup_B == default_coding) {
    DV_diff_name <-
      as.character(glue("DV_{IV_cond[[1]]}_{IV_cond[[2]]}"))
    M_diff_name <-
      as.character(glue("IV_{IV_cond[[1]]}_{IV_cond[[2]]}"))

    data_long <-
      data_long %>%
      dplyr::mutate(!!sym(DV_diff_name) := !!sym(DV_cond_1_name) - !!sym(DV_cond_2_name),
                    !!sym(M_diff_name)  := !!sym(M_cond_1_name)  - !!sym(M_cond_2_name))
  } else {
    DV_diff_name <-
      as.character(glue("DV_{IV_cond[[2]]}_{IV_cond[[1]]}"))

    M_diff_name <-
      as.character(glue("M_{IV_cond[[2]]}_{IV_cond[[1]]}"))

    data_long <-
      data_long %>%
      dplyr::mutate(!!sym(DV_diff_name) := !!sym(DV_cond_2_name) - !!sym(DV_cond_1_name),
                    !!sym(M_diff_name)  := !!sym(M_cond_2_name)  - !!sym(M_cond_1_name))
  }

  data_long <-
    data_long %>%
    dplyr::mutate(!!M_mean_name :=
                    scale((!!sym(M_cond_1_name) + !!sym(M_cond_2_name)) / 2,
                          scale = FALSE))

  # bulding models ------------------------------------------------------------
  model1 <-
    stats::as.formula(glue("{DV} ~ 1",
                           DV = DV_diff_name))

  model2 <-
    stats::as.formula(glue("{M}  ~ 1",
                           M = M_diff_name))

  model3 <-
    stats::as.formula(glue("{DV}  ~ 1 + {M} + {M_mean}",
                           DV     = DV_diff_name,
                           M      = M_diff_name,
                           M_mean = M_mean_name))

  # model fitting and cleaning ------------------------------------------------
  js_models <-
    list("1 -> DV_diff"                   = model1,
         "1 -> M_diff"                    = model2,
         "1 + M_diff + M_mean -> DV_diff" = model3) %>%
    purrr::map(~lm(.x, data_long))

  # paths ---------------------------------------------------------------------
  paths <-
    list("a"  = create_path(js_models, "1 -> M_diff", "(Intercept)"),
         "b"  = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", M_diff_name),
         "c"  = create_path(js_models, "1 -> DV_diff", "(Intercept)"),
         "c'" = create_path(js_models, "1 + M_diff + M_mean -> DV_diff", "(Intercept)"))

  # bulding mediation model object --------------------------------------------
  mediation_model(
    type      = "within-participant_mediation",
    params    = list("IV" = glue("{IV_name} (difference: {score})",
                                 score = ifelse(DV_A_sup_B == default_coding,
                                                paste0(IV_cond[[1]], " - ", IV_cond[[2]]),
                                                paste0(IV_cond[[2]], " - ", IV_cond[[1]]))),
                     "DV" = DV_name,
                     "M"  = M_name),
    paths     = paths,
    js_models = js_models,
    data      = data_long,
    subclass  = "within_participant_mediation"
  )
}
