#' Standardize variables in a data set.
#'
#' @description `standardize_variables()` standardizes the selected columns in a
#'   data.frame using \code{\link[base:scale]{scale}}. By default, this function
#'   overwrites the column to be scaled.
#'
#'   `standardize_variables()` and `standardise_variables()` are synonyms.
#'
#' @param data A data frame containing the variables to standardize.
#' @param cols <[`tidy-select`][dplyr_tidy_select]> Columns to standardize.
#'   Defaults to [`dplyr::everything()`].
#' @param suffix A character suffix to be added to the scaled variables names.
#'   When suffix is set to`NULL`, the  `standardize_variables()` function will
#'   overwrite the scaled variables. Defaults to `NULL`.
#'
#' @return A data frame with the standardized columns.
#' @export
#'
#' @examples
#' ho_et_al %>%
#'   standardize_variables(sdo)
#'
#' ho_et_al %>%
#'   standardize_variables(c(sdo, linkedfate), suffix = "scaled")
#'
standardize_variables <- function(data, 
                                  cols = dplyr::everything(), 
                                  suffix = NULL) {
  UseMethod("standardize_variables")
}

#' @rdname standardize_variables
#' @export
standardise_variables <- standardize_variables

#' @export
standardize_variables.data.frame <- function(data,
                                             cols = dplyr::everything(),
                                             suffix = NULL) {
  # defines suffix
  if( ! is.null(suffix)) {
    suffix <- glue::glue("_{suffix}")
  } else {
    suffix <- ""
  }
  # scales variables
  data %>%
    dplyr::mutate(dplyr::across(.cols = {{ cols }},
                                .fns = scale, 
                                .names = "{.col}{suffix}"))
}
