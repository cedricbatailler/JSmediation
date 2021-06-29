#' Standardize variables in a data set.
#'
#' @description
#' `standardize_variables()` standardizes the selected columns in a data.frame
#' using \code{\link[base:scale]{scale}}. This function overwrites
#' the column to be scaled.
#'
#' `standardize_variables()` and `standardise_variables()` are synonyms.
#'
#' @param .data A data frame containing the variables to standardize.
#' @param .cols <[`tidy-select`][dplyr_tidy_select]> Columns to standardize.
#'
#' @return A data frame with the standardized columns.
#' @export
#'
#' @examples
#' ho_et_al %>%
#'   standardize_variables(sdo)
#'
#' ho_et_al %>%
#'   standardize_variables(c(sdo, linkedfate))
#'
standardize_variables <- function(.data, .cols = dplyr::everything()) {
  UseMethod("standardize_variables")
}

#' @rdname standardize_variables
#' @export
standardise_variables <- standardize_variables

#' @export
standardize_variables.data.frame <- function(.data, .cols = dplyr::everything()) {

  .data %>%
    dplyr::mutate(dplyr::across({{ .cols }},
                                scale))

}
