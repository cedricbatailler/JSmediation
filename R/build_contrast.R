#' @title Builds a contrast code from character vector
#'
#' @description This function constructs a contrast code from a character
#'   variable. It is useful when one needs to recode a two-category character
#'   variable to a numeric one.
#'
#' @param vector A character vector.
#' @param cond_a A character string to be coded -0.5.
#' @param cond_b A character string to be coded  0.5.
#'
#' @return A numeric vector.
#'
#' @details The [`lm`] method supports factor and character variables by dummy
#'   coding them. Dummy coding can make the interpretation of regression
#'   coefficient difficult or at least more difficult than contrast coding.
#'   Contrast-coded-variable coefficients interpretation is particularly useful
#'   when conducting a joint-significance test.
#'
#' @examples
#'   data(ho_et_al)
#'
#'   ho_et_al$condition_contrast <- build_contrast(ho_et_al$condition,
#'                                                 "Low discrimination",
#'                                                 "High discrimination")
#'
#'  head(ho_et_al)
#'
#' @seealso [`scale`] for centering continuous numeric variable.
#'
#' @export
build_contrast <- function(vector, cond_a, cond_b) {
  UseMethod("build_contrast")
}

#' @export
build_contrast.default <- function(vector, cond_a, cond_b) {
  # coercing to character
  vector <- as.character(vector)

  # recode
  vector[vector == cond_a] <- -.5
  vector[vector == cond_b] <-  .5

  # return
  as.numeric(vector)
}

#' @export
build_contrast.character <- function(vector, cond_a, cond_b) {
  # recode
  vector[vector == cond_a] <- -.5
  vector[vector == cond_b] <-  .5

  # return
  as.numeric(vector)
}
