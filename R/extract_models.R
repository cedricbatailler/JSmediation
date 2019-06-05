#' @title Extracts models from a mediation_model object
#'
#' @description When conducting a joint-significant test, different models are
#'   fitted to the data. This function helps accessing the models used in an
#'   object of class \code{mediation_model}.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#'
#' @return A list of \code{lm} objects.
#' 
#' @family extract functions
#'
#' @examples
#' data(ho_et_al)
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' my_model <-
#'   mdt_simple(data = ho_et_al,
#'              IV = condition_c,
#'              DV = hypodescent,
#'              M = linkedfate)
#'
#' extract_models(my_model)
#'
#' @export

extract_models <- function(mediation_model) {
  UseMethod("extract_models")
}

#' @export

extract_models.mediation_model <- function(mediation_model) {
    purrr::pluck(mediation_model, "js_models")
}

#' @title Extracts a single model from a mediation_model object
#'
#' @description When conducting a joint-significant test, different models are
#'   fitted to the data. This function helps you access the models used in an
#'   object of class \code{mediation_model}.
#'
#' @seealso \code{\link{extract_models}} to access a list of every model
#'   relevant to joint-significance testing.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#' @param step An integer or a string corresponding to the model to extract.
#'
#' @return An \code{lm} object.
#'
#' @family extract functions
#' 
#' @examples
#' data(ho_et_al)
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' my_model <-
#'   mdt_simple(data = ho_et_al,
#'              IV = condition_c,
#'              DV = hypodescent,
#'              M = linkedfate)
#'
#' extract_model(my_model, step = "X -> M")
#'
#' @export

extract_model <- function(mediation_model, step = NULL) {
  UseMethod("extract_model")
}

#' @export

extract_model.mediation_model <- function(mediation_model, step = NULL) {
  if (is.null(step)) {
    error_message <-
      "You forgot to specify the `step` argument.
    Did you mean to use extract_models() instead?"

    stop(call. = FALSE, error_message)
  }
  else {
    purrr::pluck(mediation_model, "js_models", step)
  }
}

#' @title Extracts models from a mediation object as a data frame
#'
#' @description When conducting a joint significant test, different models are
#' fitted to the data. This function helps you access the models  used in an
#' object of class \code{mediation_model}.
#'
#' @param mediation_model An object of class \code{mediation_model}.
#'
#' @return A data frame.
#'
#' @family extract functions
#'
#' @examples
#' data(ho_et_al)
#' ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
#'                                        "Low discrimination",
#'                                        "High discrimination")
#' my_model <-
#'   mdt_simple(data = ho_et_al,
#'              IV = condition_c,
#'              DV = hypodescent,
#'              M = linkedfate)
#'
#' extract_tidy_models(my_model)
#' 
#' @export

extract_tidy_models <- function(mediation_model) {
  UseMethod("extract_tidy_models")
}

#' @export

extract_tidy_models.mediation_model <- function(mediation_model) {
  purrr::pluck(mediation_model, "js_models") %>%
    purrr::map_df(~broom::tidy(.x), .id = "model")

}
