#' @title Adds an indirect effect index to a fitted mediation model
#'
#' @description [`add_index`] is a generic function that adds a
#'   (moderated) indirect effect index to an object created with an `mdt_*`
#'   family function. This index is computed using Monte Carlo methods. This
#'   function invokes particular methods depending of the class of the mediation
#'   model. For example, with a model fitted with [`mdt_simple`],
#'   [`add_index`] will invoke
#'   [`add_index.simple_mediation`].
#'
#' @param mediation_model A mediation model fitted with an `mdt_*` family
#'   function.
#' @param times Number of simulations to use to compute the Monte Carlo index's
#'   confidence interval.
#' @param level Alpha threshold to use for the confidence interval.
#' @param ... Further arguments to be passed to specific methods.
#'
#' @return An object of the same class as `mediation_model`, but with index
#'   added for later use.
#'
#' @export
add_index <- function(mediation_model, times = 5000, level = .05, ...) {
  UseMethod("add_index")
}
