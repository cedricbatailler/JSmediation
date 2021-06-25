#' @export
compute_indirect_effect_for <- function() {
  UseMethod("compute_indirect_effect_for")
}

#' @export
compute_indirect_effect_for.moderated_mediation <- 
  function(mediation_model, 
           M = 0) {

  if (!is.numeric(M)) {
    rlang::abort("`M` argument must be numeric.")
  }

    if (length(M) != 1) {
      rlang::abort("`M` argument must be a single numeric value.")
    }

}
