# Checks if the input can be considered as a contrast-coded vector.
# Args:
#   x: a numeric vector
# Returns:
#   A boolean.
is_contrast <- function(x) {
  length(unique(x)) == 2L & sum(unique(x)) == 0
}

# Checks if the input can be considered as a centered vector.
# Args:
#   x: a numeric vector
# Returns:
#   A boolean.
is_centered <- function(x) {
  # implementation to deal with floating number rounding error
  isTRUE(all.equal(mean(x), 0))
}



access_data <- function(mediation_model, variable) {
  variable_q <- enquo(variable)

  purrr::pluck(mediation_model, "data") %>%
    dplyr::pull( !! variable_q )
}
