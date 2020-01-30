# Performs tests on a mediation_model object. Variable coding could impact
# underlying linear model for mediation model like moderated mediation,
# check_variables
# Generic. Used for its side effect.
# Args:
#   model: a mediation_model object
check_variables <- function(model) {
  UseMethod("check_variables")
}

# Default method for check_variables, do not perform any test.
check_variables.default <- function(model) {
  NULL
}

# Method for moderated_mediation class. Checks whether IV, Mediator, or
# Moderator is either a contrast-coded or a centered variable. Throws a
# message if it is not the case.
check_variables.moderated_mediation <- function(model) {
  IV_n <- purrr::pluck(model, "params", "IV")
  M_n <- purrr::pluck(model, "params", "M")
  Mod_n <- purrr::pluck(model, "params", "Mod")

  Var_n <-
    list(
      IV_n,
      M_n,
      Mod_n
    )

  Var_check <-
    Var_n %>%
    purrr::map(~ access_data(model, .x)) %>%
    purrr::map_lgl(~ (is_centered(.x) | is_contrast(.x)))

  Var_n_check <-
    Var_n[!Var_check]

  if (length(Var_n_check) != 0) {
    message("\nMessage:")
    message(
"It appears that the following variables are not contrast-coded or centered, 
please make sure it is intended as regression coefficients depends on the 
variables' coding:"
    )
    for (var in Var_n_check) {
      message(glue::glue("* {var}"))
    }
  }
}
