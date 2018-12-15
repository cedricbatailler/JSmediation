# Create an object of class "mediation_model" and performs type check so
# that every subcomponent is the class it is suppose to be.
# Args:
#   type:
#   params: List of params involved in the model
#   paths: Paths involved in the model (list)
#   indirect_index: Has the indirect index been compute ? (boolean)
# Returns:
#   An object of class "mediation_model".

create_path <- function(model_list, model_name, term_name) {

  model <- purrr::pluck(model_list, model_name)
  tidy_model <- broom::tidy(model)
  term_to_keep <- purrr::pluck(tidy_model, "term") == term_name

  tidy_term <- dplyr::filter(tidy_model, term_to_keep)

  list(
    point_estimate = purrr::pluck(tidy_term, "estimate"),
    se = purrr::pluck(tidy_term, "std.error"),
    APA = apastylr(model, term_name)
  )
}

