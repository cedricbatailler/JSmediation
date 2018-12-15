# Create an object of class "mediation_model".
# Args:
#   type:
#   params: List of params involved in the model
#   paths: Paths involved in the model (list)
#   indirect_index: Has the indirect index been compute ? (boolean)
# Returns:
#   An object of class "mediation_model".

mediation_model <- function(type,
                            params,
                            paths,
                            indirect_index = FALSE,
                            indirect_index_infos = NULL,
                            js_models,
                            data,
                            subclass) {
  structure(
    list(
      type = type,
      method = "joint significant",
      params = params,
      paths = paths,
      indirect_index = indirect_index,
      indirect_index_infos = indirect_index_infos,
      js_models = js_models,
      data = data
    ),
    class = c(subclass, "mediation_model")
  )
}
