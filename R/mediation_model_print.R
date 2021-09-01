#' @title Print method for object of class `mediation_model`
#'
#' @description Print a summary for a mediation model represented by a
#'   `mediation_model` object.
#'
#' @param x      An object of class `mediation_model`.
#' @param digits How many significant digits are to be used for numerics.
#' @param ...    Further arguments.
#'
#' @export
print.mediation_model <- function(x, digits = 3, ...) {

  # object  -------------------------------------------------------------------
  type <- x$type

  params <- x %>% purrr::pluck("params")
  paths  <- x %>% purrr::pluck("paths")
  models <- x %>% purrr::pluck("js_models")

  # summary -------------------------------------------------------------------
  cat(glue("Test of mediation ({type})\n\n"))
  cat("==============================================\n")

  cat("\nVariables:\n\n")

  purrr::map2(params,
              names(params),
              ~ cat(glue("- {.y}: {.x} \n\n")))

  check_variables(x)

  cat("\nPaths:")

  purrr::map2_df(paths,
                as.character(names(paths)),
                ~data.frame(Path = .y,
                            PE   = purrr::pluck(.x, "point_estimate"),
                            SE   = purrr::pluck(.x, "se"),
                            APA  = as.character(purrr::pluck(.x, "APA")),
                            stringsAsFactors = FALSE)) %>%
    knitr::kable(col.names = c("Path", "Point estimate", "SE", "APA"),
                 digits = digits,
                 format = "rst") %>%
    print()

  cat("\nIndirect effect index:\n\n")
  if (! x$indirect_index) {
    cat("Indirect effect index is not computed by default.",
        "Please use add_index() to compute it.",
        sep = "\n")
  } else {
    print(x$indirect_index_infos, digits = digits)
  }

  cat("\nFitted models:\n\n")
  names(models) %>%
    purrr::map(~cat("-", .x, "\n"))
}
