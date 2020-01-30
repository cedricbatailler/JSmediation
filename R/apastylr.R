#' @title Creates an APA formatted report from a significance test
#'
#' @description Create an APA formatted report from the test of a specific term
#'   in a linear model.
#'
#' @param model A linear model created using \code{lm()}.
#' @param term A character string representing a term in the linear model.
#'
#' @return An APA formatted character string.
#'
#' @examples
#'
#' data(ho_et_al)
#' test <- lm(hypodescent ~ linkedfate, ho_et_al)
#' apastylr(test, "linkedfate")
#'
#' @export
apastylr <- function(model, term) {
  UseMethod("apastylr")
}

#' @export
apastylr.lm <- function(model, term) {

  summary_statistics <-
    broom::tidy(model)

  if(nrow(summary_statistics[summary_statistics$term == term, ]) == 0)
     stop(glue::glue("Could not find \"{term}\" term in the model."),
          call. = FALSE)

  summary_statistics <- summary_statistics[summary_statistics$term == term, ]

  t <-
    format(abs(round(summary_statistics$statistic[1], 2)), nsmall = 2)

  df <-
    model$df.residual
  
  pvalue <-
    summary_statistics$p.value[1]
  
  as.character(
    glue::glue("t({df}) = {t}, p {p}",
               p = ifelse(
                 pvalue < .001,
                 "< .001",
                 sub(
                   ".",
                   "= ",
                   format(round(summary_statistics$p.value, 3),
                          nsmall = 3
                   )
                 )
               )
    )
  )
  
}
