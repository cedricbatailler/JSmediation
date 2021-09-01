#' @title Print method for object of class `indirect_index`
#'
#' @description Print a summary for an indirect effect index created with
#'   `add_index()` method.
#'
#' @param x      An object of class `indirect_index`.
#' @param digits How many significant digits are to be used for numerics.
#' @param ...    Further arguments.
#'
#' @export
print.indirect_index <- function(x, digits = 3, ...) {
  cat("- type:", x$type, "\n")
  cat("- point estimate:", format(x$estimate, digits = digits), "\n")
  cat("- confidence interval:\n")
  cat("  - method: ", x$method, " (", x$times, " iterations)\n", sep = "")
  cat("  - level:", x$level, "\n")
  cat("  - CI: [",
      format(x$CI[[1]], digits = digits),
      "; ",
      format(x$CI[[2]], digits = digits),
      "]\n",
      sep = "")
}
