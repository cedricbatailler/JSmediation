indirect_effect <- function(type,
                            method = "Monte Carlo",
                            estimate,
                            level,
                            times,
                            sampling) {

  CI <- stats::quantile(
    sampling,
    c(level / 2, 1 - level / 2)
  )
  contains_zero <- (CI[[1]] < 0 & CI[[2]] > 0)

  structure(
    list(
      type          = type,
      method        = method,
      estimate      = estimate,
      CI            = CI,
      level         = level,
      times         = times,
      contains_zero = contains_zero,
      sampling      = sampling
    ),
    class = c("indirect_index")
  )
}
