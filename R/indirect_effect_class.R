indirect_effect <- function(type,
                            method = "Monte Carlo",
                            estimate,
                            alpha,
                            iterations,
                            sampling) {
  
  CI <- stats::quantile(
    sampling,
    c(alpha / 2, 1 - alpha / 2)
  )
  contains_zero <- (CI[[1]] < 0 & CI[[2]] > 0)
  
  structure(
    list(
      type          = type,
      method        = method,
      estimate      = estimate,
      CI            = CI,
      alpha         = alpha,
      iterations    = iterations,
      contains_zero = contains_zero,
      sampling      = sampling
    ),
    class = c("indirect_index")
  )
}
