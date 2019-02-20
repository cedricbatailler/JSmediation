#' @section Models: For within-participant mediation, three models will be
#'   fitted:
#'
#'   - \eqn{Y_{2i} - Y_{1i} = c_{11}}{Y2i - Y1i = c11}
#'   - \eqn{M_{2i} - M_{1i} = a_{21}}{M2i - M1i = a21}
#'   - \eqn{Y_{2i} - Y_{1i} = c'_{31} + b_{32}(M_{2i} - M_{1i}) +
#'   d_{33}[0.5(M_{1i} + M_{2i}) - 0.5(\overline{M_{1} + M_{2}})]}{Y2i - Y1i =
#'   c'31 + b32 * (M2i + M1i) + d33 * [0.5 * (M1i + M2i) - 0.5 * mean(M1 + M2)]}
#'
#'   with \eqn{Y_{2i} - Y_{1i}}{Y2i - Y1i} the difference score between DV
#'   conditions for the outcome variable for the \emph{i}th observation,
#'   \eqn{M_{2i} - M_{1i}}{M2i - M1i} the difference score between DV conditions
#'   for the mediator variable for the \emph{i}th observation, \eqn{M_{1i} +
#'   M_{2i}}{M1i + M2i} the sum of mediator variables values for DV conditions
#'   for the \emph{i}th observation, and \eqn{\overline{M_{1} + M_{2}}}{mean(M1i
#'   + M2i)} the mean sum of mediator variables values for DV conditions across
#'   observations (see Montoya & Hayes, 2011).
#'
#'   Coefficients associated with \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{c'} paths
#'   are respectively \eqn{a_{21}}{a21}, \eqn{b_{32}}{b32}, \eqn{c_{11}}{c11},
#'   and \eqn{c'_{31}}{c'31}.
