#' @details With within-participant mediation analysis, one tests whether the
#'   effect of \eqn{X} on \eqn{Y} goes through a third variable \eqn{M}. The
#'   specificity of within-participant mediation analysis lies in the
#'   repeated-measure design it relies on. With such design, each sampled unit
#'   (e.g., participant) is measured on the dependent variable \eqn{Y} and the
#'   mediator \eqn{M} in the two conditions of \eqn{X}. The hypothesis behind
#'   this test is that \eqn{X} has an effect on \eqn{M} (\eqn{a}) which has an
#'   effect on \eqn{Y} (\eqn{b}), meaning that \eqn{X} has an indirect effect on
#'   \eqn{Y} through \eqn{M}.
#'
#'   As with simple mediation, the total effect of \eqn{X} on \eqn{Y} can be
#'   conceptually described as follows:
#'
#'   \deqn{c = c' + ab}
#'
#'   with \eqn{c} the total effect of \eqn{X} on \eqn{Y}, \eqn{c'} the direct of
#'   \eqn{X} on \eqn{Y}, and \eqn{ab} the indirect effect of \eqn{X} on \eqn{Y}
#'   through {M} (see Models section).
#'
#'   To assess whether the indirect effect is different from the null, one has
#'   to assess the significance against the null for both \eqn{a} (the effect of
#'   \eqn{X} on \eqn{M}) and \eqn{b} (effect of \eqn{M} on \eqn{Y} controlling
#'   for the effect of \eqn{X}). Both \eqn{a} and \eqn{b} need to be
#'   simultaneously significant for an indirect effect to be claimed (Judd,
#'   Kenny, & McClelland, 2001; Montoya & Hayes, 2011).
