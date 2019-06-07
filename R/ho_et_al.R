#' @title Data set showing simple and moderated mediation analysis
#'
#' @description A data set containing data from Experiment 3 from Ho, Kteiley,
#'   and Chen (2017). In this experiment, the authors hypothesized that
#'   presenting a text stating that Black-White biracials were discriminated
#'   against would lead Black participants to associate Black-White biracials
#'   more with their lower status parent group than their higher status parent
#'   group, according to the rule of \emph{hypodescent}. In this experiment, the
#'   authors tested if this effect was mediated by the sense of linked fate
#'   between discriminated Black-White biracials and Black participants.
#'
#'   Note that this data set does not include the participants who were in the
#'   discrimination control condition in the study conducted by Ho, Kteiley and
#'   Chen (2017).
#'
#'   See \code{\link{mdt_simple}} and \code{\link{mdt_moderated}} to conduct a
#'   simple mediation or a moderated mediation analysis with this dataset.
#'   
#' @format A data frame with 824 rows and 5 variables: 
#' \describe{ 
#'   \item{id}{An incremental index.} 
#'   \item{condition}{Experimental condition (High discrimination vs. Low
#'   discrimination).} 
#'   \item{sdo}{Score at an SDO scale.}
#'   \item{linkedfate}{Score at an 8-item linked fate measure.}
#'   \item{hypodescent}{Score at a 3-item measure of hypodescent.} 
#'   }
#'
#' @references Ho, A. K., Kteily, N. S., & Chen, J. M. (2017). “You’re one of
#'   us”: Black Americans’ use of hypodescent and its association with
#'   egalitarianism. \emph{Journal of Personality and Social Psychology},
#'   \emph{113}(5), 753-768. doi: 10.1037/pspi0000107
#'   
#' @usage data("ho_et_al")
"ho_et_al"
