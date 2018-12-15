#' @title Data set showing within-subject analysis (long-format)
#'
#' @description A data set containing data from Dohle and Siegrist (2014)'s
#'   Experiment 1 that can be used to conduct within-subject joint-significance
#'   test. In this experiment, authors are intrested in the effect of name
#'   complexity on willingness to buy a drug. The specific hypothesis would be
#'   that complex drug name would be percieved as more hazardous, which would in
#'   turn make someone less likely to buy the drug. Because in real life, one
#'   have to choose between several drugs, this experiment where name complexity
#'   was manipulated and where hazardousness and willingness to buy were
#'   measured was conducted in a within-subject design.
#'
#'   This data set is in a long-format, see \code{\link{mdt_within}} to conduct
#'   a within-participant mediation analysis with this dataset.
#'
#' @format A data frame with 44 rows and 4 variables: \describe{
#'   \item{participant}{Participant number.} \item{name}{Name of the drugs
#'   ("simple" vs. "complex").} \item{hazardousness}{Mean estimated
#'   hazardousness.} \item{willingness}{Mean willigness to buy.} }
#'
#' @references Dohle, S., & Siegrist, M. (2014). Fluency of pharmaceutical drug
#'   names predicts perceived hazardousness, assumed side effects and
#'   willingness to buy. \emph{Journal of Health Psychology}, \emph{19}(10),
#'   1241‑1249. doi: 10.1177/1359105313488974


#' @usage data("dohle_siegrist")
"dohle_siegrist"
