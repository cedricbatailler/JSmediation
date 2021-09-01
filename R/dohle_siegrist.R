#' @title Dohle and Siegrist (2014, Exp 1) illustrating within-subject analysis
#'   (long-format)
#'
#' @description A data set containing data from Dohle and Siegrist (2014)'s
#'   Experiment 1 that can be used to conduct within-subject joint-significance
#'   test. In this experiment, researchers are interested in the effect of name
#'   complexity on willingness to buy a drug. The specific hypothesis is that
#'   complex drug names are perceived as more hazardous, which makes someone
#'   less likely to buy the drug. Researchers used a within-subject design.
#'
#'   This data set is in a long-format, see [`mdt_within`] to conduct
#'   a within-participant mediation analysis with this data set.
#'
#' @format A data frame with 44 rows and 4 variables:
#' \describe{
#'   \item{participant}{Participant number.}
#'   \item{name}{Name of the drugs ("simple" vs. "complex").}
#'   \item{hazardousness}{Mean estimated hazardousness.}
#'   \item{willingness}{Mean willingness to buy.}
#'   }
#'
#' @references Dohle, S., & Siegrist, M. (2014). Fluency of pharmaceutical drug
#'   names predicts perceived hazardousness, assumed side effects and
#'   willingness to buy. *Journal of Health Psychology*, *19*(10),
#'   1241-1249. doi: 10.1177/1359105313488974


#' @usage data("dohle_siegrist")
"dohle_siegrist"
