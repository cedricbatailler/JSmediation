% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/display_models.R
\name{display_models}
\alias{display_models}
\title{Displays models from a mediation object}
\usage{
display_models(mediation_model)
}
\arguments{
\item{mediation_model}{An object of class \code{mediation_model}.}
}
\value{
A list of \code{summary.lm} objects.
}
\description{
When conducting a joint-significance test, different models are
fitted to the data. This function helps you see a summary of the models that
have been used in an object of class \code{mediation_model}.
}
\examples{
data(ho_et_al)
ho_et_al$condition_c <- build_contrast(ho_et_al$condition,
                                       "Low discrimination",
                                       "High discrimination")
my_model <-
  mdt_simple(data = ho_et_al,
             IV = condition_c,
             DV = hypodescent,
             M = linkedfate)

display_models(my_model)

}
