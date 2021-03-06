% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func.logit.CES.R
\name{logit.sw.cali}
\alias{logit.sw.cali}
\title{logit.sw.cali}
\usage{
logit.sw.cali(q, p, logit.exponent, sw_relative = "sw_share")
}
\arguments{
\item{q}{A vector of quantities in the nest}

\item{p}{A vector of prices in the nest}

\item{logit.exponent}{A numeric value of logit exponent}

\item{sw_relative}{Options of scaling method in share-weights:
"sw_share" returns share-weights summing to 1 and
"sw_default" returns share-weights anchored to the first item}
}
\value{
Calibrated share-weights
}
\description{
Calibrating share-weights to be used in relative cost logit sharing function
}
