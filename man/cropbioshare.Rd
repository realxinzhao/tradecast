% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataproc.OECD_FAO.R
\name{cropbioshare}
\alias{cropbioshare}
\title{dataproc.OECD_FAO}
\usage{
cropbioshare(
  oecddatadir = paste0(system.file("extdata", package = "tradecast", mustWork = T),
    "/OECD_FAO/")
)
}
\arguments{
\item{oecddatadir}{path to data folder (default = "./inst/extdata/OECD_FAO/")}
}
\value{
the share of crops used for biofuels
}
\description{
processes OECD_FAO raw data to get historical biofuel feedstock data for tradecast
}
\author{
Xin Zhao 2021
}
