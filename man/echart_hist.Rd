% Please edit documentation in R/echart_hist.R
\name{echart_hist}
\alias{echart_hist}
\title{Create an histogram}
\usage{
echart_hist(data, binwidth = NULL)
}
\arguments{
\item{a}{numeric vector}
}
\description{
Create an histogram
}
\examples{
library(recharts)
echart_hist(rnorm(100))
}
