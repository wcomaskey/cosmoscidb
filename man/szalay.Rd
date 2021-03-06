% Generated by roxygen2 (4.0.2.9000): do not edit by hand
% Please edit documentation in R/szalay.R
\name{szalay}
\alias{szalay}
\title{szalay
Landy & Szalay: Two-Point correlation Function Estimator (1993)}
\usage{
szalay(DD, RR, DR, ND, NR)
}
\arguments{
\item{DD,DR,RR:}{numeric arrays of DATA-DATA and DATA-RANDOM counts
ND,NR: integer values of the size of the DATA and RANDOM sets respectively}
}
\description{
szalay
Landy & Szalay: Two-Point correlation Function Estimator (1993)
}
\examples{
RESULTS <- szalay(DD=DD.counts,RR=RR.counts,DR=DR.counts,ND=100,NR=1000)
}

