% Generated by roxygen2 (4.0.2.9000): do not edit by hand
% Please edit documentation in R/hamilton.R
\name{hamilton}
\alias{hamilton}
\title{hamilton
Hamilton: Two-Point correlation Function Estimator (1993)}
\usage{
hamilton(DD, RR, DR)
}
\arguments{
\item{DD,DR,RR:}{numeric arrays of DATA-DATA and DATA-RANDOM counts}
}
\description{
hamilton
Hamilton: Two-Point correlation Function Estimator (1993)
}
\examples{
RESULTS <- hamilton(DD=DD.counts,RR=RR.counts,DR=DR.counts)
}

