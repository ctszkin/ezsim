\name{merge.ezsim}
\title{Merge two ezsim objects}
\usage{\method{merge}{ezsim} (x, y)}
\description{Merge two ezsim objects. Either \code{m} or \code{parameter_def} of two ezsim objects must be the same. If \code{parameter_def} are the same, the merging is regarded as increasing the number of simulation \code{m}. If the \code{parameter_def} are different and \code{m} are the same, the merging is regarded as extending the \code{parameter_def}.}
\alias{merge.ezsim}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\seealso{\code{\link{ezsim}}}
\arguments{\item{x}{A ezsim to merge with}
\item{y}{A ezsim to merge with}}
\examples{\dontrun{
}}
