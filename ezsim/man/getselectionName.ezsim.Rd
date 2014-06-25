\name{getselectionName.ezsim}
\title{Get Names of selection Parameters.}
\usage{\method{getselectionName}{ezsim}(x,simple=FALSE,parameters_priority,...)}
\description{Get names of selection parameters from an ezsim object.}
\alias{getselectionName.ezsim}
\value{\item{selection_length_greater_one}{ Name of selection parameters with more than one elements} \item{selection_length_one}{Name of selection parameters with only one element} \item{subtitle}{subtitle for fixed selection parameters}}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\note{For internal use of ezsim.}
\seealso{\code{\link{getselectionName.summary.ezsim}}}
\arguments{\item{x}{an ezsim object}
\item{simple}{If true, return only the name of selection parameters. If False, split the selection into two groups, one with fixed value, one with varied value. Also, subtitle is returned.}
\item{parameters_priority}{Priority in sorting parameters.}
\item{\dots}{unused}}
\examples{\dontrun{
ezsim_basic<-ezsim(
m             = 100,
run           = TRUE,
core          = 1,
display_name  = c(mean_hat="hat(mu)",sd_mean_hat="hat(sigma[hat(mu)])"),
parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
dgp           = function() rnorm(n,mu,sigma),
estimator     = function(x) c(mean_hat = mean(x), 
sd_mean_hat=sd(x)/sqrt(length(x)-1)),
true_value    = function() c(mu, sigma / sqrt(n-1))
)

getSelectionName(ezsim_basic)
getSelectionName(summary(ezsim_basic))
}}
