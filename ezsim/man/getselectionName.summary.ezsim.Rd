\name{getselectionName.summary.ezsim}
\title{Get Names of selection Parameters.}
\usage{\method{getselectionName}{summary.ezsim}(x,simple=FALSE,parameters_priority,...)}
\description{Get names of selections parameters from an summary.ezsim object.}
\alias{getselectionName.summary.ezsim}
\value{Names of selection parameters.}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\note{For internal use of ezsim.}
\seealso{\code{\link{getselectionName.ezsim}}}
\arguments{\item{x}{an summary.ezsim object}
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

getselectionName(ezsim_basic)
getselectionName(summary(ezsim_basic))
}}
