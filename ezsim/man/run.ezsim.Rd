\name{run.ezsim}
\title{Run the Simulation}
\usage{\method{run}{ezsim} (x)}
\description{Run the Simulation of an ezsim object. The simulation result is store into the ezsim object in the argument directly, reassignment is not needed.}
\alias{run.ezsim}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\arguments{\item{x}{An ezsim object}
\item{core}{Number of core to be used in parallel computing. Default is 1.}
\item{auto_save}{Number of auto save during the simulation.}
\item{create_worker}{If TRUE, create worker for parallelization automatically.}
\item{\dots}{unused}}
\examples{\dontrun{
ezsim_basic<-ezsim(
m             = 100,
run           = FALSE,
display_name  = c(mean_hat="hat(mu)",sd_mean_hat="hat(sigma[hat(mu)])"),
parameter_def = createParDef(list(n=seq(20,80,20),mu=c(0,2),sigma=c(1,3,5))),
dgp           = function() rnorm(n,mu,sigma),
estimator     = function(x) c(mean_hat = mean(x), 
sd_mean_hat=sd(x)/sqrt(length(x)-1)),
true_value    = function() c(mu, sigma / sqrt(n-1))
)
run(ezsim_basic)
}}
