\name{evalFunctionOnParameterDef}
\title{Test Whether a parameterDef Ojbect Work Properly for a dgp.}
\usage{evalFunctionOnParameterDef(x, fun, index=1, ...)}
\description{several set of parameters is generated from parameterDef. Function \code{fun} is evaulated under the \code{index-th} set of parameters and returns its value.}
\alias{evalFunctionOnParameterDef}
\author{TszKin Julian Chan \email{ctszkin@gmail.com}}
\keyword{internal}
\seealso{\code{\link{parameterDef}}}
\arguments{\item{x}{A parameterDef object}
\item{fun}{A function to be evaluated.}
\item{index}{Which set of parameters to use.}
\item{\dots}{unused}}
\examples{par_def<-createParDef()
setScalars(par_def,mean=1,sd=2,n=seq(10,50,10))

evalFunctionOnParameterDef(par_def, function() rnorm(n,mean,sd) )  # 10 random number
evalFunctionOnParameterDef(par_def, function() rnorm(n,mean,sd), index=3)  # 30 random number

## Example 2
par_def <-createParDef()
par_def <- setOthers(par_def,xs=1,b=1)
par_def <- setScalars(par_def,n=seq(20,100,20),es=c(1,10))

dgp<-function(){
x<-rnorm(n,0,xs)
e<-rnorm(n,0,es)
y<-b * x + e
data.frame(y,x)
}
estimator<-function(d){
r<-summary(lm(y~x-1,data=d))
c(b=r$coef[,1],t=(r$coef[,1]-1)/r$coef[,2] )
}

true<-function(){
c(b,(b-1)/(es/sqrt(n)/xs)) 
}
evalFunctionOnParameterDef(par_def,dgp)
estimator(evalFunctionOnParameterDef(par_def,dgp))
evalFunctionOnParameterDef(par_def,true)}
