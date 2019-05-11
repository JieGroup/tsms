\name{FSARMAfit}
\alias{FSARMA.fit}

\title{
Fit Frequentist Seasonal ARMA modeling
}

\description{
Fit the model with specified parameters.
}

\usage{
FSARMA.fit(X,U,p,q,S,k=2)
}

\arguments{
\item{X}{a vector or matrix data}

\item{U}{n-burnin}

\item{p}{order of AR effects}

\item{q}{order of MA effects}

\item{S}{seasonality periods}

\item{k}{multiplier on 2*(p+q+r). k=2 is AIC}
}

\details{
The model is estimated by BFGS algorithm in optim(). Note that in univariate ARMA estimation, quasi-Newton method usually provide a robust result rather than aggressive ML with second order algorithms. 

The algorithm optimize conditional likelihood based on burned in samples. This is specified by argument U. U has to be greater than p,q,r or any element in S. Thus the maximal value in the ranges for these parameters.

For models that the algorithm doesn't converge, the aic value is recorded as Inf.
}

\value{

\item{U}{n-burnin}

\item{p}{order of AR effects}

\item{phi}{a vector of AR coefficients. =NULL if p=0}

\item{q}{order of MA effects}

\item{psi}{a vector of MA coefficients. =NULL if q=0}

\item{r}{order of SE effects}

\item{S}{seasonality periods}

\item{tau}{a vector of SE coefficients. =NULL if r=0}

\item{sigma}{se of error}

\item{aic}{k=2,aic value}
}

\author{
Tianyang Xie
}


\examples{
library(BSD)

data(AirPassengers)
air = as.matrix(AirPassengers)

t = c(1:nrow(air))
m = lm(air~I(t^1.5))
trend = as.matrix(fitted.values(m))
X = air - trend

U = 14
p = 4
q = 1
S = c(12)
obj = FSARMA.fit(as.matrix(X[1:100,1]),U,p,q,S)

}

