\name{FSARMAselect}
\alias{FSARMA.select}

\title{
Select Best Frequentist Seasonal ARMA modeling via AIC
}

\description{
Select Best Frequentist Seasonal ARMA modeling via AIC.
}

\usage{
FSARMA.select(X,U,p_range,q_range,r_range,S_range,k=2)
}

\arguments{
\item{X}{a vector or matrix data}

\item{U}{n-burnin}

\item{p_range}{range of p}

\item{q_range}{range of q}

\item{r_range}{range of r}

\item{S_range}{union of possible S}

\item{k}{multiplier on 2*(p+q+r). k=2 is AIC}
}

\details{
The model is estimated by BFGS algorithm in optim(). Note that in univariate ARMA estimation, quasi-Newton method usually provide a robust result rather than aggressive ML with second order algorithms. 

The algorithm optimize conditional likelihood based on burned in samples. This is specified by argument U. U has to be greater than p,q,r or any element in S. Thus the maximal value in the ranges for these parameters.

For models that the algorithm doesn't converge, the aic value is recorded as Inf.
}

\value{

\item{obj}{best fitted FSARMA.fit object}

\item{aic_panel}{recorded aic values}
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
p_range = c(0:5)
q_range = c(0:3)
S_range = c(6,8,10,12)
r_range = c(0,1,2)
obj = FSARMA.select(as.matrix(X[1:100,1]),U,p_range,q_range,r_range,S_range)

}

