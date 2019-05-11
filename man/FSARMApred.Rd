\name{FSARMApred}
\alias{FSARMA.pred}

\title{
Predict on fitted Frequentist Seasonal ARMA modeling
}

\description{
Predict on fitted Frequentist Seasonal ARMA modeling.
}

\usage{
FSARMA.pred(obj,X,pred_t)
}

\arguments{

\item{obj}{best fitted FSARMA.fit object}

\item{X}{a vector or matrix data}

\item{pred_t}{n-outsample prediction}
}

\details{
The function make static prediction within observation period, while use dynamic prediction for out sampled period. Due to the property of stationary SARMA process, the out sampled prediction will converge.
}

\value{

\item{pred_X}{a vector recording predicted values. Length=n+pred_t}
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
prediction = FSARMA.pred(obj,as.matrix(X[1:100]),50)

}

