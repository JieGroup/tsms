\name{FSARMAauto}
\alias{FSARMA.auto}

\title{
Automatic Best Seasonal ARMA model analysis process
}

\description{
Fit, select, predict on seasonal ARMA modeling formulation. 
}

\usage{
FSARMA.auto (X,U,pred_t=NULL,p_range,q_range,r_range,S_range=NULL,k=2,
                       width,level=0.05,specplot=TRUE)
}

\arguments{
\item{X}{a vector or matrix data}

\item{U}{n-burnin}

\item{pred_t}{n-predict out of samples. =NULL then will not predict}

\item{p_range}{range of p}

\item{q_range}{range of q}

\item{r_range}{range of r}

\item{S_range}{union of possible S. =NULL then will suggest automatically}

\item{k}{multiplier on 2*(p+q+r). k=2 is AIC}

\item{width}{width of moving average to determine S_range}

\item{level}{level of z-score test on spectral spikes}

\item{specplot}{=TRUE, draw the spectrum plot}

}

\details{
The model is estimated by BFGS algorithm in optim(). Note that in univariate ARMA estimation, quasi-Newton method usually provide a robust result rather than aggressive ML with second order algorithms. 

The algorithm optimize conditional likelihood based on burned in samples. This is specified by argument U. U has to be greater than p,q,r or any element in S. Thus the maximal value in the ranges for these parameters.

For models that the algorithm doesn't converge, the aic value is recorded as Inf.

The function make static prediction within observation period, while use dynamic prediction for out sampled period. Due to the property of stationary SARMA process, the out sampled prediction will converge.
}

\value{

\item{obj}{best fitted FSARMA.fit object}

\item{aic_panel}{recorded aic values}

\item{prediction}{a vector recording predicted values. Length=n+pred_t}

\item{p_range}{range of p}

\item{q_range}{range of q}

\item{r_range}{range of r}

\item{S_range}{union of possible S. =NULL then will suggest automatically}

\item{k}{multiplier on 2*(p+q+r). k=2 is AIC}

\item{width}{width of moving average to determine S_range}

\item{level}{level of z-score test on spectral spikes}

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
result = FSARMA.auto(as.matrix(X[1:100,1]),U,pred_t=50,p_range,q_range,r_range,S_range=NULL,k=2,width=5,level=0.05,specplot=TRUE)
prediction = result$prediction
}
