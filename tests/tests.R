library(tsms)

data(AirPassengers)
air = as.matrix(AirPassengers)

t = c(1:nrow(air))
m = lm(air~I(t^1.5))
trend = as.matrix(fitted.values(m))
X = air - trend

# 1.FSARMA.fit: Fit model by specifyied parameters
U = 14
p = 4
q = 1
S = c(12)
obj = FSARMA.fit(as.matrix(X[1:100,1]),U,p,q,S)

# 2.FSARMA.select: Fit multiple models and select by aic
p_range = c(0:5)
q_range = c(0:3)
S_range = c(6,8,10,12)
r_range = c(0,1,2)
obj = FSARMA.select(as.matrix(X[1:100,1]),U,p_range,q_range,r_range,S_range)

# 3.FSARMA.predict: Prediction
prediction = FSARMA.pred(obj[[1]],as.matrix(X[1:100]),50)

# 4.FSARMA.auto: fit & select & predict in all (S_range is automatically suggestted)
result = FSARMA.auto(as.matrix(X[1:100,1]),U,pred_t=50,p_range,q_range,r_range,S_range=NULL,k=2,width=5,level=0.05,specplot=TRUE)
prediction = result$prediction