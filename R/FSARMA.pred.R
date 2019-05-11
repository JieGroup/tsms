FSARMA.pred <-
function(obj,X,pred_t){
  
  # init arguments
  n = nrow(X)
  U = obj[[1]]
  p = obj[[2]]
  phi = obj[[3]]
  q = obj[[4]]
  psi = obj[[5]]
  r = obj[[6]]
  S = obj[[7]]
  tau = obj[[8]]
  sigma = obj[[9]]
  
  pred_X = Pred(X,U,pred_t,
                   as.double(phi),p,
                   as.double(psi),q,
                   as.double(tau), as.integer(S),r)
  
  return(pred_X)
}
