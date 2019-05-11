likelihood <-
function(X,U,phi,psi,tau,S,sigma=1){
  X = as.vector(X)
  p = length(phi)
  q = length(psi)
  r = length(tau)
  loglikeli = likeli(X,U,
                  as.double(phi),p,
                  as.double(psi),q,
                  as.double(tau),as.integer(S),r,
                  sigma)
  return(loglikeli)
}
