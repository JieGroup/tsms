FSARMA.fit <-
function(X,U,p,q,S,k=2){
  
  r = length(S)
  
  Rfit = function(beta){
    SS = fit(X,U,beta,p,q,S,r)
    return(SS)
  }
  Rgrad = function(beta){
    gradient = grad(X,U,beta,p,q,S,r)
    return(gradient)
  }
  
  ult = tryCatch({
    beta = rep(0,p+q+r)
    beta = optim(beta,Rfit,Rgrad,method='BFGS')$par
    if (p==0){phi=NULL
    }else{phi=beta[1:p]}
    if (q==0){psi=NULL
    }else{psi=beta[(p+1):(p+q)]}
    if (r==0){tau=NULL
    }else{tau=beta[(p+q+1):(p+q+r)]}
    sigma = sqrt(Rfit(beta)/(nrow(X)-U))
    mdl = likelihood(X,U,phi,psi,tau,S,sigma)
    aic =  - 2*mdl + k*(p+q+r+1)
    ult = list(U,p,phi,q,psi,r,S,tau,sigma,aic)
  },error=function(cond){
    if (p>0){phi=rep(0,p)}
    else{phi=NULL}
    if (q>0){psi=rep(0,q)}
    else{psi=NULL}
    if (r>0){tau=rep(0,r)}
    else{tau=NULL}
    sigma = 1
    aic = Inf
    ult = list(U,p,phi,q,psi,r,S,tau,sigma,aic)
    message(paste(c("Failure of fitting for 'p,q,S' as:",p,' ',q,' ',S)))
    return(ult)
  })
  
  names(ult) = c('U','p','phi','q','psi','r','S','tau','sigma','aic')
  return(ult)
  
}
