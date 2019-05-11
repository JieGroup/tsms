# Suggest function, provide significant S range
Suggest = function(X,level=0.05,specplot=TRUE){
  
  spec = spectrum(X,log='no',plot=FALSE)
  spx = spec$freq
  spy = 2*spec$spec
  
  z.score = scale(spy,scale=TRUE)
  p.value = 2*(1-pnorm(abs(z.score)))
  S = 1/spx[p.value<=level]
  
  if (specplot==TRUE){
    plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")
    
    if (length(S)>0){
      S.list = list()
      temp_S = c()
      j = 1
      num = 1
      for (i in c(1:length(S))){
        temp_S[j] = S[i]
        j = j + 1
        if (!(S[i]+1)%in%S){
          S.list[[num]] = temp_S
          temp_S = c()
          num = num + 1
          j = 1
        }
      }
      for (n in c(1:(num-1))){
        temp.X = S.list[[n]]
        if (length(temp.X)==1){
          points(spy[match(1/temp.X,spx)]~spx[match(1/temp.X,spx)],col='red')
        }
        else{lines(spy[match(1/temp.X,spx)]~spx[match(1/temp.X,spx)],col='red')}

      }
    }
  }
  return(unique(sort(round(S))))
}