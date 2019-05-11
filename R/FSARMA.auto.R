FSARMA.auto <-
function(X,U,pred_t=NULL,p_range,q_range,r_range,S_range=NULL,k=2,
                       width,level=0.05,specplot=TRUE){
  
  # step 1 : prepare
  X =as.matrix(X)
  if (is.null(S_range)){
    trend = Moving(X,width)
    S_range = Suggest(trend,level,specplot)
    S_range = S_range[S_range<=U]}
  
  # step 2 : genearte table & select
  select = FSARMA.select(X,U,p_range,q_range,r_range,S_range,k)
  best.m = select[[1]]
  aic_panel = select[[2]]
  
  # step 3 : predict (or not)
  if (is.null(pred_t)){pred=NULL
  }else{pred=FSARMA.pred(best.m,X,pred_t)}
  
  # step 4 : conclude and output
  output = list(obj = best.m,
                aic_panel = aic_panel,
                prediction = pred,
                S_range = S_range,
                r_range = r_range,
                p_range = p_range,
                q_range = q_range,
                k = k,
                width = width,
                level = level)
  return(output)
}
