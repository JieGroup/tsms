FSARMA.select <- function(X,U,p_range,q_range,r_range,S_range,k=2){

  # construct para_panel
  S_combn = 1
  for (j in 1:tail(r_range,1)){S_combn=S_combn+ncol(combn(S_range,j))}
  para_panel = as.data.frame(matrix(0,nrow=length(p_range)*length(q_range)*S_combn,ncol=4))
  colnames(para_panel) = c('p','q','S','aic')
  para_panel[,1] = rep(p_range,rep(length(q_range)*S_combn,length(p_range)))
  para_panel[,2] = rep(rep(q_range,rep(S_combn,length(q_range))),length(p_range))
  S_panel = as.data.frame(matrix(nrow=1,ncol=1))
  for (j in 1:tail(r_range,1)){
    S_comb = as.matrix(apply(t(combn(S_range,j)),1,paste,collapse=' '))
    S_panel = rbind(S_panel,S_comb)
  }
  para_panel[,3] = rep(S_panel[,1],length(p_range)*length(q_range))
  para_panel = para_panel[-1,]
  
  valid_index = c()
  for (i in 1:nrow(para_panel)){
    temp_row = para_panel[i,]
    if (is.na(temp_row[1,3])){S=NULL
    }else{S = as.integer(strsplit(temp_row[1,3],' ')[[1]])}
    r = length(S)
    if (r%in%r_range){
      valid_index=c(valid_index,i)
    }
  }
  para_panel = para_panel[valid_index,]
  
  # # Fit:Parallel
  # ncores = detectCores()
  # cl = makePSOCKcluster(ncores)
  # l = floor(nrow(para_panel)/ncores)
  # assign_loc = rep(l,ncores)
  # if (nrow(para_panel)%%ncores!=0){
  #   for (i in 1:(nrow(para_panel)-l*ncores)){
  #     assign_loc[i] = assign_loc[i] + 1
  #   }
  # }
  # assign_list = list()
  # assign_loc_cums = cumsum(assign_loc)
  # a = 1
  # for (i in 1:ncores){
  #   assign_list[[i]] = c(a:assign_loc_cums[i])
  #   a = 1 + assign_loc_cums[i]
  # }
  # 
  # singleFit = function(core_ind,assign_list,para_panel){
  #   index_set = assign_list[[core_ind]]
  #   aic_set = rep(0,length(index_set))
  #   for (i in 1:length(index_set)){
  #     index = index_set[i]
  #     para = para_panel[index,]
  #     p = para[1,1]
  #     q = para[1,2]
  #     if (is.na(para[1,3])){S=NULL
  #     }else{S = as.integer(strsplit(para[1,3],' ')[[1]])}
  #     r = length(S)
  #     aic = FSARMA.fit(X,U,p,q,S,k)$aic
  #     aic_set[i] = aic
  #   }
  #   return(aic_set)
  # }
  # 
  # clusterExport(cl, list("FSARMA.fit", "singleFit", "assign_list","para_panel","X","U","k")
  #               , envir=environment())
  # pout = parLapply(cl, c(1:ncores), singleFit, assign_list=assign_list
  #                  ,para_panel=para_panel)
  # aic_col = unlist(pout)
  # para_panel[,4] = aic_col
  
  # # Fit:single core
  for (i in 1:nrow(para_panel)){
    temp_row = para_panel[i,]
    p = temp_row[1,1]
    q = temp_row[1,2]
    if (is.na(temp_row[1,3])){S=NULL
    }else{S = as.integer(strsplit(temp_row[1,3],' ')[[1]])}
    r = length(S)

    m = FSARMA.fit(X,U,p,q,S,k)
    para_panel[i,4] = m$aic
  }
  
  # Select
  best.row = para_panel[which.min(para_panel[,4]),1:3]
  p = best.row[1,1]
  q = best.row[1,2]
  if (is.na(best.row[1,3])){S=NULL
  }else{S = as.integer(strsplit(best.row[1,3],' ')[[1]])}
  best.m = FSARMA.fit(X,U,p,q,S,k)
  
  # output
  output = list(obj=best.m,aic_panel=para_panel)
  return(output)
}
