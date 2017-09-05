library(roll)
library(smooth)
library(flux)
library(tidyr)
library(covmat, quietly = TRUE)  
library(RMTstat, quietly = TRUE)  
library(doParallel, quietly = TRUE)

time_chunk_split <- function(y,chunk=20){
  l =  floor(length(y)/chunk)
  ind = matrix(1,nrow = l,ncol = chunk)
  for(i in 1:chunk){
    ind[,i] = ((i-1)*l+1):(i*l)
  }
  return(ind)
  
}




computeRollingVal <- function(series, ord1 = 15,ord2 = 300, doPlot=FALSE){
  s_ord1 = sma(data = series, order = ord1,silent = "all")$fitted  
  s_ord2 = sma(data = series,order= ord2,silent = "all")$fitted
  
  # s_ord1 = forecast::ma(x = series,order=ord1)
  # s_ord2 = forecast::ma(x = series,order=ord2)
  sd_ord2 = roll::roll_sd(as.matrix(series),width = ord2)
  res = array((s_ord1-s_ord2))/array(sd_ord2)
  
  if(doPlot){
    par(mfrow=c(4,1))
    plot(s_ord1)
    plot(s_ord2)
    plot(sd_ord2)
    plot(res)
  }
  return(res)
}


roc_computation = function(true_pos,false_pos, cancelFinalAdvantage = FALSE){
  df = data.frame(true_pos = as.vector(true_pos), false_pos = as.vector(false_pos))
  df = df[order(false_pos, true_pos),]
  
  if(cancelFinalAdvantage){
    lp = df[dim(df)[1],]
    lp$false_pos = lp$false_pos+0.0001
    lp$true_pos = lp$false_pos
    df = rbind(c(0,0),df,lp,c(1,1))
  }else{
    df = rbind(c(0,0),df,c(1,1))
    
  }
  
  p = ggplot(df, aes(x = false_pos)) +
    geom_line(aes(y=true_pos,color="roc")) +
    geom_abline(slope = 1,intercept = 0,color="blue")+
    geom_point(aes(y=true_pos),size=0.00001,color="red")+
    scale_x_continuous(limits = c(0, 1))+
    geom_area(aes(y = true_pos), alpha = 0.1)
  
  
  area = auc(x = df$false_pos, y = df$true_pos, thresh = NULL, dens = 100)
  
  res = list(df = df,plot = p,auc = area)
  
  class(res) = "roc_obj"
  
  return(res)
}

roc_computation <- function(roc1, roc2, label = c("model 1","model 2")){
  m1 = roc1$df$false_pos[length(roc1$df$false_pos)-1]
  m2 = roc2$df$false_pos[length(roc2$df$false_pos)-1]
  
  if(dim(roc1$df)[1]>dim(roc2$df)[1]){
    d = as.double(dim(roc1$df)[1])-as.double(dim(roc2$df)[1])
    for(i in 1:d){
      roc2$df = rbind(roc2$df,c(1,1)) 
    }
  }else{
    d = as.double(dim(roc2$df)[1])-as.double(dim(roc1$df)[1])
    for(i in 1:d){
      roc1$df = rbind(roc1$df,c(1,1)) 
    }
  }
  
  df = cbind(roc1$df,roc2$df)
  colnames(df) = c("true_pos_1","false_pos_1","true_pos_2","false_pos_2")
  p1 = ggplot(df, aes(x = false_pos_1)) +
    geom_line(aes(x = false_pos_1,y=true_pos_1,color="roc 1")) +
    geom_line(aes(x = false_pos_2,y=true_pos_2,color="roc 2")) +
    geom_abline(slope = 1,intercept = 0,color="blue")+
    geom_point(aes(y=true_pos_1),size=0.00001,color="red")+
    geom_point(aes(y=true_pos_2),size=0.00001,color="red")+
    scale_x_continuous(limits = c(0, 1))+
    geom_area(aes(y = true_pos_1), alpha = 0.1) +
    geom_area(aes(y = true_pos_2), alpha = 0.1)
  
  id = min(which(df$false_pos_1==m1),which(df$false_pos_2==m2))
  df2 = df[1:id,]
  
  p2 = ggplot(df2, aes(x = false_pos_1)) +
    geom_line(aes(x = false_pos_1,y=true_pos_1,color=label[1])) +
    geom_line(aes(x = false_pos_2,y=true_pos_2,color=label[2])) +
    geom_abline(slope = 1,intercept = 0,color="black",alpha=0.2)+
    geom_point(aes(x = false_pos_1,y=true_pos_1),size=0.00001,color="red")+
    geom_point(aes(x = false_pos_2,y=true_pos_2),size=0.00001,color="red")+
    geom_ribbon(aes(ymin=0,ymax=true_pos_1),alpha = 0.1) +
    geom_ribbon(aes(x=false_pos_2,ymin=0,ymax=true_pos_2),alpha = 0.1) +
    geom_hline(yintercept = max(df2$true_pos_1),color = "red",alpha=0.3)+
    geom_hline(yintercept = max(df2$true_pos_2),color = "blue",alpha=0.3) +
    geom_vline(xintercept = df2$false_pos_1[which(df2$true_pos_1==max(df2$true_pos_1))],color = "red",alpha=0.3)+
    geom_vline(xintercept = df2$false_pos_2[which(df2$true_pos_2==max(df2$true_pos_2))],color = "blue",alpha=0.3)+
    scale_y_continuous(limits = c(0,max(max(df2$true_pos_1),max(df2$true_pos_2))*1.1))
  
  res = list(pRoc = p,p_comp = p2)
  class(res) = "roc_comparaison"
  
  return(res)
}
  

# Analysing correct_pred position -----------------------------------------

position_of_prediction <- function(pred_position,X,y){
  L = NULL
  for(l in pred_position){
    L = c(L,l)  
  }
  rm(l)
  hist(L)
  
  df = data.frame(date = as.Date(unique(L)), count = NA)
  head(df)
  for(i in 1:dim(df)[1]){
    date = df$date[i]
    df[i,2] = length(L[which(L==date)])
  }
  
  
  df2 = data.frame(date=index(y), coredata(y))
  df = merge(x = df2,y = df,all.x = TRUE)
  rm(df2)
  head(df)
  df$percentage=df$count/sum(df$count[!is.na(df$count)])
  
  # df = 
  df3 = data.frame(date=index(X), coredata(X))
  head(df3)
  df = merge(x=df,y=df3,all.x=TRUE,by = "date")
  df$roro_scale = (df$roro_equ-mean(df$roro_equ[which(!is.na(df$roro_equ))]))/(70*sd(df$roro_equ[which(!is.na(df$roro_equ))]))
  df$roro_scale = df$roro_scale-min(df$roro_scale[which(!is.na(df$roro_scale))])
  head(df)
  
  p = ggplot(data = df, aes(x=date))+
    geom_point(aes(y=percentage, color = "true_pred"), size=2,shape=18)+
    geom_line(aes(y=roro_scale,color="roro_equ (scaled)"))+
    geom_vline(xintercept = as.numeric(df$date[which(df$stress==1)]),alpha=0.1, color = "blue") +
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  df$count[is.na(df$count)]=0
  ind = which(df$count>0)
  
  stat_table = matrix(data = NA,nrow = 3,ncol = 2)
  colnames(stat_table) = c("Count","Percentage")
  rownames(stat_table) = c("Before","After","Both")
  
  stat_table[1,1] = sum(df$stress[ind-1]*df$count[ind])
  stat_table[2,1] = sum(df$stress[ind+1]*df$count[ind])
  stat_table[3,1] = sum(df$stress[ind-1]*df$stress[ind+1]*df$count[ind])
  stat_table[,2] = stat_table[,1]/sum(df$count)
  
  
  res = list(stat_table = stat_table, graph = p)
  class(res)="pos_prediction"
  return(res)
}

cleanCorrMatRMT <- function(dirtyCorrMat, T, M, eigenTreat = c("average", "delete")){
  #  library(rrcov, quietly = TRUE)
  #  library(tseries, quietly = TRUE)
  #  library(GARPFRM, quietly = TRUE)
  #  library(xts, quietly = TRUE)
  
  
  # Marchenko Pastur density is defined for eigenvalues of correlation matrix
  eigen.C <- eigen(dirtyCorrMat,symmetric=T)
  lambdas <- eigen.C$values
  
  #minimize log-likelihood. 
  loglik.marpas <- function( params ) {
    Q         <- params[1]
    sigma     <- params[2]
    sigma.sq  <- sigma^2
    val <- sapply(lambdas,     
                  function(x) dmp(x,svr = Q, var=sigma.sq))
    
    val <- val[val > 0]
    ifelse(is.infinite(-sum(log(val))), .Machine$double.xmax, -sum(log(val)))        
  }
  
  lbQ <- 1; ubQ <- max(T/M,5)
  lbSig <- 0.01; ubSig <- 1
  
  startsQ <- seq(lbQ, ubQ, length.out = 50)
  startsSig <- seq(lbSig, ubSig, length.out = 100)
  
  optLik <- Inf
  optQ <- NA
  optSig <- NA
  grid <- matrix(,50,100)
  j_idx <- 0
  for (j in startsQ){
    j_idx <-  j_idx + 1
    k_idx <- 0
    for (k in startsSig){
      k_idx <-  k_idx + 1
      lik <- loglik.marpas(c(j,k))
      grid[j_idx,k_idx]<-lik
      if (lik < optLik){
        optLik <- lik
        optQ <- j
        optSig <- k
      }
    }
  }
  
  Q <- optQ
  sigma <- optSig    
  sigma.sq <- sigma^2
  
  lambda.max <- qmp(1, svr=Q, var = sigma.sq)  
  # now that we have a fit. lets denoise eigenvalues below the cutoff
  
  idx <- which(lambdas > lambda.max)
  val <- eigen.C$values[idx]; vec <- eigen.C$vectors
  sum <- 0; for (i in 1:length(val)) sum <- sum + val[i]*vec[,i] %*% t(vec[,i])
  
  # trace of correlation matrix is 1. Use this to determine all the remaining
  # eigenvalues
  lambdas.cleaned <- c()
  
  if (eigenTreat=='delete'){
    lambdas.cleaned <- c(val, rep(0,M-length(val)))
    diag(sum) <- 1
  }
  else if (eigenTreat=='average'){
    lambdaNoisy <- sum(eigen.C$values[-idx])/(M-length(val))
    lambdas.cleaned <- c(val, rep(lambdaNoisy,M-length(val)))
    for (i in ( (length(val)+1):M) ) {
      sum <- sum + lambdaNoisy*vec[,i] %*% t(vec[,i])
    }
    diag(sum) <- 1
  }  
  
  clean.C <- sum
  
  fit <- list(corr = clean.C, Q = Q, var = sigma.sq, eigVals = lambdas, 
              eigVals.cleaned = lambdas.cleaned, lambdascutoff = lambda.max)
  
  return( fit )
}

