rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots




# library(readr)
library(imputeTS)
library(roll)
library(xts)
library(kernlab)
library(MASS)
library(imputeTS)
library(covmat)



setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')

# add functions -----------------------------------------------------------
library(smooth)
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

# load covariance ---------------------------------------------------------



# save(list  = ls(), file = "covariance_templates")
load("covariance_templates")

# creating data -----------------------------------------------------------
set.seed(1)

nb_stock = 96
mu = rep(0,nb_stock)
# Sigma1 = diag(0.1,nrow = nb_stock,ncol = nb_stock)+0.0003022328
# Sigma2 = diag(0.1,nrow = nb_stock,ncol = nb_stock)+5.855651e-05
Sigma1 = best_guess_extreme
Sigma2 = best_guess_med


eigen(Sigma1)$values
eigen(Sigma2)$values

eigen(Sigma1)$values[1]/sum(eigen(Sigma1)$values)
eigen(Sigma2)$values[1]/sum(eigen(Sigma2)$values)

window = 20
data = NULL
label = NULL

length_stress_period = 10

for(i in 1:300){
  if(sample(x = 1:10,size = 1)==1){
    data = rbind(data,mvrnorm(n = length_stress_period,mu = mu,Sigma = Sigma2,empirical = FALSE))
    label = c(label,rep(0,length_stress_period-1),1)
  }else{
    data = rbind(data,mvrnorm(n = window,mu = mu,Sigma = Sigma1,empirical = FALSE))
    label = c(label,rep(0,20))
  }
  
}
plot(label)


# improve stationarity ----------------------------------------------------

# data = data - rowMeans(data) (shitty idea)

# creating roro -----------------------------------------------------------

time_lag = 365
label = as.data.frame(label)
label[,2] = NA
list_estRMT = list()
# for(i in (time_lag+1):dim(data)[1]){
for(i in 4605:dim(data)[1]){
  # "normal way"
  X = data[(i-time_lag):i,1:(dim(data)[2])]
  # pca = princomp(X)
  # eigs <- pca$sdev^2
  # label[i,2] = eigs[1]/sum(eigs)
  
  # "denoised"
  est = estRMT(X)
  list_estRMT[[i]]=est
  eigs = est$eigVals.cleaned
  label[i,2] = eigs[1]/sum(eigs)
  
  if( round(100*i/dim(data)[1])%%5 ==0 ){
    print(paste(round(100*i/dim(data)[1]),"%"))
    save(list  = ls(), file = "cloud_rmt_pca_simulation_p3")
    
  }
  
}
rm(X,pca,eigs,i)
colnames(label)[2] = "roro"

# creating rolling roro ---------------------------------------------------

label$delta_roro = NA
tmp_no_na = label$roro
tmp_no_na[is.na(tmp_no_na)] = 0
label$delta_roro = computeRollingVal(tmp_no_na,doPlot = FALSE)
rm(tmp_no_na)


# creating X,y ------------------------------------------------------------

predictor = label[,-1]

for(i in 1:dim(predictor)[2]){
  predictor[which(is.na(predictor[,i])),i]=0
  predictor[which(!is.finite(predictor[,i])),i]=0
}

# Add differenced
o=dim(predictor)[2]
for(i in 1:o){
  for(j in 1:1){
    name = paste(colnames(predictor)[i],"lag",j)
    temp = predictor[,i]
    temp = diff(temp,j)
    temp[is.na(temp)]=0
    temp = c(rep(0,j),temp)
    
    predictor[,dim(predictor)[2]+1] = temp
    colnames(predictor)[length(colnames(predictor))]=name
  }
  
}
rm(o,temp)
# Introducing time lag between X and y
prediction_lag = 0

X = predictor[1:(dim(predictor)[1]-prediction_lag),]
y = label[(1+prediction_lag):(dim(label)[1]),1]

rm(predictor)

rm(i,j,mu,name,nb_stock)

remove_tresh = round(length(y)/6)

X = X[remove_tresh:length(y),]
y = y[remove_tresh:length(y)]


# Saving results ----------------------------------------------------------



save(list  = ls(), file = "cloud_rmt_pca_simulation_p3")
