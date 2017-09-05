rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots


library(readr)
library(imputeTS)
library(roll)
library(xts)
library(kernlab)
library(MASS)
library(imputeTS)
library(covmat)



setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
source('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Code/my_functions.R', encoding = 'UTF-8')

# save(list  = ls(), file = "covariance_templates")
load("covariance_templates")



# creating data -----------------------------------------------------------
set.seed(1)

nb_stock = 96
mu = rep(0,nb_stock)
# Sigma1 = diag(0.1,nrow = nb_stock,ncol = nb_stock)+0.0003022328
# Sigma2 = diag(0.1,nrow = nb_stock,ncol = nb_stock)+5.855651e-05

# Sigma1 = diag(0.9,nrow = nb_stock,ncol = nb_stock)+0.1
# Sigma2 = diag(0.1,nrow = nb_stock,ncol = nb_stock)+0.9

Sigma1 = best_guess_extreme
Sigma2 = best_guess_med

# Sigma1 = estRMT(best_guess_extreme_v)$cov
# Sigma2 = estRMT(best_guess_med_v)$cov




eigen(Sigma1)$values
eigen(Sigma2)$values

eigen(Sigma1)$values[1]/sum(eigen(Sigma1)$values)
eigen(Sigma2)$values[1]/sum(eigen(Sigma2)$values)

window = 10
data = NULL
label = NULL

length_stress_period = 10

# for(i in 1:150){
#   # if(sample(x = 1:10,size = 1)==1){
#   #   data = rbind(data,mvrnorm(n = length_stress_period,mu = mu,Sigma = Sigma2,empirical = FALSE))
#   #   label = c(label,rep(0,length_stress_period-1),1)
#   # }else{
#     # data = rbind(data,mvrnorm(n = window,mu = mu,Sigma = Sigma1,empirical = FALSE))
#     # label = c(label,rep(0,window))
#   # }
#   
# }

nb_data = 3000
nb_point = 150
crisis_index = sample(1:nb_data,size = nb_point)
label = rep(0,nb_data)
label[crisis_index]=1
for(i in 1:nb_data){
  data = rbind(data,mvrnorm(n=1,mu=mu,Sigma = Sigma2))
  if(label[i]==1){
    start = max(1,i-length_stress_period)
    data[(start+1):i,] = mvrnorm(n=(i-start),mu=mu,Sigma = Sigma1)
  }
  
}

rm(nb_data,nb_point,crisis_index)

plot(label)
sum(label)
sum(label)/length(label)



# improve stationarity ----------------------------------------------------

# data = data - rowMeans(data) (shitty idea)

# creating roro -----------------------------------------------------------

time_lag = 100
label = as.data.frame(label)
label[,2] = NA
list_estRMT = list()
for(i in (time_lag+1):dim(data)[1]){
  # "normal way"
  X = data[(i-time_lag):i,1:(dim(data)[2])]
  pca = princomp(X)
  eigs <- pca$sdev^2
  label[i,2] = eigs[1]/sum(eigs)
  
  # # "denoised"
  # est = estRMT(X)
  # list_estRMT[[i]]=est
  # eigs = est$eigVals.cleaned
  # label[i,2] = eigs[1]/sum(eigs)
  
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
sum
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
