
rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots
library(MASS)
library(moments)
library(covmat)

source('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Code/my_functions.R', encoding = 'UTF-8')


setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')

nb_stock = 100

cov_val = 0.00
Sigma = diag(x=0.1,nrow = nb_stock)+cov_val
mu = matrix(data = 0,nrow = nb_stock)

window = 100
data = mvrnorm(n = window, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
for(i in 1:10){
  data = rbind(data,mvrnorm(n = 100, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE))
  cov_val = cov_val+0.1
  Sigma = diag(x=0.1,nrow = nb_stock)+cov_val
}
rm(i,cov_val,Sigma,mu)

roro = NULL
for(i in (window+1):dim(data)[1]){
  C = cov(data[(i-window):(i-1),])
  e = eigen(C)
  r = e$values[1]/sum(e$values)
  roro = rbind(roro,r)
  
}
rm(i,e,r)
plot(roro)
hist((diff(roro)))
# Illustrate that, imo, as such there is too much noise in the indicator
absorbtion_ratio = NULL
nb_ind = ceiling(nb_stock*0.2)
for(i in (window+1):dim(data)[1]){
  C = cov(data[(i-window):(i-1),])
  e = eigen(C)
  r = sum(e$values[1:nb_ind])/sum(e$values)
  absorbtion_ratio = rbind(absorbtion_ratio,r)
  
}
rm(i,e,r)
plot(absorbtion_ratio)
hist((diff(absorbtion_ratio)))


# smooth absorvtio ratio --------------------------------------------------
smooth_absorbtion = computeRollingVal(absorbtion_ratio,doPlot = FALSE,ord1 = 7,ord2 = 100)
smooth_absorbtion = smooth_absorbtion[150:length(smooth_absorbtion)]
which(is.na(smooth_absorbtion))
plot(smooth_absorbtion)
hist(smooth_absorbtion)


 