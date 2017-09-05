rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots

library(readr)
library(imputeTS)
library(roll)
library(xts)
library(kernlab)
# library("zoo")
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
source('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Code/my_functions.R', encoding = 'UTF-8')
load("equ_vol_roro")

X = df_equ_return[,1:(dim(df_equ_return)[2]-1)]
C = cov(X)
eigs = eigen(C)

library(covmat)

rmt = estRMT(X,)

plot(rmt)
# fit Mark ----------------------------------------------------------------
# dirtyCorrMat = C
# T = TRUE
# M = dim(df_equ_return)[1]
# # Marchenko Pastur density is defined for eigenvalues of correlation matrix
# eigen.C <- eigen(dirtyCorrMat,symmetric=T)
# lambdas <- eigen.C$values
# 
# #minimize log-likelihood. 
# loglik.marpas <- function( params ) {
#   Q         <- params[1]
#   sigma     <- params[2]
#   sigma.sq  <- sigma^2
#   val <- sapply(lambdas,     
#                 function(x) dmp(x,svr = Q, var=sigma.sq))
#   
#   val <- val[val > 0]
#   ifelse(is.infinite(-sum(log(val))), .Machine$double.xmax, -sum(log(val)))        
# }
# 
# lbQ <- 1; ubQ <- max(T/M,5)
# lbSig <- 0.01; ubSig <- 1
# 
# startsQ <- seq(lbQ, ubQ, length.out = 50)
# startsSig <- seq(lbSig, ubSig, length.out = 100)
# 
# optLik <- Inf
# optQ <- NA
# optSig <- NA
# grid <- matrix(,50,100)
# j_idx <- 0
# for (j in startsQ){
#   j_idx <-  j_idx + 1
#   k_idx <- 0
#   for (k in startsSig){
#     k_idx <-  k_idx + 1
#     lik <- loglik.marpas(c(j,k))
#     grid[j_idx,k_idx]<-lik
#     if (lik < optLik){
#       optLik <- lik
#       optQ <- j
#       optSig <- k
#     }
#   }
# }
# 
# Q <- optQ
# sigma <- optSig    
# sigma.sq <- sigma^2
# 
# lambda.max <- qmp(1, svr=Q, var = sigma.sq)  

