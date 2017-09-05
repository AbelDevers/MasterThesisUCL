rm(list = ls())
cat("\014") # clear console
dev.off() # clear all plots

library(readr)
library(imputeTS)
library(roll)
library(xts)
library(ggplot2)
library(kernlab)
library(zoo)
library(moments)

setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
source('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Code/my_functions.R', encoding = 'UTF-8')

load("Cross_res/xy_5percent")
# load("Cross_res/xy_5percent_rmt")
# load("Cross_res/xy_5percent_absorbtionRatio")
# load("Cross_res/xy_5percent_stationary")

#

# set paparams function ---------------------------------------------------


full_cross_validation <- function(
  X_,
  # y_,
  y_lagReturn_,
  par_vec = c((1:5)*4,20+(1:10)*5,100),
  par_vec_2 = c((1:5)*0.16/5,(1:5)/5),
  tresh = 0.05
){
  y_lagReturn_[is.na(y_lagReturn_)] = 0
  q = quantile(x = y_lagReturn_,probs = tresh)
  y_ = (y_lagReturn_<q)

  set.seed(1)
  t_chunk = 20
  number_test_ch = 4
  chunk_mat = time_chunk_split(y_,chunk = t_chunk)
  K = 10
  test_chunk_list = list()
  train_chunk_list = list()
  for(i in 1:K){
    test_chunk = sort(sample.int(t_chunk, number_test_ch))
    test_chunk_list[[i]] = test_chunk
    train_chunk_list[[i]] = (1:20)[-test_chunk]
  }
  rm(i,test_chunk)
  # check if we have a duplicate in the cross val session
  if(any(duplicated(test_chunk_list))){
    print("Potential problem in the k-fold, duplicate in the sampling")
  }else{
    print("No duplicate in k-fold spliting")
  }
  
  # par_vec_2=0.096
  true_pos = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
  false_neg = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
  correct_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
  nb_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
  pred_return = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
  
  full_return = matrix(data=list(),nrow = length(par_vec),ncol = length(par_vec_2))
  full_pred_return = matrix(data=list(),nrow = length(par_vec),ncol = length(par_vec_2))
  
  pred_position = matrix(data=list(),nrow = length(par_vec),ncol = length(par_vec_2))
  
  y_df = matrix(data=data.frame(),nrow = length(par_vec),ncol = length(par_vec_2))
  
  rownames(true_pos) = par_vec
  rownames(false_neg) = par_vec
  rownames(correct_pred) = par_vec
  rownames(nb_pred) = par_vec
  rownames(pred_return) = par_vec
  rownames(full_return) = par_vec
  rownames(full_pred_return) = par_vec
  rownames(pred_position) = par_vec
  
  colnames(true_pos) = par_vec_2
  colnames(false_neg) = par_vec_2
  colnames(correct_pred) = par_vec_2
  colnames(nb_pred) = par_vec_2
  colnames(pred_return) = par_vec_2
  colnames(full_return) = par_vec_2
  colnames(full_pred_return) = par_vec_2
  colnames(pred_position) = par_vec_2
  
  all_pred_returns = NULL
  all_returns = NULL
  for(par_ind_2 in 1:length(par_vec_2)){
    param_2 = par_vec_2[par_ind_2]
    
    for(par_ind in 1:length(par_vec)){
      param = par_vec[par_ind]
      
      print(paste("Starting process for param: ",param,"and par 2: ",param_2))
      
      k_correct_positive = matrix(data = NA, nrow = K, ncol = 1)
      k_false_negative = matrix(data = NA, nrow = K, ncol = 1)
      k_correct_pred = matrix(data = NA, nrow = K, ncol = 1)
      k_nb_pred = matrix(data = NA, nrow = K, ncol = 1)
      k_pred_return = matrix(data = NA, nrow = K, ncol = 1)
      k_pred_position = matrix(data = list(), nrow = K, ncol = 1)
      for(k in 1:K){
        
        test_chunk =unlist(test_chunk_list[k])
        train_chunk = unlist(train_chunk_list[k])
        
        
        ind_train = c(chunk_mat[,train_chunk])
        ind_test = c(chunk_mat[,test_chunk])
        
        X_train = as.matrix(X_[ind_train,])
        y_train = as.matrix(y_[ind_train,])
        X_test = as.matrix(X_[ind_test,])
        y_test = as.matrix(y_[ind_test,])
        
        # fit = ksvm(x=X_train,y=factor(y_train),kernel=rbfdot(sigma = param),C=10)
        fit = ksvm(x=X_train,y=factor(y_train),kernel=rbfdot(sigma = param_2),C = param)
        
        pred = kernlab::predict(object = fit,newdata=X_test)
        # pred =  as.numeric(levels(pred))[pred]
        pred = as.numeric((pred)) -1
        correct = pred == (y_test)
        # correct positive
        k_correct_positive[k,1] = sum(correct[which(y_test==1)])/sum(y_test)
        # false positive
        k_false_negative[k,1] = sum(sum(!correct[which(y_test==0)]))/(sum(1-y_test))
        # absolute number of correct pred
        k_correct_pred[k,1] = sum(correct[which(y_test==1)])
        # absolute number of pred
        k_nb_pred[k,1] = sum(pred)
        # average return of the predicted days
        temp_return =as.matrix(y_lagReturn_[index(y_[ind_test,])])
        
        ind = index(y_[ind_test,])
        ind = ind[which(y_test==1 & correct==TRUE)]
        
        if(length(ind)>0){
          k_pred_position[k,1][[1]]= ind
        }else{
          k_pred_position[k,1][[1]]=list()
        }
        
        rm(ind)
        m = mean(temp_return[pred==1])
        if(is.na(m)){
          m = 0
        }
        k_pred_return[k,1] = m
        
        full_pred_return[par_ind,par_ind_2][[1]] = c(full_pred_return[par_ind,par_ind_2][[1]],
                                                     temp_return[pred==1])
        full_return[par_ind,par_ind_2][[1]] = c(full_return[par_ind,par_ind_2][[1]],temp_return)
        
        
        ind = which(is.na(temp_return))
        # # temp_return = temp_return[-ind]
        # temp_df = data.frame(y_return=temp_return[-ind],prediction = pred[-ind])
        temp_df = data.frame(y_return=temp_return,prediction = pred)
        y_df[par_ind,par_ind_2][[1]]=rbind(temp_df,y_df[par_ind,par_ind_2][[1]])
        
        rm(temp_return,temp_df,ind)
      }
      true_pos[par_ind,par_ind_2]=mean(k_correct_positive[!is.na(k_correct_positive)])
      # true_pos[par_ind,2]=var(k_correct_positive)
      false_neg[par_ind,par_ind_2]=mean(k_false_negative[!is.na(k_false_negative)])
      # false_neg[par_ind,2]=var(k_false_negative)
      correct_pred[par_ind,par_ind_2]=sum(k_correct_pred)
      # correct_pred[par_ind,2]=var(k_correct_pred)
      nb_pred[par_ind,par_ind_2]=sum(k_nb_pred)
      # nb_pred[par_ind,2]=var(k_nb_pred)
      pred_return[par_ind,par_ind_2]=mean(k_pred_return)
      # position
      L = NULL
      for(l in k_pred_position){
        L = c(l,L)
      }
      pred_position[par_ind,par_ind_2][[1]] = L
      rm(L,l)
      
      
    }
    
  }
  
  par(mfrow=c(1,1))
  
  # plot(x = 1:length(par_vec),true_pos[,1],type="l",col="red",xlab=par_vec)
  # lines(x = 1:length(par_vec),false_neg[,1],col="green")
  
  # true_pos
  # false_neg
  # pred_return
  p_values = round(1-phyper(sum(y_)*true_pos,m=sum(y_),n=length(y_)-sum(y_),nb_pred),2)
  ratio = correct_pred/nb_pred

  full_return= nb_pred * pred_return
  full_return[which(p_values>0.01)] = 1000
  if(min(full_return)==0){
    ##### we can not trust the model
    full_return= nb_pred * pred_return
    significant = 0
  }else{
    significant = 1
  }
  
  
  ind = which(full_return==min(full_return),arr.ind = TRUE)
  
  C_ = par_vec[ind[1]]
  sigma_ = par_vec[ind[2]]
  
  res = list(
    C = C_,
    sigma = sigma_,
    
  )
  
}



