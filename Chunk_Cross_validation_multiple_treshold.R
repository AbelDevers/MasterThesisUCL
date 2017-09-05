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

# load("Cross_res/xy_5percent_secondEig")
# load("Cross_res/xy_5percent_BIG")
# load("Cross_res/xy_5percent")
# load("Cross_res/xy_5percent_rmt")
# load("Cross_res/xy_5percent_absorbtionRatio")
# load("Cross_res/xy_5percent_stationary")
load("Cross_res/xy_5percent_rmt_second")

if(FALSE){
  ########################  Only equ (no volume) ############################
  X = (X[,c(1,2,5,6)])  
}

if(FALSE){
  ########################  Only vol (no prices) ############################
  X = (X[,-c(1,2,5,6)])  
}

# time chunk spliting -----------------------------------------------------

full_cross_validation <- function(
  X,
  y,
  y_lagReturn,
  par_vec = c((1:5)*4,20+(1:10)*5,100),
  par_vec_2 = c((1:5)*0.16/5,(1:5)/5),
  tresh = "not specified",
  doOutliers = FALSE
){
  set.seed(1)
  t_chunk = 20
  number_test_ch = 4
  chunk_mat = time_chunk_split(y,chunk = t_chunk)
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
        
        X_train = as.matrix(X[ind_train,])
        y_train = as.matrix(y[ind_train,])
        X_test = as.matrix(X[ind_test,])
        y_test = as.matrix(y[ind_test,])
        
        
        if(doOutliers){
          X_train = X_train[which(y_train == 0),]
          
          if(param_2 ==-1){
            fit = ksvm(x=X_train, kernel = "rbfdot",nu = param)
          }else{
            fit = ksvm(x=X_train,kernel=rbfdot(sigma = param_2),nu = param)
          }
          
        }else{
          ### regular no outliers
          fit = ksvm(x=X_train,y=factor(y_train),kernel=rbfdot(sigma = param_2),C=param)
        }
         
        
        
        
        pred = kernlab::predict(object = fit,newdata=X_test)
        if(doOutliers){
          pred = pred*1
        }else{
          pred =  as.numeric(levels(pred))[pred]
        }
        
        
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
        temp_return =as.matrix(y_lagReturn[index(y[ind_test,])])
        
        ind = index(y[ind_test,])
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
      
      print("Recall")
      print(true_pos)
      print("Return")
      print(pred_return)
      
    }
    
  }
  
  par(mfrow=c(1,1))
  
  # plot(x = 1:length(par_vec),true_pos[,1],type="l",col="red",xlab=par_vec)
  # lines(x = 1:length(par_vec),false_neg[,1],col="green")
  
  # true_pos
  # false_neg
  # pred_return
  p_values = round(1-phyper(sum(y)*true_pos,m=sum(y),n=length(y)-sum(y),nb_pred),2)
  ratio = correct_pred/nb_pred
  
  rm(fit,ind_test,ind_train,k,K,number_test_ch,par_ind,param,pred,t_chunk,test_chunk
     ,test_chunk_list,train_chunk,train_chunk_list,chunk_mat,correct,k_correct_positive,k_false_negative,
     X_test,X_train,y_test,y_train,k_correct_pred,k_nb_pred, k_pred_position,k_pred_return)
  
  
  
  res = list(recall = true_pos,
             precision = ratio,
             false_neg = false_neg,
             pred_return = pred_return,
             y_df =y_df,
             nb_pred = nb_pred,
             full_pred_return=full_pred_return,
             full_return=full_return,
             pred_position = pred_position,
             p_values = p_values,
             X = X,
             y = y,
             length_stress_periode, 
             tresh = tresh
  )
  
  
  class(res) = "k-cross-mats"
  return(res)
}


# vary treshold and compute multiple full_cross ---------------------------


# res = full_cross_validation(
#   X=X,
#   y=y,
#   y_lagReturn = y_lagReturn,
#   par_vec_2 = c((1:5)*0.16/5)
# )

tresh_list = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9)
# tresh_list = c(0.01,0.05,0.1,0.25)
# tresh_list = c(0.01,0.05,0.1,0.25)
# tresh_list = c(0.25,0.5,0.75,0.9)
res_mat = matrix(data = list(),nrow = length(tresh_list),ncol=1)
y_lagReturn[is.na(y_lagReturn)]=0
for(i in 1:length(tresh_list)){
  tresh = quantile(x = y_lagReturn,probs = tresh_list[i])  
  # print(sum(y_lagReturn<tresh)/length(y))
  y = (y_lagReturn<=tresh)*1
  
  res = full_cross_validation(
    X=X,
    y=y,
    y_lagReturn = y_lagReturn,
    # par_vec_2 = c((1:5)/5,2,3),
    # par_vec_2 = -1,
    # par_vec = (1:10)/10-0.01,
    tresh = tresh,
    doOutliers = FALSE
  )
  print(res$precision)
  print(res$recall)
  print(res$p_values)
  
  # save(list  = ls(), file = "multiple_tresh_secondEig_2")
  
  
  res_mat[i,][[1]] = res
  
  print(paste("--------------------------","End of tresh",tresh_list[i],"--------------------------"))
  
}


# save(list  = ls(), file = "multiple_tresh_vanilla")
# # save(res_mat, file = "multiple_tresh_vanilla_res")
# save(res_mat, file = "multiple_tresh_vanilla_RMT_res")
# save(res_mat, file = "multiple_tresh_NoVol_res")
# save(res_mat, file = "multiple_tresh_VolOnly_res")
# save(res_mat, file = "multiple_tresh_absorbtionRatio_res")
# save(res_mat, file = "multiple_tresh_stationary_res")
# save(res_mat, file = "multiple_tresh_rmt_second_res")

# save(list  = ls(), file = "multiple_tresh_secondEig")
# save(list  = ls(), file = "multiple_tresh_secondEig_2")
# save(list  = ls(), file = "multiple_tresh_secondEig_3")
# save(list  = ls(), file = "multiple_tresh_secondEig_sparseRVM")





# save(list  = ls(), file = "multiple_tresh_res_BIG_outliers")

# save(list  = ls(), file = "multiple_tresh_rmt_second")
# save(list  = ls(), file = "temp")


# save(list  = ls(), file = "multiple_tresh_rmt_second_withVanilla")






















