cat("\014") # clear console
dev.off() # clear all plots

library(readr)
library(imputeTS)
library(roll)
library(xts)
library(ggplot2)
library(kernlab)
# library("zoo")
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
source('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Code/my_functions.R', encoding = 'UTF-8')



# time chunk spliting -----------------------------------------------------
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


par_vec = c((1:5)*0.2/5,0.2+(1:5)*0.7/5)
par_vec_2 = 1
# par_vec_2 = c((1:5)*0.16/5,(1:5)/5)

true_pos = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
false_neg = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
correct_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
nb_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
pred_return = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))

rownames(true_pos) = par_vec
rownames(false_neg) = par_vec
rownames(correct_pred) = par_vec
rownames(nb_pred) = par_vec
rownames(pred_return) = par_vec

colnames(true_pos) = par_vec_2
colnames(false_neg) = par_vec_2
colnames(correct_pred) = par_vec_2
colnames(nb_pred) = par_vec_2
colnames(pred_return) = par_vec_2

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
    for(k in 1:K){
      
      test_chunk =unlist(test_chunk_list[k])
      train_chunk = unlist(train_chunk_list[k])
      
      
      ind_train = c(chunk_mat[,train_chunk])
      ind_test = c(chunk_mat[,test_chunk])
      
      X_train = as.matrix(X[ind_train,])
      y_train = as.matrix(y[ind_train,])
      
      # For the novelty we remove the extrem value from our X
      X_train = X_train[which(y_train==0),]
      
      X_test = as.matrix(X[ind_test,])
      y_test = as.matrix(y[ind_test,])
      
      # fit = ksvm(x=X_train,y=factor(y_train),kernel=rbfdot(sigma = param),C=10)
      # fit = ksvm(x=X_train,kernel=rbfdot(sigma = param_2),nu = param)
      
      fit = ksvm(x=X_train,kernel = polydot(),nu = param)
      
      
      pred = kernlab::predict(object = fit,newdata=X_test)
      pred =  1-pred*1
      
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
      m = mean(SandP$returnLag[pred==1])
      if(is.na(m)){
        m = 0
      }
      k_pred_return[k,1] = m
      
    }
    true_pos[par_ind,par_ind_2]=mean(k_correct_positive)
    # true_pos[par_ind,2]=var(k_correct_positive)
    false_neg[par_ind,par_ind_2]=mean(k_false_negative)
    
    # false_neg[par_ind,2]=var(k_false_negative)
    correct_pred[par_ind,par_ind_2]=sum(k_correct_pred)
    # correct_pred[par_ind,2]=var(k_correct_pred)
    nb_pred[par_ind,par_ind_2]=sum(k_nb_pred)
    # nb_pred[par_ind,2]=var(k_nb_pred)
    pred_return[par_ind,par_ind_2]=mean(k_pred_return)
    
    
  }
  
}

par(mfrow=c(1,1))

# plot(x = 1:length(par_vec),true_pos[,1],type="l",col="red",xlab=par_vec)
# lines(x = 1:length(par_vec),false_neg[,1],col="green")

true_pos
max(true_pos)
false_neg
false_neg[which(true_pos==max(true_pos))]
pred_return[which(true_pos==max(true_pos))]

ratio = correct_pred/nb_pred
correct_pred[which(true_pos==max(true_pos))]
ratio[which(true_pos==max(true_pos))]

nb_pred[which(true_pos==max(true_pos))]
correct_pred[which(true_pos==max(true_pos))]

rm(fit,ind_test,ind_train,k,K,number_test_ch,par_ind,param,pred,t_chunk,test_chunk
   ,test_chunk_list,train_chunk,train_chunk_list,chunk_mat,correct,k_correct_positive,k_false_negative,
   X_test,X_train,y_test,y_train,k_correct_pred,k_nb_pred)

r_ind = sort(sample.int(length(y), 35))

sum(y[r_ind])/sum(y)

save(list  = ls(), file = "Cross_res/rbf_novelty_roro_equ_l1")

