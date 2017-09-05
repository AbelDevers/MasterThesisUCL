cat("\014") # clear console
dev.off() # clear all plots

# library(readr)
library(imputeTS)
library(roll)
library(xts)
library(ggplot2)
library(kernlab)
library(zoo)
library(moments)

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



par_vec = c((1:5)*4,20+(1:10)*5,500)
# par_vec = c(12,35,70,1000)


par_vec_2 = c((1:5)*0.16/5,(1:5)/5)
# par_vec_2 = c(2,5,10)

true_pos = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
false_neg = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
correct_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
nb_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))

pred_pos = matrix(data=list(),nrow = length(par_vec),ncol = length(par_vec_2))

rownames(true_pos) = par_vec
rownames(false_neg) = par_vec
rownames(correct_pred) = par_vec
rownames(nb_pred) = par_vec
rownames(pred_pos) = par_vec

colnames(true_pos) = par_vec_2
colnames(false_neg) = par_vec_2
colnames(correct_pred) = par_vec_2
colnames(nb_pred) = par_vec_2
colnames(pred_pos) = par_vec_2

for(par_ind_2 in 1:length(par_vec_2)){
  param_2 = par_vec_2[par_ind_2]
  
  for(par_ind in 1:length(par_vec)){
    param = par_vec[par_ind]
    
    print(paste("Starting process for param: ",param,"and par 2: ",param_2))
    
    k_correct_positive = matrix(data = NA, nrow = K, ncol = 1)
    k_false_negative = matrix(data = NA, nrow = K, ncol = 1)
    k_correct_pred = matrix(data = NA, nrow = K, ncol = 1)
    k_nb_pred = matrix(data = NA, nrow = K, ncol = 1)
    for(k in 1:K){
      
      test_chunk =unlist(test_chunk_list[k])
      train_chunk = unlist(train_chunk_list[k])
      
      
      ind_train = c(chunk_mat[,train_chunk])
      ind_test = c(chunk_mat[,test_chunk])
      
      X_train = as.matrix(X[ind_train,])
      y_train = as.matrix(y[ind_train])
      X_test = as.matrix(X[ind_test,])
      y_test = as.matrix(y[ind_test])
      
      # fit = ksvm(x=X_train,y=factor(y_train),kernel=rbfdot(sigma = param),C=10)
      fit = ksvm(x=X_train,y=factor(y_train),kernel=rbfdot(sigma = param_2),C = param)
      
      pred = kernlab::predict(object = fit,newdata=X_test)
      pred =  as.numeric(levels(pred))[pred]
      
      correct = pred == (y_test)
      # correct positive
      k_correct_positive[k,1] = sum(correct[which(y_test==1)])/sum(y_test)
      # false positive
      k_false_negative[k,1] = sum(sum(!correct[which(y_test==0)]))/(sum(1-y_test))
      # absolute number of correct pred
      k_correct_pred[k,1] = sum(correct[which(y_test==1)])
      # absolute number of pred
      k_nb_pred[k,1] = sum(pred)
      
      # postion
      ind = which(correct*y_test==1)
      pos = list()
      k=1
      for(i in ind){
        if(i>11){
          pos[[k]]=y_test[(i-10):(i+10)]
          k = k+1
        }
        
      }
      
      pred_pos[par_ind,par_ind_2][[1]]= append(pred_pos[par_ind,par_ind_2][[1]],pos)
        
    }
    true_pos[par_ind,par_ind_2]=mean(k_correct_positive[!is.na(k_correct_positive)])
    # true_pos[par_ind,2]=var(k_correct_positive)
    false_neg[par_ind,par_ind_2]=mean(k_false_negative[!is.na(k_false_negative)])
    # false_neg[par_ind,2]=var(k_false_negative)
    correct_pred[par_ind,par_ind_2]=sum(k_correct_pred)
    # correct_pred[par_ind,2]=var(k_correct_pred)
    nb_pred[par_ind,par_ind_2]=sum(k_nb_pred)
    
    print(paste(true_pos[par_ind,par_ind_2],"true pos"))
    print(paste(false_neg[par_ind,par_ind_2],"false pos"))
    
  }
  
}

par(mfrow=c(1,1))

# plot(x = 1:length(par_vec),true_pos[,1],type="l",col="red",xlab=par_vec)
# lines(x = 1:length(par_vec),false_neg[,1],col="green")

true_pos
max(true_pos)
false_neg
false_neg[which(true_pos==max(true_pos))]

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

# save(list  = ls(), file = "Cross_res/simulation_05_01")
# save(list  = ls(), file = "Cross_res/simulation_05_01_constVar")
# save(list  = ls(), file = "Cross_res/simulation_sample_var")


# save(list  = ls(), file = "Cross_res/simulation_20")
# save(list  = ls(), file = "Cross_res/simulation_10")
# save(list  = ls(), file = "Cross_res/simulation_5")


# save(list  = ls(), file = "Cross_res/simulation_10_stationary")


# save(list  = ls(), file = "Cross_res/simulation_10_denoised1")
# save(list  = ls(), file = "Cross_res/simulation_10_denoised2")
# save(list  = ls(), file = "Cross_res/simulation_10_denoised_secondEigs1")


# load("Cross_res/simulation_10")
# roc1 = roc_computation(true_pos = true_pos,false_pos =false_neg)
# 
# load("Cross_res/simulation_20")
# roc2 = roc_computation(true_pos = true_pos,false_pos =false_neg)




# Draw results graphs -----------------------------------------------------
# library(gplots)
# load("Cross_res/rbf_simple_equ_roro_1dayLag_15dayStress")

# true_pos/false_neg
# nb_pred
# length(y)*0.05
# 
p_values = round(1-phyper(sum(y)*true_pos,m=sum(y),n=length(y)-sum(y),nb_pred),2)


res = list(recall = true_pos,
           precision = ratio,
           false_neg = false_neg,
           nb_pred = nb_pred,
           p_values = p_values,
           pred_pos = pred_pos,
           id = "Absorbtion ratio, 10points"
           
)
class(res)="k-cross-mat-simulation"



# save(list  = ls(), file = "Cross_res/simulation_10_back_up")
# save(res, file = "Cross_res/simulation_10")
# save(res, file = "Cross_res/simulation_10_RMT")
# save(res, file = "Cross_res/simulation_10_absorbtionRatio")
# save(res, file = "Cross_res/simulation_10_stationary")
# save(res, file = "Cross_res/simulation_10_absorbtionRatio_big_var")
# save(res, file = "Cross_res/simulation_10_secondEig")
# save(res, file = "Cross_res/simulation_10_RMTsecond")
# load("Cross_res/simulation_10")



# rm(list=ls())
# load("Cross_res/simulation_10_absorbtionRatio")
# res1 = res
# load("Cross_res/simulation_10_absorbtionRatio_big_var")
# res2 = res
# 
# for(i in 1:6){
#   res[[i]] = cbind(res1[[i]],res2[[i]])
# }
# 
# save(res, file = "Cross_res/simulation_10_absorbtionRatio_mergeAllVar")


