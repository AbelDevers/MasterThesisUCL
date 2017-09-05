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


par_vec = c((1:5)*4,20+(1:10)*5)
# par_vec = 60
par_vec_2 = c((1:5)*0.16/5,(1:5)/5)
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
      # average return of the predicted days
      # temp_return =SandP$returnLag[index(y[ind_test,])]
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
   X_test,X_train,y_test,y_train,k_correct_pred,k_nb_pred, k_pred_position,k_pred_return)


# save(list  = ls(), file = "Cross_res/rbf_simple_equ_roro_1dayLag_15dayStress_2dot5PercentTresh")

# save(list  = ls(), file = "equit_data")

# save(list  = ls(), file = "Cross_res/rbf_simple_equ_roro_1dayLag_15dayStress")
# load("Cross_res/rbf_simple_equ_roro_1dayLag_15dayStress")


# Draw results graphs -----------------------------------------------------
# library(gplots)



# true_pos/false_neg
# nb_pred
# length(y)*0.05
# 
p_values = round(1-phyper(sum(y)*true_pos,m=sum(y),n=length(y)-sum(y),nb_pred),2)

p_values
# 
# true_pos[p_values>0.01]=0
# 
# true_pos[13,2]
# false_neg[13,2]
# pred_return[13,2]
# ratio[13,2]
# 
# heatmap.2(round(true_pos,2),
#           cellnote=round(true_pos,2),
#           main = "p_values", # heat map title
#           notecol="black",      # change font color of cell labels to black
#           # density.info="none",  # turns off density plot inside color legend
#           trace="none",         # turns off trace lines inside the heat map
#           dendrogram="none",     # only draw a row dendrogram
#           Colv="NA",
#           Rowv = "NA",
#           key =FALSE,
#           lmat=rbind(c(2),c(3),c(1),c(4)),
#           lhei=c(1,1,9,0),
#           lwid=c(1)
# )
# 
# 
# all_returns = full_return[13,2][[1]]
# all_pred_returns = full_pred_return[13,2][[1]]
# 
# all_returns[is.na(all_returns)] = 0
# all_pred_returns[is.na(all_pred_returns)] = 0
# 
# 
# col1=rgb(0,0,0,0.5)
# col2=rgb(0,1,0,0.5)
# hist(all_returns, col=col1,xlim=c(range(all_returns)*1.05), breaks = 30, probability = TRUE, main="Histogram of returns")
# hist(all_pred_returns, col=col2, add=T,probability = TRUE, breaks = 30)
# legend(x="topleft",legend=c("All_returns","Pred_returns"),fill = c(col1,col2))
# box()
# 
# round(mean(all_returns),3)
# round(var(all_returns),3)
# round(skewness(all_returns),3)
# round(kurtosis(all_returns),3)
# 
# 
# round(mean(all_pred_returns),3)
# round(var(all_pred_returns),3)
# round(skewness(all_pred_returns),3)
# round(kurtosis(all_pred_returns),3)







