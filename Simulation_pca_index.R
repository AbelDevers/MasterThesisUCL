rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots
library(MASS)
library(moments)
library(covmat)
library(kernlab)

source('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Code/my_functions.R', encoding = 'UTF-8')


setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')

# creating data -----------------------------------------------------------
set.seed(1)

nb_stock = 100

cov_val = 5.855651e-05
Sigma = diag(x=0.1,nrow = nb_stock)+cov_val
mu = matrix(data = 0,nrow = nb_stock)
label = NULL
window = 100
data = mvrnorm(n = window, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
for(i in 1:150){
  if(sample(1:10,1)==1){
    label = c(label,rep(1,10),rep(1,10))
    # cov_val = 0.0003022328
    cov_val = 0.5
  }else{
    label = c(label,rep(0,20))
    # cov_val = 5.855651e-05
    cov_val = 0.01
  }
  
  Sigma = diag(x=0.1,nrow = nb_stock)+cov_val
  
  data = rbind(data,mvrnorm(n = 20, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE))
}
rm(i,cov_val,Sigma,mu)


plot(label)

# computing roro ----------------------------------------------------------


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
# smooth_absorbtion = smooth_absorbtion[150:length(smooth_absorbtion)]
which(is.na(smooth_absorbtion))
plot(smooth_absorbtion)
hist(smooth_absorbtion)


# creating y and x --------------------------------------------------------
y = label[101:length(label)]
X = cbind(roro,absorbtion_ratio,smooth_absorbtion)[101:length(roro),]
# d = dim(X)[1]
# X = cbind(X[1:(d-1),],X[2:d,])
# y = y[2:d]
rm(d)

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
par_vec_2 = c((1:5)*0.16/5,(1:5)/5)


# par_vec =c((1:8)*5+40,80+(1:2)*20)
# par_vec_2 = c((1:10)/2.5)

true_pos = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
false_neg = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
correct_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))
nb_pred = matrix(data=NA,nrow = length(par_vec),ncol = length(par_vec_2))

rownames(true_pos) = par_vec
rownames(false_neg) = par_vec
rownames(correct_pred) = par_vec
rownames(nb_pred) = par_vec

colnames(true_pos) = par_vec_2
colnames(false_neg) = par_vec_2
colnames(correct_pred) = par_vec_2
colnames(nb_pred) = par_vec_2


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
      # fit = ksvm(x=X_train,y=factor(y_train),kernel=rbfdot(sigma = param_2),C = param)
      fit = ksvm(x=X_train,y=factor(y_train),kernel="rbfdot",c=50
                 )
     
      # fit
      
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
      
    }
    true_pos[par_ind,par_ind_2]=mean(k_correct_positive[!is.na(k_correct_positive)])
    # true_pos[par_ind,2]=var(k_correct_positive)
    false_neg[par_ind,par_ind_2]=mean(k_false_negative[!is.na(k_false_negative)])
    # false_neg[par_ind,2]=var(k_false_negative)
    correct_pred[par_ind,par_ind_2]=sum(k_correct_pred)
    # correct_pred[par_ind,2]=var(k_correct_pred)
    nb_pred[par_ind,par_ind_2]=sum(k_nb_pred)
    # nb_pred[par_ind,2]=var(k_nb_pred)
    
    
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

# save(list  = ls(), file = "Cross_res/simulation_2")
load("Cross_res/simulation_1")



1-phyper(sum(y)*true_pos,m=sum(y),n=length(y)-sum(y),nb_pred)
# ploting for slides ------------------------------------------------------
# library(gplots)
# par(mfrow=c(2,2))
# plot(y)
# title("Covariance state (1=stressed)")
# plot(roro,type="l")
# title("RoRo")
# plot(absorbtion_ratio,type="l")
# title("Absorbtion ratio")
# plot(smooth_absorbtion, type="l")
# title("smoothed as")
# 
# par(mfrow=c(2,2))
# heatmap.2(round(true_pos,2),
#           cellnote=round(true_pos,2),
#           main = "True_pos", # heat map title
#           notecol="black",      # change font color of cell labels to black
#           density.info="none",  # turns off density plot inside color legend
#           trace="none",         # turns off trace lines inside the heat map
#           margins =c(12,9),     # widens margins around plot
#           # col=my_palette,       # use on color palette defined earlier
#           # breaks=col_breaks,    # enable color transition at specified limits
#           dendrogram="none",     # only draw a row dendrogram
#           Colv="NA"
# )   
# 
# heatmap.2(round(false_neg,2),
#           cellnote=round(false_neg,2),
#           main = "False_neg", # heat map title
#           notecol="black",      # change font color of cell labels to black
#           density.info="none",  # turns off density plot inside color legend
#           trace="none",         # turns off trace lines inside the heat map
#           margins =c(12,9),     # widens margins around plot
#           # col=my_palette,       # use on color palette defined earlier
#           # breaks=col_breaks,    # enable color transition at specified limits
#           dendrogram="none",     # only draw a row dendrogram
#           Colv="NA",
#           Key= FALSE
# 
# )   
# 
