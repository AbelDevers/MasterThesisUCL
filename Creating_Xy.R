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
# load("equ_vol_roro")
# load("equ_vol_rmt")
# load("equ_vol_SecondEig")
load("equ_vol_rmt_second")

doSecondEigen = FALSE
load("sandp")


# params ------------------------------------------------------------------


# rm(df_equ,df_equ_return,df_volume)
# index return and graph analysis -----------------------------------------

SandP = SandP[index(df_equ)]
colnames(SandP) = "price"

par(mfrow=c(3,1))
plot(SandP)
plot(df_equ_return$roro)
plot(df_volume$roro)

SandP$return = diff(log(SandP$price))
plot(SandP$price)
plot(SandP$return)
hist((SandP$return))

SandP = SandP[index(df_equ_return)]

# creating the MA and delta of roro ---------------------------------------
# orders params

df_equ_return$delta_equ = computeRollingVal(df_equ_return$roro,doPlot = FALSE)
df_volume$delta_vol = computeRollingVal(df_volume$roro,doPlot = FALSE)

if(doSecondEigen){
  df_equ_return$delta_second_roro = computeRollingVal(df_equ_return$second_roro,doPlot = FALSE)
  df_volume$delta_vol_second_roro = computeRollingVal(df_volume$second_roro,doPlot = FALSE)
}

# par(mfrow=(c(2,1)))
# plot(df_equ_return$delta_equ)
# plot(df_volume$delta_vol)


# Defining the stress -----------------------------------------------------

par(mfrow=(c(1,1)))
plot(SandP$return)

length_stress_periode = 15
SandP$returnLag = diff(log(SandP$price),length_stress_periode)

SandP[is.na(SandP)] = 0
head(SandP)
hist(SandP$returnLag)
hist(SandP$return)
# tresh = -0.075
tresh = quantile(x = SandP$returnLag,probs = 0.05)
SandP$stress = 0

# SandP$return[is.na(SandP$return)] = 0
add_condition_count = 0
for(i in 1:(length(SandP[,1])-length_stress_periode)){
  if(SandP$returnLag[i]<=tresh){
    nb_neg_in_future_lag = sum(SandP$returnDays[i:(i+length_stress_periode)]<0)/length_stress_periode
    if(nb_neg_in_future_lag>0.5|| TRUE){
      # SandP$stress[i:(i+length_stress_periode)] = 1
      
      SandP$stress[i] = 1
    }else{
      add_condition_count = add_condition_count + 1
    }
    rm(nb_neg_in_future_lag)
    
  }  
}

par(mfrow=(c(2,1)))
plot(SandP$stress)
plot(SandP$price)
sum(SandP$stress)

# creating the x and y ----------------------------------------------------


if(doSecondEigen){
  predictor = merge.xts(x = df_equ_return[,-(1:(dim(df_equ_return)[2]-4))], y= df_volume[,-(1:(dim(df_volume)[2]-4))],
                        join='left')
}else{
  predictor = merge.xts(x = df_equ_return[,-(1:(dim(df_equ_return)[2]-2))], y= df_volume[,-(1:(dim(df_volume)[2]-2))],
                        join='left')
}
colnames(predictor)[3] = "roro_volume"
colnames(predictor)[1] = "roro_equ"

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
    predictor = merge.xts(x=predictor,y=temp)
    colnames(predictor)[length(colnames(predictor))]=name
  }
  
}
rm(o,temp)
head(predictor)


all_data = merge.xts(x=predictor, y = SandP[,3:4], join='left')
head(all_data,5)

rm(predictor)
# Introducing time lag between X and y
prediction_lag = 1

X = all_data[1:(dim(all_data)[1]-prediction_lag),-c(dim(all_data)[2],dim(all_data)[2]-1)]
y = all_data[(1+prediction_lag):(dim(all_data)[1]),c(dim(all_data)[2],dim(all_data)[2]-1)]

y$stress[which(is.na(y$stress))]=0
y_lagReturn = y$returnLag
y = y$stress

# reducing sample ---------------------------------------------------------
ind = which(X[,1]!=0)[1]:length(X[,1])
X = X[ind,]
y = y[ind,]
y_lagReturn = y_lagReturn[ind,]

SandP = merge.xts(x=y,y=SandP,all = c(TRUE,FALSE))
head(SandP)
rm(add_condition_count,i,ind,j,name,all_data,df_equ,df_equ_return,df_volume)
# simple split testing ----------------------------------------------------
# ind1 = 1:(length(X[,1])/2)
# ind2 = (length(X[,1])/2 + 1):(length(X[,1]))
# X_train = as.matrix(X[ind1,])
# y_train = as.matrix(y[ind1,])
# X_test = as.matrix(X[ind2,])
# y_test = as.matrix(y[ind2,])
# 
# fit = ksvm(x=X_train,y=factor(y_train),kernel="rbfdot",C=5)
# fit
# pred = kernlab::predict(object = fit,newdata=X_test)
# pred =  as.numeric(levels(pred))[pred]
# plot(pred)
# sum(pred)
# plot(sign(pred))
# 
# correct = pred == (y_test)
# # correct positive
# sum(correct[which(y_test==1)])
# # false positive
# sum(sum(!correct[which(y_test==0)]))
# 
# rm(name,pred,prediction_lag,j,ind2,ind1,add_condition_count,fit,i,ind,X_train,y_test,X_test,
#    y_test)
# 

# save(list  = ls(), file = "Cross_res/xy_5percent")
# save(list  = ls(), file = "Cross_res/xy_5percent_rmt")
save(list  = ls(), file = "Cross_res/xy_5percent_rmt_second")
# save(list  = ls(), file = "Cross_res/xy_5percent_absorbtionRatio")
# save(list  = ls(), file = "Cross_res/xy_5percent_stationary")
# save(list  = ls(), file = "Cross_res/xy_5percent_secondEig")




