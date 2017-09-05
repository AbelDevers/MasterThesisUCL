rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots.

setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')

load(file = "df_and_indexes")

library(xts)

library(imputeTS)
library(e1071)

SandP <- xts(SandP[,-1], order.by=SandP[,1])
head(SandP)
# Transform all int xts ---------------------------------------------------

df_index <- xts(df_index[,-1], order.by=df_index[,1])
head(df_index)

df_index_return <- xts(df_index_return[,-1], order.by=df_index_return[,1])
head(df_index_return)

index_roro <- xts(index_roro[,-1], order.by=index_roro[,1])
colnames(index_roro) = "index_roro"
head(index_roro)


# create teh target -------------------------------------------------------
sp = df_index_return$Sand
length_stress_periode = 21
sp$returnLag = diff(sp,length_stress_periode)/sp
sp$returnDays = diff(sp$Sand,1)/sp$Sand

sp[is.na(sp)] = 0
head(sp)
hist(sp$returnLag)
hist(sp$returnDays)
# tresh = -0.075
tresh = quantile(x = sp$returnLag,probs = 0.05)
sp$stress_start = 0

# sp$return[is.na(sp$return)] = 0
add_condition_count = 0
for(i in 1:(length(sp[,1])-length_stress_periode)){
  if(sp$returnLag[i]<=tresh){
    nb_neg_in_future_lag = sum(sp$returnDays[i:(i+length_stress_periode)]<0)/length_stress_periode
    if(nb_neg_in_future_lag>0.5|| TRUE){
      # sp$stress[i:(i+length_stress_periode)] = 1
  
      sp$stress[i] = 1
    }else{
      add_condition_count = add_condition_count + 1
    }
    rm(nb_neg_in_future_lag)
    
  }  
}
plot(sp$stress)
sum(sp$stress)


# creating the x and y ----------------------------------------------------
rm(add_condition_count,i)


predictor = merge.xts(x = index_roro, y= df_equ_return$roro, join='left')
predictor = merge.xts(x = predictor, y= df_volume$roro, join='left')
colnames(predictor)[3] = "roro_volume"

predictor$roro = na.ma(as.numeric(predictor$roro))
predictor$roro_volume = na.ma(as.numeric(predictor$roro_volume))

head(predictor,20)

# Add differenced
o=dim(predictor)[2]
for(i in 1:o){
  for(j in 1:5){
    name = paste(colnames(predictor)[i],"lag",j)
    temp = predictor[,i]
    temp = diff(temp,j)
    temp[is.na(temp)]=0
    predictor = merge.xts(x=predictor,y=temp)
    colnames(predictor)[length(colnames(predictor))]=name
  }
}
rm(o,temp)
all_data = merge.xts(x=predictor, y = sp$stress, join='left')
head(all_data,20)

rm(predictor)
# Introducing time lag between X and y
prediction_lag = 1

X = all_data[1:(dim(all_data)[1]-prediction_lag),-dim(all_data)[2]]
y = all_data[(1+prediction_lag):(dim(all_data)[1]),dim(all_data)[2]]



# walk forward testing ----------------------------------------------------

start_train = 1
train_window = 180
test_window = 180

still_data = TRUE
detected_percenteage = c()
detected = c()
false_positive = c()
false_positive_percentage = c()
while(still_data){
  X_train = X[1:(start_train+train_window-1),]
  y_train = y[1:(start_train+train_window-1),]
  X_test = X[(start_train+train_window):(start_train+train_window+test_window-1),]
  y_test = y[(start_train+train_window):(start_train+train_window+test_window-1),]
  
  if(sum(y_train)>0){
    y_train = factor(y_train)
    # Train model
    fit = svm(x = X_train,y = y_train,type = "C-classification",kernel = "radial")
    # Test model
    pred = predict(fit,X_test)
    pred = as.numeric(levels(pred))[pred]
    correct = pred == array(y_test)
    
    detected_percenteage = c(detected_percenteage,sum(correct[which(y_test==1)])/sum(y_test==1))
    detected = c(detected,sum(correct[which(y_test==1)]))
    false_positive = c(false_positive,sum(sum(!correct[which(y_test==0)])))
    false_positive_percentage = c(false_positive_percentage,sum(sum(!correct[which(y_test==0)]))/sum(y_test==0))  
  }
  
  # update value and check condition
  start_train = start_train + train_window
  still_data = ((start_train+train_window+test_window-1)<length(y))
}
plot(detected_percenteage)
plot(detected)















