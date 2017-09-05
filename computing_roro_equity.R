rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots


library(readr)
library(imputeTS)
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
load("equit_data")
#the time lag is for the rolling pc (minimum > than nb of variable)
doAbsorbtion = FALSE
doStationary = FALSE
doSecondEigen = TRUE
if(doAbsorbtion){
  ind = 1:round(dim(df_equ_return)[2]*0.2)
}else{
  ind = 1
}



time_lag = 365

df_equ_return$roro = 0
df_volume$roro = 0
if(doSecondEigen){
  df_equ_return$second_roro = 0
  df_volume$second_roro = 0
}

for(i in (time_lag+1):dim(df_equ_return)[1]){
  # Equ
  X = df_equ_return[(i-time_lag):i,1:(dim(df_equ_return)[2]-1)]
  if(doStationary){
    X = X-rowMeans(X)
  }
  
  pca = princomp(X)
  eigs <- pca$sdev^2
  if(doSecondEigen){
    df_equ_return[i,dim(df_equ_return)[2]] = sum(eigs[2])/sum(eigs[2:length(eigs)])
    df_equ_return[i,dim(df_equ_return)[2]-1] = sum(eigs[1])/sum(eigs)
  }else{
    df_equ_return[i,dim(df_equ_return)[2]] = sum(eigs[ind])/sum(eigs)
  }
  
  # Volume
  X = df_volume[(i-time_lag):i,1:(dim(df_volume)[2]-1)]
  pca = princomp(X)
  eigs <- pca$sdev^2
  # df_volume[i,dim(df_volume)[2]] = sum(eigs[ind])/sum(eigs)
  if(doSecondEigen){
    df_volume[i,dim(df_volume)[2]] = sum(eigs[2])/sum(eigs[2:length(eigs)])
    df_volume[i,dim(df_volume)[2]-1] = sum(eigs[1])/sum(eigs)
  }else{
    df_volume[i,dim(df_volume)[2]] = sum(eigs[ind])/sum(eigs)
  }
  
}


for(i in 1:dim(X)[2]){
  print(i)
  print(X[which(!is.finite(X[,i])),i])
  
}

par(mfrow=c(1,1))
plot(df_equ_return$roro)
plot(df_volume$roro)


# plot(df_equ_return["2007-12-12/"]$roro)

rm(eigs,i,pca,time_lag,X,ind)

# head(df_equ_return[,1:(dim(df_equ_return)[2]-1)])

# save(list=ls(), file = "df_and_indexes")
# save(list=ls(), file = "equ_vol_roro")
# save(list  = ls(), file = "temp")
# save(list  = ls(), file = "equ_vol_BIG")
# save(list  = ls(), file = "equ_vol_SecondEig")



