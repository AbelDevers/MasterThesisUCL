# rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots

library(readr)
library(imputeTS)
library(xts)
# setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')

#the time lag is for the rolling pc (minimum > than nb of variable)
time_lag = 365

index_roro = c()
for(i in (time_lag+1):dim(df_index)[1]){
  X = df_index_return[(i-time_lag):i,2:dim(df_index)[2]]
  pca = princomp(X)
  eigs <- pca$sdev^2
  new_line = as.data.frame(list(Date = df_index[i,1],index_roro = as.numeric(eigs[1]/sum(eigs))))
  index_roro = rbind(index_roro,new_line)
}
rm(new_line,X,eigs,i,pca,time_lag)
  
head(index_roro)
plot(index_roro$index_roro, x = index_roro$Date, type = "l")

# qxts <- xts(index_roro[,-1], order.by=index_roro[,1])

