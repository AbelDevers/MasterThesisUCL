rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots


library(readr)
library(imputeTS)
library(ggplot2)
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
load("equit_data")

vanilla = NULL
stationary = NULL
time_lag = 365
for(i in (time_lag+1):dim(df_equ_return)[1]){
  # Equ
  X = df_equ_return[(i-time_lag):i,1:(dim(df_equ_return)[2]-1)]
  Xstationary = X-rowMeans(X)
  
  
  pca = princomp(X)
  eigs <- pca$sdev^2
  vanilla = rbind(vanilla,eigs)
  
  
  pca = princomp(Xstationary)
  eigs <- pca$sdev^2
  stationary = rbind(stationary,eigs)
}

plot(colMeans(vanilla))
plot(colMeans(stationary))

df = data.frame(vanilla = colMeans(vanilla),stationary = colMeans(stationary), index = 1:length(colnames(stationary)))
df_vanilla = as.data.frame(vanilla)

pV = ggplot(data = df,aes(x=index))+
  geom_bar(aes(y = vanilla),stat = "identity", alpha = 0.4, fill = "blue", color = "grey")+
  scale_y_continuous(limits=c(0, 0.015))+
  ggtitle("Vanilla RoRo, mean eigenvalues bar chart")

pV2 = ggplot(data = df,aes(x=index))+
  geom_bar(aes(y = stationary),stat = "identity", alpha = 0.4, fill = "red", color = "grey")+ 
  scale_y_continuous(limits=c(0, 0.015))+
  ggtitle("Stationary RoRo, mean eigenvalues bar chart")
pV
pV2

# eigenMeanDist_vanilla
# eigenMeanDist_stationary


