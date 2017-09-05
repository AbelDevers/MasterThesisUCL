rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots

N = 6805
percentage = 0.05
y = rep(0,N)
y[1:ceiling(percentage*N)] = 1


nb_pred = ceiling(N*percentage)
Sim = 1000*1000
ratio = NULL
for(s in 1:Sim){
  ind = sample(1:length(y),nb_pred)
  true_pos = sum(y[ind])/ceiling(percentage*N)
  false_neg = sum(y[ind]==0)/(N-ceiling(percentage*N))
  
  ratio = c(ratio,true_pos/false_neg)
  
}

rm(false_neg,ind,s,Sim,true_pos)

par(mfrow=c(1,2))

hist(ratio,probability = TRUE,breaks = 100)

dist = ecdf(ratio)
plot(dist)
dist(1)
dist(0.1/0.04)
