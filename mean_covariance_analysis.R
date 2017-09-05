

window = 365 # 15
mean_c = NULL
for(i in (window+1):dim(df_equ_return)[1]){
  X= df_equ_return[(i-window):(i-1),]
  C=cov(X)
  mean_c = c(mean_c,mean(C))
}
med = quantile(mean_c,probs = 0.5)
extreme = quantile(mean_c,probs = 0.95)


best_guess_med = C
best_guess_extreme = C
best_guess_med_v = NULL
best_guess_extreme_v = NULL
for(i in (window+1):dim(df_equ_return)[1]){
  X= df_equ_return[(i-window):(i-1),]
  C=cov(X)
  if(abs(mean(C)-med)<abs(mean(C)-mean(best_guess_med))){
    best_guess_med=C
    best_guess_med_v = X
  }
  if(abs(mean(C)-extreme)<abs(mean(C)-mean(best_guess_extreme))){
    best_guess_extreme=C
    best_guess_extreme_v = X
  }
}

best_guess_med
eigen(best_guess_med)$values[1]/sum(eigen(best_guess_med)$values)
eigen(best_guess_extreme)$values[1]/sum(eigen(best_guess_extreme)$values)

mean_extreme = mean(diag(best_guess_extreme))
mean_med = mean(diag(best_guess_med))

rm(df_equ,df_equ_return,df_volume)
rm(extreme,med,mean_extreme,mean_med)
rm(i,mean_c,window,X,C)

save(list  = ls(), file = "covariance_templates")


