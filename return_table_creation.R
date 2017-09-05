rm(list=ls())
cat("\014") # clear console
dev.off() # clear all plots

library(ggplot2)
library(RColorBrewer)  
library(reshape2) 
library(flux)
library(moments)
library(xtable)

# loading data ------------------------------------------------------------
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
# load("Cross_res/rbf_simple_equ_roro_1dayLag_15dayStress")

####### Done


table1 =NULL
table5 =NULL
table10 = NULL
table25 = NULL

load_str = c(
  "multiple_tresh_vanilla_res",
  "multiple_tresh_vanilla_RMT_res",
  "multiple_tresh_rmt_second_res",
  "multiple_tresh_absorbtionRatio_res",
  "multiple_tresh_stationary_res",
  "multiple_tresh_VolOnly_res",
  "multiple_tresh_NoVol_res"
)
label_str = c(
  "RoRo",
  "RMT",
  "RMT'",
  "Absorbtion_ratio",
  "Stationary roro",
  "VolOnly RoRo",
  "NoVol RoRo"
)

for(i in 1:length(label_str)){
  load(load_str[i])
  str = label_str[i]
  
  
  returnAnalysis <- function(ind_res_mat=2){
    test = (res_mat[[ind_res_mat]]$p_values<0.01)
    m = -res_mat[[ind_res_mat]]$pred_return*test*res_mat[[ind_res_mat]]$nb_pred
    
    ind_1 = which(m == max(m), arr.ind = TRUE) [1]
    ind_2 = which(m == max(m), arr.ind = TRUE) [2]
    
    cum_return = res_mat[[ind_res_mat]]$nb_pred[ind_1,ind_2] * res_mat[[ind_res_mat]]$pred_return[ind_1,ind_2]
    
    
    
    
    full_pred_return = res_mat[[ind_res_mat]]$full_pred_return[ind_1,ind_2][[1]]
    full_return = res_mat[[ind_res_mat]]$full_return[ind_1,ind_2][[1]]
    
    sig = colnames(res_mat[[ind_res_mat]]$nb_pred)[ind_2]
    c_ = rownames(res_mat[[ind_res_mat]]$nb_pred)[ind_1]
    
    df = data.frame(full_pred_return = c(full_pred_return,rep(NA,length(full_return)-length(full_pred_return))), 
                    full_return=full_return)
    
    mFull = mean(df$full_return)
    mPred = mean(df$full_pred_return[which(!is.na(df$full_pred_return))])
    
    if(TRUE){
      return_plot = ggplot(df, aes(full_return))+
        geom_histogram(aes(y =..density.., x=full_return, fill = "All returns"),bins = 30, col="red",alpha=0.3)+
        geom_histogram(aes(y =..density..,x =full_pred_return,fill="Pred returns"),bins = 30, col="black",alpha=0.3)+
        geom_vline(aes(colour = "All returns",xintercept = mFull),size=1, alpha = 0.5)+
        geom_vline(aes(colour = "Pred returns",xintercept = mPred),size=1, alpha = 0.5)+
        ylab(label = "Percentage")+
        xlab(label = "Return")+
        scale_fill_manual("Returns",values = c("blue","black"))+
        scale_color_manual("Means",values = c("blue","black"))+
        ggtitle(label = paste("Histogram of return |", str,"| tresh",names(res_mat[[ind_res_mat]]$tresh),"| sigma",sig,"| C",c_))
      
    }
    
    
    
    return_summary = matrix(data = NA, ncol = 2, nrow = 6)
    colnames(return_summary) = c("Pred return", "Full return")
    rownames(return_summary) = c("mean","var*100","skewness","kurtosis","Sharp ratio","Sum return")
    
    return_summary[1,] = round(c(mean(full_pred_return),mean(full_return)),3)
    return_summary[2,] = round(c(100*var(full_pred_return),100*var(full_return)),3)
    return_summary[3,] = round(c(skewness(full_pred_return),skewness(full_return)),3)
    return_summary[4,] = round(c(kurtosis(full_pred_return),kurtosis(full_return)),3)
    return_summary[5,] = c(round((mean(full_pred_return)-mean(full_return))/var(full_pred_return),3),NA)
    return_summary[6,] = c(round(cum_return,3),NA)
    
    
    pred_return_summary = matrix(data = NA, ncol = 1, nrow = 3)
    colnames(pred_return_summary) = c("Pred return")
    rownames(pred_return_summary) = c("Cumulated","var","Cumulated/Var")
    pred_return_summary = round(c(cum_return,var(full_pred_return),cum_return/var(full_pred_return)),3)
    
    return(
      list(
        plot = return_plot,
        tresh = res_mat[[ind_res_mat]]$tresh,
        summary = return_summary,
        pred_return_summary = pred_return_summary
      )
    )
  }
  
  return1 = returnAnalysis(1)$summary
  return5 = returnAnalysis(2)$summary
  return10 = returnAnalysis(3)$summary
  return25 = returnAnalysis(4)$summary
  
  table1 = cbind(table1,return1[,1])
  table5 = cbind(table5,return5[,1])
  table10 = cbind(table10,return10[,1])
  table25 = cbind(table25,return25[,1])
  
  
}

label_str = c("Market",label_str)
table1 = cbind(return1[,2],table1)
table5 = cbind(return5[,2],table5)
table10 = cbind(return10[,2],table10)
table25 = cbind(return25[,2],table25)

colnames(table1) = label_str
colnames(table5) = label_str
colnames(table10) = label_str
colnames(table25) = label_str


cat("\014")
xtable(table1,label = "table:1percent",caption = "Return results | 1 percent treshold")

cat("\014")
xtable(table5,label = "table:5percent",caption = "Return results | 5 percent treshold")

cat("\014")
xtable(table10,label = "table:10percent",caption = "Return results | 10 percent treshold")

cat("\014")
xtable(table25,label = "table:25percent",caption = "Return results | 25 percent treshold")

tableRoRo = cbind(table1[,1],table5[,1],table10[,1],table25[,1])
colnames(tableRoRo) = c("1","5","10","25")

cat("\014")
xtable(tableRoRo,label = "table:RoRoReturn",caption = "RoRo Return")




