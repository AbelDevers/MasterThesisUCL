rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots
library(stockPortfolio)
library(quantmod)
library(readr)

library(imputeTS)

setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')

# downloading data --------------------------------------------------------

if(FALSE){
  
  s_pList <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/s&pList.csv"))
  tickers = s_pList$tickers
  rm(s_pList)
  # sp500 <- new.env()
  df = c()
  df_volume = c()
  black_list = c("CAT","COP","EMC","NOV")
  for(i in 1:length(tickers)){
    tick = tickers[i]
    if(!(tick %in% black_list)){
      temp = getSymbols(tick,  src = "yahoo", from = as.Date("1990-01-01"), to = as.Date("2017-01-01"),auto.assign = FALSE)
      # temp = as.data.frame(temp)
      df = cbind(df,temp[,6])
      df_volume=cbind(df_volume,temp[,5])
    }
    # temp = setNames(temp[,4],tick)
    # df = cbind(df,temp)
    print(paste(i,tick))
    
  }
  names_vec = tickers[-which(tickers%in%black_list)]
  rm(temp,tickers,tick,i)
  
  save(list = ls(), file = "raw_equity")
}else{
  # If we don't load directly we just load the save on dropbox
  load(file = "raw_equity")
}



# add blacklist -----------------------------------------------------------

# s_pList <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/s&pList.csv"))



# NA and standardization --------------------------------------------------

for(i in 1:dim(df)[2]){
  # first put 0 everywhere if starting values are NA as they represent non traded values (google in 1990 etc)
  # Adding zero is the right move as it will not impact PCA (zero variance on a rolling window)
  if(is.na(df[1,i])){
    start_non_na = which(!is.na(df[,i]))[1]
    df[1:(start_non_na-1),i]=0
    df_volume[1:(start_non_na-1),i]=0
    df[(start_non_na):length(df[,i]),i] = na.ma(as.numeric(df[(start_non_na):length(df[,i]),i]))
    df_volume[(start_non_na):length(df[,i]),i] = na.ma(as.numeric(df_volume[(start_non_na):length(df[,i]),i]))
  }else{
    df[,i] = na.ma(as.numeric(df[,i]))
    df_volume[,i] = na.ma(as.numeric(df_volume[,i]))
  }
  
  # for the other missing values we simply use ma 
  
}

# old version: 
# out_index = c()
# out_names = c()
# for(i in 1:dim(df)[2]){
#   perc_na = sum(is.na(df[1:365,i]))/365
#   
#   if(perc_na<0.5){
#     df[,i] = na.ma(as.numeric(df[,i]))
#     df_volume[,i] = na.ma(as.numeric(df_volume[,i]))
#     # df[,i] = (df[,i]-mean(df[,i]))/sd(df[,i])
#   }else{
#     print(paste(i,names_vec[i],"is out"))
#     out_index = c(out_index,i)
#     out_names = c(out_names,names_vec[i])
#   }
# }
# rm(i)
# 
# df=df[,-out_index]
# df_volume=df_volume[,-out_index]
# dim(df)
# transofrm in return -----------------------------------------------------
# log return
# for(i in 1:dim(df)[2]){
#   df[,i] = log(df[-1,i]/df[-length(df[,i]),i])
# }




df_return = df#[-1,]
dim(df_return)
dim(df)
for(i in 1:dim(df)[2]){
   # df_return[,i] = diff(df[,i])/df[-length(df[,i]),i]
  df_return[,i] = diff(log(df[,i]))
  # tranform the log 0/0 into 0 return
  df_return[is.na(df_return[,i]),i]=0
  df_return[!is.finite(df_return[,i]),i]=0
}



# for the volumes, we just standardize the entries: 
for(i in 1:dim(df_volume)[2]){
  df_volume[,i] = (df_volume[,i]-mean(df_volume[,i])) / sd(df_volume[,i])
}


# par(mfrow=c(2,1))
# plot(df[,i])
# plot(df_return[,i])


# for(i in 1:dim(df)[2]){
#   print(any(is.na(df[,i])))
# }


# df= df_return
# df = df[2:dim(df)[1],]


df_equ = df
df_equ_return = df_return[-1,]
df_volume = df_volume[-(1:2),]
rm(df,df_return,black_list,i,names_vec,start_non_na)

