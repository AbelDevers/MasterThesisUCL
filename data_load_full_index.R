
# rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots

library(readr)
library(imputeTS)
library(quantmod)
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')


# first load everything ---------------------------------------------------

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/^DJI.csv"))
DJI = setNames(df[,which(names(df) %in% c("Date","Adj Close"))],c("Date","DJI"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/^STOXX50E.csv"))
stoxx50e = setNames(df[,which(names(df) %in% c("Date","Adj Close"))],c("Date","Stoxx50E"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/^vix.csv"))
vix = setNames(df[,which(names(df) %in% c("Date","Adj Close"))],c("Date","vix"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/CaC40.csv"))
cac40 = setNames(df[,which(names(df) %in% c("Date","Adj Close"))],c("Date","cac40"))

df = getSymbols("^FTSE",  src = "yahoo", from = as.Date("1990-01-01"), to = as.Date("2017-01-01"),auto.assign = FALSE)
df = data.frame(date=index(df), coredata(df))
ftse = setNames(df[,which(names(df) %in% c("date","FTSE.Adjusted"))],c("Date","ftse"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/cad.csv"))
cad = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","cad"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/chf.csv"))
chf = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","chf"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/copper.csv"))
copper = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","copper"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/cotton.csv"))
cotton = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","cotton"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/crude_oil.csv"))
crude_oil = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","crude_oil"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/dax.csv"))
dax = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","dax"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/eur.csv"))
eur = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","eur"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/GBP.csv"))
gbp = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","gbp"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/gold.csv"))
gold = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","gold"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/heating_oil.csv"))
heating_oil = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","heating_oil"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/jpy.csv"))
jpy = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","jpy"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/nadaq.csv"))
nadaq = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","nadaq"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/natural_gas.csv"))
natural_gas = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","natural_gas"))
natural_gas$natural_gas[which(natural_gas$natural_gas==min(natural_gas$natural_gas))]=NA

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/nzd.csv"))
nzd = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","nzd"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/RU2000PR.csv"))
RU2000PR = setNames(df[,which(names(df) %in% c("DATE","RU2000PR"))],c("Date","RU2000PR"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/S&P.csv"))
SandP = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","SandP"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/silver.csv"))
silver = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","silver"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/soybean.csv"))
soybean = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","soybean"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/usd.csv"))
usd = setNames(df[,which(names(df) %in% c("DATE","DTWEXM"))],c("Date","usd"))

df <- as.data.frame(read_csv("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data/wheat.csv"))
wheat = setNames(df[,which(names(df) %in% c("Date","Settle"))],c("Date","wheat"))

rm(df)

# then merge them ---------------------------------------------------------

all_df = list(ftse,cac40,cad,chf,copper,cotton,crude_oil,dax,DJI,eur,gbp,gold,heating_oil,jpy,RU2000PR,stoxx50e,
              nadaq,natural_gas,nzd,SandP,silver,soybean,usd,vix,wheat)
rm(ftse,cac40,cad,chf,copper,cotton,crude_oil,dax,DJI,eur,gbp,gold,heating_oil,jpy,RU2000PR,stoxx50e,
   nadaq,natural_gas,nzd,SandP,silver,soybean,usd,vix,wheat)


min_dates = c()
max_dates = c()
for(df in all_df){
  min_dates = c(min_dates,min(df$Date))
  max_dates = c(max_dates,max(df$Date))
}
library("zoo")
min_dates = as.Date(max(min_dates))
max_dates = as.Date(min(max_dates))

df = setNames(as.data.frame(as.Date(min_dates:max_dates),col.names = c("Date")),"Date")

for(d in all_df){
  df = merge(x = df, y=d, by = "Date",all.x = TRUE)
}

rm(all_df,d,max_dates,min_dates)


# NA and standardization --------------------------------------------------

for(i in 2:dim(df)[2]){
  df[,i] = na.ma(as.numeric(df[,i]))
  # plot(df[,i])
  # print(paste(i,any(is.na(df[,i]))))
  # title(i)
  # df[,i] = (df[,i]-mean(df[,i]))/sd(df[,i])
}
rm(i)


# transofrm in return -----------------------------------------------------

df_return = df[-1,]
for(i in 2:dim(df)[2]){
  # df_return[,i] = log(df[-1,i]/df[-length(df[,i]),i])
  df_return[,i] = diff(df[,i])/df[-length(df[,i]),i]
}

for(i in 2:dim(df)[2]){
  # print(any(is.na(df[,i])))
  print(paste(i,any(df[,i]==0)))
}

for(i in 2:dim(df)[2]){
  plot(df_return[,i],x = df_return$Date,type = "l")
  # plot(df[,i],x = df$Date,type = "l")
  title(i)
}


rm(i)

df_index = df
df_index_return = df_index

rm(df,df_return)
