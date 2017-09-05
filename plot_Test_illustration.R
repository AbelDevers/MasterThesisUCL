rm(list=ls()) # clear variables
cat("\014") # clear console
dev.off() # clear all plots

library(ggplot2)


?phyper

n = 6800
m = n*0.05
n = n-m

q = 0:m
k = m

df =data.frame(success = q, probability = NA)
df$probability = phyper(q=q,m=m,n,k=k)

df = df[1:min(which(df$probability==1)),]
df$probability = 1-df$probability
# df$rejection = (df$probability<0.1)*df$probability
df$rejection = (df$probability<0.1)*max(df$probability)*1.0
df$rejection[which(df$rejection==0)]=NA


# df$rejection[57] = df$probability[57]

p = ggplot(df, aes(x=success,y=probability))+
  # geom_line()+
  geom_bar(stat = "identity")+
  # geom_ribbon(aes(ymin =rejection,ymax=1, colour = "Reject H_0"),alpha = 0.3)
  geom_ribbon(aes(ymin =0,ymax=rejection, colour = "Reject H_0 (1%)"),alpha = 0.3)
p
df$density = dhyper(x=df$success,m=m,n=n,k=k)

# df$rejection = (df$probability<0.1)*df$density
df$rejection = (df$probability<0.1)*max(df$density)*1.1
df$rejection[which(df$rejection==0)]=NA



p2 = ggplot(df, aes(x=success,y=density))+
  # geom_line()+
  geom_bar(stat = "identity")+
  geom_ribbon(aes(ymin =0,ymax=rejection, colour = "Reject H_0 (1%)"),alpha = 0.3)

p2 








p_name = "ill_CDF"
png(filename=paste("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Report/images/General/",
                   "test",
                   p_name,
                   ".png",
                   sep=""
    ),
    width = 600,
    height = 450
)
print(p)
dev.off()

p_name = "ill_PMF"
png(filename=paste("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Report/images/General/",
                   "test",
                   p_name,
                   ".png",
                   sep=""
      ),
      width = 600,
      height = 450
)
print(p2)
dev.off()






