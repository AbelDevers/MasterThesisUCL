rm(list=ls())
cat("\014") # clear console
dev.off() # clear all plots

library(ggplot2)
library(RColorBrewer)  
library(reshape2) 
library(flux)
library(mc2d)

# loading data ------------------------------------------------------------
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
# load("Cross_res/simulation_10")
# load("Cross_res/simulation_10_RMT")
# load("Cross_res/simulation_10_absorbtionRatio")
# load("Cross_res/simulation_10_stationary")
# load("Cross_res/simulation_10_secondEig")
load("Cross_res/simulation_10_RMTsecond")



# cleaning errors ---------------------------------------------------------



res$precision[is.na(res$precision)] = 0
res$recall[is.na(res$recall)]=0
res$false_neg[is.na(res$recall)]=1


# setting params ----------------------------------------------------------

toDo = list(
  heatMap = TRUE,
  pos_return = TRUE,
  varVvar = TRUE,
  rangePrint = TRUE,
  print = TRUE,
  # id = "_rmt"
  # id = "_absorbtionRatio"
  # id = "_stationary"
  # id = "_vanilla"
  # id = "_secondEigen",
  id = "_rmtSecond"
)

if(toDo$id=="_vanilla"){
  benchmark = matrix(data = list(),nrow=3,ncol=1)
}else{
  load("benchmark_simulation")
}


# heatmap -----------------------------------------------------------------
if(toDo$heatMap){
  prepare_matrix <- function(mat,col_name = c("C","sigma","values"),p_values = NULL){
    

    
    if(!is.null(p_values)){
      mat[which(p_values>0.01)] <- NA
    }
    
    
    df = melt(mat)
    colnames(df) = col_name
    df$ax1 =1
    df$ax2 =1
    un1 = unique(df[,1])
    un2 = unique(df[,2])
    for(i in 1:length(df$ax1)){
      df$ax1[i]=which(df[i,1]==un1)
      df$ax2[i]=which(df[i,2]==un2)
    }
    return(df)
  }
  
  heatmapHM <- function(melt.mat){
    c_names = rownames(res$recall)
    c_names[length(c_names)] = bquote("\U221E")
    p <- ggplot(melt.mat,aes(x=ax1,y=ax2,fill = values))+
      geom_tile()+
      geom_text(aes(label = round(values, 2)),size = 3) +
      scale_fill_gradient(low = "blue", high = "white")+
      theme(text = element_text(size=12))+ 
      scale_y_discrete("Sigma",limits = colnames(res$recall))+
      scale_x_discrete("C",limits=c_names)
    p
    
    return(p)
  }
  
  
  true_pos.melt = prepare_matrix(res$recall)
  
  true_pos_post.melt = prepare_matrix(res$recall,p_values = res$p_values)
  
  precision.melt = prepare_matrix(res$precision)
  
  precision_post.melt = prepare_matrix(res$precision,p_values = res$p_values)
  
  false_neg.melt = prepare_matrix(res$false_neg)
  
  false_neg_post.melt = prepare_matrix(res$false_neg,p_values = res$p_values)
  
  p_values.melt = prepare_matrix(res$p_values)
  
  heatmapsList = list(
    recall = heatmapHM(true_pos.melt) + ggtitle(("Recall")),
    recall_post_test = heatmapHM(true_pos_post.melt) + ggtitle(("Recall, post test")),
    precision = heatmapHM(precision.melt) + ggtitle(("Precision")),
    precision_post_test = heatmapHM(precision_post.melt) + ggtitle("Precision, post test"),
    falsePositive = heatmapHM(false_neg.melt) + ggtitle(("False positive")),
    falsePositive_post_test = heatmapHM(false_neg_post.melt) + ggtitle(("False positive, post test")),
    p_values = heatmapHM(p_values.melt) + ggtitle(("P values"))
  )
  
  rm(false_neg.melt,precision.melt,true_pos.melt,heatmapHM,prepare_matrix)
  
}


# position of return ------------------------------------------------------
bestID = 11
if(toDo$pos_return){
  findPosition <- function(posVec){
    posVec[is.na(posVec)]=0
    firstOne = which(posVec==1)[1]
    lastOne = which(posVec==1)[length(which(posVec==1))]
    
    r = c(left=11-firstOne,right=lastOne-11)
    return(r)
  }
  
  statsOnPosition <- function(list_pos){
    df = NULL
    for(i in 1:length(list_pos)){
      temp_p = list_pos[[i]]
      if(!any(is.na(temp_p))){
        positions = findPosition(temp_p)
        df = rbind(df,positions)
      }
    }
    df = as.data.frame(df)
    df$absPos = df$left+1
    df = as.data.frame(df$absPos)
    colnames(df) = "absPos"
    
    # p = ggplot(df, aes(absPos))+
    #   geom_histogram(aes(y =..density..),breaks=1:11, col="red",fill="blue",alpha=0.3)+
    #   labs(x="Position of pred inside batch of 10")
    
    temp_df = data.frame(probabilities = dempiricalD(1:10,df$absPos), position = 1:10)
    
    if(toDo$id=="_vanilla"){
      p = ggplot(temp_df,aes(x = position, y = probabilities))+
        geom_bar(stat = "identity")+
        # geom_point(aes(y=probabilities))+
        # geom_ribbon(aes(ymin = 0,ymax=probabilities),alpha = 0.3)+
        scale_x_discrete(limits = 1:10)+
        ggtitle("Empirical probabilities of position")
      p
    }else{
      temp_df = cbind(temp_df,benchmark[[1]])
      colnames(temp_df)[3]="benchmark"
      
      p = ggplot(temp_df,aes(position))+
        # geom_point(aes(y=probabilities, color = toDo$id))+
        geom_bar(mapping = aes(y=probabilities, color = toDo$id),stat = "identity")+
        geom_bar(mapping = aes(y=benchmark,color="_benchmark"),stat = "identity",alpha = 0.2)+
        # geom_point(aes(y=benchmark, color = "_benchmark"), alpha = 0.5)+
        # geom_ribbon(aes(ymin = 0,ymax=probabilities),alpha = 0.3,fill = "red")+
        # geom_ribbon(aes(ymin = 0,ymax=benchmark),alpha = 0.1, fill = "blue")+
        scale_x_discrete(limits = 1:10)+
        ggtitle("Empirical probabilities of position")


    }
    
    
    r = list(
      p = p,
      df = df,
      summary = summary(df),
      probabilities = dempiricalD(1:10,df$absPos)
    )
    
    
    return(r)
  }
  
  
  
  # One param
  # list_pos = res$pred_pos[bestID][[1]]
  # statsPositionBest = statsOnPosition(list_pos = list_pos)
  
  # cumulate all test passing ones 
  list_pos = NULL
  test = res$p_values<=0.01
  for(i in 1:dim(test)[1]){
    for(j in 1:dim(test)[2]){
      if(test[i,j]){
        list_pos = append(list_pos,res$pred_pos[i,j][[1]])
      }
    } 
  }  
  rm(i,j)
  statsPositionCumulated = statsOnPosition(list_pos = list_pos)
  
  if(toDo$id=="_vanilla"){
    # updating the benchmark values
    benchmark[[1]]=statsPositionCumulated$probabilities
  }
  
  rm(list_pos,test)
    
}






# varVvar -----------------------------------------------------------------

if(toDo$varVvar){
  
  df = data.frame( precision = as.vector(res$precision),
                   recall = as.vector(res$recall), 
                   false_neg = as.vector(res$false_neg)
                   )
  df = df[with(df, order(-recall, -precision)), ]
  df$precisionCumMax = cummax(df$precision)
  df$OptimalPoint = (df$precisionCumMax==df$precision)
  head(df)
  df_save = df
  
  if(toDo$id=="_vanilla"){
    # updating the benchmark values
    benchmark[[2]]=df
    pPreRecall = ggplot(df,aes(x = recall))+
      geom_point(aes(y = precision,colour = OptimalPoint))+
      geom_ribbon(aes(ymin = 0, ymax = precisionCumMax), alpha = 0.3)+
      # geom_abline(intercept = 1,slope=-1)+
      ggtitle("Precision against recall")
  }else{
    df = cbind(df,benchmark[[2]])
    # df = rbind(FALSE,df)
    colnames(df)[6:10] = paste(colnames(df)[6:10],"benchmark",sep = "_")
    
    # df$precisionCumMax[1]=max(df$precisionCumMax[which(!is.na(df$precisionCumMax))])
    # df$precisionCumMax_benchmark[1]=max(df$precisionCumMax_benchmark[which(!is.na(df$precisionCumMax_benchmark))])
    
    
    head(df)
    
    
    pPreRecall = ggplot(df,aes(x = recall))+
      geom_point(aes(y = precision,colour = OptimalPoint))+
      geom_ribbon(aes(ymin = 0, ymax = precisionCumMax,fill = toDo$id), alpha = 0.3)+
      geom_ribbon(aes(x= recall_benchmark, ymin = 0, ymax = precisionCumMax_benchmark, fill = "_benchmark"), alpha = 0.3)+
      ggtitle("Precision against recall")+
      scale_fill_manual("AUC",values =c("red","blue"))
    
    pPreRecall
    
  }
  
  

  df = df_save
  
  df = df[with(df, order(false_neg, recall)), ]
  df$recallCumMax = cummax(df$recall)
  df$OptimalPointRecall = (df$recall==df$recallCumMax)
  head(df)
  
  if(toDo$id=="_vanilla"){
    # updating the benchmark values
    benchmark[[3]]=df
    pRecallfalseNegative = ggplot(df,aes(x = false_neg))+
      geom_point(aes(y = recall,colour = OptimalPointRecall))+
      geom_ribbon(aes(ymin = 0, ymax = recallCumMax), alpha = 0.3)+
      geom_abline(intercept = 0,slope=1)+
      ggtitle("Recall against false negative")
    
  }else{
    
    head(df)
    df = cbind(df,benchmark[[3]])
    colnames(df)[8:14] = paste(colnames(df)[8:14],"benchmark",sep = "_")
    
    # if(max(df$false_neg[which(!is.na(df$false_neg))])<
    #    max(df$false_neg_benchmark_benchmark[which(!is.na(df$false_neg_benchmark_benchmark))])){
    #    df = rbind(FALSE,df)
    #    df$recall
    # }
    # 
    # df = rbind(FALSE,df)
    
    # df$precisionCumMax[1]=max(df$precisionCumMax[which(!is.na(df$precisionCumMax))])
    # df$precisionCumMax_benchmark[1]=max(df$precisionCumMax_benchmark[which(!is.na(df$precisionCumMax_benchmark))])
    
    
    pRecallfalseNegative = ggplot(df,aes(x = false_neg))+
      geom_point(aes(y = recall,colour = OptimalPointRecall))+
      geom_ribbon(aes(ymin = 0, ymax = recallCumMax, fill=toDo$id), alpha = 0.3)+
      geom_ribbon(aes(x= false_neg_benchmark, ymin = 0, ymax = recallCumMax_benchmark, fill = "_benchmark"), alpha = 0.3)+
      geom_abline(intercept = 0,slope=1)+
      ggtitle("Recall against false negative")+
      scale_fill_manual("AUC",values =c("red","blue"))
    pRecallfalseNegative
    pRecallfalseNegative
    
  }
  

  
  
  varVvarList = list(
    pRecallfalseNegative = pRecallfalseNegative,
    pPreRecall = pPreRecall
  )
  
  rm(df,pRecallfalseNegative,pPreRecall)
  
}



# Save plots --------------------------------------------------------------

if(toDo$print){
  
  saveGraph <- function(id, p_name=NULL, p, w=NULL,h=NULL){
    mainDir = "C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Report/images"
    subDir =paste("Simulations",id,"/",sep="" )
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    if(is.null(p_name)){
      p_name = gsub(" ", "_", p$labels$title, fixed = TRUE)
      p_name = gsub(",", "",p_name, fixed = TRUE)
    }
    
    
    if(is.null(w) || is.null(h)){
      png(filename=paste("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Report/images/Simulations",
                         id,
                         "/",
                         "sim",
                         p_name,
                         id,
                         ".png",
                         sep=""
      ))
    }else{
      png(filename=paste("C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Report/images/Simulations",
                         id,
                         "/",
                         "sim",
                         p_name,
                         id,
                         ".png",
                         sep=""
      ),width = w,height = h)
    }
    
    print(p)
    dev.off()
    
    
  }
  
  
  if(toDo$heatMap){
    # dev.off()
    for(p in heatmapsList){
      
      saveGraph(id = toDo$id,p=p)
    }
  }
  
  if(toDo$pos_return){
    p = statsPositionCumulated$p
    saveGraph(id = toDo$id,p=p,w = 800,h = 400)

  }
  
  if(toDo$varVvar){
    # dev.off()
    for(p in varVvarList){
      p_name = gsub(" ", "_", p$labels$title, fixed = TRUE)
      p_name = gsub(",", "",p_name, fixed = TRUE)
      saveGraph(id = toDo$id,p_name = p_name,p=p)
    }
  }
  
  
  if(toDo$id=="_vanilla"){
    # updating the benchmark values
    save(benchmark,file = "benchmark_simulation")
  }
  
  
}



# print range -------------------------------------------------------------

if(toDo$rangePrint){
  test = res$p_values<0.01
  
  print(paste("False positive range: ",range(res$false_neg[test])))
  print(paste("Recall range: ",range(res$recall[test])))
  print(paste("Precision range: ",range(res$precision[!is.na(res$precision)])))
}
