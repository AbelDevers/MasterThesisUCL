rm(list=ls())
cat("\014") # clear console
dev.off() # clear all plots

library(ggplot2)
library(RColorBrewer)  
library(reshape2) 
library(flux)
library(moments)


# loading data ------------------------------------------------------------
setwd('C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Data')
# load("Cross_res/rbf_simple_equ_roro_1dayLag_15dayStress")

####### Done
# load("multiple_tresh_vanilla_res")
# load("multiple_tresh_vanilla_RMT_res")
# load("multiple_tresh_absorbtionRatio_res")
# load("multiple_tresh_stationary_res")
# load("multiple_tresh_VolOnly_res")
# load("multiple_tresh_NoVol_res")
load("multiple_tresh_rmt_second_res")


### half done

####### Todo
# load("multiple_tresh_secondEig")

# xtable(return1$summary,caption = paste(substring(toDo$id,2,nchar(toDo$id)),"1"))
# xtable(return5$summary,caption = paste(substring(toDo$id,2,nchar(toDo$id)),"5"))
# xtable(return10$summary,caption = paste(substring(toDo$id,2,nchar(toDo$id)),"10"))
# 
# 
# (return1$pred_return_summary)
# (return5$pred_return_summary)
# (return10$pred_return_summary)

# setting params ----------------------------------------------------------

toDo = list(
  heatMap =1,
  varVvar = 1,
  returnMap = 1,
  distanceAnalysis = 1,
  print = 1,
  LATEX = 1,
  # id = "_vanilla",
  # id = "_rmt",
  id = "_rmtSecond",
  # id = "_absrobtionRatio",
  # id = "_stationary",
  # id = "_volOnly",
  # id = "_noVol",
  # id = "_secondEigen",
  benchmark = "_vanilla"
  # benchmark = "_volOnly"
  # benchmark = "_absrobtionRatio"
)
if(toDo$id==toDo$benchmark){
  benchmark = list(
    #The id
    id = toDo$benchmark,
    
    # The var to var
    prcList = NULL,
    prcPlotSignificant = NULL,
    rocAUC = NULL,
    rocAUCQuarter = NULL,
    rocPlot = NULL,
    rocPlotQuarter = NULL,
    meanReturn = matrix(data = NA,nrow =3,ncol = 1)
    
    
  )
}else{
  load(paste("Equity",toDo$benchmark,"_benchmark",sep = ""))
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
    
    r = list(
      df = df,
      labelSigma = colnames(mat),
      labelC = rownames(mat)
    )
    
    return(r)
  }

  heatmapHM <- function(mat_list){
    
    melt.mat = mat_list$df
    
    
    p <- ggplot(melt.mat,aes(x=ax1,y=ax2,fill = values))+
          geom_tile()+
          geom_text(aes(label = round(values, 2)),size = 3) +
          scale_fill_gradient(low = "blue", high = "white")+
          theme(text = element_text(size=12))+ 
          # scale_y_discrete("Sigma")+
          # scale_x_discrete("C")+ 
          scale_y_discrete("Sigma",limits = mat_list$labelSigma)+
          scale_x_discrete("C",limits=mat_list$labelC)
    p
    p
      
    return(p)
  }
  
  createHeatMapList <- function(tempRes){
    recall.melt = prepare_matrix(tempRes$recall)
    
    recall.meltTest = prepare_matrix(tempRes$recall,p_values = tempRes$p_values)
    
    precision.melt = prepare_matrix(tempRes$precision)
    
    precision.meltTest = prepare_matrix(tempRes$precision,p_values = tempRes$p_values)
    
    false_neg.melt = prepare_matrix(tempRes$false_neg)
    
    false_neg.meltTest = prepare_matrix(tempRes$false_neg,p_values = tempRes$p_values)
    
    pred_return.melt = prepare_matrix(tempRes$pred_return)
    
    pred_return.meltTest = prepare_matrix(tempRes$pred_return,p_values = tempRes$p_values)
    
    full_return.melt = prepare_matrix(tempRes$pred_return*tempRes$nb_pred)
    
    full_return.meltTest = prepare_matrix(tempRes$pred_return*tempRes$nb_pred,p_values = tempRes$p_values)
    
    heatmapList = list(
      recall = heatmapHM(recall.melt) + ggtitle(paste("Recall","tresh",names(tempRes$tresh))),
      recallPassTest = heatmapHM(recall.meltTest) + ggtitle(paste("Recall, post test","tresh",names(tempRes$tresh))),
      precision = heatmapHM(precision.melt) + ggtitle(paste("Precision","tresh",names(tempRes$tresh))),
      precisionPassTest = heatmapHM(precision.meltTest) + ggtitle(paste("Precision, post test","tresh",names(tempRes$tresh))),
      falsePositive = heatmapHM(false_neg.melt) + ggtitle(paste("False positive","tresh",names(tempRes$tresh))),
      falsePositivePassTest = heatmapHM(false_neg.meltTest) + ggtitle(paste("False positive, post test","tresh",names(tempRes$tresh))),
      meanPredReturn = heatmapHM(pred_return.melt) + ggtitle(paste("Mean pred return","tresh",names(tempRes$tresh))),
      meanPredReturnPassTest = heatmapHM(pred_return.meltTest) + ggtitle(paste("Mean pred return, post test","tresh",names(tempRes$tresh))),
      predReturn = heatmapHM(full_return.melt) + ggtitle(paste("Pred return","tresh",names(tempRes$tresh))),
      predReturnPassTest = heatmapHM(full_return.meltTest) + ggtitle(paste("Pred return, post test","tresh",names(tempRes$tresh)))
    )
    
    return(heatmapList)
  }
  
  heatmapList_5_percent = createHeatMapList(res_mat[[2]])
  heatmapList_10_percent = createHeatMapList(res_mat[[3]])
  
  
  rm(createHeatMapList,heatmapHM,prepare_matrix)
}


# varVvar -----------------------------------------------------------------


if(toDo$varVvar){
  prepare_df <- function(res_mat_, PRC, rejectP_ = NULL){
    
    
    if(PRC){
      if(class(res_mat_)=="k-cross-mats"){
        # Here we have given a single treshold
        res = res_mat_
        df = data.frame(precision = as.vector(res$precision),
                        recall = as.vector(res$recall),
                        false_neg = as.vector(res$false_neg),
                        p_value = as.vector(res$p_values),
                        treshold = names(res$tresh)
        )
      }else{
        # we want to aggregate every treshold
        df = NULL
        for(res in res_mat_){
          temp_df = data.frame(precision = as.vector(res$precision),
                               recall = as.vector(res$recall), 
                               false_neg = as.vector(res$false_neg),
                               p_value = as.vector(res$p_values),
                               treshold = names(res$tresh)
          )
          df = rbind(df,temp_df)
        }
      }
      
      if(!is.null(rejectP_)){
        df = df[which(df$p_value<rejectP_),]
      }
      
      
      df = df[with(df, order(-recall, -precision)), ]
      df$precisionCumMax = cummax(df$precision)
      df$OptimalPoint = (df$precisionCumMax==df$precision)
      head(df)
      df = rbind(df,FALSE)
      ind = dim(df)[1]
      df$precision[ind] = max(df$precisionCumMax[!is.na(df$precisionCumMax)])
      df$precisionCumMax[ind] = max(df$precisionCumMax[!is.na(df$precisionCumMax)])
      df$recall[ind] = 0
      
      head(df)
      head(df)

    }else{
      df = NULL
      for(res in res_mat_){
        temp_df = data.frame(precision = as.vector(res$precision),
                        recall = as.vector(res$recall), 
                        false_neg = as.vector(res$false_neg),
                        p_value = as.vector(res$p_values),
                        treshold = names(res$tresh)
        )
        df = rbind(df,temp_df)
      }
      if(!is.null(rejectP_)){
        
        df = df[which(df$p_value<rejectP_),]
      }
      
      df = df[with(df, order(false_neg, recall)), ]
      df$recallCumMax = cummax(df$recall)
      df$OptimalPointRecall = (df$recall==df$recallCumMax)
    }
    

    
    return(df)
  }
  
  equalize_df_size <- function(df1, df2){
    d = dim(df1)[1]-dim(df2)[1]
    if(d<0){
      naMat = matrix(NA, nrow = -d,ncol = dim(df1)[2])
      colnames(naMat)=colnames(df1)
      df1 = rbind(df1,naMat) 
    }
    if(d>0){
      naMat = matrix(NA, nrow = d,ncol = dim(df2)[2])
      colnames(naMat)=colnames(df2)
      df2 = rbind(df2,naMat) 
    }
    
    r = list(
      df1 = df1,
      df2 = df2
    )
    return(r)
    
  }

  
  if(toDo$id== toDo$benchmark){
    prcList = list()
    benchmark$prcList = list()

    for(i in 1:length(res_mat)){
      df = prepare_df(res_mat[[i]],TRUE)
      p = ggplot(df,aes(x = recall))+
        geom_point(aes(y = precision,colour = OptimalPoint))+
        geom_ribbon(aes(ymin = 0, ymax = precisionCumMax), alpha = 0.3)+
        # geom_abline(intercept = 1,slope=-1)+
        ggtitle(paste("PRC |",toDo$id,"| treshold: ",names(res_mat[[i]]$tresh)))
      prcList[[i]] = p
      benchmark$prcList[[i]]=df

    }
    
    # The siginficant only PRC
    df = prepare_df(res_mat,TRUE, rejectP_ = 0.01)
    prcPlotSignificant = ggplot(df,aes(x = recall))+
      geom_point(aes(y = precision,shape = OptimalPoint, color = treshold))+
      geom_ribbon(aes(ymin = 0, ymax = precisionCumMax), alpha = 0.3)+
      # geom_abline(intercept = 1,slope=-1)+
      ggtitle(paste("PRC | significant points |",toDo$id, "|"))
    
    benchmark$prcPlotSignificant = df
    
    
  }else{
    prcList = list()
    
    for(i in 1:length(res_mat)){
      df = prepare_df(res_mat[[i]],TRUE)
      temp_l = equalize_df_size(df1 = df, df2 = benchmark$prcList[[i]])
      df = cbind(temp_l$df1,temp_l$df2)
      
      colnames(df)[8:14] = paste(colnames(df)[8:14],"_benchmark",sep = "")
      df$recall[is.na(df$recall)]=0
      df$precision[is.na(df$precision)]=0
      
      p = ggplot(df,aes(x = recall))+
        geom_point(aes(y = precision,colour = OptimalPoint))+
        geom_ribbon(aes(ymin = 0, ymax = precisionCumMax,fill = toDo$id), color = "black", alpha = 0.3)+
        geom_ribbon(aes(ymin = 0, ymax = precisionCumMax_benchmark, fill = paste("Benchmark",toDo$benchmark,sep = "")),
                        color = "black", alpha = 0.3)+
        ggtitle(paste("PRC |",toDo$id,"| treshold: ",names(res_mat[[i]]$tresh)))
      
      prcList[[i]] = p
      
      rm(p)
    }
    
    
    # The siginficant only PRC
    df = prepare_df(res_mat,TRUE, rejectP_ = 0.01)
    temp_l = equalize_df_size(df,benchmark$prcPlotSignificant)
    df = cbind(temp_l[[1]], temp_l[[2]])
    colnames(df)[8:14] = paste(colnames(df)[8:14],"_benchmark",sep = "") 
    
    prcPlotSignificant = ggplot(df,aes(x = recall))+
      geom_point(aes(y = precision,shape = OptimalPoint, color = treshold))+
      geom_ribbon(aes(ymin = 0, ymax = precisionCumMax,fill = toDo$id), alpha = 0.3, color = "black")+
      geom_ribbon(aes(x = recall_benchmark,ymin = 0, ymax = precisionCumMax_benchmark, fill = paste("benchmark",toDo$benchmark,sep="")), 
                  alpha = 0.1, color = "black")+
      # geom_abline(intercept = 1,slope=-1)+
      ggtitle(paste("PRC | significant points |",toDo$id, "|"))
    

    }
  
  
  
  
  
  if(toDo$id== toDo$benchmark){

    
    df = prepare_df(res_mat,FALSE)
    benchmark$rocPlot=df
    
    rocPlot = ggplot(df,aes(x = false_neg))+
      geom_point(aes(y = recall,colour = treshold))+
      geom_ribbon(aes(ymin = 0, ymax = recallCumMax), alpha = 0.3)+
      geom_abline(intercept = 0,slope=1)+
      scale_x_continuous("False positive")+
      ggtitle(paste("ROC |",toDo$id))
    
    rocAUC = auc(x=df$false_neg,y=df$recallCumMax)
    benchmark$rocAUC = rocAUC
    
    #### significant point only
    df = prepare_df(res_mat_ = res_mat,PRC = FALSE,rejectP_ = 0.01)
    
    benchmark$rocPlotQuarter = df
    
    rocPlotQuarter  = ggplot(df,aes(x = false_neg))+
      geom_point(aes(y = recall,colour = treshold))+
      geom_ribbon(aes(ymin = 0, ymax = recallCumMax), alpha = 0.3)+
      geom_abline(intercept = 0,slope=1)+
      scale_x_continuous("False positive")+
      ggtitle(paste("ROC | significant points |",toDo$id))

    
    rocAUCQuarter = (auc(x=df$false_neg,y=df$recallCumMax) / ((max(df$false_neg)^2)/2) )
    benchmark$rocAUCQuarter = rocAUCQuarter
    
  }else{
    df = prepare_df(res_mat,FALSE)
    
    df = cbind(df,benchmark$rocPlot)
    colnames(df)[8:14] = paste(colnames(df)[8:14],"_benchmark",sep = "")
    
    
    rocPlot = ggplot(df,aes(x = false_neg))+
      geom_point(aes(y = recall,colour = treshold))+
      geom_ribbon(aes(ymin = 0, ymax = recallCumMax, fill = toDo$id), color = "black", alpha = 0.3)+
      geom_ribbon(aes(x = false_neg_benchmark, ymin = 0, ymax = recallCumMax_benchmark,fill = paste("Benchmark",toDo$benchmark,sep = "")),
                  color = "black", alpha = 0.3)+
      geom_abline(intercept = 0,slope=1)+
      scale_x_continuous("False positive")+
      ggtitle(paste("ROC | significant points |",toDo$id))
    
    rocPlot
    
    rocAUC = auc(x=df$false_neg,y=df$recallCumMax)
    
    #### significant point only
    df = prepare_df(res_mat_ = res_mat,PRC = FALSE,rejectP_ = 0.01)
    temp_l = equalize_df_size(df1 = df,df2 = benchmark$rocPlotQuarter)
    
    df = cbind(temp_l$df1,temp_l$df2)
    colnames(df)[8:14] = paste(colnames(df)[8:14],"_benchmark",sep = "")
    
    rocPlotQuarter = ggplot(df,aes(x = false_neg))+
      geom_point(aes(y = recall,colour = treshold))+
      geom_ribbon(aes(x= false_neg,ymin = 0, ymax = recallCumMax, fill = toDo$id), color = "black", alpha = 0.3)+
      geom_ribbon(aes(x = false_neg_benchmark,ymin = 0, ymax = recallCumMax_benchmark,fill = paste("Benchmark",toDo$benchmark,sep = "")),
                  color = "black", alpha = 0.3)+
      geom_abline(intercept = 0,slope=1)+
      scale_x_continuous("False positive")+
      ggtitle(paste("ROC |", toDo$id))
    rocPlotQuarter
    rocPlotQuarter
    
    rocAUCQuarter = (auc(x=df$false_neg,y=df$recallCumMax) / (0.25*0.25/2))*0.5
    
  }
  
  

  
  rm(df,pRecallfalseNegative,pPreRecall)
  rm(i,p,prepare_df,df)
  rm(equalize_df_size,prepare_df)
  rm(temp_l)
  
}


# return plot and anaylsis ------------------------------------------------

if(toDo$returnMap){
  
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
        ggtitle(label = paste("Histogram of return |", toDo$id,"| tresh",names(res_mat[[ind_res_mat]]$tresh),"| sigma",sig,"| C",c_))
      
    }

    
    
    return_summary = matrix(data = NA, ncol = 2, nrow = 5)
    colnames(return_summary) = c("Pred return", "Full return")
    rownames(return_summary) = c("mean","var*100","skewness","kurtosis","Sum return")
    
    return_summary[1,] = round(c(mean(full_pred_return),mean(full_return)),3)
    return_summary[2,] = round(c(100*var(full_return),100*var(full_pred_return)),3)
    return_summary[3,] = round(c(skewness(full_return),skewness(full_pred_return)),3)
    return_summary[4,] = round(c(kurtosis(full_return),kurtosis(full_pred_return)),3)
    return_summary[5,] = round(cum_return,3)

    
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
  
  return1 = returnAnalysis(1)
  return5 = returnAnalysis(2)
  return10 = returnAnalysis(3)
  

  
}


# distance analysis -------------------------------------------------------
if(toDo$distanceAnalysis){
  
  # res = res_mat[[3]]
  # 
  # # nearest neighboor per date
  # load("Cross_res/xy_5percent")
  # rm(X,y,SandP)
  # y = (y_lagReturn < res$tresh)*1
  # y[is.na(y)] = 0
  # 
  # ind = which(y == 1)
  # hist(diff(ind),breaks = 25)
  #   
  # res$pred_position[1,1]
  # typeof(res$y_df[1,1][[1]])
  # t = rownames(res$y_df[1,1][[1]])
  # diff(as.Date(t))
}


# save for benchmark ------------------------------------------------------

if(toDo$benchmark==toDo$id && toDo$heatMap && toDo$varVvar){
  save(benchmark,file=paste("Equity",toDo$id,"_benchmark",sep = ""))
}




# Save plots --------------------------------------------------------------

if(toDo$print){
  
  saveGraph <- function(id, sub = "",p_name=NULL, p, w=NULL,h=NULL){
    mainDir = "C:/Users/Antoine Didisheim/Dropbox/UCL/Project/Report/images"
    subDir =paste("SandP",id,"/",sep="" )
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
    
    if(sub != ""){
      mainDir = paste(mainDir,"/",subDir,sep="")
      subDir =paste(sub,id,"/",sep="" )
      dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
      
      mainDir = paste(mainDir,subDir,sep="")
    }else{
      mainDir = paste(mainDir,"/",subDir,sep="")
    }
    
    if(is.null(p_name)){
      p_name = gsub(" ", "_", p$labels$title, fixed = TRUE)
      p_name = gsub(",", "",p_name, fixed = TRUE)
      p_name = gsub("%", "",p_name, fixed = TRUE)
      p_name = gsub(":", "",p_name, fixed = TRUE)
      p_name = gsub("|", "",p_name, fixed = TRUE)
    }
    
    
    if(is.null(w) || is.null(h)){
      png(filename=paste(mainDir,
                         "",
                         p_name,
                         id,
                         ".png",
                         sep=""
      ))
    }else{
      png(filename=paste(mainDir,
                         "",
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
    for(p in heatmapList_5_percent){
      saveGraph(id =toDo$id,p=p, sub ="hm5")    
    }
    for(p in heatmapList_10_percent){
      saveGraph(id =toDo$id,p=p,sub ="hm10")    
    }
  }

  
  if(toDo$varVvar){
    saveGraph(id = toDo$id,p=rocPlot)
    saveGraph(id = toDo$id,p=rocPlotQuarter)
    
    saveGraph(id = toDo$id,p=prcPlotSignificant)
    for(p in prcList){
      saveGraph(id = toDo$id,p=p,sub = "PRC_all")
    }
  }
  
  if(toDo$returnMap){
    w = 800; h = 400
    saveGraph(id = toDo$id,p=return1$plot, w=w,h=h, p_name = paste("return1Graph",toDo$id,sep = ""))
    saveGraph(id = toDo$id,p=return5$plot, w=w,h=h,p_name = paste("return5Graph",toDo$id,sep = ""))
    saveGraph(id = toDo$id,p=return10$plot, w=w,h=h,p_name = paste("return10Graph",toDo$id,sep = ""))
  }
  
}


if(toDo$LATEX){
  library(xtable)
  
  xtable(return1$summary)
  xtable(return5$summary)
  xtable(return10$summary)
}

















































