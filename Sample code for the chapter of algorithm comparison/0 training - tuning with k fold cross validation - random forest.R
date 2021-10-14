#### import package ####
{
  library(ranger)
  library(dplyr)
  library(readr)
  library(spaMM)
  library(ROI.plugin.glpk)
  library(caret)
  library(pROC)
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(Matrix)
  library(data.table)
  library(neuralnet)
  library(scatterplot3d)
  library(nnet)
  library(RSNNS)
  library(caret)
  library(e1071)
  library(ciTools)
  
  
}

#### data importing ####

  
  graphics.off()
  options(digits=10)
  #import data and train the database
  {
    ### Load data and pre-processing
    CAdataset<-read.csv("Dataset.csv")
    ### response parameter
    CAdataset$ChangeC <-as.factor(as.character(CAdataset$ChangeC))
    CAdataset$ï..ChangeN<-as.numeric(as.character(CAdataset$ï..ChangeN))
    #### numerical predictor
    CAdataset$PB <-as.numeric(as.character(CAdataset$PB))
    CAdataset$Tmax <-as.numeric(as.character(CAdataset$Tmax))
    CAdataset$Tmin <-as.numeric(as.character(CAdataset$Tmin))
    CAdataset$Tave <-as.numeric(as.character(CAdataset$Tave))
    CAdataset$Latitude<-as.numeric(as.character(CAdataset$Latitude))
    CAdataset$Longitude<-as.numeric(as.character(CAdataset$Longitude))
    CAdataset$NTyear<-as.numeric(as.character(CAdataset$NTyear))
    
    ##### character parameter
    CAdataset$RCT <-as.factor(as.character(CAdataset$RCT))
    CAdataset$RNT <-as.factor(as.character(CAdataset$RNT))
    CAdataset$SCCT <-as.factor(as.character(CAdataset$SCCT))
    CAdataset$SCNT <-as.factor(as.character(CAdataset$SCNT))
    CAdataset$WPCT <-as.factor(as.character(CAdataset$WPCT))
    CAdataset$WPNT <-as.factor(as.character(CAdataset$WPNT))
    CAdataset$FCT <-as.factor(as.character(CAdataset$FCT))
    CAdataset$FNT <-as.factor(as.character(CAdataset$FNT))
    CAdataset$ST <-as.factor(as.character(CAdataset$ST))
    CAdataset$IrrigationCT<-as.factor(as.character(CAdataset$IrrigationCT))
    CAdataset$IrrigationNT<-as.factor(as.character(CAdataset$IrrigationNT))
    
    CAdataset$Crop<-as.factor(as.character(CAdataset$Crop))
    
    CAdataset_original<-CAdataset
  }
  n<-nrow(CAdataset)
  set.seed(3)
  # separate the dataset into two parts, 80% for training and tuning with CV, 20% for testing
  
  indextrain <- sample(1:n,round(0.8*n),replace=FALSE)
  
  CAdataset <- CAdataset_original[indextrain,]
  CAdataset_out_of_box<-CAdataset_original[-indextrain,]
  
  #### Classification - choose model with k-fold cross validation, then use CAdataset_oob (20%) to calculate the accuracy####
  
  #### random forest && random forest with spaMM - CV stage####
  # define the matrix to save CV - AUC data for each round of grid searching
  AUC_temp<-matrix(0, nrow =80, ncol = 1)
  Accuracy_temp<-matrix(0, nrow =80, ncol = 1)
  AUC_temp_spa<-matrix(0, nrow =80, ncol = 1)
  Accuracy_temp_spa<-matrix(0, nrow =80, ncol = 1)
  mtry1_temp<-matrix(0, nrow =80, ncol = 1)
  ntree1_temp<-matrix(0, nrow =80, ncol = 1)
  temp<-0
  
  #### searching grid #### 
  
  
  for (mtry1_seq in c(2,3,4,5,6,7,8,9,10,11)){
    for (ntree1_seq in c(300,400,500,600,700,800,900,1000)){
      temp<-temp+1
  cat("temp = ",temp,"\n")
  ### Dataset splitting for CV, one part for training, the rest (mmk) for testing in CV stage 
  n<-nrow(CAdataset)
  # K - fold cross validation, we set k = n
  kk<-10
  mmk<-floor(n/kk)
  
  # Define matrix used to save results in CV stage
  pred_rf<-matrix(0, nrow =n, ncol = 1)
  CAdataset_y_rf<-matrix(0, nrow =n, ncol = 1)
  pred_rf_spaMM<-matrix(0, nrow =n, ncol = 1)
  CAdataset_y_rf_spaMM<-matrix(0, nrow =n, ncol = 1)
  count_total<-0
  count_total_spaMM<-0
  
  # define sub-dataset for training (datatrain) and testing/prediction (dataCAdataset)
  for(k in 1:kk){
    cat(k)
    if(k==1){
      datatrain<-0
      dataCAdataset<-0
      datatrain<-CAdataset[(mmk+1):n,]
      dataCAdataset<-CAdataset[1:mmk,]
    }
    if(k==kk){
      datatrain<-0
      dataCAdataset<-0
      datatrain<-CAdataset[1:((k-1)*mmk),]
      dataCAdataset<-CAdataset[((k-1)*mmk+1):n,]
    }
    if(k>1 && k<kk){
      datatrain<-0
      dataCAdataset<-0
      datatrain1<-CAdataset[1:((k-1)*mmk),]
      datatrain2<-CAdataset[(k*mmk+1):n,]
      datatrain<-rbind(datatrain1,datatrain2)
      
      dataCAdataset<-CAdataset[((k-1)*mmk+1):(k*mmk),]
    }
    
    
    # number of random features used to build one tree, from the searching grid
    mtry1=mtry1_seq
    # number of trees, from the searching grid
    ntree1=ntree1_seq
    
    # model training - probability approach, with proximity=true, rf is for random forest, rf1 is for RF with spaMM
    rf <- ranger(ChangeC~PB+Tmax+Tmin+Tave+Crop+WPCT+WPNT+FCT+FNT+RCT+RNT+SCCT+SCNT+ST+IrrigationCT+IrrigationNT+Latitude+Longitude+NTyear,data=datatrain,num.tree = ntree1,mtry=mtry1,classification = TRUE,probability = TRUE,keep.inbag=T)
    rf1 <- ranger(ChangeC~PB+Tmax+Tmin+Tave+Crop+WPCT+WPNT+FCT+FNT+RCT+RNT+SCCT+SCNT+ST+IrrigationCT+IrrigationNT+NTyear,data=datatrain,num.tree = ntree1,mtry=mtry1,classification = TRUE,probability = TRUE,keep.inbag=T)
    
    # data prediction - RF - type probability
    pred222 <- predict(rf, data = dataCAdataset[,2:21])
    pred1<-pred222$predictions[,2]
    # Save prediction - RF
    for(hhh in 1:length(pred1)){
      count_total<-count_total+1
      pred_rf[count_total]<-as.numeric(as.character(pred1[hhh]))
      CAdataset_y_rf[count_total]<-as.numeric(as.character(dataCAdataset$ChangeC[hhh]))
    }
    
    
    
    # model training - RF with spaMM
    y_pred<-predict(rf1,data = datatrain[,2:21])
    pred_temp<-y_pred$predictions[,2]
    spa_rf<-fitme(ChangeC~pred_temp+Matern(1|Latitude+Longitude), data=datatrain,family = "binomial")
    
    # use rf to predict the probability for spa_rf CAdataset
    pred222 <- predict(rf1, data = dataCAdataset[,2:21])
    dataCAdataset$pred_temp<-pred222$predictions[,2]
    
    
    # data prediction - type probability
    pred222_spaMM <- predict(spa_rf, newdata = dataCAdataset[,19:22],type = "response",control=list(fix_predVar=FALSE))
    
    pred1_spaMM<-pred222_spaMM

    
    
    
    # Save prediction
    for(hhh in 1:length(pred1)){
      count_total_spaMM<-count_total_spaMM+1
      pred_rf_spaMM[count_total_spaMM]<-as.numeric(as.character(pred1_spaMM[hhh]))
      
      CAdataset_y_rf_spaMM[count_total_spaMM]<-as.numeric(as.character(dataCAdataset$Change[hhh]))
    }
    # Start another loop with another k
  }
  
  
  result_pred<-matrix(0, nrow =n, ncol = 2)
  result_pred<-cbind(pred_rf,CAdataset_y_rf)
  colnames(result_pred)<-c("prediction","observation")
  result_pred<-data.frame(result_pred)
  
  result_pred_spaMM<-matrix(0, nrow =n, ncol = 2)
  result_pred_spaMM<-cbind(pred_rf_spaMM,CAdataset_y_rf_spaMM)
  colnames(result_pred_spaMM)<-c("prediction","observation")
  result_pred_spaMM<-data.frame(result_pred_spaMM)
  
  ### Model accuracy - RF
  {
    lc_pred<-result_pred$prediction
    leng_total<-length(lc_pred)
    
    predict_rf_temp<-matrix(0, nrow = leng_total, ncol = 1)
    y_rf_temp<-matrix(0, nrow = leng_total, ncol = 1)
    predict_rf_temp<-result_pred$prediction
    y_rf_temp<-result_pred$observation
    
    
    
    count1111<-length(y_rf_temp)
    count11<-0
    for(i in 1:count1111){
      
      if(y_rf_temp[i]==1 && predict_rf_temp[i]>=0.5  ){
        count11=count11+1
      }
      if(y_rf_temp[i]==0 && predict_rf_temp[i]<0.5 ){
        count11=count11+1
      }
    }
    accuracy_rf<-count11/count1111
    #cat("\n RF accuracy is (when p threshold is 50%) : \n",accuracy,"\n")
    
    
    ### Plot AUC ROC curve
    
    
    
    #par(pty="s") 
    #par(mfrow=c(1,2))
    #maina<-paste("(a). RF ( mtry =",mtry1_seq,", ntree =",ntree1_seq,")")
    #roc(response= result_pred$observation,predictor= result_pred$prediction,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main=maina,col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1.5,print.auc.cex=1.2)
  }
  Accuracy_temp[temp]<-accuracy_rf
  ### Model accuracy - RF with spaMM
  {
    lc_pred<-result_pred_spaMM$prediction
    leng_total<-length(lc_pred)
    
    predict_rf_temp<-matrix(0, nrow = leng_total, ncol = 1)
    y_rf_temp<-matrix(0, nrow = leng_total, ncol = 1)
    predict_rf_temp<-result_pred_spaMM$prediction
    y_rf_temp<-result_pred_spaMM$observation
    
    
    
    count1111<-length(y_rf_temp)
    count11<-0
    for(i in 1:count1111){
      
      if(y_rf_temp[i]==1 && predict_rf_temp[i]>=0.5  ){
        count11=count11+1
      }
      if(y_rf_temp[i]==0 && predict_rf_temp[i]<0.5 ){
        count11=count11+1
      }
    }
    accuracy_spa<-count11/count1111
    #cat("\n RF with spaMM accuracy is (when p threshold is 50%) : \n",accuracy,"\n")
    
    
    ### Plot AUC ROC curve
    
    
    #mainb<-paste("(b). RF with spaMM ( mtry =",mtry1_seq,", ntree =",ntree1_seq,")")
    #roc(response= result_pred_spaMM$observation,predictor= result_pred_spaMM$prediction,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main=mainb,col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1.5,print.auc.cex=1.2)
    
  }
  # save results of CV
  AUC_temp[temp]<-auc(response= result_pred$observation,predictor= result_pred$prediction)
  cat("AUC of rf is",auc(response= result_pred$observation,predictor= result_pred$prediction),"\n")
  AUC_temp_spa[temp]<-auc(response= result_pred_spaMM$observation,predictor= result_pred_spaMM$prediction)
  Accuracy_temp_spa[temp]<-accuracy_spa
  mtry1_temp[temp]<-mtry1_seq
  ntree1_temp[temp]<-ntree1_seq
    }
  }
  # plot the results, AUC - hyperparameters
  tiff("nnt AUC-layers rf.tiff", width = 700, height = 700, units = 'mm', res = 300)
  par(mfrow=c(3,2))
  plot(mtry1_temp,AUC_temp)
  plot(ntree1_temp,AUC_temp)
  scatterplot3d(mtry1_temp,ntree1_temp,AUC_temp)
  
  plot(mtry1_temp,AUC_temp_spa)
  plot(ntree1_temp,AUC_temp_spa)
  scatterplot3d(mtry1_temp,ntree1_temp,AUC_temp_spa)
  
  dev.off()
  
  plot(mtry1_temp,AUC_temp)
  plot(ntree1_temp,AUC_temp)
  
  # find the best hyperparameter
  
  ind<-which(AUC_temp==max(AUC_temp))
  mtry1_temp[ind]
  ntree1_temp[ind]    

  ind1<-which(AUC_temp_spa==max(AUC_temp_spa))
  mtry1_temp[ind1]
  ntree1_temp[ind1]
### final model 
{
# number of random features used to build one tree
mtry1=mtry1_temp[ind]
# number of trees
ntree1=ntree1_temp[ind] 

set.seed(3)
# model training - probability approach, with proximity=true
rf <- ranger(ChangeC~PB+Tmax+Tmin+Tave+Crop+WPCT+WPNT+FCT+FNT+RCT+RNT+SCCT+SCNT+ST+IrrigationCT+IrrigationNT+Latitude+Longitude+NTyear,data=CAdataset,num.tree = ntree1_temp[ind] ,mtry=mtry1_temp[ind],classification = TRUE,probability = TRUE,keep.inbag=T)
set.seed(3)
rf1 <- ranger(ChangeC~PB+Tmax+Tmin+Tave+Crop+WPCT+WPNT+FCT+FNT+RCT+RNT+SCCT+SCNT+ST+IrrigationCT+IrrigationNT+NTyear,data=CAdataset,num.tree = ntree1_temp[ind1] ,mtry=mtry1_temp[ind1],classification = TRUE,probability = TRUE,keep.inbag=T)

# model training - RF with spaMM
y_pred<-predict(rf1,data = CAdataset[,2:21])
pred_temp<-y_pred$predictions[,2]
spa_rf<-fitme(ChangeC~pred_temp+Matern(1|Latitude+Longitude), data=CAdataset,family = "binomial")





#### evaluation of accuracy 

# data prediction - type probability
pred222 <- predict(rf, data = CAdataset_out_of_box[,2:21])
pred1_rf_oob<-pred222$predictions[,2]


# use rf to predict the probability for spa_rf CAdataset
pred222 <- predict(rf, data = CAdataset_out_of_box[,2:21])
CAdataset_out_of_box$pred_temp<-pred222$predictions[,2]


# data prediction - type probability
pred222_spaMM <- predict(spa_rf, newdata = CAdataset_out_of_box[,19:22],type = "response",control=list(fix_predVar=FALSE))
pred1_spaMM_oob<-pred222_spaMM

## performance

### Model accuracy - RF
{
  count1111<-length(pred1_rf_oob)
  count11<-0
  for(i in 1:count1111){
    
    if(CAdataset_out_of_box$ChangeC[i]==1 && pred1_rf_oob[i]>=0.5  ){
      count11=count11+1
    }
    if(CAdataset_out_of_box$ChangeC[i]==0 && pred1_rf_oob[i]<0.5 ){
      count11=count11+1
    }
  }
  accuracy_rf<-count11/count1111
  cat("\n RF accuracy is (when p threshold is 50%) : \n",accuracy_rf,"\n")
  
  tiff("ROC rf  300 dpi.tiff", width = 300, height = 300, units = 'mm', res = 300)
  ### Plot AUC ROC curve
  par(pty="s") 
  #par(mfrow=c(1,2))
  maina<-paste("RF ( mtry =",mtry1_temp[ind],", ntree =",ntree1_temp[ind],")")
  
  roc(response= CAdataset_out_of_box$ChangeC,predictor= pred1_rf_oob,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main=maina,col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1.5,print.auc.cex=1.2)
  dev.off()
  }
### Model accuracy - RF with spaMM
{
  count1111<-length(pred1_spaMM_oob)
  count11<-0
  for(i in 1:count1111){
    
    if(CAdataset_out_of_box$ChangeC[i]==1 && pred1_spaMM_oob[i]>=0.5  ){
      count11=count11+1
    }
    if(CAdataset_out_of_box$ChangeC[i]==0 && pred1_spaMM_oob[i]<0.5 ){
      count11=count11+1
    }
  }
  accuracy_spaMM<-count11/count1111
  cat("\n RF with spaMM accuracy is (when p threshold is 50%) : \n",accuracy_spaMM,"\n")
  
  
  ### Plot AUC ROC curve
  tiff("ROC rf with spaMM 300 dpi.tiff", width = 300, height = 300, units = 'mm', res = 300)
  par(pty="s")
  mainb<-paste("RF with spaMM ( mtry =",mtry1_temp[ind1],", ntree =",ntree1_temp[ind1],")")
  roc(response= CAdataset_out_of_box$ChangeC,predictor= pred1_spaMM_oob,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main=mainb,col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1.5,print.auc.cex=1.2)
  dev.off()
}

}
  
save.image(file='Random forest model.RData')
  
cat("accuracy = ",accuracy_rf,"\n")
cat("accuracy_spaMM = ",accuracy_spaMM,"\n")
AUC_temp[ind]
Accuracy_temp[ind]
AUC_temp_spa[ind1]
Accuracy_temp_spa[ind1]
  