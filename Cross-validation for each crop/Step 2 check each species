# Compare classification ability for different packages
{
  #### load package
  library(randomForest)
  library(party)
  library(partykit)
  library(quantregForest)
  library(quantreg)
  library(lme4)
  library(lmerTest)
  library(tidyverse)
  library(caret)
  library(rfUtilities)
  require(MuMIn)
  library(rfUtilities)
  library(ranger)
  library(metafor)
  library(devtools)
  library(dplyr)
  library(crunch)
  library(ggplot2)
  #library(randomForestExplainer)
  #library(ranger)
  
  library(pROC)
}

{
load("D:/PhD/CA/4 Future scenario EGU/0 revised/3 cross validation/model_cross_validation_randomforest_second_check.RData")

# find index for each crop
n_barley<-which(test$Crop %in% 'barley.spring')
n_barley_winter<-which(test$Crop %in% 'barley.winter')
n_cotton<-which(test$Crop %in% 'cotton')
n_maize<-which(test$Crop %in% 'maize')
n_rice<-which(test$Crop %in% 'rice')
n_sorghum<-which(test$Crop %in% 'sorghum')
n_soybean<-which(test$Crop %in% 'soybean')
n_sunflower<-which(test$Crop %in% 'sunflower')
n_wheat<-which(test$Crop %in% 'wheat.winter')
n_wheat_spring<-which(test$Crop %in% 'wheat.spring')


# find preditions and observations for each crop
prediction_barley<-predict_qrf_temp[n_barley]
observation_barley<-y_qrf_temp[n_barley]

prediction_barley_winter<-predict_qrf_temp[n_barley_winter]
observation_barley_winter<-y_qrf_temp[n_barley_winter]

prediction_cotton<-predict_qrf_temp[n_cotton]
observation_cotton<-y_qrf_temp[n_cotton]

prediction_maize<-predict_qrf_temp[n_maize]
observation_maize<-y_qrf_temp[n_maize]

prediction_rice<-predict_qrf_temp[n_rice]
observation_rice<-y_qrf_temp[n_rice]

prediction_sorghum<-predict_qrf_temp[n_sorghum]
observation_sorghum<-y_qrf_temp[n_sorghum]

prediction_soybean<-predict_qrf_temp[n_soybean]
observation_soybean<-y_qrf_temp[n_soybean]

prediction_sunflower<-predict_qrf_temp[n_sunflower]
observation_sunflower<-y_qrf_temp[n_sunflower]

prediction_wheat<-predict_qrf_temp[n_wheat]
observation_wheat<-y_qrf_temp[n_wheat]

prediction_wheat_spring<-predict_qrf_temp[n_wheat_spring]
observation_wheat_spring<-y_qrf_temp[n_wheat_spring]

}



{
par(mfrow=c(3,3))

par(pty="s") 
roc(response= y_qrf_temp,predictor= predict_qrf_temp,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(a). Overall',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# barley


par(pty="s") 
roc(response= observation_barley,predictor= prediction_barley,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(b). spring barley',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)


# cotton

par(pty="s") 
roc(response= observation_cotton,predictor= prediction_cotton,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(c). cotton',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# maize



par(pty="s") 
roc(response= observation_maize,predictor= prediction_maize,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(d). maize',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# rice


par(pty="s") 
roc(response= observation_rice,predictor= prediction_rice,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(e). brice',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# sorghum


par(pty="s") 
roc(response= observation_sorghum,predictor= prediction_sorghum,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(f). sorghum',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)
# soybean


par(pty="s") 
roc(response= observation_soybean,predictor= prediction_soybean,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(g). soybean',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# sunflower


par(pty="s") 
roc(response= observation_sunflower,predictor= prediction_sunflower,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(h). sunflower',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)
# wheat


par(pty="s") 
roc(response= observation_wheat,predictor= prediction_wheat,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(i). winter wheat',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)




}



{

tiff("ROC all 300 dpi.tiff", width = 300, height = 300, units = 'mm', res = 300)
par(mfrow=c(3,3))

par(pty="s") 
roc(response= y_qrf_temp,predictor= predict_qrf_temp,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(a). overall',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# barley


par(pty="s") 
roc(response= observation_barley,predictor= prediction_barley,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(b). spring barley',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)


# cotton

par(pty="s") 
roc(response= observation_cotton,predictor= prediction_cotton,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(c). cotton',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# maize



par(pty="s") 
roc(response= observation_maize,predictor= prediction_maize,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(d). maize',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# rice


par(pty="s") 
roc(response= observation_rice,predictor= prediction_rice,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(e). rice',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# sorghum


par(pty="s") 
roc(response= observation_sorghum,predictor= prediction_sorghum,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(f). sorghum',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)
# soybean


par(pty="s") 
roc(response= observation_soybean,predictor= prediction_soybean,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(g). soybean',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)

# sunflower


par(pty="s") 
roc(response= observation_sunflower,predictor= prediction_sunflower,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(h). sunflower',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)
# wheat


par(pty="s") 
roc(response= observation_wheat,predictor= prediction_wheat,plot = TRUE,legacy.axes=TRUE,percent = TRUE,xlab="False positive percentage [%]",ylab="Ture positive percentage [%]",main='(i). winter wheat',col="#377eb8",lwd=4,print.auc=TRUE,cex.lab=1.5, cex.axis=1.2, cex.main=1.5, cex.sub=1.5,print.auc.cex=1.2)
legend("bottomright",legend=c("RandomForest with ntree =500 and mtry=3"),col = c("#377eb8"),lwd = 4,cex=1.2)


dev.off()


}

#par(pty="m")

###################################################################################








