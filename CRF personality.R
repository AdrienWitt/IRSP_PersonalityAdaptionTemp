##Load the packages
library(readr)
library(randomForest)
library(party)
library(lattice)
library(plyr)
library(dplyr)
library(tidyverse)
library(purrr)
library(TopKLists)

data_subscales <- read.csv("data_subscales.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
data = data_subscales

##Mediator variables

##Anxiety

#Create a new dataset by selcting the variables of interest
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "solitaryt", "socialt", "hightemp")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(anxiety2 ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
anxiety2 <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})


##Avoidance

#Create a new dataset by selcting the variables of interest
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "solitaryt", "socialt", "hightemp")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(avoidance2 ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
avoidance2 <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

avoidance2

##Personality variables

##Wellbeing

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "wellbeing")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(wellbeing ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
wellbeing <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

##Anxious


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "anxious")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(anxious ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
anxious <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

##selfesteem

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "selfesteem")]
#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(selfesteem ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
selfesteem <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})


##selfdiscipline

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "selfdiscipline")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(selfdiscipline ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
selfdisicpline <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})


##loneliness

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "loneliness")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(loneliness ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
loneliness <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})


##sdo2
#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "sdo2")]


#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(sdo2 ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
sdo2 <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})


##rightprej


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "rightprej")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(rightprej ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
rightprej <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

rightprej

##impulsive


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "impulsive")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(impulsive ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
impulsive <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})


##thoughtful


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "thoughtful")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(thoughtful ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
thoughtful <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

thoughtful


##Trusting


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "trusting")]
#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(trusting ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
trusting <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

trusting

##rwa2


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "rwa2")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(rwa2 ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
rwa2 <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

rwa2


##stimulation


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "stimulation")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(stimulation ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
stimulation <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 


##sociable
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "sociable")]

#Create a new dataset by selcting the variables of interest

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(sociable ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
sociable <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 

sociable

##leader


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "leader")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(leader ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
leader <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 

leader


##empathic

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "empathic")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(empathic ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
empathic <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 

empathic


##Openess


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "OPEN")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(OPEN ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
OPEN <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 

OPEN

##Conscientiousness


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "avoidance2", "STRAQ", "CONSC")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(CONSC ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
CONSC <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 


##Extraversion


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "STRAQ", "avoidance2", "EXTRA")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(EXTRA ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
EXTRA <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 

EXTRA


##Agreeableness



#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "STRAQ", "avoidance2", "AGREE")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(AGREE ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
AGREE <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 


##Neuroticism

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_subscales
data <- data[, names(data)%in% c("anxiety2", "STRAQ", "avoidance2", "NEURO")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA


#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(NEURO ~ ., 
                          data = data, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = .95, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  #estimate variable importance for crf
  variable_importance_crf[seed] <- list(varimp(cforest_model))
}

#Create a new dataframe one importance list by averaging the 100 importance lists resulting from the CRF.
myvarimp <- rowMeans(as.data.frame(variable_importance_crf))

#Create a dotplot for the variable importance list
NEURO <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)}) 

NEURO
