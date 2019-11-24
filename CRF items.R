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

data_items <- read.csv("data_items.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))


##Set the names for lists of items

hightemp_items <- c("q55_1", "q55_2", "q55_3", "q55_4", "q55_5", "q55_6", "q55_7")
socialt_items <- c("q55_8", "q55_9", "q55_10", "q55_11", "q55_12")
solitaryt_items <- c("q55_13", "q55_14", "q55_15", "q55_16", "q55_17", "q55_18", "q55_19", "q55_20")

STRAQ_items <- c(hightemp_items, socialt_items, solitaryt_items)

anxiety2_items <- c("q53_1", "q53_70", "q53_72", "q53_74", "q53_76", "q53_78", "q53_80", "q53_81", "q53_82", "q53_83", "q53_84", "q53_85", "q53_87", "q53_88", "q53_90","q53_92")

avoidance2_items <- c("q53_97", "q53_98", "q53_93", "q53_99", "q53_100", "q53_101", "q53_102", "q53_104", "q53_89", "q53_91", "q53_86", "q53_79", "q53_77", "q53_94", "q53_75", "q53_73", "q53_71", "q53_69")

##Anxiety and Items 

data = data_items

data <- data[,names(data)%in% c("anxiety2", "avoidance2", STRAQ_items)]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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
anxiety <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

anxiety

##Avoidance and Items 

data <- NA
data <- read.csv("data_items.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
data <- data[,names(data)%in% c("anxiety", "avoidance", STRAQ_items)]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

#Number of seeds doubles as number of loops to iterate over
seeds <- 100

#Remove Missing rows
data <- na.omit(data)

#fit the random forest model to the test dataset
for (seed in 1:seeds){
  
  cforest_model<- cforest(avoidance ~ ., 
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
avoidance <- dotplot(sort(myvarimp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})

##Wellbeing with items

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "wellbeing")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

wellbeing
#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
wellbeingrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

wellbeingrank

##Anxious and items


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "anxious")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

anxious

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
anxiousrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

anxiousrank

##selfesteem

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "selfesteem")]
#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

selfesteem

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
selfesteemrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

selfesteemrank

##selfdiscipline


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "selfdiscipline")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

selfdisicpline

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
selfdisicplinerank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})


selfdisicplinerank

##loneliness

data <- NA

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "loneliness")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

loneliness

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
lonelinessrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

lonelinessrank

##sdo2
#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "sdo2")]


#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

sdo2

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
equaltorientrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

##rightprej


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "rightprej")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
rightprejrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

rightprejrank

##impulsive


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "impulsive")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

impulsive

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
impulsiverank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

impulsiverank

##thoughtful


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "thoughtful")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
thoughtfulrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .75), col='red', lty='longdash', lwd=2)})


##Trusting


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "trusting")]
#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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


#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
trustingrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

##rwa2


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "rwa2")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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


#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
rwa2rank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .75), col='red', lty='longdash', lwd=2)})

##stimulation


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "stimulation")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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


#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
stimulationrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

##sociable
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "sociable")]

#Create a new dataset by selcting the variables of interest

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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

#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
sociablerank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

##leader


#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "leader")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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


#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
leaderrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

##empathic

#Create a new dataset by selcting the variables of interest
data <- NA
data = data_items
data <- data[,names(data)%in% c(anxiety2_items, avoidance2_items, "STRAQ", "empathic")]

#Initialize objects
variable_importance_crf <- NA
myvarimp <- NA
myvarimprank <- NA

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


#Create a dataframe with one rank importance list by averaging the rank of the 100 importance lists
myvarimprank <-  rowMeans(as.data.frame(cbind(sapply(variable_importance_crf, rank))))

#Create a dotplot for the rank variable importance list
empathicrank <- dotplot(sort(myvarimprank), xlab="Variable Importance (predictors to right of dashed line belong to the top 25 percentile)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=quantile(myvarimprank, .90), col='red', lty='longdash', lwd=2)})

