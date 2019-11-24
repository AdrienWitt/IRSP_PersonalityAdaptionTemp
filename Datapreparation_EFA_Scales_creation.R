##Load the packages
library(haven)
library(psych)
library(dplyr)
library(outliers)
library(lubridate)
library(xtable)

testweek <- read.csv("train.csv", header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))
names(testweek) <- tolower(names(testweek))


##I first proceed through the data preparation, the EFA and the creation of the new personality scales comes after

# Only including those with a progress above 97. 
testweekfull <- testweek[ which(testweek$progress > 97), ]


## RMTQ 
## Item 10 is reverse keyed --> instead of the item 6 ??
## Alpha = .67, Omega Hierarchical = .42, Omega Total= .78. 
## Hierarchical solution ran into a number of problems: loading greater than abs(1), ultra-heywood case was detected, and diag(.) had 0 or NA entries. 

RMTQ_data <- testweekfull[,c(1, match("q67_1", names(testweekfull)):match("q67_18", names(testweekfull)))]
RMTQ_data <- cbind(RMTQ_data, testweekfull$responseid)
colnames(RMTQ_data)[colnames(RMTQ_data)=="testweekfull$responseid"] <- "responseid"

RMTQ_data_r <- c("q67_10")
RMTQ_data[RMTQ_data_r] <- 5 - RMTQ_data[RMTQ_data_r]

RMTQ_items <- c("q67_1", "q67_2", "q67_3", "q67_4", "q67_5", "q67_6", "q67_7", "q67_8", "q67_9", "q67_10", "q67_11", "q67_12", "q67_13", "q67_14", "q67_15", "q67_16", "q67_17", "q67_18")
##psych::alpha(RMTQ_data[RMTQ_items], check.keys=TRUE) ## removed check.keys after recalculating
psych::alpha(RMTQ_data[RMTQ_items]) 
omega(RMTQ_data[RMTQ_items], nfactors = 4)
RMTQ_data$RMTQ <- rowMeans(RMTQ_data[RMTQ_items])

RMTQ_data2 = RMTQ_data[c("RMTQ","responseid")]


## BIG FIVE: IPIP-NEO 
## Openness to Experience: 3, 8, 13, 18, 23, 28, 33, 38, 43, 48r, 53r, 58, 63, 68r, 73r, 78r, 83r, 88r, 93, 98r, 103r, 108r, 113r, 118r
## Alpha:                 0.81; Omega Hierarchical:    0.52; Omega Total            0.84 
## Scale does not present problems. 

OPEN_data <- testweekfull[,c("q59_3", "q59_8", "q59_13", "q59_18", "q59_23", "q59_28", "q59_33", "q59_38", "q59_43", "q59_48", "q59_53", "q59_58", "q59_63", "q59_68", "q59_73", "q59_78", "q59_83", "q59_88", "q59_93", "q59_98", "q59_103", "q59_108", "q59_113", "q59_118")]
OPEN_data <- cbind(OPEN_data, testweekfull$responseid)
colnames(OPEN_data)[colnames(OPEN_data)=="testweekfull$responseid"] <- "responseid"

OPEN_data_r <- c("q59_48", "q59_53", "q59_68", "q59_73", "q59_78", "q59_83", "q59_88", "q59_98", "q59_103", "q59_108", "q59_113", "q59_118")
OPEN_data[OPEN_data_r] <- 6 - OPEN_data[OPEN_data_r]

OPEN_items <- c("q59_3", "q59_8", "q59_13", "q59_18", "q59_23", "q59_28", "q59_33", "q59_38", "q59_43", "q59_48", "q59_53", "q59_58", "q59_63", "q59_68", "q59_73", "q59_78", "q59_83", "q59_88", "q59_93", "q59_98", "q59_103", "q59_108", "q59_113", "q59_118")
psych::alpha(OPEN_data[OPEN_items]) 
omega(OPEN_data[OPEN_items])
OPEN_data$OPEN <- rowMeans(OPEN_data[OPEN_items])

OPEN_data2 = OPEN_data[c("OPEN","responseid")]

addingOPEN <- merge(RMTQ_data2, OPEN_data2, "responseid", na.rm=TRUE)

## Conscientiousness: 40r, 70r, 100r, 75r, 105r, 80r, 110r, 85r, 115r, 30r, 60r, 90r, 120r, 5, 35, 65, 95, 10, 15, 45, 20, 50, 25, 55
## Alpha:                 0.86; Omega Hierarchical:    0.46; Omega Total            0.89 
## Scale does not present problems. 

CONSC_data <- testweekfull[,c("q59_40", "q59_70", "q59_100", "q59_75", "q59_105", "q59_80", "q59_110", "q59_85", "q59_115", "q59_30", "q59_60", "q59_90", "q59_120", "q59_5", "q59_35", "q59_65", "q59_95", "q59_10", "q59_15", "q59_45", "q59_20", "q59_50", "q59_25", "q59_55")]
CONSC_data <- cbind(CONSC_data, testweekfull$responseid)
colnames(CONSC_data)[colnames(CONSC_data)=="testweekfull$responseid"] <- "responseid"

CONSC_data_r <- c("q59_40", "q59_70", "q59_100", "q59_75", "q59_105", "q59_80", "q59_110", "q59_85", "q59_115", "q59_30", "q59_60", "q59_90", "q59_120")
CONSC_data[CONSC_data_r] <- 6 - CONSC_data[CONSC_data_r]

CONSC_items <- c("q59_40", "q59_70", "q59_100", "q59_75", "q59_105", "q59_80", "q59_110", "q59_85", "q59_115", "q59_30", "q59_60", "q59_90", "q59_120", "q59_5", "q59_35", "q59_65", "q59_95", "q59_10", "q59_15", "q59_45", "q59_20", "q59_50", "q59_25", "q59_55")
psych::alpha(CONSC_data[CONSC_items]) 
omega(CONSC_data[CONSC_items])
CONSC_data$CONSC <- rowMeans(CONSC_data[CONSC_items])

CONSC_data2 = CONSC_data[c("CONSC","responseid")]

addingCONSC<- merge(addingOPEN, CONSC_data2, "responseid", na.rm=TRUE)

## Extraversion: 62r, 92r, 67r, 97r, 102r, 107r, 2, 32, 7, 37, 12, 42, 72, 17, 47, 77, 22, 52, 82, 112, 27, 57, 87, 117
## Alpha:                 0.84; Omega Hierarchical:    0.55; Omega Total            0.87 
## Scale does not present problems. 
##Item 59_82 "J'aime agir de mani?re irr?fl?chie" correlated negatively with the scale. For this reason I dropped it from the scale

EXTRA_data <- testweekfull[,c("q59_62", "q59_92", "q59_67", "q59_97", "q59_102", "q59_107", "q59_2", "q59_32", "q59_7", "q59_37", "q59_12", "q59_42", "q59_72", "q59_17", "q59_47", "q59_77", "q59_22", "q59_52", "q59_112", "q59_27", "q59_57", "q59_87", "q59_117")]
EXTRA_data <- cbind(EXTRA_data, testweekfull$responseid)
colnames(EXTRA_data)[colnames(EXTRA_data)=="testweekfull$responseid"] <- "responseid"

EXTRA_data_r <- c("q59_62", "q59_92", "q59_67", "q59_97", "q59_102", "q59_107")
EXTRA_data[EXTRA_data_r] <- 6 - EXTRA_data[EXTRA_data_r]

EXTRA_items <- c("q59_62", "q59_92", "q59_67", "q59_97", "q59_102", "q59_107", "q59_2", "q59_32", "q59_7", "q59_37", "q59_12", "q59_42", "q59_72", "q59_17", "q59_47", "q59_77", "q59_22", "q59_52", "q59_112", "q59_27", "q59_57", "q59_87", "q59_117")
psych::alpha(EXTRA_data[EXTRA_items]) 
omega(EXTRA_data[EXTRA_items])
EXTRA_data$EXTRA <- rowMeans(EXTRA_data[EXTRA_items])

EXTRA_data2 = EXTRA_data[c("EXTRA","responseid")]

addingEXTRA<- merge(addingCONSC, EXTRA_data2, "responseid", na.rm=TRUE)

## Agreeableness: 94r, 9r, 39r, 69r, 99r, 74r, 104r, 19r, 49r, 79r, 109r, 24r, 54r, 84r, 114r, 89r, 119r, 4, 34, 64, 14, 44, 29, 59
## 84 (J'ai une tr?s bonne opinion de moi-m?me; Have a high opinion of myself) is in the original scale indicated to correlate negatively with agreeableness
## Alpha:                 0.83; Omega Hierarchical:    0.43; Omega Total            0.87 
## Scale does not present problems. 

AGREE_data <- testweekfull[,c("q59_94", "q59_9", "q59_39", "q59_69", "q59_99", "q59_74", "q59_104", "q59_19", "q59_49", "q59_79", "q59_109", "q59_24", "q59_54", "q59_114", "q59_89", "q59_119", "q59_4", "q59_34", "q59_64", "q59_14", "q59_44", "q59_29", "q59_59", "q59_84")]
AGREE_data <- cbind(AGREE_data, testweekfull$responseid)
colnames(AGREE_data)[colnames(AGREE_data)=="testweekfull$responseid"] <- "responseid"

AGREE_data_r <- c("q59_94", "q59_9", "q59_39", "q59_69", "q59_99", "q59_74", "q59_104", "q59_19", "q59_49", "q59_79", "q59_109", "q59_24", "q59_114", "q59_89", "q59_119")
AGREE_data[AGREE_data_r] <- 6 - AGREE_data[AGREE_data_r]

AGREE_items <- c("q59_94", "q59_9", "q59_39", "q59_69", "q59_99", "q59_74", "q59_104", "q59_19", "q59_49", "q59_79", "q59_109", "q59_24", "q59_54", "q59_114", "q59_89", "q59_119", "q59_4", "q59_34", "q59_64", "q59_14", "q59_44", "q59_29", "q59_59", "q59_84")
psych::alpha(AGREE_data[AGREE_items]) 
omega(AGREE_data[AGREE_items])
AGREE_data$AGREE <- rowMeans(AGREE_data[AGREE_items])

AGREE_data2 = AGREE_data[c("AGREE","responseid")]

addingAGREE<- merge(addingEXTRA, AGREE_data2, "responseid", na.rm=TRUE)

## Neuroticism: 96r, 101r, 106r, 51r, 81r, 111r, 116r, 1, 31, 61, 91, 6, 36, 66, 11, 41, 71, 16, 46, 76, 21, 26, 56, 86
## Alpha:                 0.88; Omega Hierarchical:    0.58; Omega Total            0.9 
## Scale does not present problems. 

NEURO_data <- testweekfull[,c("q59_96", "q59_101", "q59_106", "q59_51", "q59_81", "q59_111", "q59_116", "q59_1", "q59_31", "q59_61", "q59_91", "q59_6", "q59_36", "q59_66", "q59_11", "q59_41", "q59_71", "q59_16", "q59_46", "q59_76", "q59_21", "q59_26", "q59_56", "q59_86")]
NEURO_data <- cbind(NEURO_data, testweekfull$responseid)
colnames(NEURO_data)[colnames(NEURO_data)=="testweekfull$responseid"] <- "responseid"

NEURO_data_r <- c("q59_96", "q59_101", "q59_106", "q59_51", "q59_81", "q59_111", "q59_116")
NEURO_data[NEURO_data_r] <- 6 - NEURO_data[NEURO_data_r]

NEURO_items <- c("q59_96", "q59_101", "q59_106", "q59_51", "q59_81", "q59_111", "q59_116", "q59_1", "q59_31", "q59_61", "q59_91", "q59_6", "q59_36", "q59_66", "q59_11", "q59_41", "q59_71", "q59_16", "q59_46", "q59_76", "q59_21", "q59_26", "q59_56", "q59_86")
psych::alpha(NEURO_data[NEURO_items]) 
omega(NEURO_data[NEURO_items])
NEURO_data$NEURO <- rowMeans(NEURO_data[NEURO_items])

NEURO_data2 = NEURO_data[c("NEURO","responseid")]

addingNEURO<- merge(addingAGREE, NEURO_data2, "responseid", na.rm=TRUE)

## Specism Scale
## item 5 is reverse keyed.
## Alpha: 0.77; Omega Hierarchical:    0.73; Omega Total            0.8. 
## Hierarchical solution ran into a number of problems: loading greater than abs(1), ultra-heywood case was detected, and diag(.) had 0 or NA entries. 

specism_data <- testweekfull[,c(1, match("q61_1", names(testweekfull)):match("q61_6", names(testweekfull)))]
specism_data <- cbind(specism_data, testweekfull$responseid)
colnames(specism_data)[colnames(specism_data)=="testweekfull$responseid"] <- "responseid"

specism_data_r <- c("q61_5")
specism_data[specism_data_r] <- 8 - specism_data[specism_data_r]

specism_items <- c("q61_1", "q61_2", "q61_3", "q61_4", "q61_5", "q61_6")
## psych::alpha(specism_data[specism_items], check.keys=TRUE) ## removed check.keys after recalculating
psych::alpha(specism_data[specism_items]) 
omega(specism_data[specism_items], nfactors = 4)
specism_data$specism <- rowMeans(specism_data[specism_items])

specism_data2 = specism_data[c("specism","responseid")]

addingspecism<- merge(addingNEURO, specism_data2, "responseid", na.rm=TRUE)

## RSE 
## items 1, 2, 4, 6, and 7 are reverse keyed.
## Hierarchical solution ran into a number of problems: loading greater than abs(1), ultra-heywood case was detected, and diag(.) had 0 or NA entries. 
## Alpha:                 0.89; Omega Hierarchical:    0.79; Omega Total            0.91 

RSE_data <- testweekfull[,c(1, match("q56_1", names(testweekfull)):match("q56_10", names(testweekfull)))]
RSE_data <- cbind(RSE_data, testweekfull$responseid)
colnames(RSE_data)[colnames(RSE_data)=="testweekfull$responseid"] <- "responseid"

RSE_data_r <- c("q56_1", "q56_2", "q56_4", "q56_6", "q56_7")
RSE_data[RSE_data_r] <- 5 - RSE_data[RSE_data_r]

RSE_items <- c("q56_1", "q56_2", "q56_3", "q56_4", "q56_5", "q56_6", "q56_7", "q56_8", "q56_9", "q56_10")
## psych::alpha(RSE_data[RSE_items], check.keys=TRUE) ## removed check.keys after recalculating
psych::alpha(RSE_data[RSE_items]) 
omega(RSE_data[RSE_items], nfactors = 4)
RSE_data$RSE <- rowMeans(RSE_data[RSE_items])

RSE_data2 = RSE_data[c("RSE","responseid")]

addingRSE<- merge(addingspecism, RSE_data2, "responseid", na.rm=TRUE)

## Self-control
## items 1, 6, 8 and 11 are reverse keyed.
## Alpha:                 0.81; Omega Hierarchical:    0.59; Omega Total            0.84
## Scale does not present problems. 

selfcontrol_data <- testweekfull[,c(1, match("q57_1", names(testweekfull)):match("q57_13", names(testweekfull)))]
selfcontrol_data <- cbind(selfcontrol_data, testweekfull$responseid)
colnames(selfcontrol_data)[colnames(selfcontrol_data)=="testweekfull$responseid"] <- "responseid"

selfcontrol_data_r <- c("q57_1", "q57_6", "q57_8", "q57_11")
selfcontrol_data[selfcontrol_data_r] <- 6 - selfcontrol_data[selfcontrol_data_r]

selfcontrol_items <- c("q57_1", "q57_2", "q57_3", "q57_4", "q57_5", "q57_6", "q57_7", "q57_8", "q57_9", "q57_10", "q57_11", "q57_12", "q57_13")
## psych::alpha(selfcontrol_data[selfcontrol_items], check.keys=TRUE) ## removed check.keys after recalculating
psych::alpha(selfcontrol_data[selfcontrol_items]) 
omega(selfcontrol_data[selfcontrol_items])
selfcontrol_data$selfcontrol <- rowMeans(selfcontrol_data[selfcontrol_items])

selfcontrol_data2 = selfcontrol_data[c("selfcontrol","responseid")]

addingselfcontrol<- merge(addingRSE, selfcontrol_data2, "responseid", na.rm=TRUE)

## STRAQ-1
## subscales: High temperature sensitivity, Social thermoregulation, solitary thermoregulation, risk avoidance
## High Temperature Sensitivity
## Items 1-7, items 2 and 3 reversed
##Alpha:                 0.85; Omega Hierarchical:    0.74; Omega Total            0.89 
## Scale does not present problems.

hightemp_data <- testweekfull[,c(1, match("q55_1", names(testweekfull)):match("q55_7", names(testweekfull)))]
hightemp_data <- cbind(hightemp_data, testweekfull$responseid)
colnames(hightemp_data)[colnames(hightemp_data)=="testweekfull$responseid"] <- "responseid"
hightemp_data_r <- c("q55_2", "q55_3")
hightemp_data[hightemp_data_r] <- 6 - hightemp_data[hightemp_data_r]
hightemp_items <- c("q55_1", "q55_2", "q55_3", "q55_4", "q55_5", "q55_6", "q55_7")
psych::alpha(hightemp_data[hightemp_items]) 
omega(hightemp_data[hightemp_items])
hightemp_data$hightemp <- rowMeans(hightemp_data[hightemp_items])

hightemp_data2 = hightemp_data[c("hightemp","responseid")]

addinghightemp<- merge(addingselfcontrol, hightemp_data2, "responseid", na.rm=TRUE)

## Social Thermoregulation
## Items 8-12
##Alpha:                 0.79; Omega Hierarchical:    0.76; Omega Total            0.83 
## Scale does not present problems.

socialt_data <- testweekfull[,c(1, match("q55_8", names(testweekfull)):match("q55_12", names(testweekfull)))]
socialt_data <- cbind(socialt_data, testweekfull$responseid)
colnames(socialt_data)[colnames(socialt_data)=="testweekfull$responseid"] <- "responseid"
socialt_items <- c("q55_8", "q55_9", "q55_10", "q55_11", "q55_12")
psych::alpha(socialt_data[socialt_items]) 
omega(socialt_data[socialt_items])
socialt_data$socialt <- rowMeans(socialt_data[socialt_items])

socialt_data2 = socialt_data[c("socialt","responseid")]

addingsocialt<- merge(addinghightemp, socialt_data2, "responseid", na.rm=TRUE)


## Solitary Thermoregulation
## Items 13-20, items 13, 17 reversed
##Alpha:                 0.72; Omega Hierarchical:    0.61; Omega Total            0.79
## Scale does not present problems.

solitaryt_data <- testweekfull[,c(1, match("q55_13", names(testweekfull)):match("q55_20", names(testweekfull)))]
solitaryt_data <- cbind(solitaryt_data, testweekfull$responseid)
colnames(solitaryt_data)[colnames(solitaryt_data)=="testweekfull$responseid"] <- "responseid"
solitaryt_data_r <- c("q55_13", "q55_17")
solitaryt_data[solitaryt_data_r] <- 6 - solitaryt_data[solitaryt_data_r]
solitaryt_items <- c("q55_13", "q55_14", "q55_15", "q55_16", "q55_17", "q55_18", "q55_19", "q55_20")
psych::alpha(solitaryt_data[solitaryt_items]) 
omega(solitaryt_data[solitaryt_items])
solitaryt_data$solitaryt <- rowMeans(solitaryt_data[solitaryt_items])

solitaryt_data2 = solitaryt_data[c("solitaryt","responseid")]

addingsolitaryt<- merge(addingsocialt, solitaryt_data2, "responseid", na.rm=TRUE)

## Risk Avoidance
## Items 21-23
## Alpha:                 0.49; Omega Hierarchical:    0.04; Omega Total            0.53 
## diag(.) had 0 or NA entries; non-finite result is doubtful -> Scale is less than perfect. Needs to be doubted. Low reliabilities. 

riska_data <- testweekfull[,c(1, match("q55_21", names(testweekfull)):match("q55_23", names(testweekfull)))]
riska_data <- cbind(riska_data, testweekfull$responseid)
colnames(riska_data)[colnames(riska_data)=="testweekfull$responseid"] <- "responseid"
riska_items <- c("q55_21", "q55_22", "q55_23")
psych::alpha(riska_data[riska_items]) 
omega(riska_data[riska_items])
riska_data$riska <- rowMeans(riska_data[riska_items])

riska_data2 = riska_data[c("riska","responseid")]

addingriska<- merge(addingsolitaryt, riska_data2, "responseid", na.rm=TRUE)



## ECR
## ECR divided into two scales: Avoidance and Anxiety
##One item was in both scales -> q53_93 in the scale anxiety instead of 53_92

## anxiety
## Alpha:                 0.89; Omega Hierarchical:    0.72; Omega Total            0.91 
anxiety_data <- testweekfull[,c("q53_1", "q53_70", "q53_72", "q53_74", "q53_76", "q53_78", "q53_80", "q53_81", "q53_82", "q53_83", "q53_84", "q53_85", "q53_87", "q53_88", "q53_90", "q53_92","q53_95", "q53_96")]
anxiety_data <- cbind(anxiety_data, testweekfull$responseid)
colnames(anxiety_data)[colnames(anxiety_data)=="testweekfull$responseid"] <- "responseid"
anxiety_data_r <- c("q53_82", "q53_84")
anxiety_data[anxiety_data_r] <- 8 - anxiety_data[anxiety_data_r]
anxiety_items <- c("q53_1", "q53_70", "q53_72", "q53_74", "q53_76", "q53_78", "q53_80", "q53_81", "q53_82", "q53_83", "q53_84", "q53_85", "q53_87", "q53_88", "q53_90", "q53_92", "q53_95", "q53_96")
psych::alpha(anxiety_data[anxiety_items]) 
omega(anxiety_data[anxiety_items])
anxiety_data$anxiety <- rowMeans(anxiety_data[anxiety_items])

anxiety_data2 = anxiety_data[c("anxiety","responseid")]

addinganxiety<- merge(addingriska, anxiety_data2, "responseid", na.rm=TRUE)


## avoidance
## Alpha:                 0.94; Omega Hierarchical:    0.81; Omega Total            0.95
avoidance_data <- testweekfull[,c("q53_97", "q53_98", "q53_93", "q53_99", "q53_100", "q53_101", "q53_102", "q53_104", "q53_89", "q53_91", "q53_86", "q53_79", "q53_77", "q53_94", "q53_75", "q53_73", "q53_71", "q53_69")]
avoidance_data <- cbind(avoidance_data, testweekfull$responseid)
colnames(avoidance_data)[colnames(avoidance_data)=="testweekfull$responseid"] <- "responseid"
avoidance_data_r <- c("q53_98", "q53_99", "q53_104", "q53_89", "q53_91", "q53_86", "q53_79", "q53_77", "q53_75", "q53_73", "q53_71", "q53_69")
avoidance_data[avoidance_data_r] <- 8 - avoidance_data[avoidance_data_r]
avoidance_items <- c("q53_97", "q53_98", "q53_93", "q53_99", "q53_100", "q53_101", "q53_102", "q53_104", "q53_89", "q53_91", "q53_86", "q53_79", "q53_77", "q53_94", "q53_75", "q53_73", "q53_71", "q53_69")
psych::alpha(avoidance_data[avoidance_items]) 
omega(avoidance_data[avoidance_items])
avoidance_data$avoidance <- rowMeans(avoidance_data[avoidance_items])

avoidance_data2 = avoidance_data[c("avoidance","responseid")]

addingavoidance<- merge(addinganxiety, avoidance_data2, "responseid", na.rm=TRUE)

## Self-Reported Stress
## reverse keyed: 1, 2, 3, 8, 11, 12, 14
## Alpha:                 0.88; Omega Hierarchical:    0.72; Omega Total            0.9 
## Scale does not present problems.

stress_data <- testweekfull[,c(1, match("q58_1", names(testweekfull)):match("q58_14", names(testweekfull)))]
stress_data <- cbind(stress_data, testweekfull$responseid)
colnames(stress_data)[colnames(stress_data)=="testweekfull$responseid"] <- "responseid"
stress_data_r <- c("q58_1", "q58_2", "q58_3", "q58_8", "q58_11", "q58_12", "q58_14")
stress_data[stress_data_r] <- 6 - stress_data[stress_data_r]
stress_items <- c("q58_1", "q58_2", "q58_3", "q58_4", "q58_5", "q58_6", "q58_7", "q58_8", "q58_9", "q58_10", "q58_11", "q58_12", "q58_13", "q58_14")
psych::alpha(stress_data[stress_items]) 
omega(stress_data[stress_items])
stress_data$stress <- rowMeans(stress_data[stress_items])

stress_data2 = stress_data[c("stress","responseid")]

addingstress<- merge(addingavoidance, stress_data2, "responseid", na.rm=TRUE)

## SNI - Social Network Index. This consists of three different subscales: Social (network) diversity (also: complex social integration), Social Embeddedness, and Network Size. 
## calculate xsocial diversity
##  for social diversity, we re-code the types of relationship into 1 or 0
##  variables were recoded into SNI first, because I used Chuanpeng Hu's old script. 
## variables in order: q3, q5, q7, q9, q11, q13, q15, q17, q19, q21, q23, q25, q27, q29, q31, q33, q35, q37, q39, q41, q43q, q45, q47, q49, q51 

colnames(testweekfull)[colnames(testweekfull)=="q5"] <- "SNI1"
colnames(testweekfull)[colnames(testweekfull)=="q7"] <- "SNI2"
colnames(testweekfull)[colnames(testweekfull)=="q9"] <- "SNI3"
colnames(testweekfull)[colnames(testweekfull)=="q11"] <- "SNI4"
colnames(testweekfull)[colnames(testweekfull)=="q13"] <- "SNI5"
colnames(testweekfull)[colnames(testweekfull)=="q15"] <- "SNI6"
colnames(testweekfull)[colnames(testweekfull)=="q17"] <- "SNI7"
colnames(testweekfull)[colnames(testweekfull)=="q19"] <- "SNI8"
colnames(testweekfull)[colnames(testweekfull)=="q21"] <- "SNI9"
colnames(testweekfull)[colnames(testweekfull)=="q23"] <- "SNI10"
colnames(testweekfull)[colnames(testweekfull)=="q25"] <- "SNI11"
colnames(testweekfull)[colnames(testweekfull)=="q27"] <- "SNI12"
colnames(testweekfull)[colnames(testweekfull)=="q29"] <- "SNI13"
colnames(testweekfull)[colnames(testweekfull)=="q31"] <- "SNI14"
colnames(testweekfull)[colnames(testweekfull)=="q33"] <- "SNI15"
colnames(testweekfull)[colnames(testweekfull)=="q35"] <- "SNI16"
colnames(testweekfull)[colnames(testweekfull)=="q37"] <- "SNI17"
colnames(testweekfull)[colnames(testweekfull)=="q39"] <- "SNI18"
colnames(testweekfull)[colnames(testweekfull)=="q41"] <- "SNI19"
colnames(testweekfull)[colnames(testweekfull)=="q43"] <- "SNI20"
colnames(testweekfull)[colnames(testweekfull)=="q45"] <- "SNI21"
colnames(testweekfull)[colnames(testweekfull)=="q47"] <- "SNI22"
colnames(testweekfull)[colnames(testweekfull)=="q49_1.0"] <- "SNI23"
colnames(testweekfull)[colnames(testweekfull)=="q49_2.0"] <- "SNI24"
colnames(testweekfull)[colnames(testweekfull)=="q49_3.0"] <- "SNI25"
colnames(testweekfull)[colnames(testweekfull)=="q49_4.0"] <- "SNI26"
colnames(testweekfull)[colnames(testweekfull)=="q49_5.0"] <- "SNI27"
colnames(testweekfull)[colnames(testweekfull)=="q51_1"] <- "SNI28"
colnames(testweekfull)[colnames(testweekfull)=="q51_2"] <- "SNI29"
colnames(testweekfull)[colnames(testweekfull)=="q51_3"] <- "SNI30"
colnames(testweekfull)[colnames(testweekfull)=="q51_4"] <- "SNI31"
colnames(testweekfull)[colnames(testweekfull)=="q51_5"] <- "SNI32"

SNINames <- c("SNI1","SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19",
              "SNI21","SNI28","SNI29","SNI30","SNI31","SNI32")
snDivNames <- c("SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19",
                "SNI21")
extrDivName <- c("SNI28","SNI29","SNI30","SNI31","SNI32") 
SNIData <- testweekfull[,SNINames]
SNIData <- cbind(SNIData, testweekfull$responseid)
colnames(SNIData)[colnames(SNIData)=="testweekfull$responseid"] <- "responseid"

# recode Q10
SNIData$SNI1_r <- car::recode(SNIData$SNI1,"1= 1; else = 0")

# re-code Q12 ~ Q30: NA -> 0; 0 -> 0; 1~10 -> 1
socDivData_r <- apply(SNIData[,snDivNames],2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; 1:10 = 1;"); x}) 
socDivData_r <- data.frame(socDivData_r)
# add suffix to the colnames
colnames(socDivData_r) <- paste(colnames(socDivData_r),"div",  sep = "_")
socDivData_r$SNIwork <- socDivData_r$SNI17_div + socDivData_r$SNI18_div   # combine the social network for work
socDivData_r$SNIwork_r <- car::recode(socDivData_r$SNIwork,"0 = 0;1:10 = 1")
SNIData <- cbind(SNIData, socDivData_r)  # combine by columne of re-coded data

# extra groups, 0 --> 0; more than 0 --> 1

extrDivData <- testweekfull[,extrDivName]
# re-code other groups: 0/NA -> 0; else -> 1
extrDivData_r <- apply(extrDivData,2,function(x) {x <- car::recode(x,"0 = 0; NA = 0; else = 1"); x}) 
extrDivData_r <- data.frame(extrDivData_r)
# sum the other groups
extrDivData_r$extrDiv <- rowSums(extrDivData_r)
# re-code other groups again
extrDivData_r$extrDiv_r <- car::recode(extrDivData_r$extrDiv,'0 = 0; else = 1')
SNIData$extrDiv_r <- extrDivData_r$extrDiv_r

# add social diversity with other groups
snDivNames_r <- c("SNI1_r","SNI3_div","SNI5_div","SNI7_div","SNI9_div","SNI11_div","SNI13_div","SNI15_div","SNIwork_r",
                  "SNI19_div","SNI21_div","extrDiv_r")
SNIData$SNdiversity <- rowSums(SNIData[,snDivNames_r])

# Social Network size
snSizeNames <- c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9" , "SNI11"  , "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21")

# NOTE: In our experience, individuals sometimes interpret the SNI item inquiring about the number of "other group" 
# members with whom they interact at least once every 2 weeks more broadly than we intended, with some respondents 
# reporting up to 100 or more fellow group-members. To ensure that social network size scores are not artificially inflated by 
# individuals reporting large group memberships, we recommend recoding the variable so that all values over 6 are given a 
# score of 7, thus keeping it consistent with all other quantitative SNI items.
extrSizeData_r <- apply(extrDivData,2,function(x) {x <- car::recode(x,"0 = 0; NA = 0;1 = 1; 2= 2; 3= 3; 4= 4;5= 5; 6 = 6; else = 7"); x}) 
extrSizeData_r <- data.frame(extrSizeData_r)
# add suffix to the colnames
colnames(extrSizeData_r) <- paste(colnames(extrSizeData_r),"sz",  sep = "_")

SNSizeData <- cbind(SNIData,extrSizeData_r)
SNSizeNames_r <- c("SNI1_r","SNI3", "SNI5", "SNI7", "SNI9" , "SNI11", "SNI13",  "SNI15", "SNI17","SNI18","SNI19","SNI21",
                   "SNI28_sz","SNI29_sz","SNI30_sz","SNI31_sz","SNI32_sz")
SNSizeData$snSize <- rowSums(SNSizeData[,SNSizeNames_r],na.rm=TRUE)

## number of embedded networks
## family: SNI1_r, SNI3,SNI5,SNI7,SNI9 (total >4);
## friends: SNI11 (>4);
## Church: SNI13 (>4);
## Students/school: SNI 15 (>4)
## Work: SNI17 + SNI 18 >4
## neighbor: SNI19 >4
## volunteer SNI21 >4
## other groups: totoal > 4
SNSizeData$familyNW <- rowSums(SNSizeData[,c("SNI1_r","SNI3" , "SNI5", "SNI7" , "SNI9")])
SNSizeData$familyNW_r <- car::recode(SNSizeData$familyNW,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$friendNW_r <- car::recode(SNSizeData$SNI11,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$churchNW_r <- car::recode(SNSizeData$SNI13,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$studyNW_r <- car::recode(SNSizeData$SNI15,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$workNW <- SNSizeData$SNI17 + SNSizeData$SNI18 
SNSizeData$workNW_r <- car::recode(SNSizeData$workNW,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$neighbor_r <- car::recode(SNSizeData$SNI19,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$volun_r <- car::recode(SNSizeData$SNI21,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$extra <- rowSums(SNSizeData[,c("SNI28","SNI29","SNI30","SNI31","SNI32")])
SNSizeData$extra_r <- car::recode(SNSizeData$extra,"1:4 = 0; 0 = 0; else = 1")
SNSizeData$socEmbd <- rowSums(SNSizeData[,c("familyNW_r","friendNW_r","churchNW_r","studyNW_r","workNW_r",
                                            "neighbor_r","volun_r","extra_r")])
## calculate subscales of the SNI
SNSizeData$socialdiversity <- SNIData$SNdiversity # complex social integration
SNSizeData$networksize <- SNSizeData$snSize 
SNSizeData$socialembedded <- SNSizeData$socEmbd 

SNSizeData2 = SNSizeData[c("socialdiversity","networksize", "socialembedded", "responseid")]

addingSNS<- merge(addingstress, SNSizeData2, "responseid", na.rm=TRUE)

## Prejudice towards Africans
# items 1, 3, 5, 7, 9, 11, 13, 15 reverse keyed
## Alpha:                 0.9; Omega Hierarchical:    0.78; Omega Total            0.92 
## Scale does not present problems.

prejafr_data <- testweekfull[,c(1, match("q60_1", names(testweekfull)):match("q60_15", names(testweekfull)))]
prejafr_data <- cbind(prejafr_data, testweekfull$responseid)
colnames(prejafr_data)[colnames(prejafr_data)=="testweekfull$responseid"] <- "responseid"
prejafr_data_r <- c("q60_1", "q60_3", "q60_5", "q60_7", "q60_9", "q60_11", "q60_13", "q60_15")
prejafr_data[prejafr_data_r] <- 8 - prejafr_data[prejafr_data_r]
prejafr_items <- c("q60_1", "q60_2", "q60_3", "q60_4", "q60_5", "q60_6", "q60_7", "q60_8", "q60_9", "q60_10", "q60_11", "q60_12", "q60_13", "q60_14", "q60_15")
psych::alpha(prejafr_data[prejafr_items]) 
omega(prejafr_data[prejafr_items])
prejafr_data$prejafr <- rowMeans(prejafr_data[prejafr_items])

prejafr_data2 = prejafr_data[c("prejafr","responseid")]

addingprej<- merge(addingSNS, prejafr_data2, "responseid", na.rm=TRUE)

## RWA
# 2.0, 4.0, 6.0, 7.0, 9.0, 11.0, 13.0, 16, 18, 19 reverse keyed
# Alpha:                 0.87; Omega Hierarchical:    0.56; Omega Total            0.9 
## Hierarchical solution ran into a number of problems: loading greater than abs(1), ultra-heywood case was detected, and diag(.) had 0 or NA entries. 

rwa_data <- testweekfull[,c("q58_1.0", "q58_2.0", "q58_3.0", "q58_4.0", "q58_5.0", "q58_6.0", "q58_7.0", "q58_8.0", "q58_9.0", "q58_10.0", "q58_11.0", "q58_12.0", "q58_13.0", "q58_14.0", "q58_15", "q58_16", "q58_17", "q58_18", "q58_19", "q58_20")]
rwa_data <- cbind(rwa_data, testweekfull$responseid)
colnames(rwa_data)[colnames(rwa_data)=="testweekfull$responseid"] <- "responseid"
rwa_data_r <- c("q58_2.0", "q58_4.0", "q58_6.0", "q58_7.0", "q58_9.0", "q58_11.0", "q58_13.0", "q58_16", "q58_18", "q58_19")
rwa_data[rwa_data_r] <- 8 - rwa_data[rwa_data_r]
rwa_items <- c("q58_1.0", "q58_2.0", "q58_3.0", "q58_4.0", "q58_5.0", "q58_6.0", "q58_7.0", "q58_8.0", "q58_9.0", "q58_10.0", "q58_11.0", "q58_12.0", "q58_13.0", "q58_14.0", "q58_15", "q58_16", "q58_17", "q58_18", "q58_19", "q58_20")
psych::alpha(rwa_data[rwa_items]) 
omega(rwa_data[rwa_items], nfactors = 4)
rwa_data$rwa <- rowMeans(rwa_data[rwa_items])

rwa_data2 = rwa_data[c("rwa","responseid")]

addingrwa<- merge(addingprej, rwa_data2, "responseid", na.rm=TRUE)

## UCLA Loneliness Scale
## 1.0, 4.0, 5.0, 6.0, 9.0, 10.0, 15.0, 16.0, 19.0, 20.0 reverse keyed
## Alpha:                 0.92; Omega Hierarchical:    0.81; Omega Total            0.93 
## Scale does not present problems.

lonely_data <- testweekfull[,c(1, match("q55_1.0", names(testweekfull)):match("q55_20.0", names(testweekfull)))]
lonely_data <- cbind(lonely_data, testweekfull$responseid)
colnames(lonely_data)[colnames(lonely_data)=="testweekfull$responseid"] <- "responseid"
lonely_data_r <- c("q55_1.0", "q55_4.0", "q55_5.0", "q55_6.0", "q55_9.0", "q55_10.0", "q55_15.0", "q55_16.0", "q55_19.0", "q55_20.0")
lonely_data[lonely_data_r] <- 5 - lonely_data[lonely_data_r]
lonely_items <- c("q55_1.0", "q55_2.0", "q55_3.0", "q55_4.0", "q55_5.0", "q55_6.0", "q55_7.0", "q55_8.0", "q55_9.0", "q55_10.0", "q55_11.0", "q55_12.0", "q55_13.0", "q55_14.0", "q55_15.0", "q55_16.0", "q55_17.0", "q55_18.0", "q55_19.0", "q55_20.0")
psych::alpha(lonely_data[lonely_items]) 
omega(lonely_data[lonely_items])
lonely_data$lonely <- rowMeans(lonely_data[lonely_items])

lonely_data2 = lonely_data[c("lonely","responseid")]

addinglonely<- merge(addingrwa, lonely_data2, "responseid", na.rm=TRUE)

## SDO
## 1.0, 2.0, 3.0, 4.0, 6.0, 7.0, 8.0, 16.0 reverse keyed.
## Alpha:                 0.9; Omega Hierarchical:    0.69; Omega Total            0.92 
# Scale does not present problems.

SDO_data <- testweekfull[,c(1, match("q59_1.0", names(testweekfull)):match("q59_16.0", names(testweekfull)))]
SDO_data <- cbind(SDO_data, testweekfull$responseid)
colnames(SDO_data)[colnames(SDO_data)=="testweekfull$responseid"] <- "responseid"
SDO_data_r <- c("q59_1.0", "q59_2.0", "q59_3.0", "q59_4.0", "q59_6.0", "q59_7.0", "q59_8.0", "q59_16.0")
SDO_data[SDO_data_r] <- 8 - SDO_data[SDO_data_r]
SDO_items <- c("q59_1.0", "q59_2.0", "q59_3.0", "q59_4.0", "q59_5.0", "q59_6.0", "q59_7.0", "q59_8.0", "q59_9.0", "q59_10.0", "q59_11.0", "q59_12.0", "q59_13.0", "q59_14.0", "q59_15.0", "q59_16.0")
psych::alpha(SDO_data[SDO_items]) 
omega(SDO_data[SDO_items])
SDO_data$SDO<- rowMeans(SDO_data[SDO_items])

SDO_data2 = SDO_data[c("SDO","responseid")]

addingSDO<- merge(addinglonely, SDO_data2, "responseid", na.rm=TRUE)

## DASS-21
## Alpha:                 0.92; Omega Hierarchical:    0.75; Omega Total            0.93 
# Scale does not present problems.

DASS_data <- testweekfull[,c(1, match("q61_1.0", names(testweekfull)):match("q61_20", names(testweekfull)))]
DASS_data <- cbind(DASS_data, testweekfull$responseid)
colnames(DASS_data)[colnames(DASS_data)=="testweekfull$responseid"] <- "responseid"
DASS_items <- c("q61_1.0", "q61_2.0", "q61_3.0", "q61_4.0", "q61_5.0", "q61_6.0", "q61_7", "q61_8", "q61_9", "q61_10", "q61_11", "q61_12", "q61_13", "q61_14", "q61_15", "q61_16", "q61_17", "q61_18", "q61_19", "q61_20")
psych::alpha(DASS_data[DASS_items]) 
omega(DASS_data[DASS_items])
DASS_data$DASS<- rowMeans(DASS_data[DASS_items])

DASS_data2 = DASS_data[c("DASS","responseid")]

addingDASS<- merge(addingSDO, DASS_data2, "responseid", na.rm=TRUE)

## WEMWBS
## Alpha:                 0.89; Omega Hierarchical:    0.69; Omega Total            0.91 
# Scale does not present problems.

WEMWBS_data <- testweekfull[,c(1, match("q62_1", names(testweekfull)):match("q62_14", names(testweekfull)))]
WEMWBS_data <- cbind(WEMWBS_data, testweekfull$responseid)
colnames(WEMWBS_data)[colnames(WEMWBS_data)=="testweekfull$responseid"] <- "responseid"
WEMWBS_items <- c("q62_1", "q62_2", "q62_3", "q62_4", "q62_5", "q62_6", "q62_7", "q62_8", "q62_9", "q62_10", "q62_11", "q62_12", "q62_13", "q62_14")
psych::alpha(WEMWBS_data[WEMWBS_items]) 
omega(WEMWBS_data[WEMWBS_items])
WEMWBS_data$WEMWBS<- rowMeans(WEMWBS_data[WEMWBS_items])

WEMWBS_data2 = WEMWBS_data[c("WEMWBS","responseid")]

addingWEMWBS<- merge(addingDASS, WEMWBS_data2, "responseid", na.rm=TRUE)

## OVP
## 2, 6, 7, 8, 9, 11 reverse keyed. 
## Alpha:                 0.92; Omega Hierarchical:    0.76; Omega Total            0.94 
# Scale does not present problems.

OVP_data <- testweekfull[,c(1, match("q63_1", names(testweekfull)):match("q63_26", names(testweekfull)))]
OVP_data <- cbind(OVP_data, testweekfull$responseid)
colnames(OVP_data)[colnames(OVP_data)=="testweekfull$responseid"] <- "responseid"
OVP_data_r <- c("q63_2", "q63_6", "q63_7", "q63_8", "q63_9", "q63_11")
OVP_data[OVP_data_r] <- 5 - OVP_data[OVP_data_r]
OVP_items <- c("q63_1", "q63_2", "q63_3", "q63_4", "q63_5", "q63_6", "q63_7", "q63_8", "q63_9", "q63_10", "q63_11", "q63_12", "q63_24", "q63_13", "q63_25", "q63_26")
psych::alpha(OVP_data[OVP_items]) 
omega(OVP_data[OVP_items])
OVP_data$OVP<- rowMeans(OVP_data[OVP_items])

OVP_data2 = OVP_data[c("OVP","responseid")]

addingOVP<- merge(addingWEMWBS, OVP_data2, "responseid", na.rm=TRUE)

## Intuition
## Alpha:                 0.74; Omega Hierarchical:    0.66; Omega Total            0.78 
## Hierarchical solution ran into a number of problems: convergence not obtained in GPFolbq (1000 iterations used); loading greater than abs(1), ultra-heywood case was detected, and diag(.) had 0 or NA entries.

intuition_data <- testweekfull[,c(1, match("q64_1", names(testweekfull)):match("q64_4", names(testweekfull)))]
intuition_data <- cbind(intuition_data, testweekfull$responseid)
colnames(intuition_data)[colnames(intuition_data)=="testweekfull$responseid"] <- "responseid"
intuition_items <- c("q64_1", "q64_2", "q64_3", "q64_4")
psych::alpha(intuition_data[intuition_items]) 
omega(intuition_data[intuition_items], nfactors = 2)
intuition_data$intuition<- rowMeans(intuition_data[intuition_items])

intuition_data2 = intuition_data[c("intuition","responseid")]

addingintuition<- merge(addingOVP, intuition_data2, "responseid", na.rm=TRUE)



##Select items to include in the EFA 

all_data <- cbind(anxiety_data, avoidance_data, DASS_data, lonely_data, NEURO_data, prejafr_data, RMTQ_data, RSE_data, SDO_data, selfcontrol_data, specism_data, stress_data, WEMWBS_data, OPEN_data, rwa_data, EXTRA_data, AGREE_data, CONSC_data)

all_items <- c(anxiety_items, avoidance_items, DASS_items, lonely_items, NEURO_items, prejafr_items, RMTQ_items, RSE_items, SDO_items, selfcontrol_items, specism_items, stress_items, WEMWBS_items, OPEN_items, rwa_items, EXTRA_items, AGREE_items, CONSC_items)


##Estimate the number of factors
psych::fa.parallel(all_data[all_items])

nfactors(all_data[all_items], rotate ="oblimin")

## Run the factor analysis with 17 factors
fac <- psych::fa(all_data[all_items], nfactors = 17, rotate = "oblimin")

#Remove class to create a dataframe with fa loadings
str(fac$loadings)
class(fac$loadings)

table.fa <- xtable(unclass(fac$loadings))

##Write a csv file with fa loadings
write.csv2(table.fa, file ="factors")


##Creation of subscales

##anxiety2
## Alpha:,9; Omega H: 77; Omega T: ,91; Mean: 3.79; SD: 1.14

anxiety2 <- all_data[,c("q53_1", "q53_70", "q53_72", "q53_74", "q53_76", "q53_78", "q53_80", "q53_81", "q53_82", "q53_83", "q53_84", "q53_85", "q53_87", "q53_88", "q53_90", "q53_92")]
anxiety2 <- cbind(anxiety2, testweekfull$responseid)
colnames(anxiety2)[colnames(anxiety2)=="testweekfull$responseid"] <- "responseid"
anxiety2_items <- c("q53_1", "q53_70", "q53_72", "q53_74", "q53_76", "q53_78", "q53_80", "q53_81", "q53_82", "q53_83", "q53_84", "q53_85", "q53_87", "q53_88", "q53_90","q53_92")

omega(anxiety2[anxiety2_items])

anxiety2$anxiety2 <- rowMeans(anxiety2[anxiety2_items])
anxiety3 = anxiety2[c("anxiety2", "responseid")]

describe(anxiety3[,1], na.rm = TRUE)


##avoidance2
##Alpha: ,94; Omega H: ,81; Omega T: ,95; Mean: 2.83; SD: 1.17

avoidance2 <- all_data[,c("q53_97", "q53_98", "q53_93", "q53_99", "q53_100", "q53_101", "q53_102", "q53_104", "q53_89", "q53_91", "q53_86", "q53_79", "q53_77", "q53_94", "q53_75", "q53_73", "q53_71", "q53_69")]
avoidance2 <- cbind(avoidance2, testweekfull$responseid)
colnames(avoidance2)[colnames(avoidance2)=="testweekfull$responseid"] <- "responseid"
avoidance2_items <- c("q53_97", "q53_98", "q53_93", "q53_99", "q53_100", "q53_101", "q53_102", "q53_104", "q53_89", "q53_91", "q53_86", "q53_79", "q53_77", "q53_94", "q53_75", "q53_73", "q53_71", "q53_69")

omega(avoidance2[avoidance2_items])

avoidance2$avoidance2 <- rowMeans(avoidance2[avoidance2_items])

avoidance3 = avoidance2[c("avoidance2", "responseid")]
addingavoidance3<- merge(anxiety3, avoidance3, "responseid", na.rm=TRUE)

describe(avoidance3[,1], na.rm = TRUE)

##wellbeing
## Alpha:,94; Omega H: ,71; Omega T: ,95; Mean: 2.68; Sd: 0.24

wellbeing <- all_data[, c("q61_1.0","q61_3.0","q61_4.0","q61_6.0","q61_10","q61_11","q61_12","q61_13","q61_14","q61_16","q59_11","q59_71","q58_1","q58_2","q58_3","q58_5","q58_6","q58_7","q58_8","q58_10","q58_11","q58_13","q58_14", "q62_1", "q62_3", "q62_5", "q62_7", "q62_12","q62_13","q62_14")]
wellbeing <- cbind(wellbeing, testweekfull$responseid)
colnames(wellbeing)[colnames(wellbeing)=="testweekfull$responseid"] <- "responseid"
wellbeing_items <- c("q61_1.0","q61_3.0","q61_4.0","q61_6.0","q61_10","q61_11","q61_12","q61_13","q61_14","q61_16","q59_11","q59_71","q58_1","q58_2","q58_3","q58_5","q58_6","q58_7","q58_8","q58_10","q58_11","q58_13","q58_14", "q62_1", "q62_3", "q62_5", "q62_7", "q62_12","q62_13","q62_14")

wellbeing_data_r1 <- c("q61_1.0","q61_3.0","q61_4.0","q61_6.0","q61_10","q61_11","q61_12","q61_13","q61_14","q61_16")
wellbeing[wellbeing_data_r1] <- 5 - wellbeing[wellbeing_data_r1]
                      
wellbeing_data_r2 <- c("q59_11", "q59_71")                    
wellbeing[wellbeing_data_r2] <- 6 - wellbeing[wellbeing_data_r2]                     


psych::alpha(wellbeing[wellbeing_items])
omega(wellbeing[wellbeing_items])

wellbeing$wellbeing <- rowMeans(wellbeing[wellbeing_items])
wellbeing2 = wellbeing[c("wellbeing", "responseid")]
addingwellbeing2<- merge(addingavoidance3, wellbeing2, "responseid", na.rm=TRUE)

describe(wellbeing2[,1], na.rm = TRUE)


##Anxious
##Alpha:,87; Omega H: ,88; Omega T: ,9; Mean: 2.7; SD: 0.77

anxious <- all_data[,c( "q61_9", "q61_15","q61_19","q61_20","q59_116", "q59_31", "q59_61", "q59_91", "q59_26", "q67_14")]
anxious <- cbind(anxious, testweekfull$responseid)
colnames(anxious)[colnames(anxious)=="testweekfull$responseid"] <- "responseid"
anxious_items <- c("q61_9", "q61_15","q61_19","q61_20","q59_116", "q59_31", "q59_61", "q59_91", "q59_26", "q67_14")

psych::alpha(anxious[anxious_items])
omega(anxious[anxious_items])

anxious$anxious <- rowMeans(anxious[anxious_items])
anxious2 = anxious[c("anxious", "responseid")]
addinganxious2<- merge(addingwellbeing2, anxious2, "responseid", na.rm=TRUE)

describe(anxious2[,1], na.rm = TRUE)

## Selfesteem
##Alpha: ,94; Omega H: ,82; Omega T: ,95;Mean: 2.47; SD: 0.66 

selfesteem <- all_data[,c("q61_17","q59_101","q59_41","q67_10","q56_1","q56_2","q56_3","q56_4","q56_5","q56_6","q56_7","q56_8","q56_9","q56_10", "q62_2","q62_8","q59_54","q59_84")]
selfesteem <- cbind(selfesteem, testweekfull$responseid)
colnames(selfesteem)[colnames(selfesteem)=="testweekfull$responseid"] <- "responseid"

selfesteem_data_r1 <- c("q67_10")
selfesteem[selfesteem_data_r1] <- 5 - selfesteem[selfesteem_data_r1]

selfesteem_data_r2 <- c("q62_2", "q62_8", "q59_84", "q59_54")
selfesteem[selfesteem_data_r2] <- 6 - selfesteem[selfesteem_data_r2]

selfesteem_items <- c("q61_17","q59_101","q59_41","q67_10","q56_1","q56_2","q56_3","q56_4","q56_5","q56_6","q56_7","q56_8","q56_9","q56_10", "q62_2","q62_8", "q59_54","q59_84")

psych::alpha(selfesteem[selfesteem_items])
omega(selfesteem[selfesteem_items])

selfesteem$selfesteem <- rowMeans(selfesteem[selfesteem_items])
selfesteem2 = selfesteem[c("selfesteem", "responseid")]
addingselfesteem2<- merge(addinganxious2, selfesteem2, "responseid", na.rm=TRUE)

describe(selfesteem2[,1], na.rm = TRUE)


## Self discipline
##Alpha: ,89; Omega H: ,77; Omega T: ,91; Mean: 3.14; SD: 0.7

selfdiscipline <- all_data[,c("q61_5.0","q57_2","q57_3","q57_7","q57_8","q57_9","q59_17","q59_47", "q59_40", "q59_70", "q59_100", "q59_80", "q59_110", "q59_85", "q59_115", "q59_10", "q59_20", "q59_25")]
selfdiscipline <- cbind(selfdiscipline, testweekfull$responseid)
colnames(selfdiscipline)[colnames(selfdiscipline)=="testweekfull$responseid"] <- "responseid"

selfdiscipline_data_r1 <- c("q61_5.0")
selfdiscipline[selfdiscipline_data_r1] <- 5 - selfdiscipline[selfdiscipline_data_r1]

selfdiscipline_data_r2 <- c("q57_2", "q57_3", "q57_7", "q57_8", "q57_9")
selfdiscipline[selfdiscipline_data_r2] <- 6 - selfdiscipline[selfdiscipline_data_r2]

selfdiscipline_items <- c("q61_5.0","q57_2","q57_3","q57_7","q57_8","q57_9","q59_17","q59_47", "q59_40", "q59_70", "q59_100", "q59_80", "q59_110", "q59_85", "q59_115", "q59_10", "q59_20", "q59_25")

psych::alpha(selfdiscipline[selfdiscipline_items])
omega(selfdiscipline[selfdiscipline_items])

selfdiscipline$selfdiscipline <- rowMeans(selfdiscipline[selfdiscipline_items])
selfdiscipline2 = selfdiscipline[c("selfdiscipline", "responseid")]
addingselfdiscipline2 <- merge(addingselfesteem2, selfdiscipline2, "responseid", na.rm=TRUE)

describe(selfdiscipline2[,1], na.rm = TRUE)

##Loneliness
##Alpha: .92; Omega H: .75; Omega T: .93; Mean: 3.1; SD: 0.57

loneliness <- all_data[,c("q55_1.0","q55_2.0","q55_3.0","q55_4.0","q55_5.0","q55_6.0","q55_7.0","q55_8.0","q55_10.0","q55_11.0","q55_12.0","q55_13.0","q55_14.0","q55_15.0","q55_16.0","q55_18.0","q55_19.0","q55_20.0")]
loneliness <- cbind(loneliness, testweekfull$responseid)
colnames(loneliness)[colnames(loneliness)=="testweekfull$responseid"] <- "responseid"
loneliness_items <- c("q55_1.0","q55_2.0","q55_3.0","q55_4.0","q55_5.0","q55_6.0","q55_7.0","q55_8.0","q55_10.0","q55_11.0","q55_12.0","q55_13.0","q55_14.0","q55_15.0","q55_16.0","q55_18.0","q55_19.0","q55_20.0")


psych::alpha(loneliness[loneliness_items])
omega(loneliness[loneliness_items])

loneliness$loneliness <- rowMeans(loneliness[loneliness_items])
loneliness2 = loneliness[c("loneliness", "responseid")]
addingloneliness2<- merge(addingselfdiscipline2, loneliness2, "responseid", na.rm=TRUE)

describe(loneliness2[,1], na.rm = TRUE)

##Social dominance orientation 2
##Alpha: .88; Omega H: .75; Omega T: .91; mean: 5.79; SD: 0.85

sdo2 <- all_data[,c("q59_1.0","q59_3.0","q59_5.0","q59_9.0","q59_10.0","q59_11.0", "q59_12.0","q59_13.0","q59_14.0","q59_15.0", "q59_16.0")]
sdo2 <- cbind(sdo2, testweekfull$responseid)
colnames(sdo2)[colnames(sdo2)=="testweekfull$responseid"] <- "responseid"
sdo2_items <- c("q59_1.0","q59_3.0","q59_5.0","q59_9.0","q59_10.0","q59_11.0", "q59_12.0","q59_13.0","q59_14.0","q59_15.0", "q59_16.0")

psych::alpha(sdo2[sdo2_items])
omega(sdo2[sdo2_items])

sdo2$sdo2 <- rowMeans(sdo2[sdo2_items])
sdo22 = sdo2[c("sdo2", "responseid")]
addingsdo22<- merge(addingloneliness2, sdo22, "responseid", na.rm=TRUE)

describe(sdo22[,1], na.rm = TRUE)

##Right prejudice
##Alpha: .91; Omega H: .75; Omega T: .93; Mean: 3.01; SD: 0.93

rightprej <- all_data[,c("q60_1","q60_2","q60_3","q60_4","q60_5","q60_6","q60_7","q60_8","q60_9","q60_10","q60_11","q60_12","q60_13","q60_14","q60_15","q59_6.0", "q59_28","q59_88", "q59_118")]
rightprej <- cbind(rightprej, testweekfull$responseid)
colnames(rightprej)[colnames(rightprej)=="testweekfull$responseid"] <- "responseid"
rightprej_items <- c("q60_1","q60_2","q60_3","q60_4","q60_5","q60_6","q60_7","q60_8","q60_9","q60_10","q60_11","q60_12","q60_13","q60_14","q60_15","q59_6.0", "q59_28","q59_88", "q59_118")


rightprej_data_r1 <- c("q59_6.0")
rightprej[rightprej_data_r1] <- 8 - rightprej[rightprej_data_r1]

rightprej_data_r2<- c("q59_28", "q59_88", "q59_118")
rightprej[rightprej_data_r2] <- 6 - rightprej[rightprej_data_r2]

psych::alpha(rightprej[rightprej_items])
omega(rightprej[rightprej_items])

rightprej$rightprej <- rowMeans(rightprej[rightprej_items])
rightprej2 = rightprej[c("rightprej", "responseid")]
addingrightprej2<- merge(addingsdo22, rightprej2, "responseid", na.rm=TRUE)

describe(rightprej2[,1], na.rm = TRUE)

##Impulsive
##Alpha: .85; Omega H: ,61; Omega T: .89; Mean: 3.53; SD: 0.7

impulsive <- all_data[,c("q59_6", "q59_66", "q59_21", "q57_12", "q57_13", "q59_112", "q59_49", "q59_79", "q59_75", "q59_30", "q59_60", "q59_90", "q59_120")]
impulsive <- cbind(impulsive, testweekfull$responseid)
colnames(impulsive)[colnames(impulsive)=="testweekfull$responseid"] <- "responseid"

impulsive_data_r1 <- c("q59_6", "q59_66", "q59_21", "q57_12", "q57_13", "q59_112")
impulsive[impulsive_data_r1] <- 6 - impulsive[impulsive_data_r1]

impulsive_items <- c("q59_6", "q59_66", "q59_21", "q57_12", "q57_13", "q59_112", "q59_49", "q59_79", "q59_75", "q59_30", "q59_60", "q59_90", "q59_120")

psych::alpha(impulsive[impulsive_items])
omega(impulsive[impulsive_items])

impulsive$impulsive <- rowMeans(impulsive[impulsive_items])
impulsive2 = impulsive[c("impulsive", "responseid")]
addingimpulsive2<- merge(addingrightprej2, impulsive2, "responseid", na.rm=TRUE)

describe(impulsive2[,1], na.rm = TRUE)

##thoughtful
## Alpha: .88; Omega H: .66; Omega T: .91; mean: 3.09; SD: 0.65

thoughtful <- all_data[,c("q67_2","q67_4","q67_8","q67_11","q67_13","q67_15","q67_17","q59_3","q59_33", "q59_63", "q59_93")]
thoughtful <- cbind(thoughtful, testweekfull$responseid)
colnames(thoughtful)[colnames(thoughtful)=="testweekfull$responseid"] <- "responseid"
thoughtful_items <- c("q67_2","q67_4","q67_8","q67_11","q67_13","q67_15","q67_17","q59_3","q59_33", "q59_63", "q59_93")

psych::alpha(thoughtful[thoughtful_items])
omega(thoughtful[thoughtful_items])

thoughtful$thoughtful <- rowMeans(thoughtful[thoughtful_items])
thoughtful2 = thoughtful[c("thoughtful", "responseid")]
addingthoughtful2<- merge(addingimpulsive2, thoughtful2, "responseid", na.rm=TRUE)

describe(thoughtful2[,1], na.rm = TRUE)

##Trusting; Mean: 2.71; SD: 0.85

trusting <- all_data[,c("q67_7","q59_94", "q59_4", "q59_34", "q59_64")]
trusting <- cbind(trusting, testweekfull$responseid)
colnames(trusting)[colnames(trusting)=="testweekfull$responseid"] <- "responseid"

trusting_data_r1 <- c("q67_7")
trusting[trusting_data_r1] <- 5 - trusting[trusting_data_r1]

trusting_items <- c("q67_7","q59_94", "q59_4", "q59_34", "q59_64")

psych::alpha(trusting[trusting_items])
omega(trusting[trusting_items])

trusting$trusting <- rowMeans(trusting[trusting_items])
trusting2 = trusting[c("trusting", "responseid")]
addingtrusting2<- merge(addingthoughtful2, trusting2, "responseid", na.rm=TRUE)

describe(trusting2[,1], na.rm = TRUE)

##Right Wing Authoritarianism 2
##Alpha: .86; Omega H: .58; Omega T: .89; Mean: 2.37; SD: 0.85

rwa2 <- all_data[,c("q59_108","q58_1.0","q58_2.0","q58_3.0","q58_4.0","q58_5.0","q58_6.0","q58_7.0","q58_10.0","q58_11.0","q58_12.0","q58_14.0","q58_17","q58_19", "q58_20")]
rwa2 <- cbind(rwa2, testweekfull$responseid)
colnames(rwa2)[colnames(rwa2)=="testweekfull$responseid"] <- "responseid"

rwa2_data_r1 <- c("q59_108")
rwa2[rwa2_data_r1] <- 5 - rwa2[rwa2_data_r1]

rwa2_items <- c("q59_108","q58_1.0","q58_2.0","q58_3.0","q58_4.0","q58_5.0","q58_6.0","q58_7.0","q58_10.0","q58_11.0","q58_12.0","q58_14.0","q58_17","q58_19", "q58_20")

psych::alpha(rwa2[rwa2_items])
omega(rwa2[rwa2_items])

rwa2$rwa2 <- rowMeans(rwa2[rwa2_items])
rwa3 = rwa2[c("rwa2", "responseid")]
addingrwa3<- merge(addingtrusting2, rwa3, "responseid", na.rm=TRUE)

describe(rwa3[,1], na.rm = TRUE)

##Stimulation
##Alpha: .73; Omega H: .59; Omega T: .79; Mean: 3.78; SD: 0.75

stimulation <- all_data[,c("q59_8","q59_38","q59_53","q59_68","q59_98")]
stimulation <- cbind(stimulation, testweekfull$responseid)
colnames(stimulation)[colnames(stimulation)=="testweekfull$responseid"] <- "responseid"


stimulation_items <- c("q59_8","q59_38","q59_53","q59_68","q59_98")

psych::alpha(stimulation[stimulation_items])
omega(stimulation[stimulation_items])

stimulation$stimulation <- rowMeans(stimulation[stimulation_items])
stimulation2 = stimulation[c("stimulation", "responseid")]
addingstimulation2<- merge(addingrwa3, stimulation2, "responseid", na.rm=TRUE)

describe(stimulation2[,1], na.rm = TRUE)

##sociability, Alpha: 0.89; Omega H: 0.85, Omega T: 0.92; mean 3.14; sd: 0.9

sociable <- all_data[,c("q55_9.0", "q59_16", "q59_76", "q59_62", "q59_97", "q59_2", "q59_32", "q59_7", "q59_37")]
sociable <- cbind(sociable, testweekfull$responseid)
colnames(sociable)[colnames(sociable)=="testweekfull$responseid"] <- "responseid"

sociable_data_r1 <- c("q59_16", "q59_76")
sociable[sociable_data_r1] <- 6 - sociable[sociable_data_r1]

sociable_items <- c("q55_9.0", "q59_16" , "q59_76", "q59_62", "q59_97", "q59_2", "q59_32", "q59_7", "q59_37")

psych::alpha(sociable[sociable_items])
omega(sociable[sociable_items])

sociable$sociable <- rowMeans(sociable[sociable_items])
sociable2 = sociable[c("sociable", "responseid")]
addingsociable2<- merge(addingstimulation2, sociable2, "responseid", na.rm=TRUE)

describe(sociable2[,1], na.rm = TRUE)


##Leadership, Alpha: 0.88; Omega T: 0.83; Omega: 0.91, mean: 3.23; sd: 0.64

leader <- all_data[,c("q59_86", "q57_11", "q59_102", "q59_12", "q59_42", "q59_72", "q59_5", "q59_5", "q59_35", "q59_65", "q59_95", "q59_50", "q59_55")]
leader <- cbind(leader, testweekfull$responseid)
colnames(leader)[colnames(leader)=="testweekfull$responseid"] <- "responseid"
leader_items <- c("q59_86", "q57_11", "q59_102", "q59_12", "q59_42", "q59_72", "q59_5", "q59_5", "q59_35", "q59_65", "q59_95", "q59_50", "q59_55")

leader_data_r1 <- c("q59_86", "q57_11")
leader[leader_data_r1] <- 6 - leader[leader_data_r1]

psych::alpha(leader[leader_items])
omega(leader[leader_items])

leader$leader <- rowMeans(leader[leader_items])
leader2 = leader[c("leader", "responseid")]
addingleader2<- merge(addingsociable2, leader2, "responseid", na.rm=TRUE)

describe(leader2[,1], na.rm = TRUE)

##Empathic, Alpha: 0.85; Omega H: 0.66; Omega T: 0.88, Mean:4.29; sd: 0.57 

empathic <- all_data[,c("q59_2.0", "q59_13", "q59_43", "q59_103", "q59_9", "q59_39", "q59_69", "q59_74", "q59_104", "q59_24", "q59_89", "q59_14", "q59_44", "q59_29")]
empathic <- cbind(empathic, testweekfull$responseid)
colnames(empathic)[colnames(empathic)=="testweekfull$responseid"] <- "responseid"
empathic_items <- c("q59_2.0", "q59_13", "q59_43", "q59_103", "q59_9", "q59_39", "q59_69", "q59_74", "q59_104", "q59_24", "q59_89", "q59_14", "q59_44", "q59_29")

psych::alpha(empathic[empathic_items])
omega(empathic[empathic_items])

empathic$empathic <- rowMeans(empathic[empathic_items])
empathic2 = empathic[c("empathic", "responseid")]
addingempathic2<- merge(addingleader2, empathic2, "responseid", na.rm=TRUE)

describe(empathic2[,1], na.rm = TRUE)


## Create a scale STRAQ-1 with solitaryt, socialt, hightemp ##Alpha: .78; Omega H: .5; Omega T; .85

STRAQ_data <- testweekfull[,c(1, match("q55_1", names(testweekfull)):match("q55_20", names(testweekfull)))]
STRAQ_data <- cbind(STRAQ_data, testweekfull$responseid)
colnames(STRAQ_data)[colnames(STRAQ_data)=="testweekfull$responseid"] <- "responseid"
STRAQ_items <- c(socialt_items, solitaryt_items, hightemp_items)

STRAQ_data_r <- c("q55_1", "q55_4", "q55_5", "q55_6", "q55_7", "q55_13", "q55_17")
STRAQ_data[STRAQ_data_r] <- 6 - STRAQ_data[,STRAQ_data_r]

psych::alpha(STRAQ_data[STRAQ_items]) 
omega(STRAQ_data[STRAQ_items])
STRAQ_data$STRAQ <- rowMeans(STRAQ_data[STRAQ_items])

STRAQ_data2 = STRAQ_data[c("STRAQ","responseid")]

addingSTRAQ<- merge(addingempathic2, STRAQ_data2, "responseid", na.rm=TRUE)


##Create a dataframe with all scales (old scales + created scales)

data_subscales <- merge(addingintuition, addingSTRAQ, "responseid", na.rm = TRUE)

write.csv(data_subscales2, file = "data_subscales.csv.", row.names = TRUE)


## Create a dataframe for with STRAQ-1 and ECR items

data_items <- merge(STRAQ_data2, addingempathic2, "responseid", na.rm = TRUE)
data_items2 <- merge(anxiety_data, data_items, "responseid", na.rm = TRUE)
data_items3 <- merge(avoidance_data, data_items2, "responseid", na.rm = TRUE)
data_items4 <- merge(socialt_data, data_items3, "responseid", na.rm = TRUE)
data_items5 <-  merge(solitaryt_data, data_items4, "responseid", na.rm = TRUE)
data_items6 <- merge(hightemp_data, data_items5, "responseid", na.rm = TRUE)

write.csv(data_items6, file = "data_items.csv.", row.names = TRUE)
