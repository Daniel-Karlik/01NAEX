################################################################
# 01NAEX -  Lecture 07
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code are from R Companion to Montgomery's DAoE and
################################################################

######################
# get requirements 
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr",
                      "MASS","agricolae","nlme","lme4","agricolae",
                      "scatterplot3d","FrF2","rsm","DoE.base","geoR",
                      "qualityTools","alr4")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

#library(FrF2)          #for 2^k  factorial design
#library(DoE.base)      # Full factorials, orthogonal arrays
#                         and base utilities for DoE packages
######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

# Define directory, if you do not use relative path.
#setwd("H:/01NAEX/")

##############################################################################
# Follow Chapter 6 - Montgomery DAOE
##############################################################################

##############################################################################
### Single Replicate of the 2^4 design 
##############################################################################


rate_experiment             <- read.table("data/Pilot_Plant_Filtration_Rate_Experiment.csv",header=TRUE, sep = ";")
summary(rate_experiment)
rate_experiment$A           <- factor(rate_experiment$A) 
rate_experiment$B           <- factor(rate_experiment$B) 
rate_experiment$C           <- factor(rate_experiment$C)
rate_experiment$D           <- factor(rate_experiment$D) 
summary(rate_experiment)

#rate           <-  FrF2(2^4, 4, replications = 1, randomize = FALSE,factor.names = c("A_Temperature", "B_Pressure", "C_Concentration", "D_Stirring_rate"))
rate            <-  FrF2(2^4, 4, replications = 1, randomize = FALSE,factor.names = c("A", "B", "C", "D"))
Filtration      <-  rate_experiment$Rate
rate            <-  add.response(rate, Filtration)
summary(rate)
# Main Effects plotfor response variable
MEPlot(rate)
# Interaction Plot matrix for response variable
IAPlot(rate)
#Boxplots
opar <- par(mfrow=c(2,2))
with(rate, boxplot(Filtration ~ A , main = "Factor A - Temperature") )
with(rate, boxplot(Filtration ~ B , main = "Factor B - Pressure")     )
with(rate, boxplot(Filtration ~ C , main = "Factor C - Concentration") )
with(rate, boxplot(Filtration ~ D , main = "Factor D - Stirring rate")  )
title(main="Box plots of all factors", line=-1, outer=T, cex.main=1)
par(opar)    

opar <- par(mfrow=c(1,1))
## Daniel plot and Half normal plot are usually used for unreplicatd design
# Daniel Plot with alpha = 0.1 and only significant factors
DanielPlot(rate,code=TRUE)
# Classical effects qqplot
qqplot(DanielPlot(rate,alpha=0.1)$x,DanielPlot(rate)$y)
qqline(DanielPlot(rate,alpha=0.1)$y)
# half normal plot of effects
DanielPlot(rate,code=TRUE,alpha=0.1,half=TRUE)




# B not significant at SL 0.1 - discard B and have 2^3 design with two replicates
rate2            <-  FrF2(2^3, 3, replications = 2, randomize = FALSE,factor.names = c("A", "C", "D"),repeat.only=TRUE)
Filtration2      <-  c(45 , 48 , 71 , 65 , 68 ,  80 , 60 , 65 , 43 , 45 , 100 , 104 , 75 , 70 , 86 , 96)
rate2            <-  add.response(rate2, Filtration2)
anova(aov(Filtration2~A*C*D, data=rate2))
# same as original linear model without factor B
anova(aov(Filtration~A*C*D, data=rate))

# final model
anova(aov(Filtration~A*C+A*D,  data=rate))
model1 = lm(Filtration~A+C+D+A:C+A:D,  data=rate)
summary(model1)


# another approach
# model with all factors and all interactions - we do not have "pure error" 
anova(aov(Filtration~(.)^4,  data=rate))
# model with all factors and all interactions - we omit the highest interaction 
anova(aov(Filtration~(.)^3,  data=rate))
# use only significant variables
anova(aov(Filtration~A+C+D+A:C+A:D,  data=rate))
model1 = lm(Filtration~A+C+D+A:C+A:D,  data=rate)
summary(model1)

opar <- par(mfrow=c(2,2))
plot(model1)
par(opar)


#Take as a numeric -1, +1 instead of factor
rate$A.num <- 2*(as.numeric(rate$A)-1.5)
rate$C.num <- 2*(as.numeric(rate$C)-1.5)
rate$D.num <- 2*(as.numeric(rate$D)-1.5)
rate.lm       <- lm(Filtration~A.num*C.num+A.num*D.num,data=rate)
summary(rate.lm)
tmp           <- list(A.num=seq(-1,1,by=.05),C.num=seq(-1,1,by=0.05),D.num=seq(-1,1,by=0.05),data=rate)


new.data2 <- t(rbind(seq(-1,1,by=.05),seq(-1,1,by=.05),seq(-1,1,by=.05)))
colnames(new.data2) <-c("A.num","C.num","D.num")
new.data2 <- as.data.frame(new.data2)
new.data2 <- expand.grid(new.data2)

new.data       <- expand.grid(tmp)
new.data$fit   <- predict(rate.lm,new.data)
new.data$fit2  <- predict(rate.lm,new.data2)

# Countour plots - only 2 variables in the model
contourplot(fit~A.num*C.num,new.data,xlab="A Temperature",ylab="C Concentration", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~C.num*D.num,new.data,xlab="C Concentration",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")
contourplot(fit~A.num*D.num,new.data,xlab="A Temperature",ylab="D Stiring Rate", main = "Contour plot of Filtration Rate from the Pilot Plant Experiment")

# Countour plots - all 3 varaibles + interactions in the model
par(mfrow = c(2,2))
wirePlot(A, C, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(A, C, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(C, D, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")
contourPlot(A, D, Filtration_qt, data = rate_qt,form = "Filtration_qt~A*C+A*D")

rate0             <- read.table("data/Pilot_Plant_Filtration_Rate_Experiment.csv",header=TRUE, sep = ";")
rate3             <- read.table("data/Pilot_Plant_Filtration_Rate_Experiment3.csv",header=TRUE, sep = ";")


summary(rate3)
anova(aov(Rate~A*B*C*D,data=rate3))
rate3$E <- c(rep(0,times=16),rep(1,times=4))
aov_model1= aov(Rate~A*B*C*D,data=rate3)
summary(aov_model1)
aov_model2= aov(Rate~A*B*C*D+E,data=rate3)
summary(aov_model2)
#anova(aov(Rate~as.factor(A)*as.factor(B)*as.factor(C)*as.factor(D)+as.factor(E),data=rate3))

anova(aov(Rate~A*B*C*D,data=rate0))

aov_model1= aov(Rate~A*B*C*D,data=rate3)
model.tables(aov_model1, "means")

anova(aov(Filtration~A*B*C*D,data=rate))

anova(aov(Rate~A*C+A*D,data=rate3))


rate4            <-  FrF2(2^3, 3, replications = 2, randomize = FALSE,factor.names = c("A", "C", "D"),repeat.only=TRUE)
rate4            <-  add.center(rate4,2)
Filtration4      <-  c(45 , 48 , 71 , 65 , 68 ,  80 , 60 , 65 , 43 , 45 , 100 , 104 , 75 , 70 , 86 , 96, 73,75,66,69)
rate4            <-  add.response(rate4, Filtration4)
anova(aov(Filtration2~A*C+A*D,  data=rate2))
anova(aov(Filtration4~A*C+A*D,  data=rate4))

summary(lm(Filtration4~A*C+A*D+iscube(rate4), rate4))

#195.13 + 48.75 + 1.51
#245.39


# Pareto plot
rate_effects  <-  DanielPlot(rate,alpha=0.1)$x

source("R/pareto.R")  
pareto(rate_effects, names = c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD"))




