################################################################
# 01NAEX - Exercise 02
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment - Chapter 02
################################################################

######################
# get requirements for Lecture 1
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr","MASS","agricolae")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

######################
# check our settings
getwd()
#print(.libPaths())
#print(sessionInfo())
#print(version)

# Define directory, if you do not use relative path.
#setwd("M:/01NAEX/")

######################
# Read data
Ex03_7 <- read.table("data/Ex03_7.csv",header=TRUE,sep=";")
Ex03_10 <- read.table("data/Ex03_10.csv",header=TRUE,sep=";")

Ex03_7$Technique <- as.factor(Ex03_7$Technique)
print(Ex03_7)
# Solve exercises from slides

model_03_7 <- lm(Tensile_Strength~Technique - 1, data=Ex03_7)
summary(model_03_7)
confint(model_03_7)

sse <- sum((Ex03_7$Tensile_Strength - model_03_7$fitted.values)^2)
n <- length(Ex03_7$Tensile_Strength)
mse <- sse / (n - 2)

lsd.model03_7 <- LSD.test(Ex03_7$Tensile_Strength, Ex03_7$Technique, n-2,mse)
summary(lsd.model03_7)
print(lsd.model03_7)

library(ggplot2)


qqnorm(Ex03_7$Tensile_Strength)
qqline(Ex03_7$Tensile_Strength)


# Hints:
model<-aov(Tensile_Strength~Technique, data=Ex03_7)

out1<-LSD.test(model,"Technique",p.adj="hommel",console=TRUE)
plot(out1,variation="SD") # variation standard deviation

out2<-LSD.test(model,"Technique",p.adj="hommel",console=TRUE)
plot(out2,variation="SD") # variation standard deviation





 
 
