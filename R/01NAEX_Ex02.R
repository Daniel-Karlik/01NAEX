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

# Solve exercises from slides


# Hints:
model<-aov(Tensile_Strength~Technique, data=Ex03_7)

out1<-LSD.test(model,"Technique",p.adj="hommel",console=TRUE)
plot(out1,variation="SD") # variation standard deviation

out2<-LSD.test(model,"Technique",p.adj="hommel",console=TRUE)
plot(out2,variation="SD") # variation standard deviation





 
 
