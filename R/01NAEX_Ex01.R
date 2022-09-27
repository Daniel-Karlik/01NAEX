################################################################
# 01NAEX - Exercise 1
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Data and exercises come from D.C. Montgomery: Design and Analysis of Experiment - Chapter 02
################################################################

######################
# get requirements for Lecture 1
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr","MASS")
missing_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)
lapply(list_of_packages, library, character.only = TRUE)

######################
# check our settings
getwd()
print(.libPaths())
print(sessionInfo())
print(version)

# Define directory, if you do not use relative path.
#setwd("M:/01NEX_2020/")

# Read data
Problem20 <- read.table("data/Ex02_20.csv",header=TRUE,sep=";")
Problem26 <- read.table("data/Ex02_26.csv",header=TRUE,sep=";")
Problem30 <- read.table("data/Ex02_30.csv",header=TRUE,sep=";")

# Solve all three problems from lecture slides:
# Exp 2.20
#print(Problem20)
summary(Problem20)
t.test(Problem20, mu = 120)

t.test(Problem20, mu = 120, conf.level =.01)

# Exp 2.26
summary(Problem26)
var.test(Problem26$Type1, Problem26$Type2, ration = 1, conf.level = .95)
t.test(Problem26$Type1,Problem26$Type2,alternative="two.sided",mu=0,
       paired=F,var.equal=T,conf.level=0.95)
#Normality test
lillie.test(Problem26$Type1)
lillie.test(Problem26$Type2)

# Exp 2.30
summary(Problem30)
A <- var.test(Problem30$X10.seconds, Problem30$X20.seconds, ration = 1, conf.level = .95)
print(A)
print("p-value is") 
print(A$p.value)

print("Confidence interval is:")
print(A$conf.int[1:2])
p1 <- boxplot(Problem30$X10.seconds, Problem30$X20.seconds,
              names = c("10 sec", "20 sec"),
              col = c("red", "blue"),
              ylab = "Quality",
              ylim = c(1,10))

#print(colnames(Problem30))
#print(Problem30$X10.seconds)

X_help <- c(Problem30$X10.seconds,Problem30$X20.seconds)
X_1 <- rep(10,20)
X_2 <- rep(20,20)
Sec <- c(X_1,X_2)
X_fin <- cbind(X_help,Sec)

X_dat <- as.data.frame(X_fin)
X_dat$Sec <- as.factor(X_dat$Sec)

library(ggplot2)
# Basic dot plot
p<-ggplot(X_dat, aes(x=Sec, y=X_help, fill = Sec)) + 
  geom_dotplot(binaxis='y', dotsize=1.2 , stackdir='center')
p+scale_fill_brewer(palette="Dark2")+
  labs(title="Dot plot of quality by CD time",x="Cool-down times (sec)", y = "Quality")

