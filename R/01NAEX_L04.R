# 01NAEX - R codes used in Lecture 04
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code is from R Companion to Montgomery's DAoE

#Instalation of any library
library(car)          # provides a set of useful functions for ANOVA designs and Regression Models;
library(lattice)      # provides some graphical enhancements compared to traditional R graphics, as well as multivariate displays capabilities;
library(lme4)         # the newer and enhanced version of the nlme package, for which additional data structure are available (nested or hierarchical model,. . . );
library(nlme)         # for handling mixed-effects models;
library(pwr)          # power analysis
library(agricolae)    # for Fisher LSD method
library(scatterplot3d)# for 3d scatter plot
library(alr3)
library(FrF2)          #for 2^k  factorial design
library(DoE.base)      # Full factorials, orthogonal arrays and base utilities for DoE packages
library(rsm)           # Response-Surface Methods
library(plyr)   
# for opening xls files: library(gdata) library(XLConnect) library(xlsReadWrite)



# Define directory if needed
# setwd("M:/01NEX/")


##############################################################################
# Follow Chapter 4.2 - 4.4 - Montgomery DAOE
##############################################################################

###########################################################
# Latin Square Design for the Rocket Propellant  Problem

rocket <- read.table("Data/rocket.txt",header=T)

matrix(rocket$treat,nr=5,byrow=T)
names(rocket)[names(rocket)=="op"] <- "operator"
names(rocket)[names(rocket)=="y"] <- "Propellant"


rocket$operator = factor(rocket$operator)
rocket$batch = factor(rocket$batch)

plot(Propellant~operator+batch+treat,rocket)

rocket.lm <- lm(Propellant~operator+batch+treat,rocket)
anova(rocket.lm)

plot.design(Propellant~operator+batch+treat,data=rocket)

# if we do not consider factor batch
rocket.lm2 <- lm(Propellant~operator+treat,rocket)
anova(rocket.lm2)



###########################################################
# Greco - Latin Squares

greco = c("alpha","gamma","epsilon","beta","delta","beta","delta","alpha","gamma","epsilon","gamma","epsilon","beta","delta","alpha","delta","alpha","gamma","epsilon","beta","epsilon","beta","delta","alpha","gamma")

rocket$assembly = greco
rocket$assembly = factor(rocket$assembly)

plot(Propellant~operator+batch+treat+assembly,rocket)
plot.design(Propellant~operator+batch+treat+assembly,data=rocket)

rocket.lm <- lm(Propellant~operator+batch+treat+assembly,rocket)
anova(rocket.lm)


###########################################################
# Balanced incomplete Blocks design for Catalists Experiment

# Section 4.4 and Example 4.5 
Catalists      = matrix(c(73,NA,73,75,74,75,75,NA,NA,67,68,72,71,72,NA,75),nc=4)
Catalists.df   = data.frame(rep=as.vector(Catalists),
                            treat=factor(rep(1:4,4)),
                            block=factor(rep(1:4,each=4)))

boxplot(rep~treat,  data=Catalists.df,xlab="Tip",    main="Responses averaged over treatments")
boxplot(rep~block,  data=Catalists.df,xlab="blocks", main="Responses averaged over block")                          

#Block as additional error
summary(aov(rep~treat+block+Error(block),Catalists.df))
anova(lm(rep~block+treat,Catalists.df))

# Block as factor of interest
summary(aov(rep~treat+block+Error(treat),Catalists.df))
anova(lm(rep~treat+block,Catalists.df))

Catalists.lm  = lm(rep~block+treat,Catalists.df)
summary(Catalists.lm)
treat.coef    =  Catalists.lm$coef[5:7]


# Section 4.4.2

# effect for catalyst 4 (baseline) is missing, so we add it
treat.coef    = c(0,treat.coef)
names(treat.coef) = c("treatment1","treatment2","treatment3","treatment4")
pairwise.diff = outer(treat.coef,treat.coef,"-")

#sd(c(0.250,0.375,3.375,0.625,3.000,3.625))
summary(Catalists.lm)

crit.val <- qtukey(0.95,4,5)
ic.width <- crit.val*0.6982/sqrt(2)

xxx <- pairwise.diff[lower.tri(pairwise.diff)]
plot(xxx,1:6,xlab="Pairwise Difference
     (95% CI)",ylab="",xlim=c(-5,5),pch=19,cex=1.2,axes=F)
axis(1,seq(-5,5))
#mtext(c("4-1","4-2","4-3","1-2","1-3","2-3"),side=2,at=1:6,line=2,las=2)
mtext(c("1-2","1-3","1-4","2-3","2-4","3-4"),side=2,at=1:6,line=2,las=2)
segments(xxx-ic.width,1:6,xxx+ic.width,1:6,lwd=2)
abline(v=0,lty=2,col="lightgray")


Catalists.lm.crd <- lm(rep~treat,Catalists.df)
summary(lm(rep~treat,Catalists.df))
(summary(Catalists.lm.crd)$sig/summary(Catalists.lm)$sig)^2

require(lattice)
xyplot(rep~treat|block,Catalists.df,
       aspect="xy",xlab="Catalyst",ylab="Response",
       panel=function(x,y) {
           panel.xyplot(x,y)
           panel.lmline(x,y)
       })

xyplot(rep~block|treat,Catalists.df,
       aspect="xy",xlab="Catalyst",ylab="Response",
       panel=function(x,y) {
           panel.xyplot(x,y)
           panel.lmline(x,y)
       })


# Not RCBD !!!!!!!! 
# if RCBD we can use Tukey

Catalists.aov <- anova(lm(rep~block+treat,Catalists.df))
Catalists.aov <- aov(rep~treat+block+Error(block),Catalists.df)
TukeyRep = TukeyHSD(Catalists.aov,which="treat")
Catalists.aov <- aov(rep~treat+block,Catalists.df)
summary(Catalists.aov)
TukeyRep = TukeyHSD(Catalists.aov,which="treat")
plot(TukeyRep) # Misinterpretation !!!

# Use TukeyHSD only for RCBD design !!!

install.packages("multcomp")
library(multcomp)

Catalist_aov <-(aov(rep~block+treat,Catalists.df))
contr <- glht(Catalist_aov, linfct = mcp(treat = "Tukey"))
summary(contr, test = adjusted("none")) 

summary(contr, test = adjusted("bonferroni")) 
















