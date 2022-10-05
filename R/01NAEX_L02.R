################################################################
# 01NAEX -  Lecture 02
# Written by J. Franc - jiri.franc@fjfi.cvut.cz
# Some parts of code are from R Companion to Montgomery's DAoE and
# http://www.aliquote.org/articles/tech/dae/ 
################################################################

######################
# get requirements for Lecture 1
list_of_packages <- c("tidyverse", "car","nortest","lattice","pwr","MASS","agricolae","nlme","lme4")
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


# compare t-test and ANOVA for simple comparative experiment
# Tension Bond Strength data (Tab. 2-1, p. 24)    from Lecture 01
Modified        = c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
Unmodified      = c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
Response =(c(Modified,Unmodified))
Factor   = as.factor( c(rep("Modified",10), (rep("Unmodified",10))))
data.cement = data.frame(Response, Factor)
# t-test
t.test(Modified, Unmodified , alternative = "two.sided", mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
# linear models
summary(lm(Response~Factor   ,data=data.cement))    # with intercept    (t-test compare estiamted mean and difference with zero )
summary(lm(Response~Factor-1 ,data=data.cement))    # without intercept (t-test compare estiamted means with zero )
# Anova
summary(aov(Response~Factor,data=data.cement))      # first  possibility how to use it 
summary(lm(Response~Factor ,data=data.cement))      # second possibility how to use it 
                                                    # do not use model without intercept


#From lecture 01:
# Tension Bond Strength data (Tab. 2-1, p. 24)
y1      = c(16.85,16.40,17.21,16.35,16.52,17.04,16.96,17.15,16.59,16.57)
y2      = c(16.62,16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)
y       = c(y1,y2)
y.means = c(mean(y1),mean(y),mean(y2))
y.sd    = c(sd(y1),sd(y),sd(y2))
opar    = par(mfrow=c(2,1),mar=c(5,7,4,2),las=1)
stripchart(data.frame(Modified=y1,Together=y,Unmodified=y2),xlab=expression("Strength (kgf/cm^2)"),pch=19,xlim = c(16.2,17.6))
arrows(c(y.means,y.means[2]),c(1.5,1.5,2.5,2.5),c(y.means,y.means[2]),c(1.1,1.9,2.9,2.1),length=.1)
text(y.means,c(1.2,1.8,2.8),round(y.means,2),pos=4,cex=.8)
rd = rnorm(1000,mean=y.means[2],sd=y.sd[2])        # if y is data frame: sd = as.numeric(apply(y,2,sd))
hist(y,xlab="Observations",ylab="Relative frequency", main="Histogram for Tension Bold Strength Data", breaks = 7, freq = FALSE, ylim=c(0,1.6), xlim = c(16.2,17.6) )
partition = seq(min(rd ), max(rd ), 0.01)
lines(partition, dnorm(partition, y.means[2], y.sd[2]), col = "red")
lines(partition, dnorm(partition, y.means[1], y.sd[1]), col = "blue")
lines(partition, dnorm(partition, y.means[3], y.sd[3]), col = "blue")
par(opar)

boxplot(data.frame(Modified=y1,Unmodified=y2),ylab="Strength (kgf/cm^2)",las=1, main = "Box plots for the portland cement tension bond strength data",col="darkorange4")

Tension_data_w <- data.frame(Modified=y1,Unmodified=y2)
library(tidyr)

Tension_data_l <- gather(Tension_data_w, Type, Strength, factor_key=TRUE)

library(gplots)
library(ggplot2)
ggplot(Tension_data_l, aes(x=Type, y=Strength, fill)) +
   geom_boxplot(size = 1, notch = T,fill='#A4A4A4') +
   xlab("Type") +
   ylab("Tension Bond Strength") +
   theme_classic() +  
   geom_jitter(position=position_jitter(width=0.3,height=0),
               alpha=0.6,
               size=1,
               show.legend=F) +
   stat_summary(fun.y=mean, geom="point", shape=23, size=3) #+coord_flip()



# PLASMA ETCHING EXPERIMENT

etch.rate <- read.table("Data/etchrate.txt",header=T)
grp.means <- with(etch.rate, tapply(rate,RF,mean))
with(etch.rate, stripchart(rate~RF,vert=T,method="overplot",pch=1,ylab=""))
stripchart(as.numeric(grp.means)~as.numeric(names(grp.means)),pch="x", cex=1.5,vert=T,add=T)
#arrows(c(160,180,200,220) ,grp.means, c(170,190,210,230) ,grp.means,length=10)
title(main="Etch Rate data",ylab=expression(paste("Observed Etch Rate (",ring(A),"/min)")),xlab="RF Power (W)")                
legend("bottomright","Group Means",pch="x",bty="n")

# first, we convert each variable to factor
etch.rate$Power <- as.factor(etch.rate$RF)
etch.rate$Run   <- as.factor(etch.rate$run)

with(etch.rate,boxplot(rate~Power,ylab="Etch rate(A/min)",xlab="Power - 4 levels factor",las=1, main = "Comparative box plots for the Plasma Etching Experiment data"))

with(etch.rate,boxplot(rate~Run,ylab="Etch rate(A/min)",xlab="Run - 5 levels",las=1, main = "Comparative box plots for the Plasma Etching Experiment data"))
 

# next, we run the model
etch.rate.aov1 <- aov(rate~Power+Run,etch.rate)
summary(etch.rate.aov1)
etch.rate.aov2 <- aov(rate~Power,etch.rate)
summary(etch.rate.aov2)
anova(lm(rate~Power,etch.rate))
anova(etch.rate.aov1,etch.rate.aov2)




model1  = lm(rate~Power,          data=etch.rate)
model2 = lm(rate~Power - 1,      data=etch.rate)

summary(model1)
confint(model1, level = 0.95)
summary(model2)
confint(model2)    
anova(model1)
anova(model2) # do not use                 



# Contrast
contr.treatment(4)

model5 <- lm(rate~Power,data=etch.rate, contr=list(Power = contr.treatment))
summary(model5)
tapply(etch.rate$rate,etch.rate$Power,mean)
t(coef(model5)[1]+contr.treatment(4)%*%coef(model5)[-1])




model6 <- lm(rate~Power,data=etch.rate, contr=list(Power = contr.sum))
summary(model6)
tapply(etch.rate$rate,etch.rate$Power,mean)
t(coef(model6)[1]+contr.treatment(4)%*%coef(model6)[-1])


contr.helmert(4)
model7 <- lm(rate~Power,data=etch.rate, contr=list(Power = contr.helmert))
summary(model7)
tapply(etch.rate$rate,etch.rate$Power,mean)
t(coef(model7)[1]+contr.treatment(4)%*%coef(model7)[-1])


# Teprve nyni bereme v uvahu, ze urovne pouziteho faktoru jsou usporadany
# Jednotlive slozky vektoru se tedy snazi zachytit linearni, kvadraticky ci kubicky trend.
# Samozrejme, za predpokladu, ze hodnoty usporadaneho faktoru (ordinalniho znaku) 
# jsou od sebe ekvidistantne vzdalene.

contr.poly(4)

model8 <- lm(rate~Power,data=etch.rate, contr=list(Power = contr.poly))
summary(model8)

coef(model8)[1]*rep(1,4) + contr.poly(4)[,1]*as.numeric(levels(etch.rate$Power))
tapply(etch.rate$rate,etch.rate$Power,mean)

summary(lm(rate~RF,data=etch.rate))




# overall mean
(erate.mean <- mean(etch.rate$rate))
# treatment effects
with(etch.rate, tapply(rate,RF,function(x) mean(x)-erate.mean))

etch.rate.aov <- aov(rate~Power,etch.rate)
model.tables(etch.rate.aov)

MSe <- summary(etch.rate.aov)[[1]][2,3]
MSe 

SD.pool <- sqrt(MSe/16)
SD.pool

#t.crit <- c(-1,1)*qt(.975,16)
#t.crit
#
#with(etch.rate, t.test(rate[RF==160],rate[RF==180],var.equal=TRUE))
#
#mean(tapply(etch.rate$rate,etch.rate$RF,var))/5
#
#mean(c(var(etch.rate$rate[etch.rate$RF==160]),
#       var(etch.rate$rate[etch.rate$RF==180])))

#as.numeric(grp.means[4]-grp.means[1])+c(-1,1)*qt(.975,16)*sqrt(2*MSe/5)

opar <- par(mfrow=c(2,2),cex=.8)
plot(etch.rate.aov)
par(opar)

plot(etch.rate.aov)



         
durbinWatsonTest(etch.rate.aov)

bartlett.test(rate~RF,data=etch.rate)

leveneTest(etch.rate.aov)


y1 = etch.rate$rate[etch.rate$RF==160]
y2 = etch.rate$rate[etch.rate$RF==180]
y3 = etch.rate$rate[etch.rate$RF==200]
y4 = etch.rate$rate[etch.rate$RF==220]


shapiro.test(y1)
shapiro.test(y2)
shapiro.test(y3)
shapiro.test(y4)
ks.test(y1,"pnorm", mean(y1), sd(y1))
ks.test(y2,"pnorm", mean(y2), sd(y2))
ks.test(y3,"pnorm", mean(y3), sd(y3))
ks.test(y4,"pnorm", mean(y4), sd(y4))


# Regression
Erch_rate = etch.rate$rate
Power1    = etch.rate$RF
Power2    = (etch.rate$RF)^2
model_reg1 = lm(Erch_rate ~ Power1)
summary(model_reg1)
model_reg2 = lm(Erch_rate ~ Power1 + Power2)
summary(model_reg2)

plot(Erch_rate ~ Power1)
abline(model_reg1, col = "red")
lines(Power1, predict(model_reg2), col = "blue")

#Post-ANOVA Comparison of Means

pairwise.t.test(Erch_rate,Power1,p.adjust.method="bonferroni")
pairwise.t.test(Erch_rate,Power1,p.adjust.method="hochberg")

summary(etch.rate.aov)
TukeyHSD(etch.rate.aov, c("Power"), ordered = FALSE, conf.level = 0.95)
plot(TukeyHSD(etch.rate.aov, ordered = FALSE,las=1) )

TukeyHSD(etch.rate.aov, ordered = FALSE,conf.level = (1-0.0215994))
plot(TukeyHSD(etch.rate.aov, ordered = FALSE,conf.level = (1-0.0215994),las=1) )



etch.rate.aov <- aov(rate~Power,etch.rate)
summary(etch.rate.aov)
LSD.out <- LSD.test(etch.rate.aov,"Power", p.adj="bonferroni")
LSD.out
LSD.test(etch.rate$rate, etch.rate$Power, 16,334) 


grp.means <- c(575,600,650,675)     
power.anova.test(groups=4,between.var=var(grp.means),within.var=25^2, sig.level=.01,power=.90)


# Example  2 - 

nn             = seq(3,10,by=1)
sd             = 25
max_difference = 75
beta <- matrix(NA,nr=length(sd),nc=length(nn))
for (i in 1:length(nn)) 
  beta[,i] <- power.anova.test(groups=4,n=nn[i], between.var = max_difference^2 , within.var=(sd^2), sig.level=.01)$power
colnames(beta) <- nn; 
rownames(beta) <- power
beta


# Choice of sample size 5.3.5
n     = seq(3,7,by=1)
a     = 7               # number of treatment levels    A
D     = 75              # max difference in group means
sigma = 25              # standard deviation

Fi_sq = (n*(D)^2)/(2*a*(sigma^2))
Fi    = sqrt(Fi_sq)
errorDF = a*(n-1)
powers  = power.anova.test(groups=a, n=n, between.var = (D^2)/(2), within.var=sigma^2,sig.level=.01)$power
rbind(n , Fi ,errorDF, powers)

# SS = 75^2/2
# n=4 Fi^2 = 4.5   Fi= 2.12  power = 0.65
# n=5 Fi^2 = 5.625 Fi= 2.37  power = 0.8
# n=6 Fi^2 = 6.75  Fi= 2.60  power = 0.91



max_difference = 75
sd <- seq(20,80,by=1)
nn <- seq(4,12,by=1)
n_treatment = 4
beta <- matrix(NA,nr=length(sd),nc=length(nn))
for (i in 1:length(sd)) 
  beta[i,] <- power.anova.test(groups=4,n=nn,between.var = (max_difference^2/2)/(n_treatment-1), within.var=sd[i]^2,sig.level=.01)$power
colnames(beta) <- nn; 
rownames(beta) <- sd
beta
opar <- par(las=1,cex=.8)
matplot(sd,beta,type="l",xlab=expression(sigma),ylab=expression(1-beta),col=1, lty=1)
grid()
text(rep(80,10),beta[length(sd),],as.character(nn),pos=3)
title("Operating Characteristic Curve\n for a=4 treatment means")
par(opar)





sd <- seq(20,80,by=1)
nn <- seq(3,10,by=1)
beta <- matrix(NA,nr=length(sd),nc=length(nn))
for (i in 1:length(sd)) 
  beta[i,] <- power.anova.test(groups=4,n=nn,between.var=var(grp.means), within.var=sd[i]^2,sig.level=.01)$power
colnames(beta) <- nn; 
rownames(beta) <- sd



opar <- par(las=1,cex=.8)
matplot(sd,beta,type="l",xlab=expression(sigma),ylab=expression(1-beta),col=1, lty=1)
grid()
text(rep(80,10),beta[length(sd),],as.character(nn),pos=3)
title("Operating Characteristic Curve\n for a=4 treatment means")
par(opar)



summary(etch.rate.aov)
power.anova.test(groups=4, n=5, between.var = 22290 , within.var=334 , sig.level=.01)
power.anova.test(groups=4, power = 0.9, between.var = 1000 , within.var=500 , sig.level=.01)



kruskal.test(rate~RF,data=etch.rate)

# Power graph for one-way Anova Enlarge

   dex <- expand.grid( within.var = 1, groups = 3, between.var = seq(.2,1,.05), power = c(.7,.8,.85,.9,.95))

   n <- numeric(0)
   for ( i in 1:nrow(dex)) {
       ni <- do.call("power.anova.test", dex[i,])$n
   }

   dex$n <- n
   td( new = T) 
   td( col = c('black','blue','red','green'))
   xyplot( n ~ between.var, dex, groups = power, type = 'l',  lwd = 1.5,
       auto.key = list(title= "Power",columns= 2,lines = T, points = F),
       xlab = "Between variance / Within variance",
       ylab = "n within each group",
       panel = function(x,y,...) {
           panel.xyplot(x,y,...)
           panel.abline(v=seq(.2,1,.1),col='grey')
           panel.abline(h=seq(2,40,2),col='grey')
       })





