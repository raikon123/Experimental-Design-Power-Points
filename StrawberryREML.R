#source("c:/Work07/StatDesign07/Programs/R/StrawberryREML.R",print.eval=TRUE)#
#Does t-test for Strawberry data
A<-c(10.1,10.8,9.8,10.5)
B<-c(6.3,6.9,5.3,6.2)
C<-c(8.4,9.4,9.0,9.2)
#--------RCB ANOVA - A, B and C---------------------
yield<-c(A,B,C)
trt<-rep(c("A","B","C"),each=4)
block <- rep(c("1","2","3","4"),each=1,times=3)
strawdata <- data.frame(yield,block,trt)
summary(aov(yield ~ block + trt,strawdata))
#--------REML estimates---------------------
library(nlme)
strawmodel<-lme(yield ~ 1+trt,strawdata,random=~1|block)
summary(strawmodel)
VarCorr(strawmodel)
#************************************************************
#--------Now the one with negative anova estimates---------
#*************************************************************
A<-c(10.1,10.8,9.8,10.5)
B<-c(8.4,6.9,5.3,6.2)
C<-c(6.3,9.4,9.0,9.2)
#--------RCB ANOVA - A, B and C---------------------
yield<-c(A,B,C)
trt<-rep(c("A","B","C"),each=4)
block <- rep(c("1","2","3","4"),each=1,times=3)
strawdata <- data.frame(yield,block,trt)
summary(aov(yield ~ block + trt,strawdata))
#--------REML estimates---------------------
library(nlme)
strawmodel<-lme(yield ~ 1+trt,strawdata,random=~1|block)
summary(strawmodel)
VarCorr(strawmodel)