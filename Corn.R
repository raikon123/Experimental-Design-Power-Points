#source("c:/Work07/StatDesign07/Programs/R/Examples/Corn.R",print.eval=TRUE)#
#Does anova and ancova for Corn data
data<-read.table("c:/Work07/StatDesign07/DataSets/Corn.txt",sep = "",header=T)
Trt<-as.character(data[,1])
Plants<-data[,2]
Yield<-data[,3]
aovdata <- data.frame(Trt, Plants, Yield)
#----------This gives the  ancova table fitting treatments first----------------
summary(aov(Yield~Trt+Plants,data=aovdata))
#----------This gives the  ancova table fitting covariates first----------------
summary(aov(Yield~Plants+Trt,data=aovdata))
#----------This is the regression of Yield on Plants----------------
summary(aov(Yield~Plants,data=aovdata))
#----------This is the oneway on Yield----------------
summary(aov(Yield~Trt,data=aovdata))

