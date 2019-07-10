
#clear memory
rm(list=ls())

#packages and libraries
install.packages("ggplot2")
install.packages("randomForest")
install.packages("xl")
install.packages("caTools")
install.packages("rpart.plot")
install.packages("forestFloor")
install.packages("rlang", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("caret")
install.packages("tree")
install.packages("maptree")
install.packages("CORElearn")
install.packages("mclust")
install.packages("MASS")
install.packages("cluster")
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("mlbench")

library(rpart.plot)
library(rpart)	
library(readxl)
library(randomForest)
library(ggplot2)
library(caTools)
library(party)
library(forestFloor)
library(caret)
library(tree)
library(CORElearn)
library(rtf)
library(corrplot)
library(MASS)

#Import data for all players with salary information for 2016
baseball <- read_xlsx("C:\\Users\\njohnson1\\Desktop\\Exp_Design_612\\SalaryData.xlsx")
statdata <- data.frame(baseball)

##################################Block salary in factors#######################################

#turn all blanks to NA
is.na(statdata) <- statdata == '' 
is.na(statdata) <- statdata == 'Null'

#declare all factor variables
statdata$Player = as.factor(statdata$Player)
statdata$Team = as.factor(statdata$Team)
statdata$Pos = as.factor(statdata$Pos)

#remove team variable
statdata$Team <- NULL
statdata$Player <- NULL

#summary of fields used4
summary(statdata)

#set seed
set.seed(12345)


split <- sample.split(statdata, SplitRatio=0.7)
training_set = subset(statdata, split == TRUE)
test_set = subset(statdata, split == FALSE)

str(training_set)


#models for the rpart and random forest
SalaryModel <- rpart(Salary ~ ., data = training_set, na.action=na.exclude)
SalaryModel
summary(SalaryModel)

SalaryModel1 <- randomForest(Salary ~ ., data = statdata, na.action = na.exclude)
SalaryModel1
summary(SalaryModel1)

finaltree <- rpart(Salary ~ ., data=statdata, cp=.02, method = "anova")
finaltree
summary(finaltree)

anova_rf <- anova(SalaryModel, type = 3)
anova_rf
#Blocking factor
fit <- aov(Salary ~. + Pos, data=statdata)
fit
plot(fit)

fit1 <- aov(Salary ~. + G, data=statdata)
fit1
plot(fit1)

#one way anova
fit3 <- aov(Salary ~ ., data=statdata)
fit3
plot(fit3)

#https://www.statmethods.net/stats/anova.html for more testing and comparisons

#with this model and graph it shows that there isnt enough relationships between the data to use random forest.
Variable_Importance100 <- randomForest(Salary ~ ., data = training_set, ntree = 100, na.action = na.exclude)
Variable_Importance100
plot(Variable_Importance100)

#change to 20 trees it doesnt need 100
Variable_Importance <- randomForest(Salary ~ ., data = training_set, ntree = 20, na.action = na.exclude)
Variable_Importance 
plot(Variable_Importance)

#rpart
printcp(SalaryModel)
summary(SalaryModel)
plotcp(SalaryModel)

plot(SalaryModel, uniform = TRUE )
text(SalaryModel, use.n = TRUE, all = TRUE, cex = .8)

#random forest
print(SalaryModel1)
importance(SalaryModel1) #returns GINI
table(predict(SalaryModel1), training_set$Salary)
plot(SalaryModel1)

#plot(importance(finalmodel2))
#importance of variables
varImpPlot(Variable_Importance, pch = 16, col = "blue")
getTree(Variable_Importance, k=1, labelVar=FALSE)


# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html ###########################plotting options
#remove Pos because it isn't numeric
library(ggcorrplot)
statdata$Pos <- NULL
#rounds correlation calculations to 1 decimal place
corr <- round(cor(statdata), 2)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           #method="circle", 
           colors = c("tomato2", "white", "Cyan"), 
           title="Correlogram of Baseball Stats", 
           ggtheme=theme_bw)


#option	description
#col	Default plotting color. Some functions (e.g. lines) accept a vector of values that are recycled.
#col.axis	color for axis annotation
#col.lab	color for x and y labels
#col.main	color for titles
#col.sub	color for subtitles
#fg	plot foreground color (axes, boxes - also sets col= to same)
#bg	plot background color


#tree package
#too many variable...32 ir less
tr <- tree(Salary ~ ., data = statdata)
summary(tr)
plot(tr); text(tr)

#party package
(ct = ctree(Salary ~ ., data = training_set))
plot(ct, main = "nonSTD and STD")
#error predictions


#est class probabilities
tr.pred <- predict(ct, newdata = statdata, type = "prob")
tr.pred

# Model Based Clustering
library(mclust)
fit4 <- Mclust(statdata1)
plot(fit4) # plot results 
summary(fit4) # display the best model
###################################################Clustering and graphs#########################################
# Prepare Data
#statdata1 <- na.omit(statdata1) # listwise deletion of missing
#statdata1 <- scale(statdata1) # standardize variables

# K-Means Clustering with 5 clusters
#fit5 <- kmeans(statdata1, 5)

# Determine number of clusters
#wss <- (nrow(statdata1)-1)*sum(apply(statdata1,2,var))
#for (i in 2:15) wss[i] <- sum(kmeans(statdata1, 
#                                     centers=i)$Salary)
#plot(1:15, wss, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")

# Cluster Plot against 1st 2 principal components


statdata1 <- data.frame(baseball)
statdata1

#factors
statdata1$Player = as.factor(statdata$Player)
statdata1$Team = as.factor(statdata$Team)
statdata1$Pos = as.factor(statdata$Pos)

#variable removal
#statdata1$SO <- NULL
statdata1$Team <- NULL
statdata1$Player <- NULL

#statdata1$AVG.U.25BC. <- NULL

#statdata1$G <- NULL
#statdata1$X2B <- NULL
#statdata1$OBP <- NULL
#statdata1$CS <- NULL
#statdata1$SB <- NULL
#statdata1$pos <- NULL

#turn all blanks to NA
is.na(statdata1) <- statdata1 == '' 
is.na(statdata1) <- statdata1 == 'Null'

#
fit.random.forest <- CoreModel(Salary ~ ., data = statdata1)
plot(fit.random.forest)


library(cluster)
clusplot(statdata1, statdata1$RBI, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Model Based Clustering
library(mclust)
fit5 <- Mclust(statdata1)
plot(fit5) # plot results 
summary(fit5) # display the best model

#scatter plot of top two variables
ggplot(statdata1, aes(Pos, BB,  color = Salary)) + geom_point()
ggplot(statdata1, aes(Pos, RBI,  color = Salary)) + geom_point()

ggplot(statdata1, aes(BB, RBI,  color = Salary)) + geom_point() + geom_abline()
ggplot(statdata1, aes(HR, H,  color = Salary)) + geom_point() + geom_abline()



#glm
fit6 <- glm(Salary ~ ., data=statdata1, family=poisson())
fit6
summary(fit6) # display results
confint(fit6) # 95% CI for the coefficients
exp(coef(fit6)) # exponentiated coefficients
exp(confint(fit6)) # 95% CI for exponentiated coefficients
predict_glm <- predict(fit6, type="response") # predicted values
predict_glm
residuals(fit6, type="deviance") # residuals



#test with linear model

fit7 <- lm(Salary ~ ., data = statdata)
summary(fit7) 



# http://www.stat.wisc.edu/~larget/stat302/chap2.pdf   for graphing options
