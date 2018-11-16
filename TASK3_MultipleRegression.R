getwd()

rm(list = ls(all = TRUE))

#setwd("~/Google Drive/DATA ANALYSTICS/2 PREDICTING CUSTOMER PREFERENCES/3 MULTIPLE REGRESSION IN R/task2-3-profitability-BetoMarin")


getwd()

#install.packages("corrplot")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("dplyr")


library("rpart")
library("rpart.plot")
library("corrplot")
library("caret")
library(dplyr)

#importing the existing product dataset
existingprod <- read.csv("existingprod.csv", header=TRUE, sep=",")
summary(existingprod)
str(existingprod)

#creating dummy variables for the Product Type (12 levels)
existingproddummy <- dummyVars("~ProductType", data = existingprod)
existingproddummy
readyData <- data.frame(predict(existingproddummy, newdata = existingprod))
readyData

#attempt to do a for loop to join the two datasets (existingprod & readyData)

#for (i in 1:(ncol(readydata))) {
# existingprod$[] <- readyData$[i]

#}




#adding the column ProductNum in readyDATA (dataset with dummies) in order to do the merge 
#with the original dataset existingprod based on this key.
readyData$ProductNum <- existingprod$ProductNum
head(readyData)

#merging the two datasets readyData and existingprod
existingtotal <- merge(readyData,existingprod,by="ProductNum")
head(existingtotal)

#removing Product Type column
existingtot <- existingtotal [,-14]
head(existingtot)
str(existingtot)
summary(existingtot)

#removing BestSellersRank
existingtot$BestSellersRank <- NULL

#correlation matrix among all the variables
correxistingtot <- cor(existingtot) 
correxistingtot
#visualizing the correlation matrix with a heatmap
corrplot(correxistingtot)

#based on the correlation matrix, select the variables which are more correlated with the label Volume
#the correlation matrix only looks for linear relationships. we will need a decision tree to search for
#non linear relationships.

#removing variable x5starReviews because it has correlation 1 with the dependent variable Volume (very unrealistic)
#and this would bring to overfit out model.
existingprodsintype$x5StarReviews <- NULL
head(existingprodsintype)

#building a decision tree to capture also non linear relationships among variables
tree<- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree)


existingtypenox5 <- 
tree<- rpart(Volume~., data=existingprod, cp=0.001)
rpart.plot(tree)

existingprodsintype$ProductType <- existingtotal$ProductType
head(existingprodsintype)

tree_pt <- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree_pt)
#we can delete the product type because it is not relevant in predicting the volume




#we can notice that x4 star Reviews, PositiveServiceReview and x4StarReviews are the most relevant
#variable in order to predict the sales volume (by descending order).

existingprodsintype <- read.csv("existingprodsintype.csv", header=TRUE, sep=",")

#correlation matrix among all the variables
correxistingprodsintype <- cor(existingprodsintype) 
correxistingprodsintype
#visualizing the correlation matrix with a heatmap
corrplot(correxistingprodsintype)
# Visialisation of the decision tree
tree<- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree)

#Afer a deep thougth we have decided to take out the following variables

existingprodsintype$ProductType <- NULL
existingprodsintype$x1StarReviews <- NULL
existingprodsintype$NegativeServiceReview <- NULL
existingprodsintype$ProfitMargin <- NULL
existingprodsintype$ProductHeight <- NULL
existingprodsintype$ProductWidth <- NULL
existingprodsintype$ProductDepth <- NULL
existingprodsintype$ShippingWeight <- NULL
existingprodsintype$Recommendproduct <- NULL
head (existingprodsintype)

# We have begin to make the linear regression but we forgot to take out the outliers and to normalize.

set.seed(123)
existingprodtrainindex <- createDataPartition(
  y = existingprodsintype$Volume,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainexisting <- existingprodsintype[ existingprodtrainindex,]
testexisting  <- existingprodsintype[-existingprodtrainindex,]

nrow(trainexisting)
nrow(testexisting)

linearmod <- lm(Volume~., data=trainexisting)
linearmod

summary(linearmod)

Predictlm <- predict.lm(linearmod, newdata = testexisting)

Predictlm

str(Predictlm)

# Here we identify the outliers

outlier_values <- boxplot.stats(existingprodsintype$Volume)$out

boxplot(existingprodsintype$Volume, main="outlier", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Here we use a function from a package call dplyr (Pablo's suggestment) to make take out the outliers. The point is that this is a filter and
#it doesn't delete the rows from the dataset. It creates a new dataset that we call in this case filteroutliers

filteroutliers<-filter(existingprodsintype, Volume!=c(70336,11204))

#We create the partition 

set.seed(123)
existingprodtrainindex <- createDataPartition(
  y = filteroutliers$Volume,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainexisting <- filteroutliers[ existingprodtrainindex,]
testexisting  <- filteroutliers[-existingprodtrainindex,]

nrow(trainexisting)
nrow(testexisting)

#I check that I don't have outliers

outlier_values <- boxplot.stats(filteroutliers$Volume)$out

boxplot(filteroutliers$Volume, main="outlier", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#We try to normalize

linearmodelnorm<-train(Volume~.,data=filteroutliers,method="lm",
preProcess=c("center","scale"), metric="RMSE")

linearmodelnorm

#We apply the linear regression

Predictlm <- predict(linearmodelnorm, newdata = testexisting, metric="RMSE")

Predictlm

str(Predictlm)
head(testexisting)
?postResample
postResample(Predictlm, testexisting$Volume)
