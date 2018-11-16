getwd()

rm(list = ls(all = TRUE))

#setwd("~/Google Drive/DATA ANALYSTICS/2 PREDICTING CUSTOMER PREFERENCES/3 MULTIPLE REGRESSION IN R/task2-3-profitability-BetoMarin")


getwd()

#install.packages("corrplot")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")


library("rpart")
library("rpart.plot")
library("corrplot")
library("caret")

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

tree<- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree)

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



