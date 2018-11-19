getwd()

rm(list = ls(all = TRUE))

setwd("~/Google Drive/DATA ANALYSTICS/2 PREDICTING CUSTOMER PREFERENCES/3 MULTIPLE REGRESSION IN R/task2-3-profitability-BetoMarin")
#setwd("C:/Users/giorgia/Documents/Giorgia Felline/UBIQUM/2. PREDICTING CUSTOMERS PREFERENCES/Task3_MultipleRegression/task2-3-profitability-Jorj91")
getwd()

#install.packages("corrplot")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("dplyr")
#install.packages("plotly")
#install.packages("randomForest")
#install.packages("gbm")


library("rpart")
library("rpart.plot")
library("corrplot")
library("caret")
library("dplyr")
library("plotly")
library("randomForest")
library("gbm")

#importing the existing product dataset
#existingprod <- read.csv("existingproductattributes2017.2.csv", header=TRUE, sep=",")
#summary(existingprod)
#str(existingprod)

#importing the existing product dataset
existingprod <- read.csv("existingprod.csv", header=TRUE, sep=",")
summary(existingprod)
str(existingprod)

#creating dummy variables for the Product Type (12 levels)
existingproddummy <- dummyVars("~ProductType", data = existingprod)
existingproddummy
readyData <- data.frame(predict(existingproddummy, newdata = existingprod))
readyData
head(readyData)
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

#TO DO: model with all the variables (dummyvars included) and see what happens. Use dataset "existingtotal"

#removing Product Type column
existingtot <- existingtotal [,-14]
head(existingtot)

#removing BestSellersRank
existingtot$BestSellersRank <- NULL
head(existingtot)

####
#plotting producttype vs volume (we can see that there are two outliers with respect to volume)
qplot(existingprod$ProductType, existingprod$Volume, 
      main = "Sales Volume across different Product Types", 
      xlab = "Product Type", ylab = "Sales Volume", col = "red")
?qplot
#TO DO: center the title of the plot and remove the legend


#plotly (interactive plot): producttype vs volume (same conclusions as before)
typevolume <- plot_ly(data = existingprod, x = ~ProductType, y = ~Volume, type = "scatter", mode = "markers", color = I("mediumseagreen"))%>%
  layout(title = "Sales Volume across different Product Types")
typevolume
?plot_ly

#plotly showing the perfect correlation between x5 and Volume
x5Volume <- plot_ly(data = existingprod, x = ~x5StarReviews, y = ~Volume, type = "scatter", mode = "markers",color = I("mediumseagreen"))%>%
  layout(title = "Perfect Linear Relationship between X5StarReviews and Volume")

x5Volume
add_lines(x5Volume)

#N.B. to write tilde on a Italian keyboard, type alt + 126 (right hand numbers) -> ~

#TO DO: scattermatrix among all predictors and label Volume
# histograms and barplot of all variables 

head(existingprod) #18 variables

par(mfrow = c(3, 6))
for(i in 1:(ncol(existingprod))){
  cl = existingprod[,i]
  if (is.numeric( cl )) 
  {hist( cl, xlab= names(existingprod)[i], main = (paste('Frequency of',names(existingprod)[i])))}
  else if (is.factor(cl))
  {barplot(table(cl),main = (paste('Frequency of',names(existingprod)[i])))}}


###

#correlation matrix among all the variables
par(mfrow = c(1,1))
correxistingtot <- cor(existingtot) 
correxistingtot
#visualizing the correlation matrix with a heatmap
corrplot(correxistingtot, title = "Correlation Matrix", tl.cex = 0.8, type = "upper",tl.col = "blue2")
?corrplot
#TO DO: Center the title!

#based on the correlation matrix, select the variables which are more correlated with the label Volume

#N.B. choosing a threshold for the correlation index equal to |0.85|, we select
#from the correlation matrix only the variable x4StarReviews (0.87)
#N.B. the correlation matrix only looks for linear relationships. 
#we will need a decision tree to search for non linear relationships.

#removing variable x5starReviews because it has correlation 1 with the dependent variable Volume (very unrealistic)
#and this would bring to overfit out model.

existingprodsintype <- read.csv("existingprodsintype.csv", header=TRUE, sep=",")
head(existingprodsintype)
existingprodsintype$x5StarReviews <- NULL
head(existingprodsintype)


#building a decision tree to capture also non linear relationships among variables
tree <- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree)
#according to this decisional tree, the most relevant variables are:
#x4StarReviews, PositiveServiceReview (by descending order of importance)


#decision tree with the original dataset, to prove that keeping variable X5 brings to overfitting
tree1 <- rpart(Volume~., data=existingprod, cp=0.001)
rpart.plot(tree1)
#this only considers x5 for the splitting!

#addding the variable productType to the dataset existingsintype
existingprodsintype$ProductType <- existingtotal$ProductType
head(existingprodsintype)

#proving that producttype is not relevant because it does not appear in the tree
tree_pt <- rpart(Volume~., data=existingprodsintype, cp=0.001)
rpart.plot(tree_pt)
#therefore, we can delete the product type because it is not relevant in predicting the volume


#CONCLUSION from CORRELATION MATRIX AND DECISION TREE: 
#x4 star Reviews and PositiveServiceReview are the most relevant
#variables in order to predict the sales volume (by descending order).

#reloading data. this cointains 15 variables.
existingprodsintype <- read.csv("existingprodsintype.csv", header=TRUE, sep=",")
head(existingprodsintype)

#correlation matrix among all the variables
correxistingprodsintype <- cor(existingprodsintype) 
correxistingprodsintype
#visualizing the correlation matrix with a heatmap
corrplot(correxistingprodsintype,title = "Correlation Matrix", tl.cex = 0.8, type = "upper",tl.col = "blue4")

#Afer a deep thougth we have decided to take out the following variables

existingprodsintype$Price <- NULL
existingprodsintype$x5StarReviews <- NULL
existingprodsintype$x3StarReviews <- NULL
existingprodsintype$x2StarReviews <- NULL
existingprodsintype$x1StarReviews <- NULL
existingprodsintype$NegativeServiceReview <- NULL
existingprodsintype$Recommendproduct <- NULL
existingprodsintype$ShippingWeight <- NULL
existingprodsintype$ProductHeight <- NULL
existingprodsintype$ProductWidth <- NULL
existingprodsintype$ProductDepth <- NULL
existingprodsintype$ProfitMargin <- NULL

head (existingprodsintype)
#AT THE END, WE ONLY DEAL WITH THE FOLLOWING VARIABLES: X4, POSITIVESERVICEREVIEW, VOLUME

#take out the outliers and normalize!!!

# Here we identify the outliers

outlier_values <- boxplot.stats(existingprodsintype$Volume)$out
outlier_values

par(mfrow = c(1, 1))
boxplot(existingprodsintype$Volume, main="Outliers in Sales Volume", boxwex=0.2)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Here we use a function from a package called dplyr to make take out the outliers. 
#The point is that this is a filter and
#it doesn't delete the rows from the dataset. 
#It creates a new dataset that we call in this case filteroutliers

filteroutliers<-filter(existingprodsintype, Volume!=7036 & Volume!=11204)
#from now on we will work with the dataset "filteroutliers"


#We create the partition in training and test sets

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

#I check that I don't have outliers
nrow(trainexisting)
nrow(testexisting)

outlier_values <- boxplot.stats(filteroutliers$Volume)$out
boxplot(filteroutliers$Volume, main="outlier", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#Normalizing and training the LINEAR REGRESSION

linearmodelnorm <- train(Volume~.,data=filteroutliers,method="lm",
                       preProcess=c("center","scale"), metric="RMSE")

linearmodelnorm
#performance on the training: 
#RMSE      Rsquared   MAE     
#321.0796  0.7500007  198.7543


#We apply the linear regression to the test set

Predictlm <- predict(linearmodelnorm, newdata = testexisting, metric="RMSE")
Predictlm

head(testexisting)
?postResample
postResample(Predictlm, testexisting$Volume)
#evaluating the performance on the test set
#RMSE    Rsquared         MAE 
#458.1982447   0.5519506 278.1005511 
#...una mierda!

str(filteroutliers)

#Apply SVM

trctrlsvm103 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#set.seed(123)

svm_Linear103 <- train(Volume ~., data = trainexisting, method = "svmLinear",
                    trControl=trctrlsvm103,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear103

#  RMSE      Rsquared   MAE     
#221.4499  0.8847243  127.1359

trctrlsvm206 <- trainControl(method = "repeatedcv", number = 20, repeats = 6)
set.seed(123)

svm_Linear206 <- train(Volume ~., data = trainexisting, method = "svmLinear",
                       trControl=trctrlsvm206,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
svm_Linear206

#RMSE      Rsquared   MAE     
#188.5187  0.9408436  134.5126

#set.seed(123)
#svm_Linear_Grid <- train(Volume ~., data = trainexisting, method = "svmLinear",
#                           trControl=trctrlsvm103,
#                           preProcess = c("center", "scale"),
#                           tuneGrid = grid,
#                           tuneLength = 10)
#
#> svm_Linear_Grid

test_pred_svm103 <- predict(svm_Linear103, newdata = testexisting, metric="RMSE")
test_pred_svm103

postResample(test_pred_svm103, testexisting$Volume)

#RMSE    Rsquared         MAE 
#481.2416698   0.5366656 257.2789558

set.seed(123)
trctrlsvm206 <- trainControl(method = "repeatedcv", number = 20, repeats = 6)

svm_Linear206 <- train(Volume ~., data = trainexisting, method = "svmLinear",
                       trControl=trctrlsvm206,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
svm_Linear206

test_pred_svm206 <- predict(svm_Linear206, newdata = testexisting, metric="RMSE")
test_pred_svm206

postResample(test_pred_svm206, testexisting$Volume)

#APPLY RANDOM FOREST

rf300trees <- train(Volume~., data = trainexisting, method = "rf",trControl = trctrlsvm103, metric = "RMSE", ntree = 300)

rf300trees

#resultrf300<- data.frame(testpred_rf300 <- predict(rf300trees, newdata = testexisting, metric="RMSE"))

#resultrf300<- data.frame(testpred_rf300 <- predict(rf300trees, newdata = testexisting, metric="RMSE"))

#colnames(resultrf300$testpred_rf300....predict.rf300trees..newdata...testexisting..) <- resultrf300$Predictions

testpred_rf300 <- predict(rf300trees, newdata = testexisting, metric="RMSE")

rfpredictions <- testpred_rf300
rfpredictions
actual <- testexisting$Volume

plotpredandactual <- plot(predandactual$actual, predandactual$rfpredictions)

predandactual <- data.frame(rfpredictions, actual)

predandactual

predandactual$errors<-predandactual$rfpredictions-predandactual$actual



postResample(testpred_rf300, testexisting$Volume)

#RMSE         Rsquared         MAE 
#249.1708406   0.8762659 148.4440222

rf600trees <- train(Volume~., data = trainexisting, method = "rf",trControl = trctrlsvm103, metric = "RMSE", ntree = 600)

rf600trees

testpred_rf600 <- predict(rf600trees, newdata = testexisting, metric="RMSE")
testpred_rf600

postResample(testpred_rf600, testexisting$Volume)

#RMSE          Rsquared         MAE 
#249.9573519   0.8757835 149.2772859 

#PRUEBA DE RANDOM FOREST CON LA DATASET ORIGNAL SIN ALGUNAS DE LAS COLUMNAS

existingprodpruebarf <- read.csv("existingprod.csv", header=TRUE, sep=",")
head(existingprodpruebarf)

existingprodpruebarf$ShippingWeight <- NULL
existingprodpruebarf$ProductHeight <- NULL
existingprodpruebarf$ProductWidth <- NULL
existingprodpruebarf$ProductDepth <- NULL
existingprodpruebarf$ProfitMargin <- NULL
existingprodpruebarf$ProductNum <- NULL
existingprodpruebarf$x5StarReviews <- NULL
existingprodpruebarf$BestSellersRank <- NULL
existingprodpruebarf$ProductType <- NULL


head(existingprodpruebarf)
summary(existingprodpruebarf)


filteroutliersprueba<-filter(existingprodpruebarf, Volume!=7036 & Volume!=11204)

summary(filteroutliersprueba)

set.seed(123)
existingprodpruebarfindex <- createDataPartition(
  y = filteroutliersprueba$Volume,
  ## the outcome data are needed
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE
)

trainexistingprueba <- filteroutliersprueba[ existingprodpruebarfindex,]
testexistingprueba  <- filteroutliersprueba[-existingprodpruebarfindex,]

#I check that I don't have outliers
nrow(trainexistingprueba)
nrow(testexistingprueba)

rf600treesoridf <- train(Volume~., data = trainexistingprueba, method = "rf",trControl = trctrlsvm103, metric = "RMSE", ntree = 600)

rf600treesoridf
str(rf600treesoridf)


str(filteroutliersprueba)
testpred_rf600prueba <- predict(rf600treesoridf, newdata = testexistingprueba, metric="RMSE")
testpred_rf600prueba

postResample(testpred_rf600prueba, testexistingprueba$Volume)
#RMSE           Rsquared         MAE 
#272.5164137   0.8459636 156.9387037 

treeprueba <- rpart(Volume~., data=filteroutliersprueba, cp=0.001)
rpart.plot(treeprueba)

#PRUEBA APPLY GMB

BMG <- train(Volume ~., data = trainexisting, method = "gbm",
                                     trControl=trctrlsvm103,
                                       preProcess = c("center", "scale"),
                                       tuneLength = 10)

BMG

summary(filteroutliersprueba)


hist(filteroutliers$Volume)

# APPLY THE BESTO MODEL (SVM) TO THE NEW PRODUCTS 

newproducts <- read.csv("newprod.csv", header=TRUE, sep=",")

newproducts$Price <- NULL
newproducts$x5StarReviews <- NULL
newproducts$x3StarReviews <- NULL
newproducts$x2StarReviews <- NULL
newproducts$x1StarReviews <- NULL
newproducts$NegativeServiceReview <- NULL
newproducts$Recommendproduct <- NULL
newproducts$ShippingWeight <- NULL
newproducts$ProductHeight <- NULL
newproducts$ProductWidth <- NULL
newproducts$ProductDepth <- NULL
newproducts$ProfitMargin <- NULL
newproducts$ProductType <- NULL
newproducts$BestSellersRank<-NULL
newproducts$ProductNum<-NULL

finalPred <- predict(rf300trees, newdata = newproducts, metric="RMSE")
finalPred

NewProductsAndPredictions <- read.csv("newprod.csv", header=TRUE, sep=",")

NewProductsAndPredictions$VolumeFinalPredictions <- finalPred

NewProductsAndPredictions$Volume<-NULL

write.csv(NewProductsAndPredictions, file="C2.T3output.csv", row.names = TRUE)


