#BETO MARIN
#TASK 3  Multiple Regression in R
#15/11/2018

# GOALS  😁

# 1.- Predicting sales of four different product types: PC, Laptops, Netbooks and Smartphones
# 2.- Assessing the impact services reviews and customer reviews have on sales of different product types

#STEP 1 PREPROCESS DATA

# Categorical variables may be used directly as predictor or predicted variables in a 
#multiple regression model as long as they've been converted to binary values. 
#In order to pre-process the sales data as needed we first need to convert all factor
#or 'chr' classes to binary features that contain ‘0’ and ‘1’ classes. Fortunately, 
#caret has a method for creating these 'Dummy Variables' as follows:

# dummify the data

newDataframe <- dummyVars(" ~ .", data = existing_products)

readyData <- data.frame(predict(newDataFrame, newdata = existing_products))


#CORRELATION

#In order to measure the correlation between the variables in the data, 
#all variables must not contain nominal data types. 3

#1.- Use the str() to check all of the datatypes in your dataframe. 
#2.- Now use summary() to check for missing data. Missing data is represented by "NA". 
#There are many methods of addressing missing data, but for now let's delete any attribute that has missing information.

yourdataframe$attributeWithMissingData <- NULL  #Que pasa on los 0?

#While correlation doesn't always imply causation you can start your analysis by finding the 
#correlation between the relevant independent variables and the dependent variable. 

#1.- In the next steps you will use the cor() function to create a correlation matrix that 
#you can visualize to ascertain the correlation between all of the features.
#2.- Use the cor() function to build the correlation matrix:

corrData <- cor(readyData) 

corrData

#Correlation values fall within -1 and 1 with variables have string positive relationships 
#having correlation values closer to 1 and strong negative relationships with values closer to -1. 

#What kind of relationship do two variables with a correlation of '0' have?

#It is often very helpful to visualize the correlation matrix with a heat map so we can 'see' 
#the impact different variables have on one another. To generate a heat map for your correlation matrix 
#we'll use corrplot package as follows:

install.packages("corrplot")

library(corrplot)

corrplot(corrData)

#Using the heat map, review the service and customer review relationships with sales volume and note 
#the associated correlations for your report. If you would like more detailed correlation figures 
#than those available with the heat map, enter the name of your correlation object into console and 
#review the printed information.

#Now that you know the relationships between all of the variables in the data it is a good time to remove 
#any features that aren't needed for your analysis.

#2.- DEVELOP MULTIPLE REGRESSION MODELS

#1.- Using the steps in 'R Walkthrough' that outlined train a linear model, create a linear model that uses 
#volume as its dependent variable. Use the summary() function of R to evaluate the model and make a specific 
#note of the R-Squared value.


  #1.- What do you notice about the RMSE and R-Squared values?
  #2.- Did the model perform well? Why or why not? 
  #3.- If not, perhaps you used the wrong type of machine learning method on the wrong type of data. See the 
  #following resource for more information: Parametric vs non-parametric methods for data analysis 

#2.-Using the same general approach documented in the caret pipeline, and the steps outlined below, 
#make sales volume predictions on the new products dataset after training and testing your models 
#on the historical data set:

#1.- Set seed and create training and test sets
#2.- Use 2 of the following 3 algorithms for your analysis
    #1.- Support Vector Machine (SVM): You can start by using svmLinear in caret, but you might also try one 
        #of the others (and know the difference!).
    #2.-Random Forest: This might take some time!
    #3.-GBM models: Use XGBM in caret (method is your choice)

#3.- Apply each of your models to your testing data as you have done in previous tasks using the predict() 
#function in R. 

#4.- Review your models and identify the one that performed best without overfitting. You should also look 
#at the predicted values. If you have negative vales in your predictions and negative values are not possible 
#for your dependent variable, choose a different model. Be prepared to explain why you chose to use the algorithms 
#you did in your report.

#5.-After choosing a model, you will need to prepare the new products data set for prediction. Anything that 
#has been done to the structure of the existing products data needs to repeated for new products. With new products, 
#use dummyVars() and then remove any attribute that you removed from the existing products data sets. When using 
#dummyVars, be sure to change the name of the object you are creating so that you don't overwrite your earlier work. 
#Example: newDataframe should be changed to newDataframe2 where ever it appears in your dummyVar work. 

#6.-Once new products is prepared, use the predict() function again. This time with the new products dataset to 
#create your final predictions in an object called finalPred.

#Often times it is helpful for report building to output your data set and predictions from RStudio. Let’s add 
#your predictions to the new products data and then create a csv file. Use your csv and Excel to organize your 
#data for reporting.

#Add predictions to the new products data set

output <- newproductattributes 
output$predictions <- finalPred

#Create a csv file and write it to your hard drive. Note: You may need to use your computer’s search function to 
#locate your output file.

write.csv(output, file="C2.T3output.csv", row.names = TRUE)

#Use Excel to organize your predictions. Remember the four product types you need to focus on: PC, Laptops, Netbooks 
#and Smartphones

__________________________________________________________________________________________________________
__________________________________________________________________________________________________________



install.packages("corrplot")
install.packages("caret")
library(rpart)
library(rpart.plot)
library(corrplot)
library(caret)


existingprod <- read.csv("existingprod.csv",header=TRUE) 
summary(existingprod)
str(existingprod)


existingproddummy <- dummyVars("~ProductType", data = existingprod)

existingproddummy

readyData <- data.frame(predict(existingproddummy, newdata = existingprod))

readyData

existingprod$ProductType.Accessories <- NULL



readyData$ProductNum <- existingprod$ProductNum

existingtotal <- merge(readyData,existingprod,by="ProductNum")

head(existingtotal)

existingtot <- existingtotal [,-14]
head(existingtot)

str(existingtot)
summary(existingtot)

existingtot$BestSellersRank <- NULL

correxistingtot <- cor(existingtot) 

correxistingtot

corrplot(correxistingtot)

tree<- rpart(Volume~., data=existingtot, cp=0.001)
rpart.plot(tree)

str(existingtot)
hist(existingtot$x5StarReviews)

tree<- rpart(Volume~., data=existingtot, cp=.0001)

rpart.plot(tree)


existingtot$x5StarReviews <- NULL
existingtot$x1StarReviews <- NULL
existingtot$x3StarReviews <- NULL
existingtot$x3StarReviews <- NULL


