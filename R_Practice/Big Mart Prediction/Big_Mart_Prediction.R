## link: https://trainings.analyticsvidhya.com/courses/course-v1:AnalyticsVidhya+BigMS01+2018_1/courseware/0adf11d500c84ca586e6adf2950ff91a/1b7eb96a08824011929e64fe248a8cd5/1

## Loading packages 

install.packages ##reading and manipulation of data
library('data.table')

install.packages('dplyr')  #data manipulation and joining 
library(dplyr)

install.packages('ggplot2') #plotting 
library('ggplot2')

install.packages('caret')  #for modeling
library('caret')

install.packages('corrplot') #correlation plt
library('corrplot')

install.packages('xgboost') #for applying XGBoost model
library('xgboost')

install.packages('cowplot') #for combining mutltiple plots 
library('cowplot')

## ---------------------------------------------------------------------------------

## Reading data
train = fread("/home/eman/R/Big Mart Prediction/Train_UWu5bXk.csv")
test = fread("/home/eman/R/Big Mart Prediction/Test_u94Q5KV.csv")
submission = fread("/home/eman/R/Big Mart Prediction/SampleSubmission_TmnO39y.csv")

## ----------------------------------------------------------------------------------

## Data understanding .. 
head(train)
dim(train)
dim(test)

names(train) #names of the variables

str(train)

## we gonna combine train and test datasets in data viz, preprocessing, feature engineering phases .. 
test[,Item_Outlet_Sales := NA]  # add new column with NAs 
str(test)

# combine train and test
combi = rbind(train, test)
dim(combi)

## ---------------------------------------------------------

## Exploratory Data Analysis (EDA)

## we gonna plot cont. vars using histograms and categorical vars using Bar plots

## Target variable

