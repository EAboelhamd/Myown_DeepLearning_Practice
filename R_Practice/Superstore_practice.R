cat("\014")  
library(ggplot2)
install.packages("data.table") #used in one hot encoding, and this (:=) operator


library(caret)

library("data.table")
install.packages("corrplot")

df <- read.csv("/home/eman/R/Ahmed Emad/Superstore.csv", header = TRUE)

head(df)

names(df)

str(df)

summary(df)

## count NAs
sapply(df, function(x) sum(is.na(x)))

## let's make some plots

hist(df$Profit)

hist(df$Sales)

## Correlation Analysis

cor_data <- df[, c(19,20,21)]
head(cor_data)

cor_data_ <- cor(cor_data)

library(corrplot)
corrplot(cor_data_, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


## Feature Engineering .. 

## region vs Category 
table(df$Region, substr(df$Category, 1, 2))  #substr from ID two charcters starting from 1st position

# One hot encoding:
ohe = dummyVars("~.", data = df[, -c("Order.ID","Order.Date", "Ship.Date", "Order.Month.Year", "Customer.ID", "Customer.Name", "Product.Name")], fullRank = T)
ohe_df = data.table(predict(ohe, df[, -c("Order.ID","Order.Date", "Ship.Date", "Order.Month.Year", "Customer.ID", "Customer.Name", "Product.Name")]))
df = cbind(df[,"Order.ID"], ohe_df)        
df


## keda ba2i azabba6 el categorical data and build a regression model
linear_reg_mod = lm(Profit ~., data = df[, -c("Order.ID")])
linear_reg_mod
