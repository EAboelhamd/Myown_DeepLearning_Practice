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

install.packages("data.table") #used in one hot encoding, and this (:=) operator
library("data.table")
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
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") + xlab ("Item_Outlet_Sales")
#positively skewed shape

## Independent Variables:
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue") #Maximum Retail Price
#let's plot all of them in the same grid

plot_grid(p1, p2, p3, nrow = 1)  #from cowplot package

#There seems to be no clear-cut pattern in Item_Weight.
#Item_Visibility is right-skewed and should be transformed to curb its skewness.
#We can clearly see 4 different distributions for Item_MRP. It is an interesting insight.

## -------------------------------------------------------------------------------

## Categorical Vars:
## 1. Item_Fat_Content.

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1") # group_by from dplyr library
# duplicate values :S
#In the figure above, ‘LF’, ‘low fat’, and ‘Low Fat’ are the same category and can be combined into one. 
#Similarly we can be done for ‘reg’ and ‘Regular’ into one. After making these corrections we’ll plot the same figure again.

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

#plot again
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

## Other Categorical vars:
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + xlab ("")+ geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") + geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Item_Type")
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + xlab ("")+ geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Outlet_Identifier")
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + xlab ("")+ geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Outlet_Size")

#let's put all together
second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

# In Outlet_Size’s plot, for 4016 observations, Outlet_Size is blank or missing.
#We will check for this in the bivariate analysis to substitute the missing values in the Outlet_Size.

# cont. working on categorical vars
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") + geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) + xlab ("Outlet_Establishment_Year") + theme(axis.text.x = element_text(angle = 8.5)) 
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + xlab ("Outlet_Type")+ geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Type, Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot_grid(p7, p8, ncol = 2)

## comments:
#Lesser number of observations in the data for the outlets established in the year 1998 as compared to the other years.
#Supermarket Type 1 seems to be the most popular category of Outlet_Type

## Bivariate Analysis (Scatter plot)
## extrating train data from combi

train = combi[1:nrow(train),]

## 1. Target Variable vs Independent Numerical Variables
## 1.1. Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.8) + theme(axis.title = element_text(size = 8.5))
## no linear relationship between item_weight and its sales

# 1.2. Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.8) + theme(axis.title = element_text(size = 8.5))
## no linear relationship as well between Item_Visibility and its sales

# 1.3. Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.8) + theme(axis.title = element_text(size = 8.5))

## let's put the 3 graphs together 
second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)

## Comments:
#Item_Outlet_Sales is spread well across the entire range of the Item_Weight without any obvious pattern.
#In Item_Visibility vs Item_Outlet_Sales, there is a string of points at Item_Visibility = 0.0 which seems strange as item visibility cannot be completely zero. We will take note of this issue and deal with it in the later stages.
#In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 4 segments of prices that can be used in feature engineering to create a new variable.

## ---------------------------------------------------------
## 2. Target Variable vs Independent Categorical Variables
##  We will try to check the distribution of the target variable across all the categories of each of the categorical variable.
## We could have used boxplots here, 
#but instead we’ll use the violin plots as they show the full distribution of the data. 
#The width of a violin plot at a particular level indicates the concentration or density of data at that level. 
#The height of a violin tells us about the range of the target variable values.
#let's try both (Violin and boxplots)

# 2.1. Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "blue") + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 6), axis.title = element_text(size = 8.5))

# 1.2. Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "blue") + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 10), axis.title = element_text(size = 8.5))

# 1.3. Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "blue") + theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 10), axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

## Comments:
#Distribution of Item_Outlet_Sales across the categories of Item_Type is not very distinct and same is the case with Item_Fat_Content.
#The distribution for OUT010 and OUT019 categories of Outlet_Identifier are quite similar and very much different from the rest of the categories of Outlet_Identifier.

# 1.4. Outlet_Size vs Item_Outlet_Sales
## let's check the distibution of outlet_size distinct values vs sales
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

#The distribution of ‘Small’ Outlet_Size is almost identical to the distribution of the blank category (first vioin) of Outlet_Size. 
#So, we can substitute the blanks in Outlet_Size with ‘Small’ (just a proposal) ;)

# 1.5. Outlet_Location_Type, Item_Outlet_Sales
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")

# 1.6. Outlet_Type, Item_Outlet_Sales
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")
plot_grid(p15, p16, ncol = 1)

#Tier 1 and Tier 3 locations of Outlet_Location_Type look similar.
#In the Outlet_Type plot, Grocery Store has most of its data points around 
#the lower sales values as compared to the other categories. however, supermarket_3 has the majority of the sales items

## ------------------------------------------------------------------
## >> Boxplots
p_1 = ggplot(train) + geom_boxplot(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")
p_1

plot_grid(p16, p_1, ncol = 2)
#range of values of Grocery is small although its existance in the data is the most
## outlet_size

p_2 = ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")
p_3 = ggplot(train) + geom_boxplot(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

plot_grid(p_2, p_3, ncol = 2)

#small and NAs have almost the same distribution and the 3 other categories have almost the same dist.

## ------------------------------------------------------------------
## >> Missing Values Imputations:

## 1. Item_Weight
sum(is.na(combi$Item_Weight))  #sum of missing values

#  We’ll now impute Item_Weight with mean weight based on the Item_Identifier variable.
missing_index = which(is.na(combi$Item_Weight))

for (i in missing_index){
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
}
#we can test by re_running the sum() func. above again .. expected to output 0

## --------------------------------------------------------------------------------------
## 2. Item_Visibility
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

## we gonna replace zero value with wise mean of item_identifier
zero_index = which(combi$Item_Visibility == 0)

for (i in zero_index){
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
}
# :D no illogical count at zero!
## -----------------------------------------------------------------------

## FEATURE ENGINEERNG

## the following features gonna be added to serve the prediction task:
#Item_Type_new: Broader categories for the variable Item_Type.
#Item_category: Categorical variable derived from Item_Identifier.
#Outlet_Years: Years of operation for outlets.
#price_per_unit_wt: Item_MRP/Item_Weight
#Item_MRP_clusters: Binned feature for Item_MRP.

## 1. Item_Type_new: divide items to perishable (kabel ll talaf) and non_perishable
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

## Item_Type_new:
combi$Item_Type_new <- ifelse(combi$Item_Type %in% perishable, "perishable", ifelse(combi$Item_Type %in% non_perishable, "non_perishable", "not_sure"))

## 2. Item Category

#Let’s compare Item_Type with the first 2 characters of Item_Identifier, i.e., ‘DR’, ‘FD’, and ‘NC’. 
#These identifiers most probably stand for drinks, food, and non-consumable.
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))  #substr from ID two charcters starting from 1st position

#Based on the above table we can create a new feature. Let’s call it Item_category
combi$Item_category <- substr(combi$Item_Identifier, 1, 2)
#combi <- subset(combi, select = -c(tem_category))
combi$Item_category

##-----------------------------------------------------------------------------------
#3. Item_Fat_Content 
#We will also change the values of Item_Fat_Content wherever Item_category is ‘NC’ 
#because non-consumable items cannot have any fat content.
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"

## ----------------------------------------------------------------------------------
# 4. Outlet_Years (years of operation) 
# years = 2013 (when BigMart have collected the sales data) - establishment_year
combi$Outlet_Years <- 2013-combi$Outlet_Establishment_Year

combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

## -----------------------------------------------------------------------------
# 5. Price per unit weight
combi$price_per_unit_wt <- combi$Item_MRP/combi$Item_Weight

##-----------------------------------------------------------------------------
# 6. Item_MRP_clusters

#Earlier in the Item_MRP vs Item_Outlet_Sales plot, we saw Item_MRP was spread across in 4 chunks. 
#Now let’s assign a label to each of these chunks and use this label as a new variable.

combi$Item_MRP_clusters <- ifelse(combi$Item_MRP < 69, "1st", ifelse(combi$Item_MRP >= 69 & combi$Item_MRP < 136, "2nd", ifelse(combi$Item_MRP >= 136 & combi$Item_MRP < 203, "3rd", "4th")))
combi$Item_MRP_clusters

## DONE :) 

## ---------------------------------------------------------------------
# Encoding Categorical Vars (Label Encoding, One Hot Encoding)
# 1. Label Encoding:
# 1.1. Outlet_Size
combi$Outlet_Size_num <- ifelse(combi$Outlet_Size == "Small", 0, ifelse(combi$Outlet_Size == "Medium", 1, 2))

# 1.2. Outlet_Location_Type
combi$Outlet_Location_Type_num <- ifelse(combi$Outlet_Location_Type == "Tier 3", 3, ifelse(combi$Outlet_Location_Type == "Tier 2", 2, 1))

# removing categorical variables after label encoding
combi <- subset(combi, select = -c(Outlet_Size, Outlet_Location_Type))
str(combi)  #removed Succesfully :) 


## ---------------------------------------------------------------------
# 2.1. One hot encoding:
ohe = dummyVars("~.", data = combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)

ohe_df = data.table(predict(ohe, combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)        

## ---------------------------------------------------------------------
## Data Preprocessing

#pre-processing refers to the transformations applied to your data before feeding it to the algorithm. 
#It invloves further cleaning of data, data transformation, data scaling and many more things.
#Here we gonna deal with (skewness and scale the numerical variables)

## 1. Remove Skewness
# A skewed variable can be transformed by taking its log, square root, or cube root 
#so as to make its distribution as close to normal 
combi$Item_Visibility <- log(combi$Item_Visibility+1)  # log+1 to avoid division by zero
combi$price_per_unit_wt <- log(combi$price_per_unit_wt+1)  # log+1 to avoid division by zero

##-------------------------------------------------------------------
#Scaling numerical vars:
#Let’s scale and center the numeric variables to make them have a mean of zero, standard deviation of one and scale of 0 to 1. 
#Scaling and centering is required for linear regression models.
num_vars = which(sapply(combi, is.numeric))
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method = c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

## -----------------------------------------------------------------------

# removing numeric independent variables
combi[, setdiff(num_vars_names, "Item_Outlet_Sales") := NULL]
combi = cbind(combi, combi_numeric_norm)

## -----------------------------------------------------------------------
# Splitting the combined data combi back to train and test set .. 
train = combi[1:nrow(train)]
test = combi[(nrow(train)+1):nrow(combi)]
test[, Item_Outlet_Sales := NULL] # remving Item_Outlet_sales as it contains NAs for test data

## -----------------------------------------------------------------------
## Correlation :D
#It is not desirable to have correlated features if we are using linear regressions.
cor_train = cor(train[, -c("Item_Identifier")]) #corr all vars except the ID
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)

#Comments:
#The correlation plot above shows correlation between all the possible pairs of variables in out data. 
#The correlation between any two variables is represented by a pie. A blueish pie indicates positive correlation and reddish pie indicates negative correlation. The magnitude of the correlation is denoted by the area covered by the pie.
#Variables price_per_unit_wt and Item_Weight are highly correlated as the former one was created from the latter. Similarly price_per_unit_wt and Item_MRP are highly correlated for the same reason.
