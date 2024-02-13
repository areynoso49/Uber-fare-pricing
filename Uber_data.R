#Uploading dataset
library(readxl)
uber <- read_excel("/Users/alecreynoso/Desktop/Data Science Projects/Uber Project/UBER.xls")

#Exploratory Data Analysis
summary(uber)
table(uber$name)

#Create a pair plot to evaluate the relationship between name and price (name = what type of ride, e.i. Lyft XL, Uberpool)
library(GGally)
ggpairs(uber, columns = c('name', 'price'), aes(color = name), binwidth = 10)


##I want to check how many months are recorded using a visualization##
install.packages('ggplot2')
library(ggplot2)

ggplot(uber, aes(x = month)) +
   geom_bar(stat = "count", fill = "skyblue") +
   labs(title = "Frequency of Months")

#The plot shows there is only 2 months accounted for in this data set. The two months are November and December. Since there is only
#two month I will not be adding month to the regression models

#Let's evaluate the column 'days'
table(uber$day)


#I see the days are listed as the day of the month. I want the numbers to be switched to it's corresponding day of the week
#I am going to use Lubridate to do this.
install.packages("lubridate")
library(lubridate)

uber$date = as.Date(paste("2018-", uber$month, "-", uber$day, sep = ""), format = "%Y-%m-%d") 
uber$day_of_week = weekdays(uber$date)
table(uber$day_of_week)

#I want to see the relationship between day_of_week and price
model_anova = aov(price ~ day_of_week, uber)
summary(model_anova)
#The p-value is 0.79 indicating that the variance of price does attribute to the variance of the day of the week

#Correlation among independent variables
corr = cor(uber)
corr

#Month and day are negatively correlated: -0.87
#I am already planning on removing month from the models


################################################################################

#Creating testing and training data set
index = sample(seq_len(nrow(uber)), size = 0.8 * nrow(uber))
uber_training = uber[index,]
uber_testing = uber[-index,]

#Linear regression Model with all variables
model = lm(price ~. - month, data = uber_training)
summary(model)

#MSE of model
predict_values = predict(model, uber_testing)
actual_values = uber$price

mse = mean((actual_values - predict_values)^2)
print(paste("Mean Squared Error of Model", mse))
#The MSE is 164.63

#Linear regression Model with significant independent variables
model1 = lm(price ~. - hour - day - month - source - destination - temperature - wind_speed - visibility, data = uber_training)
summary(model1)

#MSE of model1
predict_values1 = predict(model1, uber_testing)
mse1 = mean((actual_values - predict_values1)^2)
print(paste("Mean Squared Error of Model1:", mse1))
#The MSE is 164.44

################################################################################

#I want to see if a regression tree model will perform better with the data set
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

tree_model = rpart(price ~., data = uber, method = "anova")
summary(tree_model)
rpart.plot(tree_model)

#MSE of regression tree model
prediction = predict(tree_model, uber_testing)

mse_tree = mean((actual_values - prediction)^2)
print(paste("Mean Squared Error for regression tree", mse_tree))
#The MSE is 160.69

################################################################################

#Based on the plot matrix let's see if a random forest model performs better
install.packages('randomForest')
library(randomForest)
rf_model = randomForest(price~., uber_training, ntree = 50)
print(rf_model$importance)
varImpPlot(rf_model)
rf_pred = predict(rf_model, uber_testing)
rf_mse = mean((actual_values - rf_pred)^2)
print(paste("MSE for Random Forest all variables", rf_mse))
#The MSE is 165.04

#Model with important variables
rf_model0 = randomForest(price~ + name + distance, uber_training, ntree = 30)
rf_predictions = predict(rf_model, uber_testing)
rf_mse0 = mean((actual_values - rf_predictions)^2)
print(paste("MSE for Random Forest with important variables", rf_mse0))
#The MSE is 165.04

#Out of all the 3 models the regression tree model was the best fit producing a MSE of 160.69









  