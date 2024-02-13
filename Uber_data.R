#Uploading dataset
library(readxl)
uber <- read_excel("/Users/alecreynoso/Desktop/Data Science Projects/Uber Project/UBER.xls")

#Exploratory Data Analysis#
summary(uber)
table(uber$name)

#Create pairplot to evaluate dataset
library(GGally)
ggpairs(uber, columns = c('name', 'price'), aes(color = name), binwidth = 10)

table(uber$name)


##I want to check how many months are recorded using a visualization##
install.packages('ggplot2')
library(ggplot2)

ggplot(uber, aes(x = month)) +
   geom_bar(stat = "count", fill = "skyblue") +
   labs(title = "Frequency of Months")
#The plot shows there is only 2 months accounted for in this dataset. The two months are November and December. This is imporant to know 
#as I can make sure to not include month in the model since it does not account for al 12 months.


#Let's evaluate the column days
table(uber$day)


#I see the days are listed as the day on the month. I want the numbers to be switched to it's corresponding day of the week
#I am going to use Lubridate to do this.
install.packages("lubridate")
library(lubridate)

uber$date = as.Date(paste("2018-", uber$month, "-", uber$day, sep = ""), format = "%Y-%m-%d") 
uber$day_of_week = weekdays(uber$date)
print(uber)
table(uber$day_of_week)

#I want to see the relationship between day_of_week and price
uber$day_of_week = as.factor(uber$day_of_week)
uber$day_of_week = as.numeric(uber$day_of_week)
plot(uber$price, uber$day_of_week)
#Doesn't seem to have a linear relationship but we will keep it in the model to see it's significance



#The hour variable does not make logical sense to me to include it in the model. However, to make sure I want to see if there is a linear relationship between them
plot(uber$price, uber$hour)
#As I suspected there is no potential linear relationship based on the scatter plot

#Correlation among independent variables
corr = cor(uber)
corr

#Month and day are negatively correlated: -0.87
#Since there is a high correlation, I am deciding to remove month from the model



################################################################################

#Creating testing and training data set
index = sample(seq_len(nrow(uber)), size = 0.8 * nrow(uber))
uber_training = uber[index,]
uber_testing = uber[-index,]

#Model with all variables
model = lm(price ~., data = uber_training)
summary(model)

#MSE of model
predict_values = predict(model, uber_testing)
actual_values = uber$price

mse = mean((actual_values - predict_values)^2)
print(paste("Mean Squared Error of Model", mse))


#Model 1 (significant independent variables)
model1 = lm(price ~. - hour - day - month - source - destination - temperature - wind_speed - visibility, data = uber_training)
summary(model1)

#MSE of model1
predict_values1 = predict(model1, uber_testing)
mse1 = mean((actual_values - predict_values1)^2)
print(paste("Mean Squared Error of Model1:", mse1))

#I got a warning when using the predict function that mentions a 'rank-deficient fit' 
# This can mean that there is multi-collinearity within the model, let's check if any variables are highly correlated

numeric_data = uber[, sapply(uber, is.numeric)]
corr = cor(numeric_data, method = "pearson")
corr
#Month and day have a high negative correlation. Since month only accounts for half of November and December I will remove month from the model
#Also I could not evaluate the categorical variables as they are numeric and changing them to numeric would affect the outcome of the model
#Based on the two categorical variables I can infer that they are highly correlated since they both list rideshare names, please evaluate the two variables for better understanding

model2 = lm(price~ + distance + cab_type, uber_training)
summary(model2)

predict_values2 = predict(model2, uber_testing)
mse2 = mean((predict_values2 - actual_values)^2)
print(paste("Mean Squared Error of Model2", mse2))

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


#Model with important variables
rf_model0 = randomForest(price~ + name + distance, uber_training, ntree = 30)
rf_predictions = predict(rf_model, uber_testing)
rf_mse0 = mean((actual_values - rf_predictions)^2)
print(paste("MSE for Random Forest with important variables", rf_mse0))











  