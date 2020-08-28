# Importing the dataset
dataset <- read.csv('Startups.csv')

head(dataset)

# Check to see if there are missing data?
sum(is.na(dataset))


# Encoding categorical data
dataset$City <- factor(dataset$City,
                       levels = c('Bangalore', 'Hyderabad', 'Mumbai'),
                       labels = c(1, 2, 3))

#checking the dataset after encoding
head(dataset)

# loading required packages
library(caTools)
library(caret)

set.seed(123)

# Splitting the dataset into the Training set and Test set
training_index <- createDataPartition(dataset$Profit, p=0.8, list = FALSE) 

training_set <- dataset[training_index,] # Training Set
test_set <- dataset[-training_index,] # Test Set


# Fitting Multiple Linear Regression to the Training set
regressor <- lm(formula = Profit ~ .,
               data = training_set)

plot(training_set$Profit, predict(regressor, training_set),
     pch=16,
     col = "blue",
     xlab = 'Target',
     ylab = 'Prediction',
     )


# Predicting the Test set results
y_pred <- predict(regressor, newdata = test_set)
y_pred

plot(test_set$Profit, y_pred,
     pch=16,
     col = "red",
     xlab = 'Target',
     ylab = 'Prediction',
     font.lab = 2
)

# Summary of the regression model created.     
summary(regressor)    
