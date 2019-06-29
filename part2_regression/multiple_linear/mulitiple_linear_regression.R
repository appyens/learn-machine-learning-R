# multiple linear regression
dataset = read.csv("data/regression/multiple _linear_regression/50_Startups.csv")

# encoding categorical data
dataset$State = factor(dataset$State, 
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

# splitting dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# no need to apply feature scaling
# fit multiple linear regression to the training set
# here we found R.D.Spend variable is most statistically significant by having the lowest p value
# here profit expressed as linear combination of all the independent variable
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = training_set)

print(summary(regressor))

# predicting test set results
y_pred = predict(regressor, newdata = test_set)

# building the optimal model using backward elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)

# It is found that state variable is not statisticaly  significancant as they have highest p value and they are not going to impact the
# the dependent variable so we need to remove it
summary(regressor)


regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)

# It is found that Administration variable is not statisticaly  significancant as they have highest p value and it is not going to impact the
# the dependent variable so we need to remove it
summary(regressor)


regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)

# It is found that Marketing spend variable is not statisticaly  significancant as they have highest p value and it is not going to impact the
# the dependent variable so we need to remove it
summary(regressor)


regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)

# It is found that Marketing spend variable is not statisticaly  significancant as they have highest p value and it is not going to impact the
# the dependent variable so we need to remove it
summary(regressor)
