# simple linear regression
# import dataset
dataset = read.csv("data/simple_linear_regression/Salary_Data.csv")
# spliting the dataset into the training set and test set
# install.packages("caTools")
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
# fitting the simple linear regression to the training set
# help(lm)
regressor = lm(formula = Salary ~ YearsExperience, data = training_set)
# predicting the test set results
y_pred = predict(regressor, newdata = test_set)
# visualising the training set results
# install.packages("ggplot2")
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab("Years of experience") + 
  ylab('Salary')
# visualising the test set results
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab("Years of experience") + 
  ylab('Salary')