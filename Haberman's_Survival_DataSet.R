# Logistic Regression

# Importing the dataset
data = read.csv('haberman.data.csv', col.names = c('Age','year of operation','positive axillary','Survival status') )

# Encoding the target feature as factor
data$Survival.status = factor(data$Survival.status, levels = c(1,2))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
seed = 123
split = sample.split(data$Survival.status, SplitRatio = 0.75)
training_set = subset(data, split == TRUE)
testing_set = subset(data, split == FALSE)

# Feature Scaling
training_set[1:3] = scale(training_set[1:3])
testing_set[1:3] = scale(testing_set[1:3])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Survival.status ~.,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = testing_set[1:3])
y_prediction = ifelse(prob_pred > 0.5, 2, 1)

# Making the Confusion Matrix
cm = table(testing_set[,4], y_prediction)
print(cm)
#   1   2
# 1 55  1
# 2 17  3

