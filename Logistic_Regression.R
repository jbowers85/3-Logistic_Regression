
# LOGISTIC REGRESSION #



## 1.) SIMPLE LOGISTIC REGRESSION - CLASSIFICATION - BINARY DEPENDENT VARIABLE ####

### read in data
data <- iris

### profile data
table(data$Species)
str(data)

### we will try to predict species, versicolor vs. virginica, since these 2 are the most similar
par(mfrow=c(2,2))
boxplot(data$Sepal.Width ~ data$Species, main="Sepal.Width", col = 'light blue')
boxplot(data$Sepal.Length ~ data$Species, main="Sepal.Length", col = 'light blue')
boxplot(data$Petal.Width ~ data$Species, main="Petal.Width", col = 'light blue')
boxplot(data$Petal.Length ~ data$Species, main="Petal.Length", col = 'light blue')
par(mfrow=c(1,1))

### remove setosa from dataset
data <- data[data$Species != "setosa",]
table(data$Species)
data$Species <- factor(data$Species)
table(data$Species)

### remove all columns besides 1 predictor and 1 class label
data <- data[,c(4,5)]
head(data)

### randomly split into training and test set (we need a holdout set of data to see how our model performs in "real life" even though 
## we use k-fold cross validation to test our model. In real life you want to build the model on all historical data to predict new records.)
trainIndex <- sample(nrow(data), floor(.7*nrow(data)), replace = F)
trainIndex

trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

### verify split
nrow(trainData)
table(trainData$Species)

nrow(testData)
table(testData$Species)


### visualize petal width by species
library(ggplot2)

ggplot(data, aes(x=Petal.Width, fill=Species)) +  
  geom_bar() + 
  theme_bw() + 
  labs(y="Count", x="Petal Width", title="Petal Width by Species")

### build multiple logistic regression using k-fold cross validation
library(caret)

trainControl <- trainControl(method="cv", number=10) # 10-fold cross validation
m <- train(Species ~ ., data=trainData, trControl=trainControl, method="glm")

print(m)
summary(m)
# names(getModelInfo()) -- view available algorithms

### apply model to test set for predictions
predicted_probs <- predict(m,testData, type="prob")
predictions <- data.frame(testData, Prob=predicted_probs[,2])
predictions

predictions <- predictions[order(predictions$Petal.Width),] # order by probability

### translate prediction probabilities into class predictions
library(dplyr)

predictions <- mutate(predictions, predClass = ifelse(Prob >= .5,"virginica","versicolor")) # adjust .5 to a different cutoff if you see fit
predictions

### generate confusion matrix to assess model
cm <- confusionMatrix(predictions$predClass, predictions$Species)
cm

### visualize confusion matrix
ggplot(as.data.frame(as.table(cm)), aes(y=Freq, x=Reference)) + geom_bar(stat="identity", aes(fill=Prediction)) + theme_bw() + labs(x="Actual", title="Confusion Matrix: Actual vs Predicted")

### plot the logistic regression
predictions$predClassBin[predictions$predClass == 'versicolor'] <- 0 
predictions$predClassBin[predictions$predClass == 'virginica'] <- 1

ggplot(predictions, aes(x=Petal.Width, y=Prob)) + 
  geom_point(aes(color=Species)) + 
  geom_smooth(color="skyblue", method="glm", method.args=list(family="binomial"), se=FALSE) +
  theme_bw() +
  labs(x="Petal Width", y="Virginiica Probability", title="Prediction using Logistic Regression")


### plot the ROC curve
library(plotROC) # an extension for ggplot allowing you to use geom_roc()
rocPlot <-  ggplot(predictions, aes(d = Species, m = Petal.Width)) + geom_roc() + theme_bw() + labs(title="ROC Curve") #m = markers d
rocPlot

### calculate area under curve
library(pROC)
auc(Species ~ predictions$Prob, data=predictions)


### Generate test predictions for new, unseen data.

Petal.Width <- c(1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0)
newData <- data.frame(Petal.Width)

predicted_probs <- predict(m,newData, type="prob")
predictions <- data.frame(newData, Prob=predicted_probs[,2])
predictions <- mutate(predictions, predClass = ifelse(Prob >= .5,"virginica","versicolor")) # adjust .5 to a different cutoff if you see fit
predictions





## 1.) MULTIPLE LOGISTIC REGRESSION - CLASSIFICATION - BINARY DEPENDENT VARIABLE ####

### read in data
data <- iris

### profile data
table(data$Species)
str(data)

### we will try to predict species, versicolor vs. virginica, since these 2 are the most similar
par(mfrow=c(2,2))
boxplot(data$Sepal.Width ~ data$Species, main="Sepal.Width", col = 'light blue')
boxplot(data$Sepal.Length ~ data$Species, main="Sepal.Length", col = 'light blue')
boxplot(data$Petal.Width ~ data$Species, main="Petal.Width", col = 'light blue')
boxplot(data$Petal.Length ~ data$Species, main="Petal.Length", col = 'light blue')
par(mfrow=c(1,1))

### remove setosa from dataset
data <- data[data$Species != "setosa",]
table(data$Species)
data$Species <- factor(data$Species)
table(data$Species)

### randomly split into training and test set (we need a holdout set of data to see how our model performs in "real life" even though 
## we use k-fold cross validation to test our model. In real life you want to build the model on all historical data to predict new records.)
trainIndex <- sample(nrow(data), floor(.7*nrow(data)), replace = F)
trainIndex

trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

### verify split
nrow(trainData)
table(trainData$Species)

nrow(testData)
table(testData$Species)

### visualize attributes by species
library(ggplot2)

ggplot(data, aes(x=Petal.Width, fill=Species)) +  
  geom_bar() + 
  theme_bw() + 
  labs(y="Count", x="Petal Width", title="Petal Width by Species")

ggplot(data, aes(x=Petal.Length, fill=Species)) +  
  geom_bar() + 
  theme_bw() + 
  labs(y="Count", x="Petal Length", title="Petal Length by Species")

ggplot(data, aes(x=Sepal.Width, fill=Species)) +  
  geom_bar() + 
  theme_bw() + 
  labs(y="Count", x="Sepal Width", title="Sepal Width by Species")

ggplot(data, aes(x=Sepal.Length, fill=Species)) +  
  geom_bar() + 
  theme_bw() + 
  labs(y="Count", x="Sepal Length", title="Sepal Length by Species")


### build simple logistic regression using k-fold cross validation
library(caret)

trainControl <- trainControl(method="cv", number=10) # 10-fold cross validation
m <- train(Species ~ ., data=trainData, trControl=trainControl, method="glm")

print(m)
summary(m)

### apply model to test set for predictions
predicted_probs <- predict(m,testData, type="prob")
predictions <- data.frame(testData, Prob=predicted_probs[,2])
predictions

options(scipen = 999)

predictions <- predictions[order(predictions$Petal.Width),] # order by probability

### translate prediction probabilities into class predictions
library(dplyr)

predictions <- mutate(predictions, predClass = ifelse(Prob >= .5,"virginica","versicolor")) # adjust .5 to a different cutoff if you see fit
predictions

### generate confusion matrix to assess model
cm <- confusionMatrix(predictions$predClass, predictions$Species)
cm

### visualize confusion matrix
ggplot(as.data.frame(as.table(cm)), aes(y=Freq, x=Reference)) + geom_bar(stat="identity", aes(fill=Prediction)) + theme_bw() + labs(x="Actual", title="Confusion Matrix: Actual vs Predicted")

### plot the logistic regression
predictions$predClassBin[predictions$predClass == 'versicolor'] <- 0 
predictions$predClassBin[predictions$predClass == 'virginica'] <- 1

ggplot(predictions, aes(y=predClassBin, color=Species)) + 
  geom_point(aes(x=Petal.Width)) + 
  geom_smooth(aes(x=Petal.Width), color="skyblue", method="glm", method.args=list(family="binomial"), se=FALSE) +
  geom_point(aes(x=Petal.Length)) + 
  geom_smooth(aes(x=Petal.Length), color="olivedrab3", method="glm", method.args=list(family="binomial"), se=FALSE) +
  geom_point(aes(x=Sepal.Width)) + 
  geom_smooth(aes(x=Sepal.Width), color="maroon2", method="glm", method.args=list(family="binomial"), se=FALSE) +
  geom_point(aes(x=Sepal.Length)) + 
  geom_smooth(aes(x=Sepal.Length), color="mediumorchid1", method="glm", method.args=list(family="binomial"), se=FALSE) +
  theme_bw() +
  labs(x="Petal/Sepal   Length/Width", y="Virginiica Probability", title="Prediction using Logistic Regression")

### plot the ROC curve
library(plotROC) # an extension for ggplot allowing you to use geom_roc()
ggplot(predictions) + geom_roc(aes(d = Species, m = Prob)) + theme_bw() + labs(title="ROC Curve") #m = markers d

### calculate area under curve
library(pROC)
auc(Species ~ predictions$Prob, data=predictions)


### Generate test predictions for new, unseen data.
Petal.Width <- rnorm(n=20, mean=1.7,sd=.25)
Petal.Length <- rnorm(n=20, mean=5, sd=1 )
Sepal.Width <- rnorm(n=20, mean=3.1, sd=.5 )
Sepal.Length <- rnorm(n=20, mean=6.5, sd=1.25 )

newData <- data.frame(Petal.Width, Petal.Length, Sepal.Width, Sepal.Length)

predicted_probs <- predict(m,newData, type="prob")
predictions <- data.frame(newData, Prob=predicted_probs[,2])
predictions <- mutate(predictions, predClass = ifelse(Prob >= .5,"virginica","versicolor")) # adjust .5 to a different cutoff if you see fit
predictions



