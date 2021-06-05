#Decision Trees in R using rpart

#Load the packages

library(rpart)
library(caTools)
library(rattle)
library(caret)
library(rpart.plot)


#load the data
data("wine")

#inspect the data
summary(wine)
str(wine)


# Training and Test set ---------------------------------------------------

splitcc <- sample.split(wine, SplitRatio = 0.7)
train <- subset(wine, splitcc == "TRUE")
test <- subset(wine, splitcc == "FALSE")


# Testing Models Rpart ----------------------------------------------------

#We can use the rpart function to classify Type of wine using remaining features

model <- rpart(Type ~., data = train, method = "class")
model

#plot the model

plot(model, uniform = TRUE, main = "Type Tree")
text(model, use.n = TRUE, all = TRUE, cex = 0.8)

#plot using rattle library

fancyRpartPlot(model, main = "Type")

summary(model)

#Compare on the training data

pred <- predict(model, train[2:14], type = "class")

#Accuracy % 99.1 
#Our model has overfit
confusionMatrix(pred, train$Type)

#Compare on the test data

test_pred <- predict(model, test[2:14], type = "class")

#Accuracy decreases to % 90.4
confusionMatrix(test_pred, test$Type)

#Our model will likely do poorly if we had a bigger test set as
#a single decision tree won't have much predictive power



# Caret Package -----------------------------------------------------------

#We can also use the caret package to tackle this problem

#Cross Validation
ctrl  <- trainControl(method  = "cv",number  = 10) 

#Model method = rpart
cv_model <- train(Type ~ ., data = train, method = "rpart",
                trControl = ctrl,  tuneLength = 30) 


#predict on the test set

cv_pred <- predict(cv_model,test[2:14])

#Accuracy % 90.4

confusionMatrix(cv_pred, test$Type)


#Optimal cp (complexity parameter) value = 0.045
print(cv_model)
plot(cv_model)

#Identical to the earlier tree

par(mfrow = c(1,2))

fancyRpartPlot(model, main = "Type")
rpart.plot(cv_model$finalModel)




