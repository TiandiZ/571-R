install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tennis <- read.csv("tennis.csv")
str(tennis)
summary(tennis)
model <- rpart(play~., data = tennis,
               method = "class",
               minsplit = 2, 
               minbucket = 1)
model2 <- rpart(play~., data = tennis,
               method = "class",
               minsplit = 5)
rpart.plot(model)
rpart.plot(model2)
?rpart.plot
?rpart
#pre-asmt4
library(caret)
library(lattice)
library(ggplot2)
library(dplyr)
iris <- sample_n(iris, 150)
train <- slice(iris, 1:90)
valid <- slice(iris, 91:150)
iris_model <- rpart(Species~., 
                    data = train, 
                    method = "class", 
                    minsplit = 2, 
                    minbucket = 1)
rpart.plot(iris_model)
pred <- predict(iris_model, valid, type = "class")
confusionMatrix(pred, valid$Species)
#accuracy rate:93.33%
iris_model_2 <- rpart(Species~., 
                     data = train, 
                     method = "class", 
                     minsplit = 2, 
                     minbucket = 1,
                     maxdepth = 1)
pred2 <- predict(iris_model_2, valid, type = "class")
confusionMatrix(pred2, valid$Species)
#acuuracy rate:63.33%
#reason: maxdepth limited the depth of the tree, therefore reduced the acurracy with less split