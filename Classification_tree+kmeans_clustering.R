library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)

#Task 1: Classification Tree
movies <- read.csv("Movies.csv")

set.seed(280)
movies <- sample_n(movies, 3238)
movies <- movies[,-1]
movies_train <- slice(movies, 1:1943)
movies_valid <- slice(movies, 1944:3238)

movies_model <- rpart(Awards~.,
                      data = movies_train,
                      method = "class")
rpart.plot(movies_model)

cv <- rpart(Awards~.,
            data = movies_train,
            method = "class",
            cp = 0.00001,
            xval = 5)
printcp(cv)

movies_model_new <-rpart(Awards~.,
                         data = movies_train,
                         method = "class",
                         cp = 0.00508259)
rpart.plot(movies_model)

pred <- predict(movies_model_new, movies_valid, type = "class")
pred2 <- predict(movies_model_new, movies_train, type = "class")
confusionMatrix(pred, movies_valid$Awards)
confusionMatrix(pred2, movies_train$Awards)

#Task 2: Clustering
airlines <- read.csv("eastwestairlines.csv")

row.names(airlines) <- airlines[,1]
airlines <- airlines[,-1]

airlines.norm <- sapply(airlines, scale)
row.names(airlines.norm) <- row.names(airlines)

elbow <- sapply(1:15,
                function(k){kmeans(airlines.norm, k, nstart = 50, iter.max = 15)
                  $tot.withinss})
plot(1:15, elbow,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
#7

km <- kmeans(airlines.norm, 7)
km$centers
