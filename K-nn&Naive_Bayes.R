install.packages("ISLR")
install.packages("MASS")
install.packages("caret", dependencies = TRUE)
install.packages("FNN")
library(MASS)
library(ISLR)
library(dplyr)
library(caret)
library(FNN)
library(ggplot2)

#KNN
fish <- read.csv("fishmarket.csv")

set.seed(280)
fish2 <- sample_n(fish, 159)
train <- slice(fish2, 1:95)
valid <- slice(fish2, 96:159)

Nemo <- data.frame(Weight=runif(1, 0, 1650), 
                   Length1=runif(1, 7.5, 59), 
                   Length2=runif(1, 8.4, 63.4), 
                   Length3=runif(1, 8.8, 68), 
                   Height=runif(1, 1.972, 18.6354), 
                   Width=runif(1, 1.16, 8.142))
print(Nemo)

train.norm <- train
valid.norm <- valid
norm.values <- preProcess(train[, 2:7])
train.norm[, 2:7] <- predict(norm.values, train[, 2:7])
valid.norm[, 2:7] <- predict(norm.values, valid[, 2:7])

nn <- knn(train = train.norm[, 2:7], test = Nemo, 
          cl = train.norm[, 1], k = 7)
row.names(train)[attr(nn, "nn.index")]
nn

accuracy <- data.frame(k=seq(1,14,1), accuracy=rep(0,14))
for(i in 1:14){
  knn.pred <- knn(train.norm[, 2:7],
                  valid.norm[, 2:7],
                  cl = train.norm[, 1],
                  k = i)
  accuracy[i, 2] <- confusionMatrix(knn.pred, valid.norm[, 1])$overall[1]
}
accuracy

ggplot(data=accuracy, aes(x=k, y=accuracy)) + 
  geom_point() + 
  ggtitle("The Accuracy of Different K Value") +
  theme(plot.title = element_text(hjust = 0.5))

nn2 <- knn(train = train.norm[, 2:7], 
          test = Nemo, 
          cl = train.norm[, 1], 
          k = 2)
row.names(train)[attr(nn2, "nn2.index")]
nn2

#Naive Bayes
police <- read.csv('denver_police.csv')
#2a
CallDisposition <- police %>%
  group_by(CALL_DISPOSITION) %>%
  summarise(count=n()) %>% 
  arrange(desc(count))
WantedCallDisposition <- c("T - Citation Issued", "Party Advised", "Arrest Made", "Warning Issued", "K - Street Check Completed")
police_2a <- filter(police, CALL_DISPOSITION %in% WantedCallDisposition)
police_2a$CALL_DISPOSITION <- droplevels(police_2a$CALL_DISPOSITION)
#2b
Neighborhood <- police_2a %>% 
  group_by(NEIGHBORHOOD_NAME) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
WantedNeighborhood <- c("Five Points", "Montbello", "West Colfax", "Stapleton", "East Colfax")
police_2b <- filter(police_2a, NEIGHBORHOOD_NAME %in% WantedNeighborhood)
police_2b$NEIGHBORHOOD_NAME <- droplevels(police_2b$NEIGHBORHOOD_NAME)
#2c
library(tidyr)
?separate
police_2c <- separate(police_2b, "TIME_PHONEPICKUP", c("Date", "Time"), sep = " ")
police_2c <- na.omit(police_2c)
#2d
library(e1071)
police_2d <- police_2c
police_2d$Time <- as.numeric(gsub("[[:punct:]]", "", police_2d$Time))
str(police_2d)
police_2d$Time <- floor(police_2d$Time/100)
police_2d$Time <- as.factor(police_2d$Time)
levels(police_2d$Time)
#2e
library(lubridate)
police_2e <- police_2d
police_2e$Date <- as.POSIXct(police_2e$Date, format="%m/%d/%Y")
police_2e$Month <- month(ymd(police_2e$Date))
police_2e$Month <- as.factor(police_2e$Month)
levels(police_2e$Month)
#3
set.seed(280)
police_3 <- sample_n(police_2e, 130438)
train_nb <- slice(police_3, 1:78262)
valid_nb <- slice(police_3, 78263:130438)

#4
nb.model <- naiveBayes(CALL_DISPOSITION ~ Time+Month+PROBLEM+NEIGHBORHOOD_NAME, data = train_nb)
#5
nb_pred_train <- predict(nb.model, train_nb, type="class")
confusionMatrix(nb_pred_train, train_nb$CALL_DISPOSITION)
nb_pred_valid <- predict(nb.model, valid_nb, type="class")
confusionMatrix(nb_pred_valid, valid_nb$CALL_DISPOSITION)
#9
ggplot(police_3, aes(x=NEIGHBORHOOD_NAME, fill=CALL_DISPOSITION)) + geom_bar()

