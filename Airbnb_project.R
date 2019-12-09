library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(readr)
library(MASS)
library(ISLR)
library(FNN)
library(ggplot2)
library(dummies)
library(e1071)
library(reshape) 
library(forecast)
library(GGally)

#Step I: Data Preparation & Exploration
#read csv and subset/filter
melbourne <- read.csv("melbourne.csv")
CBD<-subset(melbourne,neighborhood == "Central Business District")
CBDdata<-CBD[c(1,5,16,19,22,23,24,29,30,31,42,43,44,45,46,47,48,49,50,51,54,55,56,57,58,59,61:65,67,70:76,79,80:84)]

#Missing Values
CBD1<-na.omit(CBDdata)

#Summary Statistics
#1.structure of CBD1
str(CBD1)
#2.general overview of CBD1
summary(CBD1)
#3.general overview of prices
fivenum(CBD1$price)
#4.general overview of response time
table(CBD1$host_response_time)
2132/2633
#5.correlation between review scores
review <- dplyr::select(CBD1,c(33,34,35,36,37,38,39))
cor.review.df <- cor(review)

#Visualization
#1.distribution of price
ggplot(CBD1,aes(x=price,fill=as.factor(bedrooms)))+geom_histogram(binwidth = 40)+xlim(0,500)+xlab("Prices")+ylab("Frequency")+ggtitle("Overview of Prices")+theme(plot.title = element_text(hjust = 0.5))
#2.relationship between rating score and hosts
ggplot(CBD1,aes(x=review_scores_rating,fill=host_is_superhost))+geom_histogram()+xlim(75,100)+xlab("Review Rating Scores")+ylab("Frequency")+ggtitle("Relationship of Review Rating Scores and Superhosts")+theme(plot.title = element_text(hjust = 0.5))
#3.relationship between beds and prices
ggplot(CBD1)+geom_boxplot(aes(x=as.factor(beds),y=price))+xlab("Number of Beds")+ylab("Prices")+ggtitle("Relationship Between Number of Beds and Prices")+theme(plot.title = element_text(hjust = 0.5))
#4.correlation between review scores

cor.rev <- round(cor.review.df,2) 
melted.cor.rev <- melt(cor.rev)
ggplot(melted.cor.rev, aes(x = X1, y = X2, fill = value))+geom_tile()+geom_text(aes(x = X1, y = X2, label = value))+theme(axis.text.x=element_text(angle=45,hjust=1))+ggtitle("Relationship Between Review Scores")+xlab("Review Scores Criteria")+ylab("Review Scores Criteria")+theme(plot.title = element_text(hjust = 0.5))
#5.relationship between prices and accommodation
ggplot(CBD1,aes(x=price,y=accommodates,color=room_type))+geom_point()+xlim(0,1000)+ggtitle("Relationship Between Prices and Accommodations")+xlab("Prices")+ylab("Accommodation Ability")+theme(plot.title = element_text(hjust = 0.5))

#Step II: Prediction
ggplot(data=CBD1, aes(x = price)) +
  geom_histogram(col = 'black',  binwidth = 50)+
  coord_cartesian(ylim = c(0,10))

CBD2_h<-filter(CBD1,
         price <= 600,
         price > 0)
dim(CBD2_h)

ggcorr(CBD2_h ,name = "corr", label = TRUE, hjust = 1, label_size = 2.5, 
       angle = -45, size = 3)

CBD2_2_h <- CBD2_h[c(7,12:18,20:23,32,33)]
set.seed(699)
train.sample_h <- sample_n(CBD2_2_h, dim(CBD2_h)[1]) 
train.df_h <- slice(train.sample_h, 1:(0.6*dim(CBD2_h)[1]))
valid.df_h <- slice(train.sample_h, (dim(CBD2_h)[1]*0.6):dim(CBD2_h)[1])

str(CBD2_2_h)
summary(CBD2_2_h$room_type)
price.lm_h <- lm(price~.,data=train.df_h)
price.lm.step_h <- step(price.lm_h, direction = "backward")
summary(price.lm.step_h) 


price.lm_h <- lm(formula = price ~ host_is_superhost + room_type + bathrooms + 
                   bedrooms + security_deposit + cleaning_fee+ number_of_reviews + 
                   guests_included + number_of_reviews, 
                 data = train.df_h)
summary(price.lm_h)

pred_price_h<-predict(price.lm_h,train.df_h)
accuracy(pred_price_h,train.df_h$price)
pred_price2_h<-predict(price.lm_h,valid.df_h)
accuracy(pred_price2_h,valid.df_h$price)

#Step III: Classification
#Part 1: knn
set.seed(699)
CBD1_T <- sample_n(CBD1, 2633)
CBD1_knn_T <- dplyr::select(CBD1_T, 
                            cancellation_policy,
                            host_is_superhost,
                            bedrooms, 
                            price,
                            security_deposit,
                            minimum_nights,
                            review_scores_rating,
                            require_guest_profile_picture)


CBD1_knn_T$host_is_superhost <- droplevels(CBD1_knn_T$host_is_superhost, "")
str(CBD1_knn_T)
levels(CBD1_knn_T$host_is_superhost) <- c(FALSE, TRUE)
CBD1_knn_T$host_is_superhost <- as.numeric(CBD1_knn_T$host_is_superhost)

levels(CBD1_knn_T$require_guest_profile_picture) <- c(FALSE, TRUE)
CBD1_knn_T$require_guest_profile_picture <- as.numeric(CBD1_knn_T$require_guest_profile_picture)

CBD1_knn_train_T <- slice(CBD1_knn_T, 1:1580)
CBD1_knn_valid_T <- slice(CBD1_knn_T, 1581:2633)
CBD1_knn_train_T.norm <- CBD1_knn_train_T
CBD1_knn_valid_T.norm <- CBD1_knn_valid_T
norm.values <- preProcess(CBD1_knn_T[, 2:8])
CBD1_knn_train_T.norm[, 2:8] <- predict(norm.values, CBD1_knn_train_T[, 2:8])
CBD1_knn_valid_T.norm[, 2:8] <- predict(norm.values, CBD1_knn_valid_T[, 2:8])

test_room_T <- data.frame(host_is_superhost=2,bedrooms = 3, price = 600, security_deposit = 300, minimum_nights = 1, review_scores_rating = 98,require_guest_profile_picture=2)

accuracy <- data.frame(k=seq(1,100,1), accuracy=rep(0,100))
for(i in 1:100){
  knn.pred <- knn(CBD1_knn_train_T.norm[, 2:8],
                  CBD1_knn_valid_T.norm[, 2:8],
                  cl = CBD1_knn_train_T.norm[, 1],
                  k = i)
  accuracy[i, 2] <- confusionMatrix(knn.pred, CBD1_knn_valid_T.norm[, 1])$overall[1]
}
accuracy

knn_model_2_T <- knn(train = CBD1_knn_train_T.norm[, 2:8], test = test_room_T, 
                     cl = CBD1_knn_train_T.norm[, 1], k = 26)
knn_model_2_T

#Part 2: Naive Bayes
CBDv<-CBD1[c(6,7,12,15,16,17,20,21,22,33,40)]
View(CBDv)
str(CBDv)
CBDv$host_is_superhost<-as.character(CBDv$host_is_superhost)
CBDv$host_response_rate<-as.factor(CBDv$host_response_rate)
CBDv$property_type<-as.character(CBDv$property_type)
CBDv$bedrooms<-as.numeric(CBDv$bedrooms)
CBDv$beds<-as.numeric(CBDv$beds)
CBDv$price<-as.numeric(CBDv$price)
CBDv$security_deposit<-as.numeric(CBDv$security_deposit)
CBDv$cleaning_fee<-as.numeric(CBDv$cleaning_fee)
CBDv$review_scores_rating<-as.numeric(CBDv$review_scores_rating)

set.seed(699)
CBDv <- sample_n(CBDv, 2633)
trainv <- slice(CBDv, 1:1580)
validv <- slice(CBDv, 1581:2633)

modelv<- naiveBayes(instant_bookable~ ., data = trainv)
modelv.pred <- predict(modelv, newdata = trainv)
confusionMatrix(modelv.pred, trainv$instant_bookable)
modelv.pred<- predict(modelv,newdata = validv)
confusionMatrix(modelv.pred, validv$instant_bookable)


vRoom<-data.frame(host_is_superhost="t",host_response_rate="90%", property_type="Apartment", bathrooms=2,bedrooms=2, beds = 2,price= 200,security_deposit=100, cleaning_fee=50,review_scores_rating=80,instant_bookable="unknown")
vRoom.pred <- predict(modelv,vRoom)
vRoom.pred

#Part 3: Classification Tree
fivenum(CBD1$cleaning_fee) 

CBD1$cleaning_fee_T <- cut(CBD1$cleaning_fee, 
                           breaks = c(-Inf, 0.1, 50, 69, 85, Inf), 
                           labels = c("No Fee", "0-50", "50-69", "70-85",">85"), 
                           right = FALSE)
CBD1$cleaning_fee_T <- as.factor(CBD1$cleaning_fee_T)

set.seed(699)
CBD1_T <- sample_n(CBD1, 2633)
cv <- rpart(cleaning_fee_T~host_response_time
            +host_is_superhost
            +host_identity_verified
            +property_type
            +room_type
            +bathrooms
            +bedrooms
            +price
            +security_deposit
            +accommodates
            +minimum_nights,
            data = CBD1_T,
            method = "class",
            cp = 0.00001,
            xval = 5)
printcp(cv)

Tree_model_T <- rpart(cleaning_fee_T~host_response_time
                      +host_is_superhost
                      +host_identity_verified
                      +property_type
                      +room_type
                      +bathrooms
                      +bedrooms
                      +price
                      +security_deposit
                      +accommodates
                      +minimum_nights,
                      cp = 0.00204918,
                      data = CBD1_T,
                      minsplit = 30, 
                      minbucket = 20,
                      maxdepth = 5)
rpart.plot(Tree_model_T)

#Step IV: Clustering
#Data Preparation
CBD2_Y <- CBD1[c(1,13:14,17,20:23,25,28,32:33,45)]

#Data Wrangling
row.names(CBD2_Y) <- CBD2_Y[,1]
CBD2_Y <- CBD2_Y[,-1] 
CBD2_Y$price_per_guest = CBD2_Y$price / CBD2_Y$guests_included
#Data Conversion
CBD3_Y <- dummy.data.frame(CBD2_Y, names = c("room_type"), sep = ".")

#Normalization
CBD4_Y <- sapply(CBD3_Y, scale)
row.names(CBD4_Y) <- row.names(CBD3_Y)

#Elbow Chart
k.max <- 15
wss <- sapply(1:k.max,
              function(k){kmeans(CBD4_Y, k, nstart=50, iter.max=15)$tot.withinss}) 
plot(c(1:k.max), wss,
     type ="b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters K",
     ylab = "Total Within-Cluster Sum of Squared",
     main = "Elbow Chart")


#Kmeans Clustering k = 4
km <- kmeans(CBD4_Y, 4, nstart=50)
km$cluster
km$centers
dist(km$centers)

#Add cluster as a column
CBD5_Y <- cbind(CBD4_Y, km$cluster)
CBD5_Y <- as.data.frame(CBD5_Y)
class(CBD5_Y)
colnames(CBD5_Y)[colnames(CBD5_Y) == 'V16'] <- 'cluster'

#plot
CBD5_Y$cluster <- as.character(CBD5_Y$cluster)

CBD5_Y$cluster[CBD5_Y$cluster == "1"]  <- "Business Homes"
CBD5_Y$cluster[CBD5_Y$cluster == "2"]  <- "Family Vacation Homes"
CBD5_Y$cluster[CBD5_Y$cluster == "3"]  <- "The Most Economical Homes"
CBD5_Y$cluster[CBD5_Y$cluster == "4"]  <- "Young People's Homes"


#########################
plot1_Y = ggplot(data = CBD5_Y, 
                 aes(x = CBD5_Y$number_of_reviews, 
                     y = CBD5_Y$price, 
                     color = cluster)) + geom_point(size = 3) + xlim(NA, 5) + ylim(NA, 5)
plot1_Y
#########################
plot2_Y = ggplot(data = CBD5_Y, 
                 aes(x = CBD5_Y$review_scores_rating, 
                     y = CBD5_Y$price_per_guest, 
                     color = cluster)) + geom_point(size = 3) + xlim(-4.5, NA) + ylim(NA, 6)
plot2_Y

