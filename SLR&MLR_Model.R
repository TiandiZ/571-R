install.packages("ISLR")
install.packages("MASS")
install.packages("visualize")
library(MASS)
library(ISLR)
library(ggplot2)
library(dplyr)
library(forecast)

data(package="ISLR")
data(College)
?College
selective <- College$Accept / College$Apps
College_removed <- College[-2]
College_removed <- College_removed[-2]
ggplot(data = College_removed, aes(x=Expend, y=perc.alumni))+geom_point()+geom_smooth(se=FALSE)
cor(College_removed$Expend, College_removed$perc.alumni, use="complete.obs")

set.seed(280)
new_College_removed <- sample_n(College_removed, 500)
train <- slice(new_College_removed, 1:300)
valid <- slice(new_College_removed, 301:500)

slr <- lm(perc.alumni~Expend, data=train)
options(scipen = 999) #change the format of e
summary(slr)
frame <- data.frame(Expend=c(10000))
predict(slr, frame)

pred <- predict(slr, train)
accuracy(pred, train$perc.alumni)
pred2 <- predict(slr, valid)
accuracy(pred2, valid$perc.alumni)

variables <- dplyr::select(train, Enroll, Top10perc, Top25perc, F.Undergrad, P.Undergrad, Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio, Expend, Grad.Rate)
cor(variables)
train2 <- dplyr::select(train, Private, Enroll, Top25perc, P.Undergrad, Room.Board, Books, Personal, Terminal, Expend, Grad.Rate, perc.alumni)

mlr_test <- lm(perc.alumni~Private+Enroll+Top25perc+P.Undergrad+Room.Board+Books+Personal+Terminal+Expend+Grad.Rate, data=train2)
mlr_test_step <- step(mlr_test, direction = "backward")
summary(mlr_test_step)

train3 <- dplyr::select(train, Private, Room.Board, Personal, Terminal, Expend, Grad.Rate, perc.alumni)
mlr <- lm(perc.alumni~Private+Room.Board+Personal+Terminal+Expend+Grad.Rate, data=train3)
summary(mlr)

train3$outcome_diff <- train3$perc.alumni - mean(train3$perc.alumni)
train3$squared_diff <- train3$outcome_diff^2
sum(train3$squared_diff)

train3$explained <- mlr$fitted.values - mean(train3$perc.alumni)
train3$squared_explained <- train3$explained^2
sum(train3$squared_explained)

library(visualize)
visualize.t(stat=c(-0.188,0.188), df=293, section="bounded")
1-0.149

fake_college <- data.frame(Private="Yes", Room.Board=5000, Personal=1000, Terminal=80, Expend=1000, Grad.Rate=60)
predict(mlr, fake_college)

pred3 <- predict(mlr, train3)
accuracy(pred3, train3$perc.alumni)
pred4 <- predict(mlr, valid)
accuracy(pred4, valid$perc.alumni)
