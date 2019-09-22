library(dplyr)
library(ggplot2)

feecharges <- read.csv("fee-charges.csv")
dim(feecharges)
mean(feecharges$FeeAmount)

mydata <- filter(feecharges, Zip == 10452)
dim(mydata)
head(mydata$Boro)
mean(mydata$FeeAmount)

str(mydata)
mydata$FeeIssuedDate <- as.Date(mydata$FeeIssuedDate)
mydata$DoFTransferDate <- as.Date(mydata$DoFTransferDate)
mydata$paydelay <- mydata$DoFTransferDate - mydata$FeeIssuedDate
str(mydata)

anyNA(mydata)
mydata2 <- na.omit(mydata) #no NA values

mydata3 <- mydata2[-c(1:3)] #no useless variables

mydata4 <- mydata3 %>%
  group_by(StreetName) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

WantedStreetName <- c("ANDERSON AVENUE","CROMWELL AVENUE","DR M L KING JR BOULEVARD","GERARD AVENUE","WALTON AVENUE","GRAND CONCOURSE","JESUP AVENUE")
mydata5 <- filter(mydata3, StreetName %in% WantedStreetName) #the dataframe which only contains data from the seven streets


ggplot(mydata5,aes(x=StreetName)) + geom_bar(fill=rainbow(n=7))+ggtitle("The Number of Occurrences of Fees") + xlab("Street Name")+  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) +   theme(plot.title = element_text(hjust = 0.5))

meanof5 <- mydata5 %>%
  group_by(StreetName) %>%
  summarise(MeanOfFee=mean(FeeAmount))

head(meanof5)
ggplot(meanof5,aes(x=StreetName,y=MeanOfFee)) + geom_bar(color=rainbow(n=7),stat = "identity")+ggtitle("The Average Fee Amount of Seven Streets") + xlab("Street Name") + ylab("Average Fee Amount") + theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) + theme(plot.title = element_text(hjust = 0.5))

ggplot(mydata5, aes(x=paydelay,fill=StreetName)) + geom_histogram(bandwidth=0.1) + ggtitle("The Distribution of Pay Delay")+ theme(plot.title = element_text(hjust = 0.5)) 
ggplot(mydata5, aes(x=paydelay,fill=StreetName)) + geom_histogram(bandwidth=0.1) + ggtitle("The Distribution of Pay Delay")+ theme(plot.title = element_text(hjust = 0.5)) + xlim(0,38)

PD_FA <- mydata5 %>%
  group_by(FeeAmount) %>%
  summarise(MeanOfPD=mean(paydelay))
ggplot(PD_FA, aes(x=FeeAmount, y=MeanOfPD)) + geom_point() +  geom_smooth(method='lm',formula=y~x) + xlab("Fee Amount") + ylab("Average Pay Delay")

