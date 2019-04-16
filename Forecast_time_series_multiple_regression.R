install.packages("odbc")
install.packages("DBI")
install.packages("forecast")
library(odbc)
library(DBI)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(forecast)


NYCHistorical <- read.csv(file = 'NYCHistorical.csv' ,header = TRUE,sep = ',' )
Neighborhood <- read.csv(file = 'Neighborhood.csv', header = TRUE, sep = ',')
BuildingCode <- read.csv(file = 'BuildingCode.csv', header = TRUE, sep = ',')
NYCCurrent <- read.csv(file = 'NYCCurrent.csv', header = TRUE, sep = ',')

NYCHistorical<-rbind(NYCHistorical,NYCCurrent)

df <- NYCHistorical %>%
  left_join(BuildingCode, by=c("BuildingClassTimeOfSale" = "BuildingCodeID")) %>%
  left_join(Neighborhood, by="NbhoodID") %>%
  filter(SalePrice!=0, GrossSqFt!=0) %>%
  mutate(SaleQuarter=quarter(SaleDate, with_year = FALSE, fiscal_start = 1)) %>%
  mutate(SaleYear=year(SaleDate)) %>%
  select(NbhoodID,NbhoodName,SalePrice,SaleYear,SaleQuarter,BuildingClassFinalRoll, ResidentialUnits,CommercialUnits,GrossSqFt,SaleDate,YearBuilt,SalePrice) %>%
  group_by(NbhoodID)

df.236 <- filter(df, NbhoodID==236) %>%
  mutate(t=SaleYear*4+SaleQuarter-2003*4) %>%
  group_by(t) %>%
  filter(t!=0) %>%
  summarise(SalePrice=sum(SalePrice))

#time series analysis and forecast
ts.236<-ts(df.236$SalePrice,start = c(2003,1),frequency=4)
View (ts.236)

ts.model<-ets(ts.236,model = "ZAA")
forecast(ts.model,8)

#multiple regression and forecast
df.236<-cbind(df.236,c("Q1","Q2","Q3","Q4"))
names(df.236)[3]<-"Quarter"

lm(formula = SalePrice ~ t + Quarter, data = df.236)

reg.236<-lm(data=df.236,formula=SalePrice~t + Quarter)
summary(reg.236)

x<-data.frame(t=c(61,62,63,64,65,66,67,68),SalePrice=c(0,0,0,0,0,0,0,0),Quarter=c("Q1","Q2","Q3","Q4"))
predict.lm(reg.236,x,interval = "confidence")

#multiple regression on given property
df2<-NYCHistorical%>%
  left_join(BuildingCode, by=c("BuildingClassTimeOfSale" = "BuildingCodeID")) %>%
  left_join(Neighborhood, by="NbhoodID") %>%
  mutate(SaleYear=year(SaleDate)) %>%
  filter(SalePrice!=0, GrossSqFt!=0) %>%
  filter(NbhoodID==236,Status=="Residential")

df.236forLM <-df2 %>%
  filter(SaleYear>2010) %>% #filter the data before crash, can be adjusted
  select(BuildingClassFinalRoll,ResidentialUnits,CommercialUnits,GrossSqFt,YearBuilt,SalePrice,SaleYear)

model<-lm(formula = SalePrice~.,data=df.236forLM)
summary(model)#to see the if the model is good enough(high r-squared)

df.236forLM["residuals"]<-model$residuals

df.236address<-filter(df2,SaleYear>=2010) %>%
  select(Address)

df.236forLM["Address"]<-df.236address

