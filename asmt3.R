install.packages("odbc")
install.packages("DBI")
library(odbc)
library(DBI)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(tidyverse)

NYCHistorical <- read.csv(file = 'NYCHistorical.csv' ,header = TRUE,sep = ',' )
Neighborhood <- read.csv(file = 'Neighborhood.csv', header = TRUE, sep = ',')
BuildingCode <- read.csv(file = 'BuildingCode.csv', header = TRUE, sep = ',')

df <- NYCHistorical %>%
    left_join(BuildingCode, by=c("BuildingClassTimeOfSale" = "BuildingCodeID")) %>%
    left_join(Neighborhood, by="NbhoodID") %>%
    mutate(SaleYear=year(SaleDate)) %>%
    filter(SalePrice!=0, GrossSqFt!=0) %>%
    select(NbhoodID, NbhoodName, SaleYear, GrossSqFt, SalePrice,ResidentialUnits,CommercialUnits,TotalUnits) %>%
    group_by(NbhoodID)

df.236 <- filter(df, NbhoodID==236) %>%
  group_by(NbhoodID) 
  
df.257 <- filter(df, NbhoodID==257) %>%
  group_by(NbhoodID) 

#desriptive statistics
#mean,median-->tendency
summarise(df.236, meanSalePrice=mean(SalePrice), meanGrossSqFt=mean(GrossSqFt), medianSalePrice=median(SalePrice),medianGrossSqFt=median(GrossSqFt),n=n())
#standard deviation-->dispersion
summarise(df.236, sdSalePrice=sd(SalePrice), sdGrossSqFt=sd(GrossSqFt))
#k means method
#KPI1: average sale price
KPI.1 <- group_by(df, NbhoodID) %>%
  mutate(AverageSalePrice=mean(SalePrice), Residential=ResidentialUnits, Commercial=CommercialUnits)%>%
  summarise(AverageSalePrice=mean(AverageSalePrice), Residential=sum(Residential), Commercial=sum(Commercial))

zscores.1 <- scale(KPI.1[c(-1)]) %>%
  as.data.frame()

k.1<-kmeans(zscores.1,centers=5)

KPI.1<-cbind(KPI.1,k.1$cluster) 

one <- subset(KPI.1,NbhoodID==236 | NbhoodID==257| NbhoodID==262)

ggplot(zscores.1)+geom_point(mapping=aes(x=Residential,y=Commercial,size=AverageSalePrice,color=k.1$cluster))

#KPI2:average square foot
KPI.2 <- group_by(df, NbhoodID) %>%
  mutate(AverageSqFt=mean(GrossSqFt), Residential=ResidentialUnits, Commercial=CommercialUnits)%>%
  summarise(AverageSqFt=mean(GrossSqFt), Residential=sum(Residential), Commercial=sum(Commercial))

zscores.2 <- scale(KPI.2[c(-1)]) %>%
  as.data.frame()

k.2<-kmeans(zscores.2,centers=5)

KPI.2<-cbind(KPI.2,k.2$cluster) 

two <- subset(KPI.2,NbhoodID==236 | NbhoodID==257| NbhoodID==262)

ggplot(zscores.2)+geom_point(mapping=aes(x=Residential,y=Commercial,size=AverageSqFt,color=k.2$cluster))

#KPI3: average saleprice  in 2016
Sales<-subset(df, SaleYear==2016) 

KPI.3 <- group_by(Sales, NbhoodID) %>%
  mutate(Commercial=CommercialUnits, Residential=ResidentialUnits, AverageSalePrice=mean(SalePrice)) %>%
  summarise(Commercial=sum(Commercial), Residential=sum(Residential), AverageSalePrice=mean(AverageSalePrice))

zscores.3 <- scale(KPI.3[c(-1)]) %>%
  as.data.frame()

k.3<-kmeans(zscores.3,centers=5)

KPI.3<-cbind(KPI.3,k.3$cluster) 

three <- subset(KPI.3,NbhoodID==236 | NbhoodID==257| NbhoodID==262)

ggplot(zscores.3)+geom_point(mapping=aes(x=Residential,y=Commercial,size=AverageSalePrice,color=k.3$cluster))



#t test



t.test(x = df.NewBrighton$SalePrice, y = df.Stapleton$SalePrice,alternative = "g",conf.level = .95)

