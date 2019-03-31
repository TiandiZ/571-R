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


NbhoodPricePerSqFt <- function(NeighborhoodID){
  df <- NYCHistorical %>%
  left_join(BuildingCode, by = c("BuildingClassTimeOfSale" = "BuildingCodeID")) %>%
  left_join(Neighborhood, by = "NbhoodID") %>%
  mutate(SaleYear = year(SaleDate)) %>%
  filter(Status == "Residential", NbhoodID == NeighborhoodID, SalePrice != 0, GrossSqFt != 0) %>%
  select(NbhoodID, NbhoodName, SaleYear, GrossSqFt, SalePrice) %>%
  group_by(SaleYear)
  
summarise(df, TotalSalePrice = sum(SalePrice), TotalGrossSqFt = sum(GrossSqFt)) %>%
  mutate(PricePerSquareFoot = TotalSalePrice/TotalGrossSqFt)
}

NewBrighton <- NbhoodPricePerSqFt(236)
Tompkinsville <- NbhoodPricePerSqFt(262)
Stapleton <- NbhoodPricePerSqFt(257)
WestNewBrighton <- NbhoodPricePerSqFt(265)

ggplot()+geom_line(data=NewBrighton, size=2,aes(x=SaleYear, y=PricePerSquareFoot,colour="red"))+
  geom_line(data=Stapleton, size=2,aes(x=SaleYear, y=PricePerSquareFoot,colour="blue"))+
  geom_line(data=Tompkinsville, size=2,aes(x=SaleYear, y=PricePerSquareFoot,colour="yellow"))+
  geom_line(data=WestNewBrighton, size=2,aes(x=SaleYear, y=PricePerSquareFoot,colour="black"))+
  scale_color_discrete(name="Neighborhood",labels=c("WestNewBrighton","Stapleton","NewBrighton","Tompkinsville"))
