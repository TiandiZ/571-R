install.packages("odbc")
install.packages("DBI")
library(odbc)
library(DBI)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)

#connect to SQL server
con <- dbConnect(odbc(),
                 
                 Driver = "SQL Server",
                 
                 Server = "met-sql14.bu.edu",
                 
                 Database = "NYC Real Estate",
                 
                 Port = 1433)
dbListTables(con)

NYCHistorical<-dbReadTable(con,"NYCHistorical")


NewBrighton <- filter(NYCHistorical,NbhoodID == 236)
NewBrighton <- filter(NewBrighton, SalePrice != 0)
NewBrighton <- filter(NewBrighton, GrossSqFt != 0)

New_Brighton <- select(NewBrighton, GrossSqFt, SalePrice, SaleDate)


New_Brighton_aggregate <- aggregate(x= New_Brighton[c("SalePrice","GrossSqFt")],
                     FUN = sum,
                     by = list(Group.date = year(ymd(New_Brighton$SaleDate))))

New_Brighton_Ppsqft<- mutate(New_Brighton_aggregate, PpSqFt = SalePrice/GrossSqFt)




Tompkinsville <- filter(NYCHistorical,NbhoodID == 262)
Tompkinsville <- filter(Tompkinsville, SalePrice != 0)
Tompkinsville <- filter(Tompkinsville, GrossSqFt != 0)

Tompkinsville_ <- select(Tompkinsville, GrossSqFt, SalePrice, SaleDate)


Tompkinsville_aggregate <- aggregate(x= Tompkinsville_[c("SalePrice","GrossSqFt")],
                                    FUN = sum,
                                    by = list(Group.date = year(ymd(Tompkinsville_$SaleDate))))

Tompkinsville_Ppsqft<- mutate(Tompkinsville_aggregate, PpSqFt = SalePrice/GrossSqFt)


Stapleton <- filter(NYCHistorical,NbhoodID == 257)
Stapleton <- filter(Stapleton, SalePrice != 0)
Stapleton <- filter(Stapleton, GrossSqFt != 0)

Stapleton_ <- select(Stapleton, GrossSqFt, SalePrice, SaleDate)


Stapleton_aggregate <- aggregate(x= Stapleton_[c("SalePrice","GrossSqFt")],
                                     FUN = sum,
                                     by = list(Group.date = year(ymd(Stapleton_$SaleDate))))

Stapleton_Ppsqft<- mutate(Stapleton_aggregate, PpSqFt = SalePrice/GrossSqFt)





WestNewBrighton <- filter(NYCHistorical,NbhoodID == 265)
WestNewBrighton <- filter(WestNewBrighton, SalePrice != 0)
WestNewBrighton <- filter(WestNewBrighton, GrossSqFt != 0)

WestNewBrighton_ <- select(WestNewBrighton, GrossSqFt, SalePrice, SaleDate)


WestNewBrighton_aggregate <- aggregate(x= WestNewBrighton_[c("SalePrice","GrossSqFt")],
                                 FUN = sum,
                                 by = list(Group.date = year(ymd(WestNewBrighton_$SaleDate))))

WestNewBrighton_Ppsqft<- mutate(WestNewBrighton_aggregate, PpSqFt = SalePrice/GrossSqFt)

ggplot(New_Brighton_Ppsqft, aes)


# Extract PpSqFt from each region
# test
# Extract PpSqFt from WestNewBrighton
WestNewBrighton_data = WestNewBrighton_Ppsqft['PpSqFt']
print(WestNewBrighton_data)

Stapleton_data = Stapleton_Ppsqft['PpSqFt']
print(Stapleton_data)

Tompkinsville_data = Tompkinsville_Ppsqft['PpSqFt']
print(Tompkinsville_data)

New_Brighton_data = New_Brighton_Ppsqft['PpSqFt']
print(New_Brighton_data)

year_data = New_Brighton_Ppsqft['Group.date']
print(year_data)

df <- data.frame(year_data,WestNewBrighton_data,Stapleton_data,Tompkinsville_data,New_Brighton_data)
print(df)
colnames(df) <- c('Year','West_New_Brighton','Stapleton','Tompkinsville','New_Brighton')
print(df)

ggplot(df, aes(x=Year)) +ylab("Price per square foot")+
  geom_line(aes(y = West_New_Brighton,colour='West New Brighton'))+
  geom_line(aes(y = Stapleton,colour='Stapleton'))+
  geom_line(aes(y = Tompkinsville,colour='Tompkinsville'))+
  geom_line(aes(y = New_Brighton,colour='New_Brighton'))



