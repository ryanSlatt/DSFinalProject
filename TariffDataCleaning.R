#This file combines the annual tariff data files into one dataset

library(readxl)
library(tidyverse)

setwd("~/GitHub/DSFinalProject")

fileList = list.files("tariffData")
#fileList = list.files()[8:17]

#Load a year of data and get the column names
tariff_database_2018 <- read_excel("tariffData/tariff_database_2018.xlsx",n_max = 1)
listOfColumns = colnames(tariff_database_2018)

#Get the column names for each year and intersect them to see what columns are in common
for (f in fileList){
  data = read_excel(paste("tariffData/",f,sep=""),n_max=1)
  listOfColumns = intersect(listOfColumns,colnames(data))
  
  # if(!identical(colnames(data),listOfColumns)){
  #   print(f)
}

#Remove the brief description column, it is unnecessary
listOfColumns = setdiff(listOfColumns,c("brief_description"))

#Columns all of them have in common
#print(listOfColumns)

#Columns that were removed (based off of 2018)
#setdiff(colnames(tariff_database_2018),listOfColumns)

#Select those 30 common columns and create a combined dataset
#Add a column for year as well.

listOfColumns = c(listOfColumns,"year")

#Using 2018 and clearing out the data as a lazy way to set up the structure of the table
tariff_database_2018 = tariff_database_2018 %>% mutate(year = 2018)
combinedData = (tariff_database_2018 %>% select(listOfColumns))[0,]

#Loop over each year and bind the datasets together
year = 1997
for (f in fileList){
  data = read_excel(paste("tariffData/",f,sep=""))
  data = data %>% mutate("year" = year)
  data = data %>% select(listOfColumns)
  combinedData = rbind(combinedData,data)
  year = year + 1
}

combinedData = combinedData %>% relocate("year")

write.csv(combinedData,"cleanTariffData.csv")
