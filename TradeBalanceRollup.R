#This file takes the import and export data and rolls them up to HTS-8 from HTS-10
#Then substracts Ex-Im to get the balance of trade
library(readxl)
library(tidyverse)

setwd("~/GitHub/DSFinalProject")

#### Step 1: Exports rollup

exportFiles = list.files("TradeData/Exports")


# x1997A <- read_excel("TradeData/Exports/Exports1997A.xlsx",sheet=2)
# x1997A = x1997A %>% select(Year,Country,"HTS10" = 'Schedule B', "Value" = 'FAS Value')
# x1997A = x1997A %>% mutate("HTS8" = substring(HTS10,1,8))
# x1997A = x1997A %>% select(Year,Country,HTS8,Value)
# x1997A = x1997A %>% group_by(Year,Country,HTS8) %>% summarise(Value = sum(Value)) %>% ungroup()

#Get a count of observations during the process
origRows = 0 #11379727
numRows = 0 #9271409
for (f in exportFiles){
  data = read_excel(paste("TradeData/Exports/",f,sep=""),sheet=2)
  origRows = origRows + nrow(data)
  data = data %>% select(Year,Country,"HTS10" = 'Schedule B', "Value" = 'FAS Value')
  data = data %>% mutate("HTS8" = substring(HTS10,1,8))
  data = data %>% select(Year,Country,HTS8,Value)
  data = data %>% group_by(Year,Country,HTS8) %>% summarise(Value = sum(Value)) %>% ungroup()
  numRows = numRows + nrow(data)
  write.csv(data,paste("TradeData/RolledExports/rolled" ,f,".csv",sep=''))
  
}

#Clear global environment
rm(list = ls())

#Load all those export files and bind them into one
rolledExportFiles = list.files("TradeData/RolledExports")
combinedExports <- read_csv("TradeData/RolledExports/rolledExports1997A.xlsx.csv",col_select = 2:5,n_max = 0)

for (f in rolledExportFiles){
  data = read_csv(paste("TradeData/RolledExports/",f,sep=''),col_select = 2:5)
  combinedExports = rbind(combinedExports,data)
}
#Number of observations matches the earlier count

write.csv(combinedExports,"TradeData/CombinedExports.csv")


##### Step 2: Imports rollup

#Now to do the same for the imports
importFiles = list.files("TradeData/Imports")

# i1997A = read_excel("TradeData/Imports/Imports1997A.xlsx",sheet=2)
# i1997A = i1997A %>% select(Year,Country,"HTS10" = 'HTS Number', "Value" = 'General Customs Value')
# i1997A = i1997A %>% mutate("HTS8" = substring(HTS10,1,8))
# i1997A = i1997A %>% select(Year,Country,HTS8,Value)
# i1997A = i1997A %>% group_by(Year,Country,HTS8) %>% summarise(Value = sum(Value)) %>% ungroup()

#Get a count of observations during the process
origRows = 0 # 8486064
numRows = 0 # 5957393
for (f in importFiles){
  data = read_excel(paste("TradeData/Imports/",f,sep=""),sheet=2)
  origRows = origRows + nrow(data)
  data = data %>% select(Year,Country,"HTS10" = 'HTS Number', "Value" = 'General Customs Value')
  data = data %>% mutate("HTS8" = substring(HTS10,1,8))
  data = data %>% select(Year,Country,HTS8,Value)
  data = data %>% group_by(Year,Country,HTS8) %>% summarise(Value = sum(Value)) %>% ungroup()
  numRows = numRows + nrow(data)
  write.csv(data,paste("TradeData/RolledImports/rolled" ,f,".csv",sep=''))
  
}

#Load all import files and bind into one

#Clear global environment
rm(list = ls())

rolledImportFiles = list.files("TradeData/RolledImports")
combinedImports <- read_csv("TradeData/RolledImports/rolledImports1997A.xlsx.csv",col_select = 2:5,n_max = 0)

for (f in rolledImportFiles){
  data = read_csv(paste("TradeData/RolledImports/",f,sep=''),col_select = 2:5)
  combinedImports = rbind(combinedImports,data)
}

#Number of observations matches

write.csv(combinedImports,"TradeData/CombinedImports.csv")

####Step 3 : Balance of Trade

#Joining exports and imports into a balance of trade file
#Net Exports = Exports - Imports

#Clear global environment
rm(list = ls())

exports = read_csv("TradeData/CombinedExports.csv",col_select = 2:5)
imports = read_csv("TradeData/CombinedImports.csv",col_select = 2:5)

balanceData = full_join(exports,imports,by=c("Year","Country","HTS8"))
balanceData = balanceData %>% select(Year, Country, HTS8, "Exports" = "Value.x", "Imports" = "Value.y")
balanceData$Exports = balanceData$Exports %>% replace_na(0)
balanceData$Imports = balanceData$Imports %>% replace_na(0)
balanceData = balanceData %>% mutate(tradeBalance = Exports-Imports)
balanceData = balanceData %>% select(Year, Country, HTS8,tradeBalance)

write.csv(balanceData,"tradeBalance.csv")

