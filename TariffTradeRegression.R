library(readxl)
library(tidyverse)
library(fixest)
library(modelsummary)

setwd("~/GitHub/DSFinalProject")

#Loading the data
tariffData = read_csv("cleanTariffData.csv",col_select = 2:31)
tradeData = read_csv("tradeBalance.csv",col_select = 2:7)

# #Problem - some HTS codes have multiple entries per year
# #See HTS 2009 9040 for the year 1997
# #There GSP (Generalized System of Preferences) indicator changed, effective dates are different
# 
# #Identifying "duplicates"
# test = tariffData %>% count(year,hts8)
# test = test %>% filter(n>1)
# 
# test2 = left_join(test,tariffData, by = c('year','hts8'))
# 
# #But very few - only 1334 - are actually changes to the ad valorem tariff rate
# test2 = test2 %>% group_by(year,hts8) %>% mutate(avgAdVal = mean(mfn_ad_val_rate))
# test2 = test2 %>% filter(mfn_ad_val_rate!=avgAdVal)
# #So taking the average of the ad valorem tariff rate seems to be a reasonable enough approach, at least for the regression

#There was some data in the tariff rate dataset with extremely large ad val rates
#For example, a value of 10000 where most entries are something like 0.05
#After examining the data, these appear to be placeholders for categories where the calculation is
#complex or not an ad valorem rate at all (a specific rate, like 0.05 cents /kg, for example)
#See USITC code key
#https://www.usitc.gov/applications/dataweb/td-codes.pdf
#So we will filter to just the categories with an ad valorem component (excluding
#the placeholders in category 9)

tariffData = tariffData %>% filter(mfn_rate_type_code %in% c(0,4,5,6,7,9))
#tariffData = tariffData %>% filter(mfn_rate_type_code == 7)
tariffData = tariffData %>% filter(mfn_ad_val_rate < 100)

# This excludes some large values like 114.0000   9999.9900  10000.0000  10000.0000 100000.0000
#But keeps some other still fairly large values like 1.0000      1.3100 1.3180      1.3570      1.3950      1.4340      1.6300      1.6380      1.6860      1.7340  1.7830      2.0000      3.5000
#This threshold may need some adjustment


#Selecting a few variables from tariff data (unlikely to use them all)
#Comment this line out if making the copy for random forests
tariffData = tariffData %>% select(year,hts8,mfn_ad_val_rate)

#Handling "duplicates" as discussed above
tariffData = tariffData %>% group_by(year,hts8) %>% summarise(adValRate = mean(mfn_ad_val_rate))

data = full_join(tradeData,tariffData, join_by("Year"=="year","HTS8"=="hts8"))
data$adValRate = data$adValRate %>% replace_na(0) #Omitting this step results in a positive coef in the regression. But it seems to be a reasonable approach to me - anything that doesn't match up clearly has a 0% tariff rate.

#Creating a less cleaned up version for use with random forests in Python
# data$mfn_ad_val_rate = data$mfn_ad_val_rate %>% replace_na(0) #Omitting this step results in a positive coef in the regression. But it seems to be a reasonable approach to me - anything that doesn't match up clearly has a 0% tariff rate.
# write.csv(data,"MLReadyCombinedDataset.csv")
# write.csv(countryData,"countryData.csv")

#Creating a series ID for use with feols function (needed when using lags)
data = data %>% group_by(Country,HTS8) %>% mutate(seriesID = cur_group_id())

#Aggregating to country level (adding across HTS)
countryData = data %>% group_by(Year,Country) %>% summarise(tradeBalance = sum(tradeBalance,na.rm=TRUE),
                                                            Exports= sum(Exports),
                                                            Imports = sum(Imports),
                                                            adValRate = sum(Imports*adValRate,na.rm=TRUE)/sum(Imports,na.rm=TRUE))

#Aggregating to global level (adding across country)
globalData = countryData %>% group_by(Year) %>% summarise(tradeBalance = sum(tradeBalance,na.rm=TRUE),
                                                            Exports= sum(Exports,na.rm = TRUE),
                                                            Imports = sum(Imports, na.rm = TRUE),
                                                            adValRate = sum(Imports*adValRate,na.rm=TRUE)/sum(Imports,na.rm=TRUE))

#Unscaled regressions
unscaledProductLevelFE = feols(data, tradeBalance ~ l(adValRate,0:4) | Year + Country + HTS8, panel.id = ~seriesID+Year)
unscaledCountryLevelFE = feols(countryData, tradeBalance ~ l(adValRate,0:4) | Year + Country,panel.id = ~Country+Year)
globalLevelModel = lm(tradeBalance~adValRate + lag(adValRate)+ lag(adValRate,2)+ lag(adValRate,3)+ lag(adValRate,4),globalData)

#Scale each over time and repeat regressions
data = data %>% group_by(Country,HTS8) %>% mutate(tradeBalance = scale(tradeBalance),Exports = scale(Exports),Imports = scale(Imports))
scaledProductLevelFE = feols(data, tradeBalance ~ adValRate | Year + Country + HTS8)


countryData = countryData %>% group_by(Country) %>% mutate(tradeBalance = scale(tradeBalance),Exports = scale(Exports),Imports = scale(Imports))
scaledCountryLevelFE = feols(countryData, tradeBalance ~ adValRate | Year + Country)


rm(tariffData)
rm(tradeData)





#We have an issue with some strange values for ad valorem tariff rate
#Notice values like 10,000 or larger. Probably some kind of data entry issue?
#This is in all years, not like a recent change in data entry/units
#test = data %>% filter(adValRate > 1 & Imports>0)
#unique(test$adValRate)



#Lag regressions
countryLevelLags =  feols(countryData, tradeBalance ~ l(adValRate,0:4) | Year + Country, panel.id = ~Country+Year)
countryLevelLags
countryLevelLagsExports =  feols(countryData, Exports ~ l(adValRate,0:4) | Year + Country, panel.id = ~Country+Year)
countryLevelLagsImports =  feols(countryData, Imports ~ l(adValRate,0:4) | Year + Country, panel.id = ~Country+Year)


data = data %>% group_by(Country,HTS8) %>% mutate(seriesID = cur_group_id())
productLevelLags =  feols(data, tradeBalance ~ l(adValRate,0:4) | Year + Country + HTS8, panel.id = ~seriesID+Year)
productLevelLags
productLevelLagsExports =  feols(data, Exports ~ l(adValRate,0:4) | Year + Country + HTS8, panel.id = ~seriesID+Year)
productLevelLagsImports =  feols(data, Imports ~ l(adValRate,0:4) | Year + Country + HTS8, panel.id = ~seriesID+Year)



#Creating tables for the models
#Balance of Trade Table

coef_map = c(
  'l(adValRate, 0)'= "lag 0",
  'l(adValRate, 1)'= "lag 1",
  'l(adValRate, 2)'= "lag 2",
  'l(adValRate, 3)'= "lag 3",
  'l(adValRate, 4)'= "lag 4"
)

#modelsummary(stars = c("*" = .05, "**" = .01, "***" = 0.001), list("HTS8-Level" = productLevelLags, "Country-Level   " =countryLevelLags))
modelsummary(stars = c("*" = .05, "**" = .01, "***" = 0.001), list("HTS8-Level" = productLevelLags, "Country-Level   " =countryLevelLags),coef_map = coef_map,output = "regressionTable.png")

#Exports Table
modelsummary(stars = c("*" = .05, "**" = .01, "***" = 0.001), list("HTS8-Level" = productLevelLagsExports, "Country-Level   " =countryLevelLagsExports),coef_map = coef_map,output = "exportsRegressionTable.png")

#Imports Table
modelsummary(stars = c("*" = .05, "**" = .01, "***" = 0.001), list("HTS8-Level" = productLevelLagsImports, "Country-Level   " =countryLevelLagsImports),coef_map = coef_map,output = "importsRegressionTable.png")


#Removing NA country observations. The regressions drop them automatically but ccf does not
countryData = countryData %>% filter(!is.na(Country) & !is.na(Year))


#Initializing a table to store the results in
columns = c(-15:15)
lagsTable = data.frame(matrix(nrow=0,ncol=length(columns)))
colnames(lagsTable) = columns

#Running ccf for every country and extracting "significant" lags
for (c in unique(countryData$Country)){
  tempData = countryData %>% filter(Country == c)
  x = ts(tempData$adValRate)
  y = ts(drop(tempData$tradeBalance))
  ccfTest = ccf(y,x,na.action = na.pass,plot = FALSE) #Swapped x,y to y,x to make it so the positive lags on the plot match up with ad valorem tariff rate now affecting balance of trade in the future
  results = ccfTest$acf
  lagLabels = ccfTest$lag
  signficantLags = (results < -1.96/sqrt(length(x)) | results > 1.96/sqrt(length(x)))
  tempLagTable = rbind(signficantLags)
  tempLagTable = data.frame(tempLagTable)
  colnames(tempLagTable) = lagLabels
  lagsTable = bind_rows(lagsTable,tempLagTable)
}

#Summing across time period to get the number of significant lags in each time period
summedLags = colSums(lagsTable,na.rm=TRUE)
summedLags = data.frame(summedLags)
barplot(summedLags$summedLags,names.arg = columns,ylab = "Number of Significant Lags", xlab = "Lag", main = "Bar Plot of Significant Lags Across Country")
