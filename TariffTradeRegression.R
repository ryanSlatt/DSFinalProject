library(readxl)
library(tidyverse)
library(fixest)

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
tariffData = tariffData %>% filter(mfn_ad_val_rate < 100)
# This excludes some large values like 114.0000   9999.9900  10000.0000  10000.0000 100000.0000
#But keeps some other still fairly large values like 1.0000      1.3100 1.3180      1.3570      1.3950      1.4340      1.6300      1.6380      1.6860      1.7340  1.7830      2.0000      3.5000



#Selecting a few variables from tariff data (unlikely to use them all)
tariffData = tariffData %>% select(year,hts8,mfn_ad_val_rate)

#Handling "duplicates" as discussed above
tariffData = tariffData %>% group_by(year,hts8) %>% summarise(adValRate = mean(mfn_ad_val_rate))

data = full_join(tradeData,tariffData, join_by("Year"=="year","HTS8"=="hts8"))
data$adValRate = data$adValRate %>% replace_na(0)


#Aggregating to country level (adding across HTS)
countryData = data %>% group_by(Year,Country) %>% summarise(tradeBalance = sum(tradeBalance,na.rm=TRUE),
                                                            Exports= sum(Exports),
                                                            Imports = sum(Imports),
                                                            adValRate = sum(Imports*adValRate)/sum(Imports))

#Unscaled regressions
feols(data, tradeBalance ~ adValRate | Year + Country + HTS8)
feols(countryData, tradeBalance ~ adValRate | Year + Country)


#Scale each over time and repeat regressions
data = data %>% group_by(Country,HTS8) %>% mutate(tradeBalance = scale(tradeBalance))
feols(data, tradeBalance ~ adValRate | Year + Country + HTS8)

countryData = countryData %>% group_by(Country) %>% mutate(tradeBalance = scale(tradeBalance))
feols(countryData, tradeBalance ~ adValRate | Year + Country)


rm(tariffData)
rm(tradeData)





#We have an issue with some strange values for ad valorem tariff rate
#Notice values like 10,000 or larger. Probably some kind of data entry issue?
#This is in all years, not like a recent change in data entry/units
#test = data %>% filter(adValRate > 1 & Imports>0)
#unique(test$adValRate)

#Lag regressions
#Single year lag
feols(data, tradeBalance ~ lag(adValRate) | Year + Country + HTS8)
feols(countryData, tradeBalance ~ lag(adValRate) | Year + Country)

#Using the ccf function to see if there is evidence for a lag and how long
# dataDropNA = data %>% filter(!is.na(tradeBalance))
# 
# x = ts(dataDropNA$adValRate)
# y = ts(drop(dataDropNA$tradeBalance))
# 
# ccf(x,y)
# 
# 
# 
# feols(data, tradeBalance ~ adValRate | Year + Country + HTS8)
