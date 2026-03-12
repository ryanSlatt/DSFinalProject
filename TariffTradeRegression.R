library(readxl)
library(tidyverse)
library(fixest)

setwd("~/GitHub/DSFinalProject")

#Loading the data
tariffData = read_csv("cleanTariffData.csv",col_select = 2:31)
tradeData = read_csv("tradeBalance.csv",col_select = 2:5)

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


#Selecting a few variables from tariff data (unlikely to use them all)
tariffData = tariffData %>% select(year,hts8,mfn_ad_val_rate)

#Handling "duplicates" as discussed above
tariffData = tariffData %>% group_by(year,hts8) %>% summarise(adValRate = mean(mfn_ad_val_rate))

data = full_join(tradeData,tariffData, join_by("Year"=="year","HTS8"=="hts8"))
data$adValRate = data$adValRate %>% replace_na(0)

#Unscaled regression
feols(data, tradeBalance ~ adValRate | Year + Country + HTS8)

#Scale each good over time and repeat regression
data = data %>% group_by(Country,HTS8) %>% mutate(tradeBalance = scale(tradeBalance))
feols(data, tradeBalance ~ adValRate | Year + Country + HTS8)


rm(tariffData)
rm(tradeData)

feols(data, tradeBalance ~ adValRate | Year + Country + HTS8)
