###############################################################################################################
# exploring changes in measures over time among rural census tracts -- ACS 5-year estimates
###############################################################################################################

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
rural_pa_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-pa-rural.csv")

# load packages
library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
rural_pa_ct_income_2021 = apply_labels(rural_pa_ct_income_2021,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2021$households <- rural_pa_ct_income_2021$S1901_C01_001E
rural_pa_ct_income_2021$income <- rural_pa_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2021 = rural_pa_ct_income_2021[-1,]

# identify NA values
table(rural_pa_ct_income_2021$income) ## there are 12 "-" values
rural_pa_ct_income_2021["income"][rural_pa_ct_income_2021["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2021$income)) ## 12 CTs with missing data out of 443

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2021$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2021$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2021$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2021$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2020 = apply_labels(rural_pa_ct_income_2020,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2020$households <- rural_pa_ct_income_2020$S1901_C01_001E
rural_pa_ct_income_2020$income <- rural_pa_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2020 = rural_pa_ct_income_2020[-1,]

# identify NA values
table(rural_pa_ct_income_2020$income) ## there are 12 "-" values
rural_pa_ct_income_2020["income"][rural_pa_ct_income_2020["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2020$income)) ## 12 CTs with missing data out of 443

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2020$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2020$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2020$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2020$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2019 = apply_labels(rural_pa_ct_income_2019,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2019$households <- rural_pa_ct_income_2019$S1901_C01_001E
rural_pa_ct_income_2019$income <- rural_pa_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2019 = rural_pa_ct_income_2019[-1,]

# identify NA values
table(rural_pa_ct_income_2019$income) ## there are 5 "-" values
rural_pa_ct_income_2019["income"][rural_pa_ct_income_2019["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2019$income)) ## 5 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2019$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2019$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2019$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2019$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2018 = apply_labels(rural_pa_ct_income_2018,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2018$households <- rural_pa_ct_income_2018$S1901_C01_001E
rural_pa_ct_income_2018$income <- rural_pa_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2018 = rural_pa_ct_income_2018[-1,]

# identify NA values
table(rural_pa_ct_income_2018$income) ## there are 6 "-" values
rural_pa_ct_income_2018["income"][rural_pa_ct_income_2018["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2018$income)) ## 6 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2018$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2018$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2018$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2018$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2017 = apply_labels(rural_pa_ct_income_2017,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2017$households <- rural_pa_ct_income_2017$S1901_C01_001E
rural_pa_ct_income_2017$income <- rural_pa_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2017 = rural_pa_ct_income_2017[-1,]

# identify NA values
table(rural_pa_ct_income_2017$income) ## there are 4 "-" values
rural_pa_ct_income_2017["income"][rural_pa_ct_income_2017["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2017$income)) ## 4 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2017$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2017$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2017$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2017$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2016 = apply_labels(rural_pa_ct_income_2016,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2016$households <- rural_pa_ct_income_2016$S1901_C01_001E
rural_pa_ct_income_2016$income <- rural_pa_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2016 = rural_pa_ct_income_2016[-1,]

# identify NA values
table(rural_pa_ct_income_2016$income) ## there are 6 "-" values
rural_pa_ct_income_2016["income"][rural_pa_ct_income_2016["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2016$income)) ## 6 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2016$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2016$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2016$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2016$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2015 = apply_labels(rural_pa_ct_income_2015,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2015$households <- rural_pa_ct_income_2015$S1901_C01_001E
rural_pa_ct_income_2015$income <- rural_pa_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2015 = rural_pa_ct_income_2015[-1,]

# identify NA values
table(rural_pa_ct_income_2015$income) ## there are 2 "-" values
rural_pa_ct_income_2015["income"][rural_pa_ct_income_2015["income"] == "-"] <- NA
# make "(X)" values as NA
rural_pa_ct_income_2015["income"][rural_pa_ct_income_2015["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2015$income)) ## 4 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2015$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2015$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2015$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2015$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2014 = apply_labels(rural_pa_ct_income_2014,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2014$households <- rural_pa_ct_income_2014$S1901_C01_001E
rural_pa_ct_income_2014$income <- rural_pa_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2014 = rural_pa_ct_income_2014[-1,]

# identify NA values
table(rural_pa_ct_income_2014$income) ## there are 2 "-" values
rural_pa_ct_income_2014["income"][rural_pa_ct_income_2014["income"] == "-"] <- NA
# make "(X)" values as NA
rural_pa_ct_income_2014["income"][rural_pa_ct_income_2014["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2014$income)) ## 2 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2014$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2014$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2014$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2014$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2013 = apply_labels(rural_pa_ct_income_2013,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2013$households <- rural_pa_ct_income_2013$S1901_C01_001E
rural_pa_ct_income_2013$income <- rural_pa_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2013 = rural_pa_ct_income_2013[-1,]

# identify NA values
table(rural_pa_ct_income_2013$income) ## there are 2 "-" values
rural_pa_ct_income_2013["income"][rural_pa_ct_income_2013["income"] == "-"] <- NA
# make "(X)" values as NA
rural_pa_ct_income_2013["income"][rural_pa_ct_income_2013["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2013$income)) ## 2 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2013$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2013$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2013$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2013$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2012 = apply_labels(rural_pa_ct_income_2012,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2012$households <- rural_pa_ct_income_2012$S1901_C01_001E
rural_pa_ct_income_2012$income <- rural_pa_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2012 = rural_pa_ct_income_2012[-1,]

# identify NA values
table(rural_pa_ct_income_2012$income) ## there are 2 "-" values
rural_pa_ct_income_2012["income"][rural_pa_ct_income_2012["income"] == "-"] <- NA
# make "(X)" values as NA
rural_pa_ct_income_2012["income"][rural_pa_ct_income_2012["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2012$income)) ## 2 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2012$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2012$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2012$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2012$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2011 = apply_labels(rural_pa_ct_income_2011,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2011$households <- rural_pa_ct_income_2011$S1901_C01_001E
rural_pa_ct_income_2011$income <- rural_pa_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2011 = rural_pa_ct_income_2011[-1,]

# identify NA values
table(rural_pa_ct_income_2011$income) ## there are 2 "-" values
rural_pa_ct_income_2011["income"][rural_pa_ct_income_2011["income"] == "-"] <- NA
# make "(X)" values as NA
rural_pa_ct_income_2011["income"][rural_pa_ct_income_2011["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2011$income)) ## 2 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2011$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2011$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2011$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2011$income))

###############################################################################################################
## Pennsylvania rural census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
rural_pa_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-pa-rural.csv")

# label and rename variables so it's easier for me
rural_pa_ct_income_2010 = apply_labels(rural_pa_ct_income_2010,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_pa_ct_income_2010$households <- rural_pa_ct_income_2010$S1901_C01_001E
rural_pa_ct_income_2010$income <- rural_pa_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
rural_pa_ct_income_2010 = rural_pa_ct_income_2010[-1,]

# identify NA values
table(rural_pa_ct_income_2010$income) ## there are 4 "-" values
rural_pa_ct_income_2010["income"][rural_pa_ct_income_2010["income"] == "-"] <- NA
# make "(X)" values as NA
rural_pa_ct_income_2010["income"][rural_pa_ct_income_2010["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_pa_ct_income_2010$income)) ## 4 CTs with missing data out of 410

# understand the distribution at this one time period
range(as.numeric(rural_pa_ct_income_2010$income), na.rm=T)
mean(as.numeric(rural_pa_ct_income_2010$income), na.rm=T)
median(as.numeric(rural_pa_ct_income_2010$income), na.rm=T)
hist(as.numeric(rural_pa_ct_income_2010$income))

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Rural Pennsylvania census tracts -- percent change in average median household income
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 

joined_df_1_rural_pa <- rural_pa_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(rural_pa_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_rural_pa$pct_change <- (as.numeric(joined_df_1_rural_pa$income.x)-as.numeric(joined_df_1_rural_pa$income.y))/as.numeric(joined_df_1_rural_pa$income.y)*100

# make histogram
hist(joined_df_1_rural_pa$pct_change)
hist(joined_df_1_rural_pa$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=40,xlim=c(-35, 60), xaxp=c(-40,60,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_rural_pa <- rural_pa_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(rural_pa_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_rural_pa$pct_change <- (as.numeric(joined_df_2_rural_pa$income.x)-as.numeric(joined_df_2_rural_pa$income.y))/as.numeric(joined_df_2_rural_pa$income.y)*100

# make histogram
hist(joined_df_2_rural_pa$pct_change)
hist_rural_pa_2020_2010 <- hist(joined_df_2_rural_pa$pct_change, main="Percent Change in Household Income between 2020 and 2010, Rural Pennsylvania", xlab="% change",breaks=50,xlim=c(-20, 100), xaxp=c(-20,100,12))

###############################################################################################################
## North Carolina rural census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-rural-nc.csv")

library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
rural_nc_ct_income_2021 = apply_labels(rural_nc_ct_income_2021,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2021$households <- rural_nc_ct_income_2021$S1901_C01_001E
rural_nc_ct_income_2021$income <- rural_nc_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2021 = rural_nc_ct_income_2021[-1,]

# identify NA values
table(rural_nc_ct_income_2021$income) ## there are 56 "-" values
rural_nc_ct_income_2021["income"][rural_nc_ct_income_2021["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2021$income)) ## 56 CTs with missing data out of 629

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2021$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2021$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2021$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2021$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2020 = apply_labels(rural_nc_ct_income_2020,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2020$households <- rural_nc_ct_income_2020$S1901_C01_001E
rural_nc_ct_income_2020$income <- rural_nc_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2020 = rural_nc_ct_income_2020[-1,]

# identify NA values
table(rural_nc_ct_income_2020$income) ## there are 51 "-" values
rural_nc_ct_income_2020["income"][rural_nc_ct_income_2020["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2020$income)) ## 51 CTs with missing data out of 629

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2020$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2020$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2020$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2020$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2019 = apply_labels(rural_nc_ct_income_2019,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2019$households <- rural_nc_ct_income_2019$S1901_C01_001E
rural_nc_ct_income_2019$income <- rural_nc_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2019 = rural_nc_ct_income_2019[-1,]

# identify NA values
table(rural_nc_ct_income_2019$income) ## there are 16 "-" values
rural_nc_ct_income_2019["income"][rural_nc_ct_income_2019["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2019$income)) ## 16 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2019$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2019$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2019$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2019$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2018 = apply_labels(rural_nc_ct_income_2018,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2018$households <- rural_nc_ct_income_2018$S1901_C01_001E
rural_nc_ct_income_2018$income <- rural_nc_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2018 = rural_nc_ct_income_2018[-1,]

# identify NA values
table(rural_nc_ct_income_2018$income) ## there are 15 "-" values
rural_nc_ct_income_2018["income"][rural_nc_ct_income_2018["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2018$income)) ## 16 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2018$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2018$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2018$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2018$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2017 = apply_labels(rural_nc_ct_income_2017,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2017$households <- rural_nc_ct_income_2017$S1901_C01_001E
rural_nc_ct_income_2017$income <- rural_nc_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2017 = rural_nc_ct_income_2017[-1,]

# identify NA values
table(rural_nc_ct_income_2017$income) ## there are 13 "-" values
rural_nc_ct_income_2017["income"][rural_nc_ct_income_2017["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2017$income)) ## 13 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2017$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2017$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2017$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2017$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2016 = apply_labels(rural_nc_ct_income_2016,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2016$households <- rural_nc_ct_income_2016$S1901_C01_001E
rural_nc_ct_income_2016$income <- rural_nc_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2016 = rural_nc_ct_income_2016[-1,]

# identify NA values
table(rural_nc_ct_income_2016$income) ## there are 11 "-" values
rural_nc_ct_income_2016["income"][rural_nc_ct_income_2016["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2016$income)) ## 11 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2016$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2016$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2016$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2016$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2015 = apply_labels(rural_nc_ct_income_2015,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2015$households <- rural_nc_ct_income_2015$S1901_C01_001E
rural_nc_ct_income_2015$income <- rural_nc_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2015 = rural_nc_ct_income_2015[-1,]

# identify NA values
table(rural_nc_ct_income_2015$income) ## there are 10 "-" values
rural_nc_ct_income_2015["income"][rural_nc_ct_income_2015["income"] == "-"] <- NA
# make "(X)" values NA
rural_nc_ct_income_2015["income"][rural_nc_ct_income_2015["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2015$income)) ## 17 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2015$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2015$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2015$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2015$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2014 = apply_labels(rural_nc_ct_income_2014,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2014$households <- rural_nc_ct_income_2014$S1901_C01_001E
rural_nc_ct_income_2014$income <- rural_nc_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2014 = rural_nc_ct_income_2014[-1,]

# identify NA values
table(rural_nc_ct_income_2014$income) ## there are 10 "-" values
rural_nc_ct_income_2014["income"][rural_nc_ct_income_2014["income"] == "-"] <- NA
# make "(X)" values NA
rural_nc_ct_income_2014["income"][rural_nc_ct_income_2014["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2014$income)) ## 10 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2014$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2014$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2014$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2014$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2013 = apply_labels(rural_nc_ct_income_2013,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2013$households <- rural_nc_ct_income_2013$S1901_C01_001E
rural_nc_ct_income_2013$income <- rural_nc_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2013 = rural_nc_ct_income_2013[-1,]

# identify NA values
table(rural_nc_ct_income_2013$income) ## there are 10 "-" values
rural_nc_ct_income_2013["income"][rural_nc_ct_income_2013["income"] == "-"] <- NA
# make "(X)" values NA
rural_nc_ct_income_2013["income"][rural_nc_ct_income_2013["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2013$income)) ## 10 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2013$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2013$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2013$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2013$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2012 = apply_labels(rural_nc_ct_income_2012,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2012$households <- rural_nc_ct_income_2012$S1901_C01_001E
rural_nc_ct_income_2012$income <- rural_nc_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2012 = rural_nc_ct_income_2012[-1,]

# identify NA values
table(rural_nc_ct_income_2012$income) ## there are 10 "-" values
rural_nc_ct_income_2012["income"][rural_nc_ct_income_2012["income"] == "-"] <- NA
# make "(X)" values NA
rural_nc_ct_income_2012["income"][rural_nc_ct_income_2012["income"] == "(X)"] <- NA
# make "2,500-" values NA
rural_nc_ct_income_2012["income"][rural_nc_ct_income_2012["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2012$income)) ## 10 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2012$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2012$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2012$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2012$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2011 = apply_labels(rural_nc_ct_income_2011,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2011$households <- rural_nc_ct_income_2011$S1901_C01_001E
rural_nc_ct_income_2011$income <- rural_nc_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2011 = rural_nc_ct_income_2011[-1,]

# identify NA values
table(rural_nc_ct_income_2011$income) ## there are 10 "-" values
rural_nc_ct_income_2011["income"][rural_nc_ct_income_2011["income"] == "-"] <- NA
# make "(X)" values NA
rural_nc_ct_income_2011["income"][rural_nc_ct_income_2011["income"] == "(X)"] <- NA
# make "2,500-" values NA
rural_nc_ct_income_2011["income"][rural_nc_ct_income_2011["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2011$income)) ## 11 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2011$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2011$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2011$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2011$income))

###############################################################################################################
## North Carolina rural census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
rural_nc_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-rural-nc.csv")


# label and rename variables so it's easier for me
rural_nc_ct_income_2010 = apply_labels(rural_nc_ct_income_2010,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_nc_ct_income_2010$households <- rural_nc_ct_income_2010$S1901_C01_001E
rural_nc_ct_income_2010$income <- rural_nc_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
rural_nc_ct_income_2010 = rural_nc_ct_income_2010[-1,]

# identify NA values
table(rural_nc_ct_income_2010$income) ## there are 10 "-" values
rural_nc_ct_income_2010["income"][rural_nc_ct_income_2010["income"] == "-"] <- NA
# make "(X)" values NA
rural_nc_ct_income_2010["income"][rural_nc_ct_income_2010["income"] == "(X)"] <- NA
# make "2,500-" values NA
rural_nc_ct_income_2010["income"][rural_nc_ct_income_2010["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(rural_nc_ct_income_2010$income)) ## 11 CTs with missing data out of 512

# understand the distribution at this one time period
range(as.numeric(rural_nc_ct_income_2010$income), na.rm=T)
mean(as.numeric(rural_nc_ct_income_2010$income), na.rm=T)
median(as.numeric(rural_nc_ct_income_2010$income), na.rm=T)
hist(as.numeric(rural_nc_ct_income_2010$income))

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Rural North Carolina census tracts -- percent change in average median household income
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 
joined_df_1_rural_nc <- rural_nc_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(rural_nc_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_rural_nc$pct_change <- (as.numeric(joined_df_1_rural_nc$income.x)-as.numeric(joined_df_1_rural_nc$income.y))/as.numeric(joined_df_1_rural_nc$income.y)*100

# make histogram
hist(joined_df_1_rural_nc$pct_change)
hist(joined_df_1_rural_nc$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=40,xlim=c(-35, 60), xaxp=c(-40,60,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_rural_nc <- rural_nc_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(rural_nc_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_rural_nc$pct_change <- (as.numeric(joined_df_2_rural_nc$income.x)-as.numeric(joined_df_2_rural_nc$income.y))/as.numeric(joined_df_2_rural_nc$income.y)*100

# make histogram
hist(joined_df_2_rural_nc$pct_change)
hist_rural_nc_2020_2010 <- hist(joined_df_2_rural_nc$pct_change, main="Percent Change in Household Income between 2020 and 2010, Rural North Carolina", xlab="% change",breaks=30,xlim=c(-50, 100), xaxp=c(-50,100,15))

###############################################################################################################
## Ohio rural census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-rural-ohio.csv")

library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
rural_ohio_ct_income_2021 = apply_labels(rural_ohio_ct_income_2021,
                                       S1901_C01_001E = "Total number of households",
                                       S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2021$households <- rural_ohio_ct_income_2021$S1901_C01_001E
rural_ohio_ct_income_2021$income <- rural_ohio_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2021 = rural_ohio_ct_income_2021[-1,]

# identify NA values
table(rural_ohio_ct_income_2021$income) ## there are 6 "-" values
rural_ohio_ct_income_2021["income"][rural_ohio_ct_income_2021["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2021$income)) ## 6 CTs with missing data out of 629

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2021$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2021$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2021$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2021$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2020 = apply_labels(rural_ohio_ct_income_2020,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2020$households <- rural_ohio_ct_income_2020$S1901_C01_001E
rural_ohio_ct_income_2020$income <- rural_ohio_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2020 = rural_ohio_ct_income_2020[-1,]

# identify NA values
table(rural_ohio_ct_income_2020$income) ## there are 4 "-" values
rural_ohio_ct_income_2020["income"][rural_ohio_ct_income_2020["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2020$income)) ## 4 CTs with missing data out of 629

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2020$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2020$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2020$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2020$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2019 = apply_labels(rural_ohio_ct_income_2019,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2019$households <- rural_ohio_ct_income_2019$S1901_C01_001E
rural_ohio_ct_income_2019$income <- rural_ohio_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2019 = rural_ohio_ct_income_2019[-1,]

# identify NA values
table(rural_ohio_ct_income_2019$income) ## there are 3 "-" values
rural_ohio_ct_income_2019["income"][rural_ohio_ct_income_2019["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2019$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2019$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2019$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2019$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2019$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2018 = apply_labels(rural_ohio_ct_income_2018,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2018$households <- rural_ohio_ct_income_2018$S1901_C01_001E
rural_ohio_ct_income_2018$income <- rural_ohio_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2018 = rural_ohio_ct_income_2018[-1,]

# identify NA values
table(rural_ohio_ct_income_2018$income) ## there are 3 "-" values
rural_ohio_ct_income_2018["income"][rural_ohio_ct_income_2018["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2018$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2018$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2018$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2018$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2018$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2017 = apply_labels(rural_ohio_ct_income_2017,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2017$households <- rural_ohio_ct_income_2017$S1901_C01_001E
rural_ohio_ct_income_2017$income <- rural_ohio_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2017 = rural_ohio_ct_income_2017[-1,]

# identify NA values
table(rural_ohio_ct_income_2017$income) ## there are 3 "-" values
rural_ohio_ct_income_2017["income"][rural_ohio_ct_income_2017["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2017$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2017$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2017$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2017$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2017$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2016 = apply_labels(rural_ohio_ct_income_2016,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2016$households <- rural_ohio_ct_income_2016$S1901_C01_001E
rural_ohio_ct_income_2016$income <- rural_ohio_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2016 = rural_ohio_ct_income_2016[-1,]

# identify NA values
table(rural_ohio_ct_income_2016$income) ## there are 3 "-" values
rural_ohio_ct_income_2016["income"][rural_ohio_ct_income_2016["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2016$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2016$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2016$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2016$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2016$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2015 = apply_labels(rural_ohio_ct_income_2015,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2015$households <- rural_ohio_ct_income_2015$S1901_C01_001E
rural_ohio_ct_income_2015$income <- rural_ohio_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2015 = rural_ohio_ct_income_2015[-1,]

# identify NA values
table(rural_ohio_ct_income_2015$income) ## there are 3 "-" values
rural_ohio_ct_income_2015["income"][rural_ohio_ct_income_2015["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2015$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2015$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2015$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2015$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2015$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2014 = apply_labels(rural_ohio_ct_income_2014,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2014$households <- rural_ohio_ct_income_2014$S1901_C01_001E
rural_ohio_ct_income_2014$income <- rural_ohio_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2014 = rural_ohio_ct_income_2014[-1,]

# identify NA values
table(rural_ohio_ct_income_2014$income) ## there are 3 "-" values
rural_ohio_ct_income_2014["income"][rural_ohio_ct_income_2014["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2014$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2014$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2014$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2014$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2014$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2013 = apply_labels(rural_ohio_ct_income_2013,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2013$households <- rural_ohio_ct_income_2013$S1901_C01_001E
rural_ohio_ct_income_2013$income <- rural_ohio_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2013 = rural_ohio_ct_income_2013[-1,]

# identify NA values
table(rural_ohio_ct_income_2013$income) ## there are 3 "-" values
rural_ohio_ct_income_2013["income"][rural_ohio_ct_income_2013["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2013$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2013$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2013$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2013$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2013$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2012 = apply_labels(rural_ohio_ct_income_2012,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2012$households <- rural_ohio_ct_income_2012$S1901_C01_001E
rural_ohio_ct_income_2012$income <- rural_ohio_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2012 = rural_ohio_ct_income_2012[-1,]

# identify NA values
table(rural_ohio_ct_income_2012$income) ## there are 3 "-" values
rural_ohio_ct_income_2012["income"][rural_ohio_ct_income_2012["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2012$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2012$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2012$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2012$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2012$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2011 = apply_labels(rural_ohio_ct_income_2011,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2011$households <- rural_ohio_ct_income_2011$S1901_C01_001E
rural_ohio_ct_income_2011$income <- rural_ohio_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2011 = rural_ohio_ct_income_2011[-1,]

# identify NA values
table(rural_ohio_ct_income_2011$income) ## there are 3 "-" values
rural_ohio_ct_income_2011["income"][rural_ohio_ct_income_2011["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2011$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2011$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2011$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2011$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2011$income))

###############################################################################################################
## Ohio rural census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
rural_ohio_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-rural-ohio.csv")

# label and rename variables so it's easier for me
rural_ohio_ct_income_2010 = apply_labels(rural_ohio_ct_income_2010,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_ohio_ct_income_2010$households <- rural_ohio_ct_income_2010$S1901_C01_001E
rural_ohio_ct_income_2010$income <- rural_ohio_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
rural_ohio_ct_income_2010 = rural_ohio_ct_income_2010[-1,]

# identify NA values
table(rural_ohio_ct_income_2010$income) ## there are 3 "-" values
rural_ohio_ct_income_2010["income"][rural_ohio_ct_income_2010["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_ohio_ct_income_2010$income)) ## 3 CTs with missing data out of 591

# understand the distribution at this one time period
range(as.numeric(rural_ohio_ct_income_2010$income), na.rm=T)
mean(as.numeric(rural_ohio_ct_income_2010$income), na.rm=T)
median(as.numeric(rural_ohio_ct_income_2010$income), na.rm=T)
hist(as.numeric(rural_ohio_ct_income_2010$income))

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Rural Ohio census tracts -- percent change in average median household income
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 
joined_df_1_rural_ohio <- rural_ohio_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(rural_ohio_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_rural_ohio$pct_change <- (as.numeric(joined_df_1_rural_ohio$income.x)-as.numeric(joined_df_1_rural_ohio$income.y))/as.numeric(joined_df_1_rural_ohio$income.y)*100

# make histogram
hist(joined_df_1_rural_ohio$pct_change)
hist(joined_df_1_rural_ohio$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=40,xlim=c(-35, 60), xaxp=c(-40,60,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_rural_ohio <- rural_ohio_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(rural_ohio_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_rural_ohio$pct_change <- (as.numeric(joined_df_2_rural_ohio$income.x)-as.numeric(joined_df_2_rural_ohio$income.y))/as.numeric(joined_df_2_rural_ohio$income.y)*100

# make histogram
hist(joined_df_2_rural_ohio$pct_change)
hist_rural_ohio_2020_2010 <- hist(joined_df_2_rural_ohio$pct_change, main="Percent Change in Household Income between 2020 and 2010, Rural North Carolina", xlab="% change",breaks=30,xlim=c(-50, 100), xaxp=c(-50,100,15))

###############################################################################################################
## Montana rural census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-rural-montana.csv")

library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
rural_montana_ct_income_2021 = apply_labels(rural_montana_ct_income_2021,
                                         S1901_C01_001E = "Total number of households",
                                         S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2021$households <- rural_montana_ct_income_2021$S1901_C01_001E
rural_montana_ct_income_2021$income <- rural_montana_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2021 = rural_montana_ct_income_2021[-1,]

# identify NA values
table(rural_montana_ct_income_2021$income) ## there are 3 "-" values
rural_montana_ct_income_2021["income"][rural_montana_ct_income_2021["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2021$income)) ## 3 CTs with missing data out of 229

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2021$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2021$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2021$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2021$income))

###############################################################################################################
## Montana rural census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2020 = apply_labels(rural_montana_ct_income_2020,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2020$households <- rural_montana_ct_income_2020$S1901_C01_001E
rural_montana_ct_income_2020$income <- rural_montana_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2020 = rural_montana_ct_income_2020[-1,]

# identify NA values
table(rural_montana_ct_income_2020$income) ## there are 2 "-" values
rural_montana_ct_income_2020["income"][rural_montana_ct_income_2020["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2020$income)) ## 2 CTs with missing data out of 229

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2020$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2020$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2020$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2020$income))

###############################################################################################################
## Montana rural census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2019 = apply_labels(rural_montana_ct_income_2019,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2019$households <- rural_montana_ct_income_2019$S1901_C01_001E
rural_montana_ct_income_2019$income <- rural_montana_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2019 = rural_montana_ct_income_2019[-1,]

# identify NA values
table(rural_montana_ct_income_2019$income) ## there are 2 "-" values
rural_montana_ct_income_2019["income"][rural_montana_ct_income_2019["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2019$income)) ## 2 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2019$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2019$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2019$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2019$income))

###############################################################################################################
## Montana rural census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2018 = apply_labels(rural_montana_ct_income_2018,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2018$households <- rural_montana_ct_income_2018$S1901_C01_001E
rural_montana_ct_income_2018$income <- rural_montana_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2018 = rural_montana_ct_income_2018[-1,]

# identify NA values
table(rural_montana_ct_income_2018$income) ## there are 2 "-" values
rural_montana_ct_income_2018["income"][rural_montana_ct_income_2018["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2018$income)) ## 2 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2018$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2018$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2018$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2018$income))

###############################################################################################################
## Montana rural census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2017 = apply_labels(rural_montana_ct_income_2017,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2017$households <- rural_montana_ct_income_2017$S1901_C01_001E
rural_montana_ct_income_2017$income <- rural_montana_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2017 = rural_montana_ct_income_2017[-1,]

# identify NA values
table(rural_montana_ct_income_2017$income) ## there are 3 "-" values
rural_montana_ct_income_2017["income"][rural_montana_ct_income_2017["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2017$income)) ## 3 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2017$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2017$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2017$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2017$income))

###############################################################################################################
## Montana rural census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2016 = apply_labels(rural_montana_ct_income_2016,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2016$households <- rural_montana_ct_income_2016$S1901_C01_001E
rural_montana_ct_income_2016$income <- rural_montana_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2016 = rural_montana_ct_income_2016[-1,]

# identify NA values
table(rural_montana_ct_income_2016$income) ## there are 3 "-" values
rural_montana_ct_income_2016["income"][rural_montana_ct_income_2016["income"] == "-"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2016$income)) ## 3 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2016$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2016$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2016$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2016$income))

###############################################################################################################
## Montana rural census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2015 = apply_labels(rural_montana_ct_income_2015,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2015$households <- rural_montana_ct_income_2015$S1901_C01_001E
rural_montana_ct_income_2015$income <- rural_montana_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2015 = rural_montana_ct_income_2015[-1,]

# identify NA values
table(rural_montana_ct_income_2015$income) ## there are 2 "-" values
rural_montana_ct_income_2015["income"][rural_montana_ct_income_2015["income"] == "-"] <- NA
rural_montana_ct_income_2015["income"][rural_montana_ct_income_2015["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2015$income)) ## 3 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2015$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2015$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2015$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2015$income))

###############################################################################################################
## Montana rural census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2014 = apply_labels(rural_montana_ct_income_2014,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2014$households <- rural_montana_ct_income_2014$S1901_C01_001E
rural_montana_ct_income_2014$income <- rural_montana_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2014 = rural_montana_ct_income_2014[-1,]

# identify NA values
table(rural_montana_ct_income_2014$income) ## there are 2 "-" values
rural_montana_ct_income_2014["income"][rural_montana_ct_income_2014["income"] == "-"] <- NA
rural_montana_ct_income_2014["income"][rural_montana_ct_income_2014["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2014$income)) ## 2 CTs with missing data out of 193

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2014$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2014$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2014$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2014$income))

###############################################################################################################
## Montana rural census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2013 = apply_labels(rural_montana_ct_income_2013,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2013$households <- rural_montana_ct_income_2013$S1901_C01_001E
rural_montana_ct_income_2013$income <- rural_montana_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2013 = rural_montana_ct_income_2013[-1,]

# identify NA values
table(rural_montana_ct_income_2013$income) ## there are 2 "-" values
rural_montana_ct_income_2013["income"][rural_montana_ct_income_2013["income"] == "-"] <- NA
rural_montana_ct_income_2013["income"][rural_montana_ct_income_2013["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2013$income)) ## 2 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2013$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2013$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2013$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2013$income))

###############################################################################################################
## Montana rural census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2012 = apply_labels(rural_montana_ct_income_2012,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2012$households <- rural_montana_ct_income_2012$S1901_C01_001E
rural_montana_ct_income_2012$income <- rural_montana_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2012 = rural_montana_ct_income_2012[-1,]

# identify NA values
table(rural_montana_ct_income_2012$income) ## there are 2 "-" values
rural_montana_ct_income_2012["income"][rural_montana_ct_income_2012["income"] == "-"] <- NA
rural_montana_ct_income_2012["income"][rural_montana_ct_income_2012["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2012$income)) ## 2 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2012$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2012$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2012$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2012$income))

###############################################################################################################
## Montana rural census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2011 = apply_labels(rural_montana_ct_income_2011,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2011$households <- rural_montana_ct_income_2011$S1901_C01_001E
rural_montana_ct_income_2011$income <- rural_montana_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2011 = rural_montana_ct_income_2011[-1,]

# identify NA values
table(rural_montana_ct_income_2011$income) ## there are 2 "-" values
rural_montana_ct_income_2011["income"][rural_montana_ct_income_2011["income"] == "-"] <- NA
rural_montana_ct_income_2011["income"][rural_montana_ct_income_2011["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2011$income)) ## 2 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2011$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2011$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2011$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2011$income))

###############################################################################################################
## Montana rural census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
rural_montana_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-rural-montana.csv")

# label and rename variables so it's easier for me
rural_montana_ct_income_2010 = apply_labels(rural_montana_ct_income_2010,
                                            S1901_C01_001E = "Total number of households",
                                            S1901_C01_012E = "Median household income ($)")
rural_montana_ct_income_2010$households <- rural_montana_ct_income_2010$S1901_C01_001E
rural_montana_ct_income_2010$income <- rural_montana_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
rural_montana_ct_income_2010 = rural_montana_ct_income_2010[-1,]

# identify NA values
table(rural_montana_ct_income_2010$income) ## there are 2 "-" values
rural_montana_ct_income_2010["income"][rural_montana_ct_income_2010["income"] == "-"] <- NA
rural_montana_ct_income_2010["income"][rural_montana_ct_income_2010["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(rural_montana_ct_income_2010$income)) ## 2 CTs with missing data out of 197

# understand the distribution at this one time period
range(as.numeric(rural_montana_ct_income_2010$income), na.rm=T)
mean(as.numeric(rural_montana_ct_income_2010$income), na.rm=T)
median(as.numeric(rural_montana_ct_income_2010$income), na.rm=T)
hist(as.numeric(rural_montana_ct_income_2010$income))

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Rural Montana census tracts -- percent change in average median household income
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 
library(dplyr)
joined_df_1_rural_montana <- rural_montana_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(rural_montana_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_rural_montana$pct_change <- (as.numeric(joined_df_1_rural_montana$income.x)-as.numeric(joined_df_1_rural_montana$income.y))/as.numeric(joined_df_1_rural_montana$income.y)*100

# make histogram
hist(joined_df_1_rural_montana$pct_change)
hist(joined_df_1_rural_montana$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=40,xlim=c(-35, 60), xaxp=c(-40,60,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_rural_montana <- rural_montana_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(rural_montana_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_rural_montana$pct_change <- (as.numeric(joined_df_2_rural_montana$income.x)-as.numeric(joined_df_2_rural_montana$income.y))/as.numeric(joined_df_2_rural_montana$income.y)*100

# make histogram
hist(joined_df_2_rural_montana$pct_change)
hist_rural_montana_2020_2010 <- hist(joined_df_2_rural_montana$pct_change, main="Percent Change in Household Income between 2020 and 2010, Rural North Carolina", xlab="% change",breaks=30,xlim=c(-50, 100), xaxp=c(-50,100,15))

################################################################################
# adding in RUCA code CT data for urban/rural instead of using US census regions
################################################################################

library(readxl)
library(dplyr)
ruca2010revised <- read_excel("ruca2010revised.xlsx", 
                              sheet = "Data")

# select out only RUCA codes 7-10 for rural CTs
ruca2010revised_rural <- filter(ruca2010revised, `Primary RUCA Code 2010` %in%  c(7, 8, 9, 10))

#rename state-county FIPS variable
ruca2010revised_rural <- ruca2010revised_rural %>% 
  rename("FIPS" = "State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)")

# we need to match GEOID variable in the US datasets to state-county-tract FIPS code in a join
# join the dataset with the income information to the dataset with the FIPS code
# load the data
# load packages
us_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-us-ct.csv")

library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
us_ct_income_2021 = apply_labels(us_ct_income_2021,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2021$households <- us_ct_income_2021$S1901_C01_001E
us_ct_income_2021$income <- us_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset and select out only the GEOID, households, and income variable -- make it more clean
us_ct_income_2021 = us_ct_income_2021[-1,]
us_ct_income_2021 <- us_ct_income_2021 %>%
  select(GEO_ID, households, income)

# identify NA values
table(us_ct_income_2021$income) ## there are 8537 "-" values
us_ct_income_2021["income"][us_ct_income_2021["income"] == "-"] <- NA
us_ct_income_2021["income"][us_ct_income_2021["income"] == "(X)"] <- NA
us_ct_income_2021["income"][us_ct_income_2021["income"] == "2,500-"] <- NA
us_ct_income_2021["income"][us_ct_income_2021["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2021$income)) ## 1875 CTs with missing data out of 85395

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2021$income), na.rm=T)
mean(as.numeric(us_ct_income_2021$income), na.rm=T)
median(as.numeric(us_ct_income_2021$income), na.rm=T)
hist(as.numeric(us_ct_income_2021$income))

# change GEO_ID variable to match FIPS code
# Replace first 9 characters with empty string ""
us_ct_income_2021$GEO_ID <- gsub("^.{0,9}", "", us_ct_income_2021$GEO_ID)
#rename "GEO_ID" variable to "FIPS"
us_ct_income_2021 <- us_ct_income_2021 %>% 
  rename("FIPS" = "GEO_ID")

# now join the two datasets on the FIPS variables
ruca_rural_2021 <- us_ct_income_2021 %>% select(FIPS, income) %>% inner_join(ruca2010revised_rural %>% select(FIPS), by=c('FIPS'='FIPS'))
# ended up with 5,833 CTs that matched from 6,796
# how many NA values?
sum(is.na(ruca_rural_2021$income)) ## 339 CTs with missing data out of 5,833
hist(as.numeric(ruca_rural_2021$income))

## now do 2020
# load the data
# load packages
us_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2020 = apply_labels(us_ct_income_2020,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2020$households <- us_ct_income_2020$S1901_C01_001E
us_ct_income_2020$income <- us_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset and select out only the GEOID, households, and income variable -- make it more clean
us_ct_income_2020 = us_ct_income_2020[-1,]
us_ct_income_2020 <- us_ct_income_2020 %>%
  select(GEO_ID, households, income)

# identify NA values
table(us_ct_income_2020$income) ## there are 1571 "-" values
us_ct_income_2020["income"][us_ct_income_2020["income"] == "-"] <- NA
us_ct_income_2020["income"][us_ct_income_2020["income"] == "(X)"] <- NA
us_ct_income_2020["income"][us_ct_income_2020["income"] == "2,500-"] <- NA
us_ct_income_2020["income"][us_ct_income_2020["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2020$income)) ## 1764 CTs with missing data out of 85395

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2020$income), na.rm=T)
mean(as.numeric(us_ct_income_2020$income), na.rm=T)
median(as.numeric(us_ct_income_2020$income), na.rm=T)
hist(as.numeric(us_ct_income_2020$income))

# change GEO_ID variable to match FIPS code
# Replace first 9 characters with empty string ""
us_ct_income_2020$GEO_ID <- gsub("^.{0,9}", "", us_ct_income_2020$GEO_ID)
#rename "GEO_ID" variable to "FIPS"
us_ct_income_2020 <- us_ct_income_2020 %>% 
  rename("FIPS" = "GEO_ID")

# now join the two datasets on the FIPS variables
ruca_rural_2020 <- us_ct_income_2020 %>% select(FIPS, income) %>% inner_join(ruca2010revised_rural %>% select(FIPS), by=c('FIPS'='FIPS'))
# ended up with 5,833 CTs that matched from 6,796
# how many NA values?
sum(is.na(ruca_rural_2020$income)) ## 333 CTs with missing data out of 5,833

# now join together 2021 and 2020 to calculate % change
library(dplyr)
joined_ruca_rural_1 <- ruca_rural_2021 %>% select(FIPS, income) %>% inner_join(ruca_rural_2020 %>% select(FIPS, income), by=c('FIPS'='FIPS'))
#rename income variables
joined_ruca_rural_1 <- joined_ruca_rural_1 %>% 
  rename("income_2020" = "income.y")
joined_ruca_rural_1 <- joined_ruca_rural_1 %>% 
  rename("income_2021" = "income.x")

# create percent change variable (between 2021 and 2020)
joined_ruca_rural_1$pct_change <- (as.numeric(joined_ruca_rural_1$income_2021)-as.numeric(joined_ruca_rural_1$income_2020))/as.numeric(joined_ruca_rural_1$income_2020)*100

# make histogram
hist(joined_ruca_rural_1$pct_change)
hist_rural_2021_2020 <- hist(joined_ruca_rural_1$pct_change, 
                          main="Percent Change in Household Income between 2021 and 2020, Rural Census Tracts",
                          xlab="% change",
                          breaks=70,
                          xlim=c(-30, 50),
                          xaxp=c(-30,50,16)) #most around 5% change in median income

## now let's do 2020 vs. 2010
## start by adding in 2010
# load the data
# load packages
us_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2010 = apply_labels(us_ct_income_2010,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2010$households <- us_ct_income_2010$S1901_C01_001E
us_ct_income_2010$income <- us_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset and select out only the GEOID, households, and income variable -- make it more clean
us_ct_income_2010 = us_ct_income_2010[-1,]
us_ct_income_2010 <- us_ct_income_2010 %>%
  select(GEO_ID, households, income)

# identify NA values
table(us_ct_income_2010$income) ## there are 975 "-" values
us_ct_income_2010["income"][us_ct_income_2010["income"] == "-"] <- NA
us_ct_income_2010["income"][us_ct_income_2010["income"] == "(X)"] <- NA
us_ct_income_2010["income"][us_ct_income_2010["income"] == "2,500-"] <- NA
us_ct_income_2010["income"][us_ct_income_2010["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2010$income)) ## 1014 CTs with missing data out of 74002

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2010$income), na.rm=T)
mean(as.numeric(us_ct_income_2010$income), na.rm=T)
median(as.numeric(us_ct_income_2010$income), na.rm=T)
hist(as.numeric(us_ct_income_2010$income))

# change GEO_ID variable to match FIPS code
# Replace first 9 characters with empty string ""
us_ct_income_2010$GEO_ID <- gsub("^.{0,9}", "", us_ct_income_2010$GEO_ID)
#rename "GEO_ID" variable to "FIPS"
us_ct_income_2010 <- us_ct_income_2010 %>% 
  rename("FIPS" = "GEO_ID")

# now join the two datasets on the FIPS variables
ruca_rural_2010 <- us_ct_income_2010 %>% select(FIPS, income) %>% inner_join(ruca2010revised_rural %>% select(FIPS), by=c('FIPS'='FIPS'))
# ended up with 6,796 CTs that matched from 6,796 (really close because the RUCA codes are from 2010--they should all match!!)
# how many NA values?
sum(is.na(ruca_rural_2010$income)) ## 330 CTs with missing data out of 6,796

# now join together 2020 and 2010 to calculate % change
library(dplyr)
joined_ruca_rural_2 <- ruca_rural_2020 %>% select(FIPS, income) %>% inner_join(ruca_rural_2010 %>% select(FIPS, income), by=c('FIPS'='FIPS'))
#rename income variables
joined_ruca_rural_2 <- joined_ruca_rural_2 %>% 
  rename("income_2010" = "income.y")
joined_ruca_rural_2 <- joined_ruca_rural_2 %>% 
  rename("income_2020" = "income.x")

# create percent change variable (between 2021 and 2020)
joined_ruca_rural_2$pct_change <- (as.numeric(joined_ruca_rural_2$income_2020)-as.numeric(joined_ruca_rural_2$income_2010))/as.numeric(joined_ruca_rural_2$income_2010)*100

# make histogram
hist(joined_ruca_rural_2$pct_change)
hist_rural_2020_2010 <- hist(joined_ruca_rural_2$pct_change, 
                          main="Percent Change in Household Income between 2020 and 2010, Rural Census Tracts",
                          xlab="% change",
                          xlim=c(-100,200),
                          breaks=50,
                          xaxp=c(-100,200,15)) #most between 20-30% change in median income





























