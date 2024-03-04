###############################################################################################################
# exploring changes in measures over time among urban census tracts -- ACS 5-year estimates
###############################################################################################################


###############################################################################################################
## Philly census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
philly_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-Philly.csv")

# load packages
library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
philly_ct_income_2021 = apply_labels(philly_ct_income_2021,
                      S1901_C01_001E = "Total number of households",
                      S1901_C01_012E = "Median household income ($)")
philly_ct_income_2021$households <- philly_ct_income_2021$S1901_C01_001E
philly_ct_income_2021$income <- philly_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2021 = philly_ct_income_2021[-1,]

# identify NA values
table(philly_ct_income_2021$income) ## there are 65 "-" values
philly_ct_income_2021["income"][philly_ct_income_2021["income"] == "-"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2021$income)) ## 65 CTs with missing data out of 408

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2021$income), na.rm=T)
mean(as.numeric(philly_ct_income_2021$income), na.rm=T)
median(as.numeric(philly_ct_income_2021$income), na.rm=T)
hist(as.numeric(philly_ct_income_2021$income), na.rm=T)




###############################################################################################################
## Philly census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

philly_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2020 = apply_labels(philly_ct_income_2020,
                                S1901_C01_001E = "Total number of households",
                                S1901_C01_012E = "Median household income ($)")
philly_ct_income_2020$households <- philly_ct_income_2020$S1901_C01_001E
philly_ct_income_2020$income <- philly_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2020 = philly_ct_income_2020[-1,]

# identify NA values
table(philly_ct_income_2020$income) ## there are "-" values
philly_ct_income_2020["income"][philly_ct_income_2020["income"] == "-"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2020$income)) ## 54 CTs with missing data out of 408

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2020$income), na.rm=T)
mean(as.numeric(philly_ct_income_2020$income), na.rm=T)
median(as.numeric(philly_ct_income_2020$income), na.rm=T)
hist(as.numeric(philly_ct_income_2020$income))


###############################################################################################################
## Philly census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

philly_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2019 = apply_labels(philly_ct_income_2019,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2019$households <- philly_ct_income_2019$S1901_C01_001E
philly_ct_income_2019$income <- philly_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2019 = philly_ct_income_2019[-1,]

# identify NA values
table(philly_ct_income_2019$income) ## there are "-" values
philly_ct_income_2019["income"][philly_ct_income_2019["income"] == "-"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2019$income)) ## 28 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2019$income), na.rm=T)
mean(as.numeric(philly_ct_income_2019$income), na.rm=T) 
median(as.numeric(philly_ct_income_2019$income), na.rm=T)
hist(as.numeric(philly_ct_income_2019$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

philly_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2018 = apply_labels(philly_ct_income_2018,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2018$households <- philly_ct_income_2018$S1901_C01_001E
philly_ct_income_2018$income <- philly_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2018 = philly_ct_income_2018[-1,]

# identify NA values
table(philly_ct_income_2018$income) ## there are "-" values
philly_ct_income_2018["income"][philly_ct_income_2018["income"] == "-"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2018$income)) ## 20 CTs with missing data out of 385

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2018$income), na.rm=T)
mean(as.numeric(philly_ct_income_2018$income), na.rm=T) 
median(as.numeric(philly_ct_income_2018$income), na.rm=T)
hist(as.numeric(philly_ct_income_2018$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

philly_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2017 = apply_labels(philly_ct_income_2017,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2017$households <- philly_ct_income_2017$S1901_C01_001E
philly_ct_income_2017$income <- philly_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2017 = philly_ct_income_2017[-1,]

# identify NA values
table(philly_ct_income_2017$income) ## there are "-" values
philly_ct_income_2017["income"][philly_ct_income_2017["income"] == "-"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2017$income)) ## 20 CTs with missing data out of 385

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2017$income), na.rm=T)
mean(as.numeric(philly_ct_income_2017$income), na.rm=T) 
median(as.numeric(philly_ct_income_2017$income), na.rm=T)
hist(as.numeric(philly_ct_income_2017$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

philly_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2016 = apply_labels(philly_ct_income_2016,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2016$households <- philly_ct_income_2016$S1901_C01_001E
philly_ct_income_2016$income <- philly_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2016 = philly_ct_income_2016[-1,]

# identify NA values
table(philly_ct_income_2016$income) ## there are "-" values
philly_ct_income_2016["income"][philly_ct_income_2016["income"] == "-"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2016$income)) ## 18 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2016$income), na.rm=T)
mean(as.numeric(philly_ct_income_2016$income), na.rm=T) 
median(as.numeric(philly_ct_income_2016$income), na.rm=T)
hist(as.numeric(philly_ct_income_2016$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

philly_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2015 = apply_labels(philly_ct_income_2015,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2015$households <- philly_ct_income_2015$S1901_C01_001E
philly_ct_income_2015$income <- philly_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2015 = philly_ct_income_2015[-1,]

# identify NA values
table(philly_ct_income_2015$income) ## there are 7 "(X)" values
philly_ct_income_2015["income"][philly_ct_income_2015["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2015$income)) ## 16 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2015$income), na.rm=T)
mean(as.numeric(philly_ct_income_2015$income), na.rm=T) 
median(as.numeric(philly_ct_income_2015$income), na.rm=T)
hist(as.numeric(philly_ct_income_2015$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

philly_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2014 = apply_labels(philly_ct_income_2014,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2014$households <- philly_ct_income_2014$S1901_C01_001E
philly_ct_income_2014$income <- philly_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2014 = philly_ct_income_2014[-1,]

# identify NA values
table(philly_ct_income_2014$income) ## there are 7 "(X)" values
philly_ct_income_2014["income"][philly_ct_income_2014["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(philly_ct_income_2014$income)) ## 16 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2014$income), na.rm=T)
mean(as.numeric(philly_ct_income_2014$income), na.rm=T) 
median(as.numeric(philly_ct_income_2014$income), na.rm=T)
hist(as.numeric(philly_ct_income_2014$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

philly_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2013 = apply_labels(philly_ct_income_2013,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2013$households <- philly_ct_income_2013$S1901_C01_001E
philly_ct_income_2013$income <- philly_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2013 = philly_ct_income_2013[-1,]

# identify NA values
table(philly_ct_income_2013$income) ## there are 9 "-" values
philly_ct_income_2013["income"][philly_ct_income_2013["income"] == "-"] <- NA

# there is one value of"2,500-", probably a typo, replace with "NA"
philly_ct_income_2013["income"][philly_ct_income_2013["income"] == "2,500-"] <- NA

# confirm -- how many NA values?
sum(is.na(philly_ct_income_2013$income)) ## 9 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2013$income), na.rm=T)
mean(as.numeric(philly_ct_income_2013$income), na.rm=T) 
median(as.numeric(philly_ct_income_2013$income), na.rm=T)
hist(as.numeric(philly_ct_income_2013$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

philly_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2012 = apply_labels(philly_ct_income_2012,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2012$households <- philly_ct_income_2012$S1901_C01_001E
philly_ct_income_2012$income <- philly_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2012 = philly_ct_income_2012[-1,]

# identify NA values
table(philly_ct_income_2012$income) ## there are 10 "-" values
philly_ct_income_2012["income"][philly_ct_income_2012["income"] == "-"] <- NA

# there is one value of"2,500-", probably a typo, replace with "NA"
philly_ct_income_2012["income"][philly_ct_income_2012["income"] == "2,500-"] <- NA

# confirm -- how many NA values?
sum(is.na(philly_ct_income_2012$income)) ## 10 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2012$income), na.rm=T)
mean(as.numeric(philly_ct_income_2012$income), na.rm=T) 
median(as.numeric(philly_ct_income_2012$income), na.rm=T)
hist(as.numeric(philly_ct_income_2012$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

philly_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2011 = apply_labels(philly_ct_income_2011,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2011$households <- philly_ct_income_2011$S1901_C01_001E
philly_ct_income_2011$income <- philly_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2011 = philly_ct_income_2011[-1,]

# identify NA values
table(philly_ct_income_2011$income) ## there are 11 "-" values
philly_ct_income_2011["income"][philly_ct_income_2011["income"] == "-"] <- NA

# there is one value of"2,500-", probably a typo, replace with "NA"
philly_ct_income_2011["income"][philly_ct_income_2011["income"] == "2,500-"] <- NA

# confirm -- how many NA values?
sum(is.na(philly_ct_income_2011$income)) ## 10 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2011$income), na.rm=T)
mean(as.numeric(philly_ct_income_2011$income), na.rm=T) 
median(as.numeric(philly_ct_income_2011$income), na.rm=T)
hist(as.numeric(philly_ct_income_2011$income), na.rm=T)

###############################################################################################################
## Philly census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

philly_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-philly.csv")

# label and rename variables so it's easier for me
philly_ct_income_2010 = apply_labels(philly_ct_income_2010,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
philly_ct_income_2010$households <- philly_ct_income_2010$S1901_C01_001E
philly_ct_income_2010$income <- philly_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
philly_ct_income_2010 = philly_ct_income_2010[-1,]

# identify NA values
table(philly_ct_income_2010$income) ## there are 9 "-" values
philly_ct_income_2010["income"][philly_ct_income_2010["income"] == "-"] <- NA

# confirm -- how many NA values?
sum(is.na(philly_ct_income_2010$income)) ## 10 CTs with missing data out of 384

# understand the distribution at this one time period
range(as.numeric(philly_ct_income_2010$income), na.rm=T)
mean(as.numeric(philly_ct_income_2010$income), na.rm=T) 
median(as.numeric(philly_ct_income_2010$income), na.rm=T)
hist(as.numeric(philly_ct_income_2010$income), na.rm=T)

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Philly census tracts -- percent change in average median household income
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 

joined_df_1_philly <- philly_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(philly_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_philly$pct_change <- (as.numeric(joined_df_1_philly$income.x)-as.numeric(joined_df_1_philly$income.y))/as.numeric(joined_df_1_philly$income.y)*100

# make histogram
hist(joined_df_1_philly$pct_change)
hist_philly_2021_2020 <- hist(joined_df_1_philly$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=30,xlim=c(-25, 80), xaxp=c(-20,80,10))

########### percent change between 2020 vs 2010 ############
joined_df_2 <- philly_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(philly_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2$pct_change <- (as.numeric(joined_df_2$income.x)-as.numeric(joined_df_2$income.y))/as.numeric(joined_df_2$income.y)*100

# make histogram
hist(joined_df_2$pct_change)
hist(joined_df_2$pct_change, main="Percent Change in Household Income between 2020 and 2010, Philadelphia", xlab="% change",breaks=50,xlim=c(-100, 200), xaxp=c(-100,200,12))

     
###############################################################################################################
## New York City census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-nyc.csv")

library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
nyc_ct_income_2021 = apply_labels(nyc_ct_income_2021,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2021$households <- nyc_ct_income_2021$S1901_C01_001E
nyc_ct_income_2021$income <- nyc_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2021 = nyc_ct_income_2021[-1,]

# identify NA values
table(nyc_ct_income_2021$income) ## there are 444 "-" values
nyc_ct_income_2021["income"][nyc_ct_income_2021["income"] == "-"] <- NA

# there re 4 valuse of"2,500-", make NA
nyc_ct_income_2021["income"][nyc_ct_income_2021["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2021$income)) ## 448 CTs with missing data out of 2327

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2021$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2021$income), na.rm=T)
median(as.numeric(nyc_ct_income_2021$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2021$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-nyc.csv")

# label and rename variables so it's easier for me
nyc_ct_income_2020 = apply_labels(nyc_ct_income_2020,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2020$households <- nyc_ct_income_2020$S1901_C01_001E
nyc_ct_income_2020$income <- nyc_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2020 = nyc_ct_income_2020[-1,]

# identify NA values
table(nyc_ct_income_2020$income) ## there are 422 "-" values
nyc_ct_income_2020["income"][nyc_ct_income_2020["income"] == "-"] <- NA

# there re 4 valuse of"2,500-", make NA
nyc_ct_income_2020["income"][nyc_ct_income_2020["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2020$income)) ## 426 CTs with missing data out of 2327

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2020$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2020$income), na.rm=T)
median(as.numeric(nyc_ct_income_2020$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2020$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2019 = apply_labels(nyc_ct_income_2019,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2019$households <- nyc_ct_income_2019$S1901_C01_001E
nyc_ct_income_2019$income <- nyc_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2019 = nyc_ct_income_2019[-1,]

# identify NA values
table(nyc_ct_income_2019$income) ## there are 263 "-" values
nyc_ct_income_2019["income"][nyc_ct_income_2019["income"] == "-"] <- NA

# there re 4 valuse of"2,500-", make NA
nyc_ct_income_2019["income"][nyc_ct_income_2019["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2019$income)) ## 448 CTs with missing data out of 2327

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2019$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2019$income), na.rm=T)
median(as.numeric(nyc_ct_income_2019$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2019$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2018 = apply_labels(nyc_ct_income_2018,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2018$households <- nyc_ct_income_2018$S1901_C01_001E
nyc_ct_income_2018$income <- nyc_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2018 = nyc_ct_income_2018[-1,]

# identify NA values
table(nyc_ct_income_2018$income) ## there are 253 "-" values
nyc_ct_income_2018["income"][nyc_ct_income_2018["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2018["income"][nyc_ct_income_2018["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2018$income)) ## 254 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2018$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2018$income), na.rm=T)
median(as.numeric(nyc_ct_income_2018$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2018$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2017 = apply_labels(nyc_ct_income_2017,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2017$households <- nyc_ct_income_2017$S1901_C01_001E
nyc_ct_income_2017$income <- nyc_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2017 = nyc_ct_income_2017[-1,]

# identify NA values
table(nyc_ct_income_2017$income) ## there are 230 "-" values
nyc_ct_income_2017["income"][nyc_ct_income_2017["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2017["income"][nyc_ct_income_2017["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2017$income)) ## 230 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2017$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2017$income), na.rm=T)
median(as.numeric(nyc_ct_income_2017$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2017$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2016 = apply_labels(nyc_ct_income_2016,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2016$households <- nyc_ct_income_2016$S1901_C01_001E
nyc_ct_income_2016$income <- nyc_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2016 = nyc_ct_income_2016[-1,]

# identify NA values
table(nyc_ct_income_2016$income) ## there are 213 "-" values
nyc_ct_income_2016["income"][nyc_ct_income_2016["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2016["income"][nyc_ct_income_2016["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2016$income)) ## 213 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2016$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2016$income), na.rm=T)
median(as.numeric(nyc_ct_income_2016$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2016$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2015 = apply_labels(nyc_ct_income_2015,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2015$households <- nyc_ct_income_2015$S1901_C01_001E
nyc_ct_income_2015$income <- nyc_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2015 = nyc_ct_income_2015[-1,]

# identify NA values
table(nyc_ct_income_2015$income) ## there are 64 "-" values
nyc_ct_income_2015["income"][nyc_ct_income_2015["income"] == "-"] <- NA
# there are 153 (X) values
nyc_ct_income_2015["income"][nyc_ct_income_2015["income"] == "(X)"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2015["income"][nyc_ct_income_2015["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2015$income)) ## 218 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2015$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2015$income), na.rm=T)
median(as.numeric(nyc_ct_income_2015$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2015$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2014 = apply_labels(nyc_ct_income_2014,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2014$households <- nyc_ct_income_2014$S1901_C01_001E
nyc_ct_income_2014$income <- nyc_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2014 = nyc_ct_income_2014[-1,]

# identify NA values
table(nyc_ct_income_2014$income) ## there are 66 "-" values
nyc_ct_income_2014["income"][nyc_ct_income_2014["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2014["income"][nyc_ct_income_2014["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2014$income)) ## 68 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2014$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2014$income), na.rm=T)
median(as.numeric(nyc_ct_income_2014$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2014$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2013 = apply_labels(nyc_ct_income_2013,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2013$households <- nyc_ct_income_2013$S1901_C01_001E
nyc_ct_income_2013$income <- nyc_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2013 = nyc_ct_income_2013[-1,]

# identify NA values
table(nyc_ct_income_2013$income) ## there are 67 "-" values
nyc_ct_income_2013["income"][nyc_ct_income_2013["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2013["income"][nyc_ct_income_2013["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2013$income)) ## 69 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2013$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2013$income), na.rm=T)
median(as.numeric(nyc_ct_income_2013$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2013$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2012 = apply_labels(nyc_ct_income_2012,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2012$households <- nyc_ct_income_2012$S1901_C01_001E
nyc_ct_income_2012$income <- nyc_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2012 = nyc_ct_income_2012[-1,]

# identify NA values
table(nyc_ct_income_2012$income) ## there are 64 "-" values
nyc_ct_income_2012["income"][nyc_ct_income_2012["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2012["income"][nyc_ct_income_2012["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2012$income)) ## 65 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2012$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2012$income), na.rm=T)
median(as.numeric(nyc_ct_income_2012$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2012$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2011 = apply_labels(nyc_ct_income_2011,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2011$households <- nyc_ct_income_2011$S1901_C01_001E
nyc_ct_income_2011$income <- nyc_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2011 = nyc_ct_income_2011[-1,]

# identify NA values
table(nyc_ct_income_2011$income) ## there are 67 "-" values
nyc_ct_income_2011["income"][nyc_ct_income_2011["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2011["income"][nyc_ct_income_2011["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2011$income)) ## 69 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2011$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2011$income), na.rm=T)
median(as.numeric(nyc_ct_income_2011$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2011$income), na.rm=T)

###############################################################################################################
## New York City census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
nyc_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-nyc.csv")


# label and rename variables so it's easier for me
nyc_ct_income_2010 = apply_labels(nyc_ct_income_2010,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
nyc_ct_income_2010$households <- nyc_ct_income_2010$S1901_C01_001E
nyc_ct_income_2010$income <- nyc_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
nyc_ct_income_2010 = nyc_ct_income_2010[-1,]

# identify NA values
table(nyc_ct_income_2010$income) ## there are 73 "-" values
nyc_ct_income_2010["income"][nyc_ct_income_2010["income"] == "-"] <- NA

# values of"2,500-", make NA
nyc_ct_income_2010["income"][nyc_ct_income_2010["income"] == "2,500-"] <- NA

# how many NA values?
sum(is.na(nyc_ct_income_2010$income)) ## 74 CTs with missing data out of 2167

# understand the distribution at this one time period
range(as.numeric(nyc_ct_income_2010$income), na.rm=T)
mean(as.numeric(nyc_ct_income_2010$income), na.rm=T)
median(as.numeric(nyc_ct_income_2010$income), na.rm=T)
hist(as.numeric(nyc_ct_income_2010$income), na.rm=T)

###############################################################################################################
###############################################################################################################
###############################################################################################################
## NYC census tracts -- percent change in average median household income
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 
library(dplyr)
joined_df_1_nyc <- nyc_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(nyc_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_nyc$pct_change <- (as.numeric(joined_df_1_nyc$income.x)-as.numeric(joined_df_1_nyc$income.y))/as.numeric(joined_df_1_nyc$income.y)*100

# make histogram
hist(joined_df_1_nyc$pct_change)
hist_nyc_2021_2020 <- hist(joined_df_1_nyc$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=40,xlim=c(-40, 80), xaxp=c(-40,80,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_nyc <- nyc_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(nyc_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_nyc$pct_change <- (as.numeric(joined_df_2_nyc$income.x)-as.numeric(joined_df_2_nyc$income.y))/as.numeric(joined_df_2_nyc$income.y)*100

# make histogram
hist(joined_df_2_nyc$pct_change)
hist_nyc_2020_2010 <- hist(joined_df_2_nyc$pct_change, main="Percent Change in Household Income between 2020 and 2010, New York City", xlab="% change",breaks=100,xlim=c(-100, 250), xaxp=c(-100,300,20))


###############################################################################################################
## Chicago census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-chicago.csv")

library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
chicago_ct_income_2021 = apply_labels(chicago_ct_income_2021,
                                  S1901_C01_001E = "Total number of households",
                                  S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2021$households <- chicago_ct_income_2021$S1901_C01_001E
chicago_ct_income_2021$income <- chicago_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2021 = chicago_ct_income_2021[-1,]

# identify NA values
table(chicago_ct_income_2021$income) ## there are 204 "-" values
chicago_ct_income_2021["income"][chicago_ct_income_2021["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2021["income"][chicago_ct_income_2021["income"] == "2,500-"] <- NA
chicago_ct_income_2021["income"][chicago_ct_income_2021["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2021$income)) ## 207 CTs with missing data out of 2335

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2021$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2021$income), na.rm=T)
median(as.numeric(chicago_ct_income_2021$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2021$income), na.rm=T)

###############################################################################################################
## Chicago census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2020 = apply_labels(chicago_ct_income_2020,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2020$households <- chicago_ct_income_2020$S1901_C01_001E
chicago_ct_income_2020$income <- chicago_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2020 = chicago_ct_income_2020[-1,]

# identify NA values
table(chicago_ct_income_2020$income) ## there are 188 "-" values
chicago_ct_income_2020["income"][chicago_ct_income_2020["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2020["income"][chicago_ct_income_2020["income"] == "2,500-"] <- NA
chicago_ct_income_2020["income"][chicago_ct_income_2020["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2020$income)) ## 192 CTs with missing data out of 2335

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2020$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2020$income), na.rm=T)
median(as.numeric(chicago_ct_income_2020$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2020$income), na.rm=T)

###############################################################################################################
## Chicago census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2019 = apply_labels(chicago_ct_income_2019,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2019$households <- chicago_ct_income_2019$S1901_C01_001E
chicago_ct_income_2019$income <- chicago_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2019 = chicago_ct_income_2019[-1,]

# identify NA values
table(chicago_ct_income_2019$income) ## there are 91 "-" values
chicago_ct_income_2019["income"][chicago_ct_income_2019["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2019["income"][chicago_ct_income_2019["income"] == "2,500-"] <- NA
chicago_ct_income_2019["income"][chicago_ct_income_2019["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2019$income)) ## 91 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2019$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2019$income), na.rm=T)
median(as.numeric(chicago_ct_income_2019$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2019$income), na.rm=T)

###############################################################################################################
## Chicago census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2018 = apply_labels(chicago_ct_income_2018,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2018$households <- chicago_ct_income_2018$S1901_C01_001E
chicago_ct_income_2018$income <- chicago_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2018 = chicago_ct_income_2018[-1,]

# identify NA values
table(chicago_ct_income_2018$income) ## there are 80 "-" values
chicago_ct_income_2018["income"][chicago_ct_income_2018["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2018["income"][chicago_ct_income_2018["income"] == "2,500-"] <- NA
chicago_ct_income_2018["income"][chicago_ct_income_2018["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2018$income)) ## 80 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2018$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2018$income), na.rm=T)
median(as.numeric(chicago_ct_income_2018$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2018$income), na.rm=T)

###############################################################################################################
## Chicago census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2017 = apply_labels(chicago_ct_income_2017,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2017$households <- chicago_ct_income_2017$S1901_C01_001E
chicago_ct_income_2017$income <- chicago_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2017 = chicago_ct_income_2017[-1,]

# identify NA values
table(chicago_ct_income_2017$income) ## there are 85 "-" values
chicago_ct_income_2017["income"][chicago_ct_income_2017["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2017["income"][chicago_ct_income_2017["income"] == "2,500-"] <- NA
chicago_ct_income_2017["income"][chicago_ct_income_2017["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2017$income)) ## 86 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2017$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2017$income), na.rm=T)
median(as.numeric(chicago_ct_income_2017$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2017$income), na.rm=T)

###############################################################################################################
## Chicago census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2016 = apply_labels(chicago_ct_income_2016,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2016$households <- chicago_ct_income_2016$S1901_C01_001E
chicago_ct_income_2016$income <- chicago_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2016 = chicago_ct_income_2016[-1,]

# identify NA values
table(chicago_ct_income_2016$income) ## there are 83 "-" values
chicago_ct_income_2016["income"][chicago_ct_income_2016["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2016["income"][chicago_ct_income_2016["income"] == "2,500-"] <- NA
chicago_ct_income_2016["income"][chicago_ct_income_2016["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2016$income)) ## 84 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2016$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2016$income), na.rm=T)
median(as.numeric(chicago_ct_income_2016$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2016$income), na.rm=T)

###############################################################################################################
## Chicago census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2015 = apply_labels(chicago_ct_income_2015,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2015$households <- chicago_ct_income_2015$S1901_C01_001E
chicago_ct_income_2015$income <- chicago_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2015 = chicago_ct_income_2015[-1,]

# identify NA values
table(chicago_ct_income_2015$income) ## there are 16 "-" values
chicago_ct_income_2015["income"][chicago_ct_income_2015["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2015["income"][chicago_ct_income_2015["income"] == "2,500-"] <- NA
chicago_ct_income_2015["income"][chicago_ct_income_2015["income"] == "250,000+"] <- NA

# values of "(X)", make NA
chicago_ct_income_2015["income"][chicago_ct_income_2015["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2015$income)) ## 87 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2015$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2015$income), na.rm=T)
median(as.numeric(chicago_ct_income_2015$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2015$income))

###############################################################################################################
## Chicago census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2014 = apply_labels(chicago_ct_income_2014,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2014$households <- chicago_ct_income_2014$S1901_C01_001E
chicago_ct_income_2014$income <- chicago_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2014 = chicago_ct_income_2014[-1,]

# identify NA values
table(chicago_ct_income_2014$income) ## there are 16 "-" values
chicago_ct_income_2014["income"][chicago_ct_income_2014["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2014["income"][chicago_ct_income_2014["income"] == "2,500-"] <- NA
chicago_ct_income_2014["income"][chicago_ct_income_2014["income"] == "250,000+"] <- NA

# values of "(X)", make NA
chicago_ct_income_2014["income"][chicago_ct_income_2014["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2014$income)) ## 16 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2014$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2014$income), na.rm=T)
median(as.numeric(chicago_ct_income_2014$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2014$income))

###############################################################################################################
## Chicago census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2013 = apply_labels(chicago_ct_income_2013,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2013$households <- chicago_ct_income_2013$S1901_C01_001E
chicago_ct_income_2013$income <- chicago_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2013 = chicago_ct_income_2013[-1,]

# identify NA values
table(chicago_ct_income_2013$income) ## there are 16 "-" values
chicago_ct_income_2013["income"][chicago_ct_income_2013["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2013["income"][chicago_ct_income_2013["income"] == "2,500-"] <- NA
chicago_ct_income_2013["income"][chicago_ct_income_2013["income"] == "250,000+"] <- NA

# values of "(X)", make NA
chicago_ct_income_2013["income"][chicago_ct_income_2013["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2013$income)) ## 16 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2013$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2013$income), na.rm=T)
median(as.numeric(chicago_ct_income_2013$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2013$income))

###############################################################################################################
## Chicago census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2012 = apply_labels(chicago_ct_income_2012,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2012$households <- chicago_ct_income_2012$S1901_C01_001E
chicago_ct_income_2012$income <- chicago_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2012 = chicago_ct_income_2012[-1,]

# identify NA values
table(chicago_ct_income_2012$income) ## there are 16 "-" values
chicago_ct_income_2012["income"][chicago_ct_income_2012["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2012["income"][chicago_ct_income_2012["income"] == "2,500-"] <- NA
chicago_ct_income_2012["income"][chicago_ct_income_2012["income"] == "250,000+"] <- NA

# values of "(X)", make NA
chicago_ct_income_2012["income"][chicago_ct_income_2012["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2012$income)) ## 17 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2012$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2012$income), na.rm=T)
median(as.numeric(chicago_ct_income_2012$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2012$income))

###############################################################################################################
## Chicago census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2011 = apply_labels(chicago_ct_income_2011,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2011$households <- chicago_ct_income_2011$S1901_C01_001E
chicago_ct_income_2011$income <- chicago_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2011 = chicago_ct_income_2011[-1,]

# identify NA values
table(chicago_ct_income_2011$income) ## there are 18 "-" values
chicago_ct_income_2011["income"][chicago_ct_income_2011["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2011["income"][chicago_ct_income_2011["income"] == "2,500-"] <- NA
chicago_ct_income_2011["income"][chicago_ct_income_2011["income"] == "250,000+"] <- NA

# values of "(X)", make NA
chicago_ct_income_2011["income"][chicago_ct_income_2011["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2011$income)) ## 21 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2011$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2011$income), na.rm=T)
median(as.numeric(chicago_ct_income_2011$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2011$income))

###############################################################################################################
## Chicago census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
chicago_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-chicago.csv")

# label and rename variables so it's easier for me
chicago_ct_income_2010 = apply_labels(chicago_ct_income_2010,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
chicago_ct_income_2010$households <- chicago_ct_income_2010$S1901_C01_001E
chicago_ct_income_2010$income <- chicago_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
chicago_ct_income_2010 = chicago_ct_income_2010[-1,]

# identify NA values
table(chicago_ct_income_2010$income) ## there are 18 "-" values
chicago_ct_income_2010["income"][chicago_ct_income_2010["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
chicago_ct_income_2010["income"][chicago_ct_income_2010["income"] == "2,500-"] <- NA
chicago_ct_income_2010["income"][chicago_ct_income_2010["income"] == "250,000+"] <- NA

# values of "(X)", make NA
chicago_ct_income_2010["income"][chicago_ct_income_2010["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(chicago_ct_income_2010$income)) ## 23 CTs with missing data out of 2215

# understand the distribution at this one time period
range(as.numeric(chicago_ct_income_2010$income), na.rm=T)
mean(as.numeric(chicago_ct_income_2010$income), na.rm=T)
median(as.numeric(chicago_ct_income_2010$income), na.rm=T)
hist(as.numeric(chicago_ct_income_2010$income))

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Chicago census tracts -- percent change in average median household income ##
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 
library(dplyr)
joined_df_1_chicago <- chicago_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(chicago_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_chicago$pct_change <- (as.numeric(joined_df_1_chicago$income.x)-as.numeric(joined_df_1_chicago$income.y))/as.numeric(joined_df_1_chicago$income.y)*100

# make histogram
hist(joined_df_1_chicago$pct_change)
hist_chicago_2021_2020 <- hist(joined_df_1_chicago$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=40,xlim=c(-35, 60), xaxp=c(-40,60,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_chicago <- chicago_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(chicago_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_chicago$pct_change <- (as.numeric(joined_df_2_chicago$income.x)-as.numeric(joined_df_2_chicago$income.y))/as.numeric(joined_df_2_chicago$income.y)*100

# make histogram
hist(joined_df_2_chicago$pct_change)
hist_chicago_2020_2010 <- hist(joined_df_2_chicago$pct_change, main="Percent Change in Household Income between 2020 and 2010, Chicago", xlab="% change",breaks=50,xlim=c(-50, 150), xaxp=c(-50,150,20))


###############################################################################################################
## Los Angeles census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2021 = apply_labels(LA_ct_income_2021,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
LA_ct_income_2021$households <- LA_ct_income_2021$S1901_C01_001E
LA_ct_income_2021$income <- LA_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2021 = LA_ct_income_2021[-1,]

# identify NA values
table(LA_ct_income_2021$income) ## there are 706 "-" values
LA_ct_income_2021["income"][LA_ct_income_2021["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2021["income"][LA_ct_income_2021["income"] == "2,500-"] <- NA
LA_ct_income_2021["income"][LA_ct_income_2021["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2021["income"][LA_ct_income_2021["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2021$income)) ## 714 CTs with missing data out of 4286

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2021$income), na.rm=T)
mean(as.numeric(LA_ct_income_2021$income), na.rm=T)
median(as.numeric(LA_ct_income_2021$income), na.rm=T)
hist(as.numeric(LA_ct_income_2021$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2020 = apply_labels(LA_ct_income_2020,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2020$households <- LA_ct_income_2020$S1901_C01_001E
LA_ct_income_2020$income <- LA_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2020 = LA_ct_income_2020[-1,]

# identify NA values
table(LA_ct_income_2020$income) ## there are 634 "-" values
LA_ct_income_2020["income"][LA_ct_income_2020["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2020["income"][LA_ct_income_2020["income"] == "2,500-"] <- NA
LA_ct_income_2020["income"][LA_ct_income_2020["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2020["income"][LA_ct_income_2020["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2020$income)) ## 644 CTs with missing data out of 4286

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2020$income), na.rm=T)
mean(as.numeric(LA_ct_income_2020$income), na.rm=T)
median(as.numeric(LA_ct_income_2020$income), na.rm=T)
hist(as.numeric(LA_ct_income_2020$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2019 = apply_labels(LA_ct_income_2019,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2019$households <- LA_ct_income_2019$S1901_C01_001E
LA_ct_income_2019$income <- LA_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2019 = LA_ct_income_2019[-1,]

# identify NA values
table(LA_ct_income_2019$income) ## there are 373 "-" values
LA_ct_income_2019["income"][LA_ct_income_2019["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2019["income"][LA_ct_income_2019["income"] == "2,500-"] <- NA
LA_ct_income_2019["income"][LA_ct_income_2019["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2019["income"][LA_ct_income_2019["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2019$income)) ## 373 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2019$income), na.rm=T)
mean(as.numeric(LA_ct_income_2019$income), na.rm=T)
median(as.numeric(LA_ct_income_2019$income), na.rm=T)
hist(as.numeric(LA_ct_income_2019$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2018 = apply_labels(LA_ct_income_2018,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2018$households <- LA_ct_income_2018$S1901_C01_001E
LA_ct_income_2018$income <- LA_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2018 = LA_ct_income_2018[-1,]

# identify NA values
table(LA_ct_income_2018$income) ## there are 346 "-" values
LA_ct_income_2018["income"][LA_ct_income_2018["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2018["income"][LA_ct_income_2018["income"] == "2,500-"] <- NA
LA_ct_income_2018["income"][LA_ct_income_2018["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2018["income"][LA_ct_income_2018["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2018$income)) ## 347 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2018$income), na.rm=T)
mean(as.numeric(LA_ct_income_2018$income), na.rm=T)
median(as.numeric(LA_ct_income_2018$income), na.rm=T)
hist(as.numeric(LA_ct_income_2018$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2017 = apply_labels(LA_ct_income_2017,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2017$households <- LA_ct_income_2017$S1901_C01_001E
LA_ct_income_2017$income <- LA_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2017 = LA_ct_income_2017[-1,]

# identify NA values
table(LA_ct_income_2017$income) ## there are 330 "-" values
LA_ct_income_2017["income"][LA_ct_income_2017["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2017["income"][LA_ct_income_2017["income"] == "2,500-"] <- NA
LA_ct_income_2017["income"][LA_ct_income_2017["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2017["income"][LA_ct_income_2017["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2017$income)) ## 330 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2017$income), na.rm=T)
mean(as.numeric(LA_ct_income_2017$income), na.rm=T)
median(as.numeric(LA_ct_income_2017$income), na.rm=T)
hist(as.numeric(LA_ct_income_2017$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2016 = apply_labels(LA_ct_income_2016,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2016$households <- LA_ct_income_2016$S1901_C01_001E
LA_ct_income_2016$income <- LA_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2016 = LA_ct_income_2016[-1,]

# identify NA values
table(LA_ct_income_2016$income) ## there are 319 "-" values
LA_ct_income_2016["income"][LA_ct_income_2016["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2016["income"][LA_ct_income_2016["income"] == "2,500-"] <- NA
LA_ct_income_2016["income"][LA_ct_income_2016["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2016["income"][LA_ct_income_2016["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2016$income)) ## 319 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2016$income), na.rm=T)
mean(as.numeric(LA_ct_income_2016$income), na.rm=T)
median(as.numeric(LA_ct_income_2016$income), na.rm=T)
hist(as.numeric(LA_ct_income_2016$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2015 = apply_labels(LA_ct_income_2015,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2015$households <- LA_ct_income_2015$S1901_C01_001E
LA_ct_income_2015$income <- LA_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2015 = LA_ct_income_2015[-1,]

# identify NA values
table(LA_ct_income_2015$income) ## there are 51 "-" values
LA_ct_income_2015["income"][LA_ct_income_2015["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2015["income"][LA_ct_income_2015["income"] == "2,500-"] <- NA
LA_ct_income_2015["income"][LA_ct_income_2015["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2015["income"][LA_ct_income_2015["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2015$income)) ## 346 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2015$income), na.rm=T)
mean(as.numeric(LA_ct_income_2015$income), na.rm=T)
median(as.numeric(LA_ct_income_2015$income), na.rm=T)
hist(as.numeric(LA_ct_income_2015$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2014 = apply_labels(LA_ct_income_2014,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2014$households <- LA_ct_income_2014$S1901_C01_001E
LA_ct_income_2014$income <- LA_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2014 = LA_ct_income_2014[-1,]

# identify NA values
table(LA_ct_income_2014$income) ## there are 53 "-" values
LA_ct_income_2014["income"][LA_ct_income_2014["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2014["income"][LA_ct_income_2014["income"] == "2,500-"] <- NA
LA_ct_income_2014["income"][LA_ct_income_2014["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2014["income"][LA_ct_income_2014["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2014$income)) ## 53 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2014$income), na.rm=T)
mean(as.numeric(LA_ct_income_2014$income), na.rm=T)
median(as.numeric(LA_ct_income_2014$income), na.rm=T)
hist(as.numeric(LA_ct_income_2014$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2013 = apply_labels(LA_ct_income_2013,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2013$households <- LA_ct_income_2013$S1901_C01_001E
LA_ct_income_2013$income <- LA_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2013 = LA_ct_income_2013[-1,]

# identify NA values
table(LA_ct_income_2013$income) ## there are 60 "-" values
LA_ct_income_2013["income"][LA_ct_income_2013["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2013["income"][LA_ct_income_2013["income"] == "2,500-"] <- NA
LA_ct_income_2013["income"][LA_ct_income_2013["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2013["income"][LA_ct_income_2013["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2013$income)) ## 60 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2013$income), na.rm=T)
mean(as.numeric(LA_ct_income_2013$income), na.rm=T)
median(as.numeric(LA_ct_income_2013$income), na.rm=T)
hist(as.numeric(LA_ct_income_2013$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2012 = apply_labels(LA_ct_income_2012,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2012$households <- LA_ct_income_2012$S1901_C01_001E
LA_ct_income_2012$income <- LA_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2012 = LA_ct_income_2012[-1,]

# identify NA values
table(LA_ct_income_2012$income) ## there are 47 "-" values
LA_ct_income_2012["income"][LA_ct_income_2012["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2012["income"][LA_ct_income_2012["income"] == "2,500-"] <- NA
LA_ct_income_2012["income"][LA_ct_income_2012["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2012["income"][LA_ct_income_2012["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2012$income)) ## 52 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2012$income), na.rm=T)
mean(as.numeric(LA_ct_income_2012$income), na.rm=T)
median(as.numeric(LA_ct_income_2012$income), na.rm=T)
hist(as.numeric(LA_ct_income_2012$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2011 = apply_labels(LA_ct_income_2011,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2011$households <- LA_ct_income_2011$S1901_C01_001E
LA_ct_income_2011$income <- LA_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2011 = LA_ct_income_2011[-1,]

# identify NA values
table(LA_ct_income_2011$income) ## there are 58 "-" values
LA_ct_income_2011["income"][LA_ct_income_2011["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2011["income"][LA_ct_income_2011["income"] == "2,500-"] <- NA
LA_ct_income_2011["income"][LA_ct_income_2011["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2011["income"][LA_ct_income_2011["income"] == "(X)"] <- NA
# values of "null", make NA
LA_ct_income_2011["income"][LA_ct_income_2011["income"] == "null"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2011$income)) ## 66 CTs with missing data out of 3926

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2011$income), na.rm=T)
mean(as.numeric(LA_ct_income_2011$income), na.rm=T)
median(as.numeric(LA_ct_income_2011$income), na.rm=T)
hist(as.numeric(LA_ct_income_2011$income))

###############################################################################################################
## Los Angeles census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
LA_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-LA.csv")

# label and rename variables so it's easier for me
LA_ct_income_2010 = apply_labels(LA_ct_income_2010,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
LA_ct_income_2010$households <- LA_ct_income_2010$S1901_C01_001E
LA_ct_income_2010$income <- LA_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
LA_ct_income_2010 = LA_ct_income_2010[-1,]

# identify NA values
table(LA_ct_income_2010$income) ## there are 57 "-" values
LA_ct_income_2010["income"][LA_ct_income_2010["income"] == "-"] <- NA

# values of "2,500-" and "250,000+", make NA
LA_ct_income_2010["income"][LA_ct_income_2010["income"] == "2,500-"] <- NA
LA_ct_income_2010["income"][LA_ct_income_2010["income"] == "250,000+"] <- NA

# values of "(X)", make NA
LA_ct_income_2010["income"][LA_ct_income_2010["income"] == "(X)"] <- NA
# values of "null", make NA
LA_ct_income_2010["income"][LA_ct_income_2010["income"] == "null"] <- NA

# how many NA values?
sum(is.na(LA_ct_income_2010$income)) ## 61 CTs with missing data out of 3925

# understand the distribution at this one time period
range(as.numeric(LA_ct_income_2010$income), na.rm=T)
mean(as.numeric(LA_ct_income_2010$income), na.rm=T)
median(as.numeric(LA_ct_income_2010$income), na.rm=T)
hist(as.numeric(LA_ct_income_2010$income))

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Los Angeles census tracts -- percent change in average median household income ##
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 
library(dplyr)
joined_df_1_la <- LA_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(LA_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_la$pct_change <- (as.numeric(joined_df_1_la$income.x)-as.numeric(joined_df_1_la$income.y))/as.numeric(joined_df_1_la$income.y)*100

# make histogram
hist(joined_df_1_la$pct_change)
hist_la_2021_2020 <- hist(joined_df_1_la$pct_change, main="Percent Change in Household Income between 2021 and 2020", xlab="% change",breaks=40,xlim=c(-35, 60), xaxp=c(-40,60,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_la <- LA_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(LA_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_la$pct_change <- (as.numeric(joined_df_2_la$income.x)-as.numeric(joined_df_2_la$income.y))/as.numeric(joined_df_2_la$income.y)*100

# make histogram
hist(joined_df_2_la$pct_change)
hist_la_2020_2010 <- hist(joined_df_2_la$pct_change, main="Percent Change in Household Income between 2020 and 2010, Los Angeles", xlab="% change",breaks=50,xlim=c(-50, 150), xaxp=c(-50,150,20))

###############################################################################################################
## Houston census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2021 <- read.csv("ACSST5Y2021.S1901-Data-houston.csv")

library(expss)
library(tidyverse)
library(dplyr)

# label and rename variables so it's easier for me
houston_ct_income_2021 = apply_labels(houston_ct_income_2021,
                                     S1901_C01_001E = "Total number of households",
                                     S1901_C01_012E = "Median household income ($)")
houston_ct_income_2021$households <- houston_ct_income_2021$S1901_C01_001E
houston_ct_income_2021$income <- houston_ct_income_2021$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2021 = houston_ct_income_2021[-1,]

# identify NA values
table(houston_ct_income_2021$income) ## there are 21 "-" values
houston_ct_income_2021["income"][houston_ct_income_2021["income"] == "-"] <- NA
houston_ct_income_2021["income"][houston_ct_income_2021["income"] == "2,500-"] <- NA
houston_ct_income_2021["income"][houston_ct_income_2021["income"] == "250,000+"] <- NA
houston_ct_income_2021["income"][houston_ct_income_2021["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2021$income)) ## 31 CTs with missing data out of 1606

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2021$income), na.rm=T)
mean(as.numeric(houston_ct_income_2021$income), na.rm=T)
median(as.numeric(houston_ct_income_2021$income), na.rm=T)
hist(as.numeric(houston_ct_income_2021$income))

###############################################################################################################
## Houston census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2020 = apply_labels(houston_ct_income_2020,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2020$households <- houston_ct_income_2020$S1901_C01_001E
houston_ct_income_2020$income <- houston_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2020 = houston_ct_income_2020[-1,]

# identify NA values
table(houston_ct_income_2020$income) ## there are 23 "-" values
houston_ct_income_2020["income"][houston_ct_income_2020["income"] == "-"] <- NA
houston_ct_income_2020["income"][houston_ct_income_2020["income"] == "2,500-"] <- NA
houston_ct_income_2020["income"][houston_ct_income_2020["income"] == "250,000+"] <- NA
houston_ct_income_2020["income"][houston_ct_income_2020["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2020$income)) ## 33 CTs with missing data out of 1606

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2020$income), na.rm=T)
mean(as.numeric(houston_ct_income_2020$income), na.rm=T)
median(as.numeric(houston_ct_income_2020$income), na.rm=T)
hist(as.numeric(houston_ct_income_2020$income))

###############################################################################################################
## Houston census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2019 = apply_labels(houston_ct_income_2019,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2019$households <- houston_ct_income_2019$S1901_C01_001E
houston_ct_income_2019$income <- houston_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2019 = houston_ct_income_2019[-1,]

# identify NA values
table(houston_ct_income_2019$income) ## there are 10 "-" values
houston_ct_income_2019["income"][houston_ct_income_2019["income"] == "-"] <- NA
houston_ct_income_2019["income"][houston_ct_income_2019["income"] == "2,500-"] <- NA
houston_ct_income_2019["income"][houston_ct_income_2019["income"] == "250,000+"] <- NA
houston_ct_income_2019["income"][houston_ct_income_2019["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2019$income)) ## 18 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2019$income), na.rm=T)
mean(as.numeric(houston_ct_income_2019$income), na.rm=T)
median(as.numeric(houston_ct_income_2019$income), na.rm=T)
hist(as.numeric(houston_ct_income_2019$income))

###############################################################################################################
## Houston census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2018 = apply_labels(houston_ct_income_2018,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2018$households <- houston_ct_income_2018$S1901_C01_001E
houston_ct_income_2018$income <- houston_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2018 = houston_ct_income_2018[-1,]

# identify NA values
table(houston_ct_income_2018$income) ## there are 10 "-" values
houston_ct_income_2018["income"][houston_ct_income_2018["income"] == "-"] <- NA
houston_ct_income_2018["income"][houston_ct_income_2018["income"] == "2,500-"] <- NA
houston_ct_income_2018["income"][houston_ct_income_2018["income"] == "250,000+"] <- NA
houston_ct_income_2018["income"][houston_ct_income_2018["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2018$income)) ## 16 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2018$income), na.rm=T)
mean(as.numeric(houston_ct_income_2018$income), na.rm=T)
median(as.numeric(houston_ct_income_2018$income), na.rm=T)
hist(as.numeric(houston_ct_income_2018$income))

###############################################################################################################
## Houston census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2017 = apply_labels(houston_ct_income_2017,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2017$households <- houston_ct_income_2017$S1901_C01_001E
houston_ct_income_2017$income <- houston_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2017 = houston_ct_income_2017[-1,]

# identify NA values
table(houston_ct_income_2017$income) ## there are 9 "-" values
houston_ct_income_2017["income"][houston_ct_income_2017["income"] == "-"] <- NA
houston_ct_income_2017["income"][houston_ct_income_2017["income"] == "2,500-"] <- NA
houston_ct_income_2017["income"][houston_ct_income_2017["income"] == "250,000+"] <- NA
houston_ct_income_2017["income"][houston_ct_income_2017["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2017$income)) ## 12 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2017$income), na.rm=T)
mean(as.numeric(houston_ct_income_2017$income), na.rm=T)
median(as.numeric(houston_ct_income_2017$income), na.rm=T)
hist(as.numeric(houston_ct_income_2017$income))

###############################################################################################################
## Houston census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2016 = apply_labels(houston_ct_income_2016,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2016$households <- houston_ct_income_2016$S1901_C01_001E
houston_ct_income_2016$income <- houston_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2016 = houston_ct_income_2016[-1,]

# identify NA values
table(houston_ct_income_2016$income) ## there are 9 "-" values
houston_ct_income_2016["income"][houston_ct_income_2016["income"] == "-"] <- NA
houston_ct_income_2016["income"][houston_ct_income_2016["income"] == "2,500-"] <- NA
houston_ct_income_2016["income"][houston_ct_income_2016["income"] == "250,000+"] <- NA
houston_ct_income_2016["income"][houston_ct_income_2016["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2016$income)) ## 12 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2016$income), na.rm=T)
mean(as.numeric(houston_ct_income_2016$income), na.rm=T)
median(as.numeric(houston_ct_income_2016$income), na.rm=T)
hist(as.numeric(houston_ct_income_2016$income))

###############################################################################################################
## Houston census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2015 = apply_labels(houston_ct_income_2015,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2015$households <- houston_ct_income_2015$S1901_C01_001E
houston_ct_income_2015$income <- houston_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2015 = houston_ct_income_2015[-1,]

# identify NA values
table(houston_ct_income_2015$income) ## there are 9 "-" values
houston_ct_income_2015["income"][houston_ct_income_2015["income"] == "-"] <- NA
houston_ct_income_2015["income"][houston_ct_income_2015["income"] == "2,500-"] <- NA
houston_ct_income_2015["income"][houston_ct_income_2015["income"] == "250,000+"] <- NA
houston_ct_income_2015["income"][houston_ct_income_2015["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2015$income)) ## 11 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2015$income), na.rm=T)
mean(as.numeric(houston_ct_income_2015$income), na.rm=T)
median(as.numeric(houston_ct_income_2015$income), na.rm=T)
hist(as.numeric(houston_ct_income_2015$income))

###############################################################################################################
## Houston census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2014 = apply_labels(houston_ct_income_2014,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2014$households <- houston_ct_income_2014$S1901_C01_001E
houston_ct_income_2014$income <- houston_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2014 = houston_ct_income_2014[-1,]

# identify NA values
table(houston_ct_income_2014$income) ## there are 9 "-" values
houston_ct_income_2014["income"][houston_ct_income_2014["income"] == "-"] <- NA
houston_ct_income_2014["income"][houston_ct_income_2014["income"] == "2,500-"] <- NA
houston_ct_income_2014["income"][houston_ct_income_2014["income"] == "250,000+"] <- NA
houston_ct_income_2014["income"][houston_ct_income_2014["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2014$income)) ## 10 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2014$income), na.rm=T)
mean(as.numeric(houston_ct_income_2014$income), na.rm=T)
median(as.numeric(houston_ct_income_2014$income), na.rm=T)
hist(as.numeric(houston_ct_income_2014$income))

###############################################################################################################
## Houston census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2013 = apply_labels(houston_ct_income_2013,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2013$households <- houston_ct_income_2013$S1901_C01_001E
houston_ct_income_2013$income <- houston_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2013 = houston_ct_income_2013[-1,]

# identify NA values
table(houston_ct_income_2013$income) ## there are 9 "-" values
houston_ct_income_2013["income"][houston_ct_income_2013["income"] == "-"] <- NA
houston_ct_income_2013["income"][houston_ct_income_2013["income"] == "2,500-"] <- NA
houston_ct_income_2013["income"][houston_ct_income_2013["income"] == "250,000+"] <- NA
houston_ct_income_2013["income"][houston_ct_income_2013["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2013$income)) ## 10 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2013$income), na.rm=T)
mean(as.numeric(houston_ct_income_2013$income), na.rm=T)
median(as.numeric(houston_ct_income_2013$income), na.rm=T)
hist(as.numeric(houston_ct_income_2013$income))

###############################################################################################################
## Houston census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2012 = apply_labels(houston_ct_income_2012,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2012$households <- houston_ct_income_2012$S1901_C01_001E
houston_ct_income_2012$income <- houston_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2012 = houston_ct_income_2012[-1,]

# identify NA values
table(houston_ct_income_2012$income) ## there are 8 "-" values
houston_ct_income_2012["income"][houston_ct_income_2012["income"] == "-"] <- NA
houston_ct_income_2012["income"][houston_ct_income_2012["income"] == "2,500-"] <- NA
houston_ct_income_2012["income"][houston_ct_income_2012["income"] == "250,000+"] <- NA
houston_ct_income_2012["income"][houston_ct_income_2012["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2012$income)) ## 9 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2012$income), na.rm=T)
mean(as.numeric(houston_ct_income_2012$income), na.rm=T)
median(as.numeric(houston_ct_income_2012$income), na.rm=T)
hist(as.numeric(houston_ct_income_2012$income))

###############################################################################################################
## Houston census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2011 = apply_labels(houston_ct_income_2011,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2011$households <- houston_ct_income_2011$S1901_C01_001E
houston_ct_income_2011$income <- houston_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2011 = houston_ct_income_2011[-1,]

# identify NA values
table(houston_ct_income_2011$income) ## there are 8 "-" values
houston_ct_income_2011["income"][houston_ct_income_2011["income"] == "-"] <- NA
houston_ct_income_2011["income"][houston_ct_income_2011["income"] == "2,500-"] <- NA
houston_ct_income_2011["income"][houston_ct_income_2011["income"] == "250,000+"] <- NA
houston_ct_income_2011["income"][houston_ct_income_2011["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2011$income)) ## 10 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2011$income), na.rm=T)
mean(as.numeric(houston_ct_income_2011$income), na.rm=T)
median(as.numeric(houston_ct_income_2011$income), na.rm=T)
hist(as.numeric(houston_ct_income_2011$income))

###############################################################################################################
## Houston census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
houston_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-houston.csv")

# label and rename variables so it's easier for me
houston_ct_income_2010 = apply_labels(houston_ct_income_2010,
                                      S1901_C01_001E = "Total number of households",
                                      S1901_C01_012E = "Median household income ($)")
houston_ct_income_2010$households <- houston_ct_income_2010$S1901_C01_001E
houston_ct_income_2010$income <- houston_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
houston_ct_income_2010 = houston_ct_income_2010[-1,]

# identify NA values
table(houston_ct_income_2010$income) ## there are 8 "-" values
houston_ct_income_2010["income"][houston_ct_income_2010["income"] == "-"] <- NA
houston_ct_income_2010["income"][houston_ct_income_2010["income"] == "2,500-"] <- NA
houston_ct_income_2010["income"][houston_ct_income_2010["income"] == "250,000+"] <- NA
houston_ct_income_2010["income"][houston_ct_income_2010["income"] == "(X)"] <- NA

# how many NA values?
sum(is.na(houston_ct_income_2010$income)) ## 10 CTs with missing data out of 1072

# understand the distribution at this one time period
range(as.numeric(houston_ct_income_2010$income), na.rm=T)
mean(as.numeric(houston_ct_income_2010$income), na.rm=T)
median(as.numeric(houston_ct_income_2010$income), na.rm=T)
hist(as.numeric(houston_ct_income_2010$income))

###############################################################################################################
###############################################################################################################
###############################################################################################################
## Houston census tracts -- percent change in average median household income ##
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 
library(dplyr)
joined_df_1_houston <- houston_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(houston_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_houston$pct_change <- (as.numeric(joined_df_1_houston$income.x)-as.numeric(joined_df_1_houston$income.y))/as.numeric(joined_df_1_houston$income.y)*100

# make histogram
hist(joined_df_1_houston$pct_change)
hist_houston_2021_2020 <- hist(joined_df_1_houston$pct_change, main="Percent Change in Household Income between 2021 and 2020, Houston", xlab="% change",breaks=40,xlim=c(-35, 60), xaxp=c(-40,60,10))

########### percent change between 2020 vs 2010 ############
joined_df_2_houston <- houston_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(houston_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_2_houston$pct_change <- (as.numeric(joined_df_2_houston$income.x)-as.numeric(joined_df_2_houston$income.y))/as.numeric(joined_df_2_houston$income.y)*100

# make histogram
hist(joined_df_2_houston$pct_change)
hist_houston_2020_2010 <- hist(joined_df_2_houston$pct_change, main="Percent Change in Household Income between 2020 and 2010, Houston", xlab="% change",breaks=50,xlim=c(-50, 150), xaxp=c(-50,150,20))


################################################################################
# adding in RUCA code CT data for urban/rural instead of using US census regions
################################################################################

library(readxl)
library(dplyr)
ruca2010revised <- read_excel("ruca2010revised.xlsx", 
                              sheet = "Data")

# select out only RUCA codes 1-3 for urban CTs
ruca2010revised_urban <- filter(ruca2010revised, `Primary RUCA Code 2010` %in%  c(1, 2, 3))

#rename state-county FIPS variable
ruca2010revised_urban <- ruca2010revised_urban %>% 
  rename("FIPS" = "State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)")


# we need to match GEOID variable in the US datasets to state-county-tract FIPS code in a join
# join the dataset with the income information to the dataset with the FIPS code
# try it with US 2021 as an example

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
ruca_urban_2021 <- us_ct_income_2021 %>% select(FIPS, income) %>% inner_join(ruca2010revised_urban %>% select(FIPS), by=c('FIPS'='FIPS'))
# ended up with 50,822 CTs that matched from 60,238
# how many NA values?
sum(is.na(ruca_urban_2021$income)) ## 607 CTs with missing data out of 50,822
hist(as.numeric(ruca_urban_2021$income))

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
ruca_urban_2020 <- us_ct_income_2020 %>% select(FIPS, income) %>% inner_join(ruca2010revised_urban %>% select(FIPS), by=c('FIPS'='FIPS'))
# ended up with 50,822 CTs that matched from 60,238
# how many NA values?
sum(is.na(ruca_urban_2020$income)) ## 532 CTs with missing data out of 50,822

# now join together 2021 and 2020 to calculate % change
library(dplyr)
joined_ruca_urban_1 <- ruca_urban_2021 %>% select(FIPS, income) %>% inner_join(ruca_urban_2020 %>% select(FIPS, income), by=c('FIPS'='FIPS'))
#rename income variables
joined_ruca_urban_1 <- joined_ruca_urban_1 %>% 
  rename("income_2020" = "income.y")
joined_ruca_urban_1 <- joined_ruca_urban_1 %>% 
  rename("income_2021" = "income.x")

# create percent change variable (between 2021 and 2020)
joined_ruca_urban_1$pct_change <- (as.numeric(joined_ruca_urban_1$income_2021)-as.numeric(joined_ruca_urban_1$income_2020))/as.numeric(joined_ruca_urban_1$income_2020)*100

# make histogram
hist(joined_ruca_urban_1$pct_change)
hist_urban_2021_2020 <- hist(joined_ruca_urban_1$pct_change, 
                          main="Percent Change in Household Income between 2021 and 2020, Urban Census Tracts",
                          xlab="% change",
                          breaks=100,
                          xlim=c(-30, 50),
                          xaxp=c(-30,50,20)) #most around 4-6% change in median income

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
ruca_urban_2010 <- us_ct_income_2010 %>% select(FIPS, income) %>% inner_join(ruca2010revised_urban %>% select(FIPS), by=c('FIPS'='FIPS'))
# ended up with 60,237 CTs that matched from 60,238 (really close because the RUCA codes are from 2010--they should all match!!)
# how many NA values?
sum(is.na(ruca_urban_2010$income)) ## 331 CTs with missing data out of 60,237

# now join together 2020 and 2010 to calculate % change
library(dplyr)
joined_ruca_urban_2 <- ruca_urban_2020 %>% select(FIPS, income) %>% inner_join(ruca_urban_2010 %>% select(FIPS, income), by=c('FIPS'='FIPS'))
#rename income variables
joined_ruca_urban_2 <- joined_ruca_urban_2 %>% 
  rename("income_2010" = "income.y")
joined_ruca_urban_2 <- joined_ruca_urban_2 %>% 
  rename("income_2020" = "income.x")

# create percent change variable (between 2021 and 2020)
joined_ruca_urban_2$pct_change <- (as.numeric(joined_ruca_urban_2$income_2020)-as.numeric(joined_ruca_urban_2$income_2010))/as.numeric(joined_ruca_urban_2$income_2010)*100

# make histogram
hist(joined_ruca_urban_2$pct_change)
hist_urban_2020_2010 <- hist(joined_ruca_urban_2$pct_change, 
                          main="Percent Change in Household Income between 2020 and 2010, Urban Census Tracts",
                          xlab="% change",
                          xlim=c(-100,200),
                          breaks=150,
                          xaxp=c(-100,200,15)) #most between 15-30% change in median income





