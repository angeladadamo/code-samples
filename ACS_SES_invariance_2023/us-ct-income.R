###############################################################################################################
# exploring changes in measures over time among all US census tracts -- ACS 5-year estimates
###############################################################################################################


###############################################################################################################
## US census tracts -- income, 2021 5-year estimates (2017-2021)
###############################################################################################################

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

# remove the first row of the dataset
us_ct_income_2021 = us_ct_income_2021[-1,]

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

###############################################################################################################
## US census tracts -- income, 2020 5-year estimates (2016-2020)
###############################################################################################################

# load the data
# load packages
us_ct_income_2020 <- read.csv("ACSST5Y2020.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2020 = apply_labels(us_ct_income_2020,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2020$households <- us_ct_income_2020$S1901_C01_001E
us_ct_income_2020$income <- us_ct_income_2020$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2020 = us_ct_income_2020[-1,]

# identify NA values
table(us_ct_income_2020$income) ## there are 1571 "-" values
us_ct_income_2020["income"][us_ct_income_2020["income"] == "-"] <- NA
us_ct_income_2020["income"][us_ct_income_2020["income"] == "(X)"] <- NA
us_ct_income_2020["income"][us_ct_income_2020["income"] == "2,500-"] <- NA
us_ct_income_2020["income"][us_ct_income_2020["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2020$income)) ## 1764 CTs with missing data out of 85394

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2020$income), na.rm=T)
mean(as.numeric(us_ct_income_2020$income), na.rm=T)
median(as.numeric(us_ct_income_2020$income), na.rm=T)
hist(as.numeric(us_ct_income_2020$income))

###############################################################################################################
## US census tracts -- income, 2019 5-year estimates (2015-2019)
###############################################################################################################

# load the data
# load packages
us_ct_income_2019 <- read.csv("ACSST5Y2019.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2019 = apply_labels(us_ct_income_2019,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2019$households <- us_ct_income_2019$S1901_C01_001E
us_ct_income_2019$income <- us_ct_income_2019$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2019 = us_ct_income_2019[-1,]

# identify NA values
table(us_ct_income_2019$income) ## there are 1088 "-" values
us_ct_income_2019["income"][us_ct_income_2019["income"] == "-"] <- NA
us_ct_income_2019["income"][us_ct_income_2019["income"] == "(X)"] <- NA
us_ct_income_2019["income"][us_ct_income_2019["income"] == "2,500-"] <- NA
us_ct_income_2019["income"][us_ct_income_2019["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2019$income)) ## 1088 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2019$income), na.rm=T)
mean(as.numeric(us_ct_income_2019$income), na.rm=T)
median(as.numeric(us_ct_income_2019$income), na.rm=T)
hist(as.numeric(us_ct_income_2019$income))

###############################################################################################################
## US census tracts -- income, 2018 5-year estimates (2014-2018)
###############################################################################################################

# load the data
# load packages
us_ct_income_2018 <- read.csv("ACSST5Y2018.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2018 = apply_labels(us_ct_income_2018,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2018$households <- us_ct_income_2018$S1901_C01_001E
us_ct_income_2018$income <- us_ct_income_2018$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2018 = us_ct_income_2018[-1,]

# identify NA values
table(us_ct_income_2018$income) ## there are 1069 "-" values
us_ct_income_2018["income"][us_ct_income_2018["income"] == "-"] <- NA
us_ct_income_2018["income"][us_ct_income_2018["income"] == "(X)"] <- NA
us_ct_income_2018["income"][us_ct_income_2018["income"] == "2,500-"] <- NA
us_ct_income_2018["income"][us_ct_income_2018["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2018$income)) ## 1154 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2018$income), na.rm=T)
mean(as.numeric(us_ct_income_2018$income), na.rm=T)
median(as.numeric(us_ct_income_2018$income), na.rm=T)
hist(as.numeric(us_ct_income_2018$income))

###############################################################################################################
## US census tracts -- income, 2017 5-year estimates (2013-2017)
###############################################################################################################

# load the data
# load packages
us_ct_income_2017 <- read.csv("ACSST5Y2017.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2017 = apply_labels(us_ct_income_2017,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2017$households <- us_ct_income_2017$S1901_C01_001E
us_ct_income_2017$income <- us_ct_income_2017$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2017 = us_ct_income_2017[-1,]

# identify NA values
table(us_ct_income_2017$income) ## there are 1058 "-" values
us_ct_income_2017["income"][us_ct_income_2017["income"] == "-"] <- NA
us_ct_income_2017["income"][us_ct_income_2017["income"] == "(X)"] <- NA
us_ct_income_2017["income"][us_ct_income_2017["income"] == "2,500-"] <- NA
us_ct_income_2017["income"][us_ct_income_2017["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2017$income)) ## 1116 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2017$income), na.rm=T)
mean(as.numeric(us_ct_income_2017$income), na.rm=T)
median(as.numeric(us_ct_income_2017$income), na.rm=T)
hist(as.numeric(us_ct_income_2017$income))

###############################################################################################################
## US census tracts -- income, 2016 5-year estimates (2012-2016)
###############################################################################################################

# load the data
# load packages
us_ct_income_2016 <- read.csv("ACSST5Y2016.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2016 = apply_labels(us_ct_income_2016,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2016$households <- us_ct_income_2016$S1901_C01_001E
us_ct_income_2016$income <- us_ct_income_2016$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2016 = us_ct_income_2016[-1,]

# identify NA values
table(us_ct_income_2016$income) ## there are 1047 "-" values
us_ct_income_2016["income"][us_ct_income_2016["income"] == "-"] <- NA
us_ct_income_2016["income"][us_ct_income_2016["income"] == "(X)"] <- NA
us_ct_income_2016["income"][us_ct_income_2016["income"] == "2,500-"] <- NA
us_ct_income_2016["income"][us_ct_income_2016["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2016$income)) ## 1090 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2016$income), na.rm=T)
mean(as.numeric(us_ct_income_2016$income), na.rm=T)
median(as.numeric(us_ct_income_2016$income), na.rm=T)
hist(as.numeric(us_ct_income_2016$income))

###############################################################################################################
## US census tracts -- income, 2015 5-year estimates (2011-2015)
###############################################################################################################

# load the data
# load packages
us_ct_income_2015 <- read.csv("ACSST5Y2015.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2015 = apply_labels(us_ct_income_2015,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2015$households <- us_ct_income_2015$S1901_C01_001E
us_ct_income_2015$income <- us_ct_income_2015$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2015 = us_ct_income_2015[-1,]

# identify NA values
table(us_ct_income_2015$income) ## there are 988 "-" values
us_ct_income_2015["income"][us_ct_income_2015["income"] == "-"] <- NA
us_ct_income_2015["income"][us_ct_income_2015["income"] == "(X)"] <- NA
us_ct_income_2015["income"][us_ct_income_2015["income"] == "2,500-"] <- NA
us_ct_income_2015["income"][us_ct_income_2015["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2015$income)) ## 1100 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2015$income), na.rm=T)
mean(as.numeric(us_ct_income_2015$income), na.rm=T)
median(as.numeric(us_ct_income_2015$income), na.rm=T)
hist(as.numeric(us_ct_income_2015$income))

###############################################################################################################
## US census tracts -- income, 2014 5-year estimates (2010-2014)
###############################################################################################################

# load the data
# load packages
us_ct_income_2014 <- read.csv("ACSST5Y2014.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2014 = apply_labels(us_ct_income_2014,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2014$households <- us_ct_income_2014$S1901_C01_001E
us_ct_income_2014$income <- us_ct_income_2014$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2014 = us_ct_income_2014[-1,]

# identify NA values
table(us_ct_income_2014$income) ## there are 993 "-" values
us_ct_income_2014["income"][us_ct_income_2014["income"] == "-"] <- NA
us_ct_income_2014["income"][us_ct_income_2014["income"] == "(X)"] <- NA
us_ct_income_2014["income"][us_ct_income_2014["income"] == "2,500-"] <- NA
us_ct_income_2014["income"][us_ct_income_2014["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2014$income)) ## 1035 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2014$income), na.rm=T)
mean(as.numeric(us_ct_income_2014$income), na.rm=T)
median(as.numeric(us_ct_income_2014$income), na.rm=T)
hist(as.numeric(us_ct_income_2014$income))

###############################################################################################################
## US census tracts -- income, 2013 5-year estimates (2009-2013)
###############################################################################################################

# load the data
# load packages
us_ct_income_2013 <- read.csv("ACSST5Y2013.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2013 = apply_labels(us_ct_income_2013,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2013$households <- us_ct_income_2013$S1901_C01_001E
us_ct_income_2013$income <- us_ct_income_2013$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2013 = us_ct_income_2013[-1,]

# identify NA values
table(us_ct_income_2013$income) ## there are 1000 "-" values
us_ct_income_2013["income"][us_ct_income_2013["income"] == "-"] <- NA
us_ct_income_2013["income"][us_ct_income_2013["income"] == "(X)"] <- NA
us_ct_income_2013["income"][us_ct_income_2013["income"] == "2,500-"] <- NA
us_ct_income_2013["income"][us_ct_income_2013["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2013$income)) ## 1037 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2013$income), na.rm=T)
mean(as.numeric(us_ct_income_2013$income), na.rm=T)
median(as.numeric(us_ct_income_2013$income), na.rm=T)
hist(as.numeric(us_ct_income_2013$income))

###############################################################################################################
## US census tracts -- income, 2012 5-year estimates (2008-2012)
###############################################################################################################

# load the data
# load packages
us_ct_income_2012 <- read.csv("ACSST5Y2012.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2012 = apply_labels(us_ct_income_2012,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2012$households <- us_ct_income_2012$S1901_C01_001E
us_ct_income_2012$income <- us_ct_income_2012$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2012 = us_ct_income_2012[-1,]

# identify NA values
table(us_ct_income_2012$income) ## there are 965 "-" values
us_ct_income_2012["income"][us_ct_income_2012["income"] == "-"] <- NA
us_ct_income_2012["income"][us_ct_income_2012["income"] == "(X)"] <- NA
us_ct_income_2012["income"][us_ct_income_2012["income"] == "2,500-"] <- NA
us_ct_income_2012["income"][us_ct_income_2012["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2012$income)) ## 975 CTs with missing data out of 74001

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2012$income), na.rm=T)
mean(as.numeric(us_ct_income_2012$income), na.rm=T)
median(as.numeric(us_ct_income_2012$income), na.rm=T)
hist(as.numeric(us_ct_income_2012$income))

###############################################################################################################
## US census tracts -- income, 2011 5-year estimates (2007-2011)
###############################################################################################################

# load the data
# load packages
us_ct_income_2011 <- read.csv("ACSST5Y2011.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2011 = apply_labels(us_ct_income_2011,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2011$households <- us_ct_income_2011$S1901_C01_001E
us_ct_income_2011$income <- us_ct_income_2011$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2011 = us_ct_income_2011[-1,]

# identify NA values
table(us_ct_income_2011$income) ## there are 971 "-" values
us_ct_income_2011["income"][us_ct_income_2011["income"] == "-"] <- NA
us_ct_income_2011["income"][us_ct_income_2011["income"] == "(X)"] <- NA
us_ct_income_2011["income"][us_ct_income_2011["income"] == "2,500-"] <- NA
us_ct_income_2011["income"][us_ct_income_2011["income"] == "250,000+"] <- NA

# how many NA values?
sum(is.na(us_ct_income_2011$income)) ## 1009 CTs with missing data out of 74134

# understand the distribution at this one time period
range(as.numeric(us_ct_income_2011$income), na.rm=T)
mean(as.numeric(us_ct_income_2011$income), na.rm=T)
median(as.numeric(us_ct_income_2011$income), na.rm=T)
hist(as.numeric(us_ct_income_2011$income))

###############################################################################################################
## US census tracts -- income, 2010 5-year estimates (2006-2010)
###############################################################################################################

# load the data
# load packages
us_ct_income_2010 <- read.csv("ACSST5Y2010.S1901-Data-us-ct.csv")

# label and rename variables so it's easier for me
us_ct_income_2010 = apply_labels(us_ct_income_2010,
                                 S1901_C01_001E = "Total number of households",
                                 S1901_C01_012E = "Median household income ($)")
us_ct_income_2010$households <- us_ct_income_2010$S1901_C01_001E
us_ct_income_2010$income <- us_ct_income_2010$S1901_C01_012E

# remove the first row of the dataset
us_ct_income_2010 = us_ct_income_2010[-1,]

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

###############################################################################################################
###############################################################################################################
###############################################################################################################
## US census tracts -- percent change in average median household income
###############################################################################################################
###############################################################################################################
###############################################################################################################

############## percent change between 2021 vs 2020 ############## 

library(dplyr)
joined_df_1_us <- us_ct_income_2021 %>% select(GEO_ID, income) %>% inner_join(us_ct_income_2020 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2021 and 2020)
joined_df_1_us$pct_change <- (as.numeric(joined_df_1_us$income.x)-as.numeric(joined_df_1_us$income.y))/as.numeric(joined_df_1_us$income.y)*100

# make histogram
hist(joined_df_1_us$pct_change)
hist_us_2021_2020 <- hist(joined_df_1_us$pct_change, main="Percent Change in Household Income between 2021 and 2020, US Census Tracts", xlab="% change",breaks=100,xlim=c(-25, 100))

########### percent change between 2020 vs 2010 ############
joined_df_2_us <- us_ct_income_2020 %>% select(GEO_ID, income) %>% inner_join(us_ct_income_2010 %>% select(GEO_ID, income), by=c('GEO_ID'='GEO_ID'))

# create percent change variable (between 2020 and 2010)
joined_df_2_us$pct_change <- (as.numeric(joined_df_2_us$income.x)-as.numeric(joined_df_2_us$income.y))/as.numeric(joined_df_2_us$income.y)*100

# make histogram
hist(joined_df_2_us$pct_change)
hist_us_2020_2010 <- hist(joined_df_2_us$pct_change, main="Percent Change in Household Income between 2020 and 2010, US Census Tracts", xlab="% change",breaks=150,xlim=c(-100, 200), xaxp=c(-100,200,12))

# trying to add density lines -- have to change it from a frequency histogram to a density histogram
densityhist_us_2020_2010 <- hist(joined_df_2_us$pct_change, prob=TRUE, col="grey", main="Percent Change in Household Income between 2020 and 2010, US Census Tracts", xlab="% change",breaks=150,xlim=c(-100, 200), xaxp=c(-100,200,12))
lines(density((joined_df_2_us$pct_change), y=NULL, na.rm=TRUE),col="blue", lwd=2)



########## preparing dataset for ArcGIS Pro mapping #############

## first for 2021-2020
# Replace first 9 characters with empty string ""
joined_df_1_us$GEO_ID <- gsub("^.{0,9}", "", joined_df_1_us$GEO_ID)

# change geoid name to FIPS

# save as csv
write.csv(joined_df_1_us, "/Users/angeladadamo/Desktop/ECHO/ACS-variables-over-time/us_arcgis_2021_2020.csv", row.names=FALSE)

## now for 2020-2010
joined_df_2_us$GEO_ID <- gsub("^.{0,9}", "", joined_df_2_us$GEO_ID)

# change geoid name to FIPS
joined_df_2_us <- joined_df_2_us %>% 
  rename("FIPS" = "GEO_ID")

# save as csv
write.csv(joined_df_2_us, "/Users/angeladadamo/Desktop/ECHO/ACS-variables-over-time/us_arcgis_2020_2010.csv", row.names=FALSE)
