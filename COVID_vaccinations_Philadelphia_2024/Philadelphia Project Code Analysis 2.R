# rerunning the analysis using the updated dataset from Usama in July of 2023
setwd("/Users/angeladadamo/Documents/THESIS PROJECT/Philadelphia Data")

cleanvaxdata_2 <- read.csv("OneDrive_1_7-11-2023/Clean_Data_Vax_Disparities_Philadelphia_WIDE_v2.csv")

library(ggplot2)
library(plotrix) #for standard errors

cleanvaxdata_2_missing <- cleanvaxdata_2[!complete.cases(cleanvaxdata_2),]
summary(cleanvaxdata_2_missing)
# no NA values

## creating new variables for MH income tertiles: low, medium, high

# find tertiles
vTert <- quantile(cleanvaxdata_2$mhi, c(0:3/3))

# classify values
cleanvaxdata_2$MHItertiles <- with(cleanvaxdata_2, 
                                 cut(mhi, 
                                     vTert, 
                                     include.lowest = T, 
                                     labels = c("Low", "Medium", "High")))


## create a box plot to check values if categorical variable
ggplot(cleanvaxdata_2, aes(x=MHItertiles, y=mhi, group= MHItertiles))+
  geom_boxplot()

# or (do not need group argument here)
ggplot(cleanvaxdata_2, aes(x=MHItertiles, y=mhi)) + geom_boxplot()


## frequency table to check values too
as.data.frame(table(cleanvaxdata_2$MHItertiles))


## create majority race/ethnicity variable
cleanvaxdata_2$majority_race_ethn <- 0

for (i in 1:nrow(cleanvaxdata_2)) {
  row <- cleanvaxdata_2[i, c('proportion_hisp', 'proportion_nhblack', 'proportion_nhwhite', 'proportion_nhapi', 'proportion_nhaian', 'proportion_nhother')]
  numMajority <- 0
  for (j in 1:ncol(row)) {
    if (row[,j] > .6) {
      numMajority <- j
    }
  }
  if (numMajority == 0) {
    numMajority <- 7
  }
  
  nameVal <- switch(numMajority, 'hisp', 'nhblack', 'nhwhite', 'nhapi', 'nhaian', 'nhother', 'mixed')
  
  cleanvaxdata_2$majority_race_ethn[i] <- nameVal
}

## create a frequency distribution table for the majority variable
table(cleanvaxdata_2$majority_race_ethn)


## transforming majority race/ethn variable into a factor
cleanvaxdata_2$majority_race_ethn <- as.factor(cleanvaxdata_2$majority_race_ethn)

## frequency table to check values again
as.data.frame(table(cleanvaxdata_2$majority_race_ethn))

## Truncating values to 1 for the vaccination rate variable
#values already all under 1!

## Running univariate statistics!

# independent variable #1 -- median household income, categorical (MHITertiles)
#central tendency = mode
#spread = frequency distribution
# Creating a function to find the modal value (no built-in mode function in R)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Using the Mode function to find the mode of MHITertiles
Mode(cleanvaxdata_2$MHItertiles)

# Creating a frequency distribution of MHITertiles
as.data.frame(table(cleanvaxdata_2$MHItertiles))

# independent variable #2 -- majority racial/ethnic group, categorical (majority_race_ethn)
#central tendency = mode
#spread = frequency distribution

# Using the Mode function to find the mode of majority_race_ethn
Mode(cleanvaxdata_2$majority_race_ethn)

# Creating a frequency distribution of majority_race_ethn
as.data.frame(table(cleanvaxdata_2$majority_race_ethn))

# dependent variable -- proportion fully vaccinated from april 13th, continuous (proportion_fully_vaccinated_total)
#central tendency = mean, median, mode
#spread = range, standard deviation

## Finding the mean of each vaccination rate variable

# Proportion fully vaccinated by april 18th, 2021
mean(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_04_18)

# Proportion fully vaccinated by september 26th, 2021
mean(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_09_26)

# Proportion fully vaccinated by november 21st, 2021
mean(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_11_21)

# Proportion fully vaccinated by April 3rd, 2022
mean(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_04_03)

# Proportion fully vaccinated by June 26th, 2022
mean(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_06_26)

# Proportion fully vaccinated by August 7th, 2022
mean(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_08_07)

## Finding the median of each vaccination rate variable

# Proportion fully vaccinated by april 18th, 2021
median(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_04_18)

# Proportion fully vaccinated by september 26th, 2021
median(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_09_26)

# Proportion fully vaccinated by november 21st, 2021
median(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_11_21)

# Proportion fully vaccinated by April 3rd, 2022
median(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_04_03)

# Proportion fully vaccinated by June 26th, 2022
median(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_06_26)

# Proportion fully vaccinated by August 7th, 2022
median(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_08_07)

## Finding the mode of each vaccination rate variable using Mode function

# Proportion fully vaccinated by april 18th, 2021
Mode(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_04_18)

# Proportion fully vaccinated by september 26th, 2021
Mode(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_09_26)

# Proportion fully vaccinated by november 21st, 2021
Mode(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_11_21)

# Proportion fully vaccinated by november 21st, 2021
Mode(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_04_03)

# Proportion fully vaccinated by november 21st, 2021
Mode(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_06_26)

# Proportion fully vaccinated by August 7th, 2022
Mode(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_08_07)

## Finding the range of each vaccination rate variable

# Proportion fully vaccinated by april 18th, 2021
range(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_04_18)

# Proportion fully vaccinated by september 26th, 2021
range(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_09_26)

# Proportion fully vaccinated by november 21st, 2021
range(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_11_21)

# Proportion fully vaccinated by November April 3rd, 2022
range(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_04_03)

# Proportion fully vaccinated by June 26th, 2022
range(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_06_26)

# Proportion fully vaccinated by August 7th, 2022
range(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_08_07)

## Finding the standard deviation of each vaccination rate variable

# Proportion fully vaccinated by april 18th, 2021
sd(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_04_18)

# Proportion fully vaccinated by september 26th, 2021
sd(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_09_26)

# Proportion fully vaccinated by november 21st, 2021
sd(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_11_21)

# Proportion fully vaccinated by April 3rd, 2022
sd(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_04_03)

# Proportion fully vaccinated by June 26th, 2022
sd(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_06_26)

# Proportion fully vaccinated by August 7th, 2022
sd(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_08_07)

## Running bivariate statistics!

#ANOVA between income level and each of the vaccination rates by date
#ANOVA between majority racial/ethnic group and each of the vaccination rates by date

#need to create a fitted model to run ANOVA
#fitting linear model #1 -- vax rate 4/18/21 ~ income level
model1 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_04_18 ~ cleanvaxdata_2$MHItertiles)

#ANOVA of model 1
anova(model1)

#summary of the model
summary(model1)

#fitting linear model #2 -- vax rate 9/26/21 ~ income level
model2 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_09_26 ~ cleanvaxdata_2$MHItertiles)

#ANOVA of model 2
anova(model2)

#summary of the model
summary(model2)

#fitting linear model #3 -- vax rate 11/21/21 ~ income level
model3 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_11_21 ~ cleanvaxdata_2$MHItertiles)

#ANOVA of model 3
anova(model3)

#summary of the model
summary(model3)

#fitting linear model #4 -- vax rate 4/3/22 ~ income level
model4 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_04_03 ~ cleanvaxdata_2$MHItertiles)

#ANOVA of model 4
anova(model4)

#summary of the model
summary(model4)

#fitting linear model #5 -- vax rate 6/26/22 ~ income level
model5 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_06_26 ~ cleanvaxdata_2$MHItertiles)

#ANOVA of model 5
anova(model5)

#summary of the model
summary(model5)

#fitting linear model #6 -- vax rate 8/7/22 ~ income level
model6 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_08_07 ~ cleanvaxdata_2$MHItertiles) #can use original data set income data because this doesn't change between the updated data

#ANOVA of model 6
anova(model6)

#summary of the model
summary(model6)

##t tests and standard errors for the different income tertiles + each vaccination rate by date
#t test for MHItertiles 'Low'

#for 4/18/21
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2021_04_18"])

#for 9/26/21
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2021_09_26"])

#for 11/21/21
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2021_11_21"])

#for 4/3/22
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2022_04_03"])

#for 6/26/22
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2022_06_26"])

#for 8/7/22
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#find the standard error of MHItertiles 'Low'
std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2021_04_18"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2021_09_26"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2021_11_21"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2022_04_03"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2022_06_26"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Low', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#t test for MHItertiles 'Medium'
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2021_04_18"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2021_09_26"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2021_11_21"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2022_04_03"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2022_06_26"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#find the standard error of MHItertiles 'Medium'
std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2021_04_18"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2021_09_26"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2021_11_21"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2022_04_03"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2022_06_26"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='Medium', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#t test for MHItertiles 'High'
t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2021_04_18"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2021_09_26"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2021_11_21"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2022_04_03"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2022_06_26"])

t.test(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2022_08_07"])

# find the standard error of MHItertiles 'High'
std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2021_04_18"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2021_09_26"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2021_11_21"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2022_04_03"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2022_06_26"])

std.error(cleanvaxdata_2[cleanvaxdata_2$MHItertiles=='High', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#fitting linear model #6 -- vax rate for 4/18/21 ~ majority racial/ethnic group
model6 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_04_18 ~ cleanvaxdata_2$majority_race_ethn)

#ANOVA of model 6
anova(model6)

#summary of the model
summary(model6)

#fitting linear model #7 -- vax rate for 9/26/21 ~ majority racial/ethnic group
model7 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_09_26 ~ cleanvaxdata_2$majority_race_ethn)

#ANOVA of model 7
anova(model7)

#summary of the model
summary(model7)

#fitting linear model #8 -- vax rate for 11/21/21 ~ majority racial/ethnic group
model8 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2021_11_21 ~ cleanvaxdata_2$majority_race_ethn)

#ANOVA of model 8
anova(model8)

#summary of the model
summary(model8)

#fitting linear model #9 -- vax rate for 4/3/22 ~ majority racial/ethnic group
model9 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_04_03 ~ cleanvaxdata_2$majority_race_ethn)

#ANOVA of model 9
anova(model9)

#summary of the model
summary(model9)

#fitting linear model #10 -- vax rate for 6/26/22 ~ majority racial/ethnic group
model10 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_06_26 ~ cleanvaxdata_2$majority_race_ethn)

#ANOVA of model 10
anova(model10)

#summary of the model
summary(model10)

#fitting linear model #11 -- vax rate for 8/7/22 ~ majority racial/ethnic group
model11 <- lm(cleanvaxdata_2$proportion_fully_vaccinated_cumulative_2022_08_07 ~ cleanvaxdata_2$majority_race_ethn) #same as before -- race doesn't change between data sets

#ANOVA of model 11
anova(model11)

#summary of the model
summary(model11)

##t tests and standard errors of the different racial/ethnic classifications + vaccination rates by date
# April 18, 2021
#t test for majority_race_ethn 'mixed' -- 4/18/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2021_04_18"])

#find the standard error of majority_race_ethn 'mixed' -- 4/18/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2021_04_18"])

#t test for majority_race_ethn 'nhwhite' -- 4/18/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2021_04_18"])

#find the standard error of majority_race_ethn 'nhwhite' -- 4/18/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2021_04_18"])

#t test for majority_race_ethn 'nhblack' -- 4/18/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2021_04_18"])

#find the standard error of majority_race_ethn 'nhblack' -- 4/18/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2021_04_18"])   

# September 26, 2021
#t test for majority_race_ethn 'mixed' -- 9/26/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2021_09_26"])

#find the standard error of majority_race_ethn 'mixed' -- 9/26/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2021_09_26"])

#t test for majority_race_ethn 'nhwhite' -- 9/26/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2021_09_26"])

#find the standard error of majority_race_ethn 'nhwhite' -- 9/26/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2021_09_26"])

#t test for majority_race_ethn 'nhblack' -- 9/26/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2021_09_26"])

#find the standard error of majority_race_ethn 'nhblack' -- 9/26/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2021_09_26"]) 

#November 21, 2021
#t test for majority_race_ethn 'mixed' -- 11/21/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2021_11_21"])

#find the standard error of majority_race_ethn 'mixed' -- 11/21/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2021_11_21"])

#t test for majority_race_ethn 'nhwhite' -- 11/21/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2021_11_21"])

#find the standard error of majority_race_ethn 'nhwhite' -- 11/21/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2021_11_21"])

#t test for majority_race_ethn 'nhblack' -- 11/21/21
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2021_11_21"])

#find the standard error of majority_race_ethn 'nhblack' -- 11/21/21
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2021_11_21"]) 

#April 3, 2022
#t test for majority_race_ethn 'mixed' -- 4/3/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2022_04_03"])

#find the standard error of majority_race_ethn 'mixed' -- 4/3/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2022_04_03"])

#t test for majority_race_ethn 'nhwhite' -- 4/3/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2022_04_03"])

#find the standard error of majority_race_ethn 'nhwhite' -- 4/3/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2022_04_03"])

#t test for majority_race_ethn 'nhblack' -- 4/3/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2022_04_03"])

#find the standard error of majority_race_ethn 'nhblack' -- 4/3/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2022_04_03"]) 

#June 26, 2022
#t test for majority_race_ethn 'mixed' -- 6/26/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2022_06_26"])

#find the standard error of majority_race_ethn 'mixed' -- 6/26/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2022_06_26"])

#t test for majority_race_ethn 'nhwhite' -- 6/26/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2022_06_26"])

#find the standard error of majority_race_ethn 'nhwhite' -- 6/26/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2022_06_26"])

#t test for majority_race_ethn 'nhblack' -- 6/26/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2022_06_26"])

#find the standard error of majority_race_ethn 'nhblack' -- 6/26/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2022_06_26"]) 

#August 7, 2022
#t test for majority_race_ethn 'mixed' -- 8/7/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#find the standard error of majority_race_ethn 'mixed' -- 8/7/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='mixed', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#t test for majority_race_ethn 'nhwhite' -- 8/7/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#find the standard error of majority_race_ethn 'nhwhite' -- 8/7/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhwhite', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#t test for majority_race_ethn 'nhblack' -- 8/7/22
t.test(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2022_08_07"])

#find the standard error of majority_race_ethn 'nhblack' -- 8/7/22
std.error(cleanvaxdata_2[cleanvaxdata_2$majority_race_ethn=='nhblack', "proportion_fully_vaccinated_cumulative_2022_08_07"]) 


