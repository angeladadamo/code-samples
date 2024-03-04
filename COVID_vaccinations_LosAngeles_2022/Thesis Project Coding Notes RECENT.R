# first, set working directory
setwd("/Users/angeladadamo/Documents/THESIS PROJECT/Thesis Data")
# load packages!
library(ggplot2)

# load the data!
cleanvaxdata<-read.csv("../Clean_Data_Vax_Disparities.csv")


# before cleaning no NA values, I'd do an exploration here. who are those NA values?
cleanvaxdata_missing <- cleanvaxdata[!complete.cases(cleanvaxdata),]
summary(cleanvaxdata_missing)
# what's the missing pattern? what variables are missing most commonly?
# lets look at missing vax
missing_vax<-cleanvaxdata_missing[is.na(cleanvaxdata_missing$proportion_fully_vaccinated_total),]
hist(missing_vax$total_pop)
# very small ZCTAs
# lets look at missing demographics
missing_demo<-cleanvaxdata_missing[is.na(cleanvaxdata_missing$proportion_hisp),]
missing_demo$total_pop
# unpopulated ZCTAs


# now we can create our anlaytic dataset
## creating dataset with no NA values
cleanvaxdata_NONA <- na.omit(cleanvaxdata)

## creating dataset with only zipcodes in LA Metropolitan
cleanvaxdata_LAMET <- cleanvaxdata[cleanvaxdata$county == 'Los Angeles' | cleanvaxdata$county == 'Ventura' | cleanvaxdata$county == 'Orange' | cleanvaxdata$county == 'Riverside' | cleanvaxdata$county == 'San Bernardino', ]
## good! just showing an alternative here:
cleanvaxdata_LAMET <- cleanvaxdata[cleanvaxdata$county %in% c('Los Angeles', 'Ventura', 'Orange', 'Riverside','San Bernardino'), ]
## of note, the seond one already excludes those pesky missing ZCTAs

## creating dataset with only LA Met zipcodes and no NA values
cleanvaxdata_LAMET_NONA <- na.omit(cleanvaxdata_LAMET)

# creating a log(y) variable
cleanvaxdata_LAMET_NONA$log_proportion_fully_vaccinated_total <- log(cleanvaxdata_LAMET_NONA$proportion_fully_vaccinated_total)

#NOTE: the data already includes them as proportions? so doing this just removes the variables. 
# creating a new variable label for each racial/ethnic variable
    #percent Hispanic
    cleanvaxdata_LAMET_NONA$proportion_hisp <- cleanvaxdata_LAMET_NONA$pct_hisp

    #percent NH Black
    cleanvaxdata_LAMET_NONA$proportion_nhblack <- cleanvaxdata_LAMET_NONA$pct_nhblack

    #percent NH White
    cleanvaxdata_LAMET_NONA$proportion_nhwhite <- cleanvaxdata_LAMET_NONA$pct_nhwhite

    #percent NH Asian/Pacific Islander
    cleanvaxdata_LAMET_NONA$proportion_nhapi <- cleanvaxdata_LAMET_NONA$pct_nhapi

    #percent NH American Indian/Alaska Native
    cleanvaxdata_LAMET_NONA$proportion_nhaian <- cleanvaxdata_LAMET_NONA$pct_nhaian

    #percent Other/Multiracial
    cleanvaxdata_LAMET_NONA$proportion_nhother <- cleanvaxdata_LAMET_NONA$pct_nhother
    

## creating new variables for MH income tertiles: low, medium, high

# find tertiles
vTert <- quantile(cleanvaxdata_LAMET_NONA$mhi, c(0:3/3))

# classify values
cleanvaxdata_LAMET_NONA$MHItertiles <- with(cleanvaxdata_LAMET_NONA, 
               cut(mhi, 
                   vTert, 
                   include.lowest = T, 
                   labels = c("Low", "Medium", "High")))

## creating numeric variables for income terciles

cleanvaxdata_LAMET_NONA$mhiLow <- ifelse(cleanvaxdata_LAMET_NONA$MHItertiles == 'Low', 1, 0)
cleanvaxdata_LAMET_NONA$mhiMedium <- ifelse(cleanvaxdata_LAMET_NONA$MHItertiles == 'Medium', 1, 0)
cleanvaxdata_LAMET_NONA$mhiHigh <- ifelse(cleanvaxdata_LAMET_NONA$MHItertiles == 'High', 1, 0)


## creating entropy, h, variable (link site)

#creating a log function
customLog <- function(x) {
  ifelse(x == 0, 0, log(x))
}

cleanvaxdata_LAMET_NONA$entropyH <- -1 * ( (cleanvaxdata_LAMET_NONA$proportion_hisp*customLog(cleanvaxdata_LAMET_NONA$proportion_hisp))+
                                             (cleanvaxdata_LAMET_NONA$proportion_nhblack*customLog(cleanvaxdata_LAMET_NONA$proportion_nhblack))+
                                             (cleanvaxdata_LAMET_NONA$proportion_nhwhite*customLog(cleanvaxdata_LAMET_NONA$proportion_nhwhite))+
                                             (cleanvaxdata_LAMET_NONA$proportion_nhapi*customLog(cleanvaxdata_LAMET_NONA$proportion_nhapi))+
                                             (cleanvaxdata_LAMET_NONA$proportion_nhaian*customLog(cleanvaxdata_LAMET_NONA$proportion_nhaian))+
                                             (cleanvaxdata_LAMET_NONA$proportion_nhother*customLog(cleanvaxdata_LAMET_NONA$proportion_nhother)))

## create a box plot to check values if categorical variable
ggplot(cleanvaxdata_LAMET_NONA, aes(x=MHItertiles, y=mhi, group= MHItertiles))+
    geom_boxplot()

# or (do not need group argument here)
ggplot(cleanvaxdata_LAMET_NONA, aes(x=MHItertiles, y=mhi)) + geom_boxplot()

## plotting income tertiles against entropy, h
ggplot(cleanvaxdata_LAMET_NONA, aes(x=MHItertiles, y=entropyH)) + geom_boxplot()

## plotting income (continuous) against entropy, h
ggplot(cleanvaxdata_LAMET_NONA, aes(x=mhi, y=entropyH)) + geom_point() + stat_smooth(method = "loess")

## plotting entropy against vaccination rates
ggplot(cleanvaxdata_LAMET_NONA, aes(x=entropyH, y=proportion_fully_vaccinated_total)) + geom_point() + stat_smooth(method = "loess")

# histograms are also useful:
ggplot(cleanvaxdata_LAMET_NONA, aes(x=entropyH)) + 
  geom_histogram(bins=30, fill="gray", color="black")
# you can also explore by county!
ggplot(cleanvaxdata_LAMET_NONA, aes(x=entropyH)) + 
  geom_histogram(bins=30, fill="gray", color="black") +
  facet_wrap(~county)
ggplot(cleanvaxdata_LAMET_NONA, aes(x=mhi)) + 
  geom_histogram(bins=30, fill="gray", color="black") +
  facet_wrap(~county)
# or even overlay
ggplot(cleanvaxdata_LAMET_NONA, aes(x=mhi)) + 
  geom_density(aes(color=county))
ggplot(cleanvaxdata_LAMET_NONA, aes(x=entropyH)) + 
  geom_density(aes(color=county))
ggplot(cleanvaxdata_LAMET_NONA, aes(x=proportion_hisp)) + 
  geom_density(aes(color=county))
ggplot(cleanvaxdata_LAMET_NONA, aes(x=proportion_nhblack)) + 
  geom_density(aes(color=county))
ggplot(cleanvaxdata_LAMET_NONA, aes(x=proportion_nhwhite)) + 
  geom_density(aes(color=county))

## frequency table to check values too
as.data.frame(table(cleanvaxdata_LAMET_NONA$MHItertiles))

## create minority variable
cleanvaxdata_LAMET_NONA$minority_group <- 0

for (i in 1:nrow(cleanvaxdata_LAMET_NONA)) {
  row <- cleanvaxdata_LAMET_NONA[i, c('proportion_hisp', 'proportion_nhblack', 'proportion_nhwhite', 'proportion_nhapi', 'proportion_nhaian', 'proportion_nhother')]
  minVal <- min(row)
  nameVal <- ''
  for (j in 1:ncol(row)) {
    if (row[,j] == minVal) {
      nameVal <- switch(j, 'hisp', 'nhblack', 'nhwhite', 'nhapi', 'nhaian', 'nhother')
    }
  }
  
  cleanvaxdata_LAMET_NONA$minority_group[i] <- nameVal
}

## create majority race/ethnicity variable
cleanvaxdata_LAMET_NONA$majority <- 0

for (i in 1:nrow(cleanvaxdata_LAMET_NONA)) {
  row <- cleanvaxdata_LAMET_NONA[i, c('proportion_hisp', 'proportion_nhblack', 'proportion_nhwhite', 'proportion_nhapi', 'proportion_nhaian', 'proportion_nhother')]
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
  
  cleanvaxdata_LAMET_NONA$majority[i] <- nameVal
}

## create a frequency distribution table for the majority variable
table(cleanvaxdata_LAMET_NONA$majority)

## create a box plot to check values if categorical variable
ggplot(cleanvaxdata_LAMET_NONA, aes(x=minority_group, y=proportion_fully_vaccinated_total))+
  geom_boxplot()

## transforming majority variable into a factor
cleanvaxdata_LAMET_NONA$majority_race_ethn <- as.factor(cleanvaxdata_LAMET_NONA$majority)

## frequency table to check values again
as.data.frame(table(cleanvaxdata_LAMET_NONA$majority_race_ethn))

## Truncating values to 1 for the vaccination rate variable
for (i in 1:nrow(cleanvaxdata_LAMET_NONA)) {
  if (cleanvaxdata_LAMET_NONA$proportion_fully_vaccinated_total[i] > 1) {
    cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated[i] <- 1
  }
  else {
    cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated[i] <- cleanvaxdata_LAMET_NONA$proportion_fully_vaccinated_total[i]
  }
}

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
        Mode(cleanvaxdata_LAMET_NONA$MHItertiles)
        
      # Creating a frequency distribution of MHITertiles
        as.data.frame(table(cleanvaxdata_LAMET_NONA$MHItertiles))
        
    # independent variable #2 -- majority racial/ethnic group, categorical (majority_race_ethn)
      #central tendency = mode
      #spread = frequency distribution
        
        # Using the Mode function to find the mode of majority_race_ethn
        Mode(cleanvaxdata_LAMET_NONA$majority_race_ethn)
        
        # Creating a frequency distribution of majority_race_ethn
        as.data.frame(table(cleanvaxdata_LAMET_NONA$majority_race_ethn))
        
    # dependent variable -- proportion fully vaccinated from april 13th, continuous (proportion_fully_vaccinated_total)
      #central tendency = mean, median, mode
      #spread = range, standard deviation
        
        # Finding the mean of vaccination rate variable
        mean(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
        # Finding the median of vaccination rate variable
        median(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
        # Finding the mode of vaccination rate variable using Mode function
        Mode(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
        
        # Finding the range of vaccination rate variable
        range(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
        # Finding the standard deviation of vaccination rate variable
        sd(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
        
## Running bivariate statistics!
        
  #ANOVA between income level and vaccination rates
  #ANOVA between majority racial/ethnic group and vaccination rates
        
    #need to create a fitted model to run ANOVA
        #fitting linear model #1 -- vax rates ~ income level
        model1 <- lm(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated ~ cleanvaxdata_LAMET_NONA$MHItertiles)
        
        #ANOVA of model 1
        anova(model1)
        
        #summary of the model
        summary(model1)
        
        #can also do **
        summary(aov(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated ~ cleanvaxdata_LAMET_NONA$MHItertiles, data = cleanvaxdata_LAMET_NONA))
    
  ##t tests and standard errors for the different income tertiles + prop vax truncated
    #t test for MHItertiles 'Low'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='Low', "prop_fully_vax_truncated"])
        
    #load plotrix package for the standard error
        library(plotrix)
        
    #find the standard error of MHItertiles 'Low'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='Low', "prop_fully_vax_truncated"])
        
    #t test for MHItertiles 'Medium'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='Medium', "prop_fully_vax_truncated"])
        
    #find the standard error of MHItertiles 'Medium'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='Medium', "prop_fully_vax_truncated"])
        
    #t test for MHItertiles 'High'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='High', "prop_fully_vax_truncated"])
        
    # find the standard error of MHItertiles 'High'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='High', "prop_fully_vax_truncated"])
    
        
    #fitting linear model #2 -- vax rates ~ majority racial/ethnic group
        model2 <- lm(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated ~ cleanvaxdata_LAMET_NONA$majority_race_ethn)
        
    #ANOVA of model 2
        anova(model2)
        
    #summary of the model
        summary(model2)
        
        
  ##t tests and standard errors of the different racial/ethnic classifications + prop vax truncated
    #t test for majority_race_ethn 'mixed'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='mixed', "prop_fully_vax_truncated"])
        
    #find the standard error of majority_race_ethn 'mixed'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='mixed', "prop_fully_vax_truncated"])
        
    #t test for majority_race_ethn 'hisp'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='hisp', "prop_fully_vax_truncated"])
        
    #find the standard error of majority_race_ethn 'hisp'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='hisp', "prop_fully_vax_truncated"])
        
    #t test for majority_race_ethn 'nhblack'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='nhblack', "prop_fully_vax_truncated"])
        
    #find the standard error of majority_race_ethn 'nhblack'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='nhblack', "prop_fully_vax_truncated"])    
      
    #t test for majority_race_ethn 'nhapi'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='nhapi', "prop_fully_vax_truncated"])
        
    #find the standard error of majority_race_ethn 'nhapi'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='nhapi', "prop_fully_vax_truncated"])
        
    #t test for majority_race_ethn 'nhwhite'
        t.test(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='nhwhite', "prop_fully_vax_truncated"])
        
    #find the standard error of majority_race_ethn 'nhwhite'
        std.error(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn=='nhwhite', "prop_fully_vax_truncated"])
        
## Trying to run an ANCOVA
#load dplyr package
library(dplyr)
library(tidyr)

    # ANCOVA cross tab
    table_example <- cleanvaxdata_LAMET_NONA %>% group_by(MHItertiles, majority_race_ethn) %>% summarise(mean_rate=mean(prop_fully_vax_truncated)) %>% spread(MHItertiles, mean_rate)

    # save as .csv
    write.csv(table_example, file="ancova cross tab.csv")



## How to save data file as .csv named "output" as an example
write.csv(cleanvaxdata_LAMET_NONA, 'output.csv')

## Running inferential statistics
#compute the standard error of the mean
## calculating SE -- inferential statistics, because although my data represents the universe, my exposures are from a sample and there may be sampling error
    # Calculate the mean and standard error
    l.model <- lm(prop_fully_vax_truncated ~ 1, cleanvaxdata_LAMET_NONA)

    # Calculate the confidence interval -- if small, we can say my sample mean is very predictive of the true population mean
    confint(l.model, level=0.95)

## Creating a clean datasat for GIS
    #select variables v1, v2, v3
    myvars <- c("prop_fully_vax_truncated", "MHItertiles", "majority_race_ethn")
    GISdata <- cleanvaxdata_LAMET_NONA[myvars]

## Finding the mean of prop_fully_vax_truncated by levels of MHItertiles
    by(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated, cleanvaxdata_LAMET_NONA$MHItertiles, mean)
    # this returns the anova
    summary(aov(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated~cleanvaxdata_LAMET_NONA$MHItertiles, data=cleanvaxdata_LAMET_NONA))
    
## Finding the mean of prop_fully_vax_truncated by racial/ethnic classification 
  by(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated, cleanvaxdata_LAMET_NONA$majority_race_ethn, mean)
    
    #showing the mean vaccination proportion by income level in a table
    cleanvaxdata_LAMET_NONA %>% 
      group_by(MHItertiles) %>% 
      summarise(mean_vax=mean(prop_fully_vax_truncated))
    
  #showing the mean vaccination proportion by racial/ethnic classification in a table
    cleanvaxdata_LAMET_NONA %>% 
      group_by(majority_race_ethn) %>% 
      summarise(mean_vax=mean(prop_fully_vax_truncated))
    
  #mean vaccination proportion by racial/ethnic classification
    vax_by_race<-cleanvaxdata_LAMET_NONA %>% 
      group_by(majority_race_ethn) %>% 
      summarise(mean_vax=mean(prop_fully_vax_truncated))
    
  #plot of mean vaccination proportion by racial/ethnic classification
    ggplot(vax_by_race, aes(x=majority_race_ethn, y=mean_vax)) +
      geom_col(color='black', aes(fill=majority_race_ethn))
    
  #mean vaccination proportion by income level
    vax_by_income<- cleanvaxdata_LAMET_NONA %>% 
      group_by(MHItertiles) %>% 
      summarise(mean_vax=mean(prop_fully_vax_truncated))
    
  #plot of mean vaccination proportion by income level
    ggplot(vax_by_income, aes(x=MHItertiles, y=mean_vax)) +
      geom_col(color='black', aes(fill=MHItertiles))
    
  #mean vaccination proportion for each income level by racial/ethnic classification
    vax_by_race_income<-cleanvaxdata_LAMET_NONA %>% 
      group_by(majority_race_ethn, MHItertiles) %>% 
      summarise(mean_vax=mean(prop_fully_vax_truncated))
    
  #plot of mean vaccination proportion for each income level by racial/ethnic classification
    ggplot(vax_by_race_income, aes(x=majority_race_ethn, y=mean_vax)) +
      geom_col(color='black', aes(fill=MHItertiles), 
               width=0.7, 
               position=position_dodge2(width=0.7))
    
  #plot of proportion vaccinated by racial/ethnic classification (data points and box)
    ggplot(cleanvaxdata_LAMET_NONA, aes(x=majority_race_ethn, y=prop_fully_vax_truncated))  +
      geom_boxplot(aes(group=majority_race_ethn)) +
      geom_jitter(aes(fill=majority_race_ethn), width=0.3, height=0,
                  color="black", pch=21)

##Find standard deviation for vaccination variable
    sd(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
    
## Finding the IQR for MHI
  #returns the actual range
    IQR(cleanvaxdata_LAMET_NONA$mhi)
  
  #returns the median value
    median(cleanvaxdata_LAMET_NONA$mhi)
    
  #these return the numbers for the range (at 25% and 75%)
    quantile(cleanvaxdata_LAMET_NONA$mhi, 1/4)
    
    quantile(cleanvaxdata_LAMET_NONA$mhi, 3/4)

## Finding the IQR for prop_fully_vax_truncated
    IQR(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
    
    #returns the median value
    median(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated)
    
    #these return the numbers for the range (at 25% and 75%)
    quantile(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated, 1/4)
    
    quantile(cleanvaxdata_LAMET_NONA$prop_fully_vax_truncated, 3/4)
    
    
## two way ANOVA test
   two_way_anova <- aov(prop_fully_vax_truncated ~ MHItertiles + majority_race_ethn, data = cleanvaxdata_LAMET_NONA)
    
   summary(two_way_anova)
   
   
## how to find the p-value for each row in the three-way cross tab 
   
   #Hispanic
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn == 'hisp',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$MHItertiles, data=tempdf))
   
   #Mixed
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn == 'mixed',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$MHItertiles, data=tempdf))
   
   #NH Asian/PI
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn == 'nhapi',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$MHItertiles, data=tempdf))
   
   #NH Black
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn == 'nhblack',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$MHItertiles, data=tempdf))
   
   #NH White
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$majority_race_ethn == 'nhwhite',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$MHItertiles, data=tempdf))
   
   
   ## how to find the p-value for each column in the three-way cross tab
   
   #Low income
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles == 'Low',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$majority_race_ethn, data=tempdf))
   
   #Medium income
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles == 'Medium',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$majority_race_ethn, data=tempdf))
   
   #High income
   tempdf <- cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles == 'High',]
   
   summary(aov(tempdf$prop_fully_vax_truncated~tempdf$majority_race_ethn, data=tempdf))
   
  ## Univariate box plots 
   #load package for color palettes
   library(RColorBrewer)
   #display the orange-red color palette
   display.brewer.pal(n = 8, name = 'OrRd')
   
   # the three colors you want to use:
   my_colors<-brewer.pal(n=8, name="OrRd")[c(4, 6, 8)]
   
   # income level univariate box plot
   ggplot(cleanvaxdata_LAMET_NONA, aes(x=MHItertiles, y=mhi, group= MHItertiles))+
     # replaced fill="red" with aes(fill=MHItertiles) and do color="black'
     #geom_boxplot(color="red", fill="orange", alpha=0.2)+
     geom_boxplot(aes(fill=MHItertiles), color="black")+
     # and changing the color scale now: this is the way you'd normally do it:
     #scale_fill_brewer(palette="OrRd")+
     # but here, i suggest you use manual colors to have some more control
     scale_fill_manual(values=my_colors)+
     labs(x="", y="Median Household Income") + 
     theme_classic() +
     # axis.text controls the labels of the axis, axis.title controls the axis title
     theme(axis.text = element_text(color="black", size=14),
           axis.title = element_text(color="black", size=14))
   
   # trying to add gradient color to income level univariate box plot

    #load package for color palettes
     library(RColorBrewer)
   
    #display the orange-red color palette
     display.brewer.pal(n = 8, name = 'OrRd')
     
     #plot with orange-red colors
     ggplot(cleanvaxdata_LAMET_NONA, aes(x=MHItertiles, y=mhi, group= MHItertiles, fill=MHItertiles))+     
       geom_boxplot()+
       scale_fill_brewer(palette="OrRd")+
        labs(x="", y="Median Household Income")+
       theme_classic()+
       theme(legend.position="none")+
       theme(text = element_text(size=15))
     
     #alternative plot design
     ggplot(cleanvaxdata_LAMET_NONA, aes(x=MHItertiles, y=mhi, group= MHItertiles, fill=MHItertiles))+     
       geom_boxplot()+
       scale_fill_brewer(palette="OrRd")
   
      ggsave("Coloring_Boxplots_with_RColorBrewer_palette_OrRd_ggplot2.jpg")
   
   ## Standard error for three-way association
   cleanvaxdata_LAMET_NONA %>% group_by(MHItertiles, majority_race_ethn) %>% summarise(mean=mean(prop_fully_vax_truncated*100), se=sd(prop_fully_vax_truncated*100)/sqrt(n()), n=n())
   
   ## Find the median of low income tertile
   median(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='Low',]$mhi)
   
    # Find the median of medium income tertile
   median(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='Medium',]$mhi)
   
   # Find the median of high income tertile
   median(cleanvaxdata_LAMET_NONA[cleanvaxdata_LAMET_NONA$MHItertiles=='High',]$mhi)
   