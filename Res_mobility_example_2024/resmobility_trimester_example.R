###### sample code from master's thesis project -- residential mobility ######
# this is code adapted from an ECHO manuscript in process.
# the original code is on the ECHO Analysis Workbench.
# this code has been changed slightly.

### example: creating a variable for trimester of pregnancy,
### determining when (what trimester) residential moves occur per person

# set working directory
setwd("/Users/angeladadamo/Documents/github/code-samples/Res_mobility_example_2024")

# load packages
library(dplyr) # for data management
library(tidyverse) # for data management
library(magrittr) # for piping: %>%
library(lubridate) # for estimating trimester of pregnancy with dates

# load the data
birth <- read.csv("birth_data.csv") # cross-sectional birth outcomes data
residential_full_v6 <- read.csv("reshis_data.csv") # long residential history data

# estimating dates of trimester
birth_trimester <- birth %>%
  mutate(child_conception_est = as.Date(child_dob) - (7 * birth_ga)) %>% # subtract number of days pregnant from child's DOB
  mutate(child_conception_prior_est = child_conception_est %m-% months(3)) %>% # date of 3 months prior to conception
  mutate(trimester2_start_est = child_conception_est + (12 * 7)) %>% # start of trimester 2
  mutate(trimester2_start_est = child_conception_est + (26 * 7)) # start of trimester 3

# trim the dataset
birth_trivar <- dplyr::select(birth_trimester, c(participantid,
                                                 pregid,
                                                 child_conception_est,
                                                 child_conception_prior_est,
                                                 trimester2_start_est,
                                                 trimester3_start_est))

# merge the data on trimesters to the long residential history file
residential_full_v6 <- inner_join(x=residential_full_v5, y=birth_trivar,
                                  by="pregid") # xxx people with residential data did not have birth data

# create the trimester variable
  ## note: first trimester starts at estimated date of conception ##
  # so, if residential history date is >/= conception date but < trimester 2 start date -> trimester 1
  # if residential history date is >/= trimester 2 start date but < trimester 3 start date -> trimester 2
  # if residential history date is >/= trimester 3 start date -> trimester 3
residential_full_v6$trimester <= ifelse(residential_full_v6$reshis_date >= residential_full_v6$child_conception_est 
                                        & residential_full_v6$reshis_date < residential_full_v6$trimester2_start_est, 1, 
                                        ifelse(residential_full_v6$reshis_date >= residential_full_v6$trimester2_start_est 
                                        & residential_full_v6$reshis_date < residential_full_v6$trimester3_start_est, 2, 
                                        ifelse(residential_full_v6$reshis_date >= residential_full_v6$trimester3_start_est, 3, NA)))

# do a check to make sure there are 3 options for trimester and every participant falls in one
addmargins(table(residential_full_v6$trimester)) # everything checks out

# now that we have the trimester variable, we want to know when (what trimester) did moves occur.
# we want a variable that tells us the trimester(s) that a move happened for each person

# we will use a for loop for identifying movement -- the idea is: 
# we have two rows of data for a person,
# check if the x or y coordinate of row 2 is the same as row 1
# if it's the same, we do nothing
# if it's not the same, we increase the number of moves by 1

# use a copy of the dataset just in case
residential_full_v6_copy <- residential_full_v6

# get the participants that had any move (ever)
movers_for_trimester <- residential_full_v6_copy %>%
  group_by(as.factor(participantid)) %>%
  filter(n_distinct(as.factor(x)) > 1 || n_distinct(as.factor(y)) > 1) %>%
  ungroup()


participants_t2 <- c(movers_for_trimester[1,1]) # stores a list of unique participant ids
trimesterMove <- c(NA) # stores a list of number of moves

# go through each row in the dataset
for(i in seq(1, nrow(movers_for_trimester)-1,1)) {
  # extract the current row and next row
  row <- movers_for_trimester[i,]
  nextRow <- movers_for_trimester[i+1,]
  # extract the current participant id and the next row's
  rowParticipant <- row$participantid
  nextRowParticipant <- nextRow$participantid
  # if the participant ids are different between rows, go to the next row
  # (we only count the number of moves within a participant)
  if(rowParticipant!=nextRowParticipant) {
    next
  }
  # if the current participant is different from the last one in our list,
  # add a new entry in our list for that participant
  # and initialize their number of moves to 0
  if(rowParticipant!=participants_t2[length(participants_t2)]) {
    participants_t2 <- append(participants_t2, rowParticipant)
    trimesterMove <- append(trimesterMove, NA)
  }
  # get the data
  rowX <- as.character(row$x)
  rowY <- as.character(row$Y)
  nextRowX <- as.character(nextRow$x)
  nextRowY <- as.character(nextRow$y)
  # if the x or y don't match, increase the number of moves by 1
  # for that participant and assign the respective trimester value
  if(rowX!=nextRowX || rowY!=nextRowY) {
    if(is.na(trimesterMove[length(trimesterMove)])) {
      trimesterMove[length(trimesterMove)] <- row$trimester
    }
  }
}

# create data frame with participants_t2 and number of moves
moves_tri <- data.frame(participantid = participants_t2, trimesterOfMove = trimesterMove)

# change participants_t2 from list to a data frame
moves_tri <- data.frame("participantid" = unlist(participants_t2, use.names=FALSE))

# final data frame with number of tract moves per participant
moves_tri <- data.frame(participantid = moves_tri, trimesterOfMove = trimesterMove)

# see how many people moved during each trimester
addmargins(table(moves_tri$trimesterOfMove))
# 100 people moved during trimester 1
# 200 people moved during trimester 2
# 300 people moved during trimester 3