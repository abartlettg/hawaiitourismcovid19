##### EDA for Hawaii COVID Tourism Study #####

# Load Libraries
################ CLEAN UP THIS SEGMENT AT END TO INCLUDE ##############
################ ONLY LIBRARIES REALLY USED              ##############

library(timevis)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(plotly)



#######################################################################
# Note that it is expected that the TourismStatsDuringCOVID.csv file
# comes in w/char values that should be int already converted in Excel.
# If this code was to be put into production for regular updates, the
# preprocessing of this file should all be done in the R code to avoid
# this manual step... changing char to num and eliminating decimal.
#######################################################################


# Reading in Data Files

setwd("C:/Users/adria/NYCDSA/DataAnalytics/HCT")

############## My Data Directory and these files disappeared on PC ############
############## Need to copy back from GitHub ##################################

ccbrs <- read.csv("Data/COVIDCasesByResidentStatus.csv")
tsdc <- read.csv("Data/TourismStatsDuringCOVID.csv")
hcrt <- read.csv("Data/HawaiiCOVIDRulesTimeline.csv")

# Clean up colnames and NAs introduced by Excel when saving as .csv

colnames(hcrt)[1] <- "uid"
colnames(ccbrs)[1] <- "DateAdded"

hcrt <- na.omit(hcrt)

# Fix up classes in Dataframes

hcrt$end_dt <- as.Date(hcrt$end_dt, "%m/%d/%Y")
hcrt$start_dt <- as.Date(hcrt$start_dt, "%m/%d/%Y")
hcrt$proc_dt <- as.Date(hcrt$proc_dt, "%m/%d/%Y")

ccbrs$DateAdded <- as.Date(ccbrs$DateAdded, "%m/%d/%Y")
ccbrs$DateAddedCopy <- as.Date(ccbrs$DateAddedCopy, "%m/%d/%Y")

#############################################################################
# Adjusted in Excel instead... Need to add to documentation for preprocessing
# if I stick to doing it this way... change to number and remove .00 so it's
# automatically read in as an integer

########### COME BACK LATER TO INCLUDE CODE TO PREPROCESS ##################
# cnlist <- colnames(tsdc)
# cnlistX <- cnlist[5:36]

# for (i in cnlistX)tsdc[,i]<-as.numeric(tsdc[,i])

#for (i in cnlistX) {
  #coltotransform = paste("tsdc$", i, sep='')
  #X = paste("tsdc$", i, sep='')
  #X <- as.Date(X, "%m/%d/%Y")
  
  #print(X)
#}
#############################################################################


# Replace NA with today's date in hcrt file since a missing end date implies
# that the rule is currently still in effect.

today = Sys.Date()
hcrt[is.na(hcrt$end_dt), "end_dt"] <- today

##############################
# vISUALIZING HCRT IN TIMELINE
##############################

# Utilized example of how to create timeline from this source...
# https://rforpoliticalscience.com/2021/02/25/make-a-timeline-graph-with-dates-in-ggplot2/

# Add number of instances where the rule is in effect... this will make it
# possible to keep multiple start and end dates in place for the same rule 
# on the graph


#hcrt <- mutate(hcrt, number = 1)
#hcrt <- mutate(hcrt, order = 1)
#hcrt <- mutate(hcrt, count = 1)

time_line <- hcrt %>% 
  ggplot(aes(x = start_dt), y = uid, color = type) +
  geom_segment(aes(xend = end_dt, y=uid, yend = uid, color =  type), size = 4) #+
  #geom_text(aes(label = order, hjust = 1.1), size = 8, show.legend = FALSE) +
  #scale_color_manual(values = c("R" = "#004266", "L" = "#FCB322", "C" = "#D62828"))

time_line

# The resulting graph is very hard to read... in the sense of interpretability
# I probably need to cluster rules by type and show tightening and loosening
# by type... such as quarantine vs test exception vs vaccine exception all as
# one type w/various stages of restriction

# Experiment w/Plotly.Express... Express not available in R
# plotly.express.timeline(hcrt, start_dt, end_dt, uid)


# Writing out files to experiment with in Python w/plotly.express
write.csv(hcrt, file='hcrt.csv')
write.csv(ccbrs, file='ccbrs.csv')
write.csv(tsdc, file='tsdc.csv')


#######################################################
# VISUALIZING COVID CASES BY RESIDENT AND TRAVEL STATUS
#######################################################

# First, check to see if assumed duplicate columns are really duplicates 
# Risk and RiskCopy and DatAdded and DateAddedCopy

all(ccbrs$Risk == ccbrs$RiskCopy) # Returns TRUE
all(ccbrs$DateAdded == ccbrs$DateAddedCopy) # Returns TRUE

#######################################################
# PREPPING FILES FOR GRAPHING IN PYTHON PLOTLY.EXPRESS
#######################################################

#CCBRS

ccbrstots = ccbrs %>%
  select(DateAdded, Risk, TotNumber) %>%
  group_by(DateAdded, Risk) %>%
  summarize(sum(TotNumber))

ccbrstots

write.csv(ccbrstots, file='ccbrstots.csv')

############### STOPPED WHILE GRAPHING IN PYTHON ########################

# Visitor arrivals (SA)
# Air total

arrivals = tsdc %>%
  filter(Indicator == 'Visitor arrivals (SA)' &
           Market == 'Air total')

arrivals = pivot_longer(arrivals, 5:36, names_to = 'Month', 
                        values_to = 'ArrivalNumSA')
  
arrivals$Month = sub('X','',arrivals$Month)
# Don't delete X at beginning... keep it char?, but then below logic doesn't work

# KEEP CHAR AND NUM VERSIONS OF MONTH 
 
arrivals = arrivals %>%
  select('Month', 'ArrivalNumSA')

arrivalsCOV = arrivals %>%
  filter(Month > 'X2020.02')

write.csv(arrivalsCOV, file='arrivalsCOV.csv')

#### Switching to Daily Passenger Count Data instead of Monthly 

dpaxfull <- read.csv("Data/DailyPassengerCountFullData.csv")

colnames(dpaxfull)[1] <- "ArrivalDt"

dpaxfull$ArrivalDt = as.Date(dpaxfull$ArrivalDt, format = "%m/%d/%Y")

dpaxfull$Total = sub(',','',dpaxfull$Total)

dpaxfull$Total = as.integer(dpaxfull$Total)

# May need to check for NAs/NULL values... could this be the issue
# with trying to compare and filter by Date?


# Filter for Passenger Counts During COVID and only fields needed

#COVStartDt = "2/29/2020"
#COVStartDt = as.Date(COVStartDt, format = "%m/%d/%Y")

sum(is.na(dpaxfull$ArrivalDt))

dpaxfull = dpaxfull %>%
  drop_na(ArrivalDt)

dpaxfull = mutate(dpaxfull, COVIDStartDt = "02/29/2020")
dpaxfull$COVIDStartDt = as.Date(dpaxfull$COVIDStartDt, format = "%m/%d/%Y")

dpaxCOV = dpaxfull %>%
  filter(ArrivalDt > COVIDStartDt)

max(dpaxfull$ArrivalDt) # This is the problem... The arrival date cannot
# be calculated with... but the COVIDStartDt can... what is causing this
# difference... NAs or Nulls

# Removed NAs, but max on ArrivalDt is indication 2017-12-25... Maybe there
# was a problem w/file creation and there are no dates in there greater than
# 02-29-202d0... If this is the case, it makes perfect sense why I've been
# having so much trouble with selecting the records for during COVID



####### THIS IS DRIVING ME NUTS... PROBLEMS W/DATES ###########


# Filter for 2019 Counts As Reference




# Remove Year and Join by Month/Day Adding Field indicating that this
# is 2019 reference data... 
#
# Day of Week will not line up, but we aren't getting that granular with 
# this data.
#
# 2020 was a leap year, but this won't impact the line graph since we start
# the COVID graph with March 2020










