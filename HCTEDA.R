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

#ccbrs <- read.csv("Data/COVIDCasesByResidentStatus.csv")
ccbrs <- read.csv("https://raw.githubusercontent.com/abartlettg/hawaiitourismcovid19/main/COVIDCasesByResidentStatus.csv")
#tsdc <- read.csv("Data/TourismStatsDuringCOVID.csv")
tsdc <- read.csv("https://raw.githubusercontent.com/abartlettg/hawaiitourismcovid19/main/TourismData.csv")
#hcrt <- read.csv("Data/HawaiiCOVIDRulesTimeline.csv")
hcrt <- read.csv("https://raw.githubusercontent.com/abartlettg/hawaiitourismcovid19/main/HawaiiCOVIDRulesTimeline.csv")

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

################################################################
#### Switching to Daily Passenger Count Data instead of Monthly 

dpaxfull <- read.csv("Data/DailyPassengerCountFullData.csv")

colnames(dpaxfull)[1] <- "ArrivalDt"
dpaxfull = subset(dpaxfull, select = names(dpaxfull) %in% c('ArrivalDt','Total'))

dpaxfull$ArrivalDt = as.Date(dpaxfull$ArrivalDt, format = "%d-%b-%y")

dpaxfull$Total = sub(',','',dpaxfull$Total)

dpaxfull$Total = as.integer(dpaxfull$Total)


# Filter for Passenger Counts During COVID and only fields needed

sum(is.na(dpaxfull$ArrivalDt))

dpaxfull = dpaxfull %>%
  drop_na(ArrivalDt)

# Having trouble with filtering with date entered in logical so adding
# a field in the dataframe to hold COVIDStartDt so I can make sure the
# logical is functioning against the exact same type of date data
dpaxfull = mutate(dpaxfull, COVIDStartDt = "02/29/2020")
dpaxfull$COVIDStartDt = as.Date(dpaxfull$COVIDStartDt, format = "%m/%d/%Y")

dpaxCOV = dpaxfull %>%
  filter(ArrivalDt > COVIDStartDt)

max(dpaxfull$ArrivalDt) 

min(dpaxCOV$ArrivalDt)

write.csv(dpaxCOV, file='dpaxCOV.csv')
############### Notes About Problems Encountered & SOLVED ###################
# This was the problem... The arrival date cannot
# be calculated with... but the COVIDStartDt can... what is causing this
# difference... NAs or Nulls

# Removed NAs, but max on ArrivalDt is indicated as 2017-12-25... Maybe there
# was a problem w/file creation and there are no dates in there greater than
# 02-29-202d0... If this is the case, it makes perfect sense why I've been
# having so much trouble with selecting the records for during COVID

# Found that date format in input file is different for later dates in the
# file.  Since we only intend to use dates from 2019 onward, I will adjust
# the output file to not have any dates prior to 2019 and hopefully find
# that the date data from 2019 onward has a consistent format
#############################################################################

# Filter for 2019 Counts As Reference

dpaxfull = mutate(dpaxfull, RefDt2019 = "12/31/2019")
dpaxfull$RefDt2019 = as.Date(dpaxfull$RefDt2019, format = "%m/%d/%Y")

dpax2019 = dpaxfull %>%
  filter(ArrivalDt <= RefDt2019)

dpax2019 = subset(dpax2019, select = -c(COVIDStartDt, RefDt2019))
dpaxCOV = subset(dpaxCOV, select = -COVIDStartDt)

min(dpax2019$ArrivalDt)

max(dpax2019$ArrivalDt)


# Creating a field in both 2019 and COV dataframes that can match on
# month-day to join and bring 2019 passenger count into COV file for
# reference... this will make it possible to graph passenger count from
# same day in 2019 to passenger counts from the start of COVID and on.

dpax2019$ArrivalDtMD = as.character(dpax2019$ArrivalDt)
dpax2019$ArrivalDtMD = sub('2019-', '', dpax2019$ArrivalDtMD)

dpaxCOV$ArrivalDtMD = as.character(dpaxCOV$ArrivalDt)
dpaxCOV$ArrivalDtMD = sub('2020-', '', dpaxCOV$ArrivalDtMD)
dpaxCOV$ArrivalDtMD = sub('2021-', '', dpaxCOV$ArrivalDtMD)

dpax2019 = subset(dpax2019, select = -ArrivalDt)


dpax = left_join(dpaxCOV, dpax2019, by="ArrivalDtMD", suffix=c(".COV",".2019"))

dpax$TotalCOV = dpax$Total.COV
dpax$Total2019 = dpax$Total.2019
dpax = subset(dpax, select = -c(Total.COV, Total.2019))

write.csv(dpax, file='dpax.csv')

################# GOING TO PYTHON NOW TO GRAPH ##########################

# Monthly Recorded Traveler Expenditure Comparison of 2019 data to COVID
# No Data Recorded from 4/20-10/20
# US Traveler Data added back in for 11/20-12/20
# All Data back in from 1/21 on
# I considered imputing traveler expenditure for the missing periods, but
# then gave further consideration as to why Hawaii would stop tracking this
# data for this time period... when all travelers were subject to 14 day
# quarantine... it cannot be accurately imputed because travelers do not
# spend nearly as much money when in quarantine... basically, buying food 
# to be delivered to their quarantine location and nothing else.  And, the
# majority of the small amount of travelers coming in being returning 
# residents, family of residents, and new residents.  Basically, you do NOT
# have Tourism during this time because most everything is closed, even
# beaches, and nobody goes to Hawaii to spend two weeks trapped in their
# hotel room.  

tourdol = tsdc %>%
  filter(Indicator == 'Expenditure' &
           Market == 'Total')

tourdol = pivot_longer(tourdol, 5:36, names_to = 'YrMonth', 
                        values_to = 'MillionDollars')

tourdol$MillionDollars = sub(',','',tourdol$MillionDollars)

tourdol$MillionDollars = as.numeric(tourdol$MillionDollars)

tourdol[is.na(tourdol)] = 0.00

tourdol$YrMonth = sub('X','',tourdol$YrMonth)

tourdol = subset(tourdol, select = names(tourdol) %in% c('YrMonth','MillionDollars'))

tourdol2019 = tourdol %>%
  filter(YrMonth < '2020.01')

tourdolCOV = tourdol %>%
  filter(YrMonth > '2020.02')

tourdol2019$Month = sub('2019.', '', tourdol2019$YrMonth)

tourdolCOV$Month = tourdolCOV$YrMonth
tourdolCOV$Month = sub('2020.', '', tourdolCOV$Month)
tourdolCOV$Month = sub('2021.', '', tourdolCOV$Month)

TourismDollars = left_join(tourdolCOV, tourdol2019, by="Month", suffix=c(".COV",".2019"))

TourismDollars = subset(TourismDollars, select = -c(Month, YrMonth.2019))

write.csv(TourismDollars, file='TourismDollars.csv')

############################### Graph in Python ###############################

  



