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

write.csv(hcrt, file='hcrt.csv')

#######################################################
# VISUALIZING COVID CASES BY RESIDENT AND TRAVEL STATUS
#######################################################

# First, check to see if assumed duplicate columns are really duplicates 
# Risk and RiskCopy and DatAdded and DateAddedCopy

all(ccbrs$Risk == ccbrs$RiskCopy) # Returns TRUE
all(ccbrs$DateAdded == ccbrs$DateAddedCopy) # Returns TRUE










