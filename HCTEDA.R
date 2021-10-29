##### EDA for Hawaii COVID Tourism Study #####

# Load Libraries

library(timevis)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)

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

# cnlist <- colnames(tsdc)
# cnlistX <- cnlist[5:36]

# for (i in cnlistX)tsdc[,i]<-as.numeric(tsdc[,i])


#as.Date(tsdc$X*, "%m/%d/%Y")

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

# Visualizing hcrt as a timeline

# Tried timevis, bu this doesn't work and the resulting chart was not
# really impressive anyway
#hcrtfortimevis = rename(hcrt, start = start_dt, end = end_dt, 
#                        content = as.char(uid))
#timevis(hcrtfortimevis)

# Moving on to different method found at...
# https://rforpoliticalscience.com/2021/02/25/make-a-timeline-graph-with-dates-in-ggplot2/

# Add number of instances where the rule is in effect... this will make it
# possible to keep multiple start and end dates in place for the same rule 
# on the graph

hcrt <- mutate(hcrt, number = 1)
hcrt <- mutate(hcrt, order = 1)
hcrt <- mutate(hcrt, count = 1)

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







