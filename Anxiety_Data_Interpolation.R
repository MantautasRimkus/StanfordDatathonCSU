#############################################################################
####This code for anxiety estimates interpolation
####Output: File anx_rate_ext.csv
###############

############################################################################
#Libraries 

library(tidyverse)
library(plyr)
library(readr)
library("lubridate")
library(fda)

###########################################################################
#Setting up weeks dates. 

dates_end<- c(Week1="May 5, 2020",Week2="May 12, 2020",Week3="May 19, 2020",
              Week4="May 26, 2020",Week5="June 2, 2020",Week6="June 9, 2020",
              Week7="June 16, 2020",Week8="June 23, 2020",Week9="June 30, 2020",
              Week10="July 7, 2020",Week11="July 14, 2020",Week12="July 21, 2020",
              Week13="August 31, 2020",Week14="September 14, 2020",
              Week15="September 28, 2020",Week16="October 12, 2020",
              Week17="October 26, 2020",Week18="November 9, 2020",
              Week19="November 23, 2020",Week20="December 7, 2020",
              Week21="December 21, 2020", Week22="January 18, 2021",
              Week23="February 1, 2021",Week24="February 15, 2021",
              Week25="March 1, 2021",Week26="March 15, 2021",
              Week27="March 29, 2021")


############################################################################
#Data Cleaning - read needed file, and filter out needed

data_clean <- read.csv("anx_Data_estimates.csv")[,-1] %>%
  dplyr::filter(!State %in% c("DC")) %>%
  dplyr::filter(Age %in% c("18 - 29")) %>%
  dplyr::select(State,date_end,Response)

#Spread data to make it matrix form
data_clean_prep <- data_clean %>%
  tidyr::spread(State,Response) %>%
  arrange(date_end)


# Finding which t represents sundays (what points to interpolate)

lub <- as.Date(data_clean_prep$date_end)
full_t <- seq(lub[1],lub[27],by="1 days")
is_Sunday <-full_t[which(wday(full_t)==1)]

#################################################################
### Extrapolation

my_list <- list()
for(i in 1:length(is_Sunday)){
  #Get date for which to interpolate
  sunday <- is_Sunday[i]
  
  #Find the closest time points around
  before <- interval(lub[tail(which(sunday>lub),1)],sunday) %/% days(1)
  after <- interval(sunday,lub[which(sunday<=lub)[1]]) %/% days(1)
  total <- interval(lub[tail(which(sunday>lub),1)],lub[which(sunday<lub)[1]]) %/% days(1)
  
  #Run algorithm described in the paper
  bf_vec <- as.numeric(data_clean_prep[tail(which(sunday>lub),1),-1])
  af_vec <- as.numeric(data_clean_prep[which(sunday<=lub)[1],-1])
  xxx <- before/total * af_vec + after/total * bf_vec
  
  #Save Data into the list
  my_list[[i]] <- xxx
}


##############################################3
#Preparation for exporting 

  #To save date format of names, we need to use loop
  template <- data.frame(matrix(NA, ncol=ncol(data_clean_prep),nrow=length(is_Sunday))) %>%
  magrittr::set_colnames(colnames(data_clean_prep))

  template$date_end <- is_Sunday
  
  #Take data from list and write it into the prepared template
  for(i in 1:nrow(template)){
    template[i,2:ncol(template)] <- my_list[[i]]
  }
  
  write.csv(template,"anx_rate_ext.csv")
