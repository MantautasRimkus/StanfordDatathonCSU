#############################################################################
####This code takes data from Household survey and prepares it for modeling
####Output: Files anx_Data_estimates.csv" and anx_Data_error.csv
###############

####Libraries
library(readxl)
library(plyr)
library(tidyverse)

##############################################################################
####Special functions

  #Function for reading all excel sheets
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

  #Settings dates for each each week 
dates_beg<- c(Week1="April 23, 2020",
              Week2="May 7, 2020",
              Week3="May 14, 2020",
              Week4="May 21, 2020",
              Week5="May 28, 2020",
              Week6="June 4, 2020",
              Week7="June 11, 2020",
              Week8="June 18, 2020",
              Week9="June 25, 2020",
              Week10="July 2, 2020",
              Week11="July 9, 2020",
              Week12="July 16, 2020",
              Week13="August 19, 2020",
              Week14="September 2, 2020",
              Week15="September 16, 2020",
              Week16="September 30, 2020",
              Week17="October 14, 2020",
              Week18="November 9, 2020",
              Week19="November 23, 2020",
              Week20="December 7, 2020",
              Week21="December 21, 2020",
              Week22="January 18, 2021",
              Week23="February 1, 2021",
              Week24="February 15, 2021",
              Week25="March 1, 2021",
              Week26="March 15, 2021",
              Week27="March 29, 2021")

dates_end<- c(Week1="May 5, 2020",
              Week2="May 12, 2020",
              Week3="May 19, 2020",
              Week4="May 26, 2020",
              Week5="June 2, 2020",
              Week6="June 9, 2020",
              Week7="June 16, 2020",
              Week8="June 23, 2020",
              Week9="June 30, 2020",
              Week10="July 7, 2020",
              Week11="July 14, 2020",
              Week12="July 21, 2020",
              Week13="August 31, 2020",
              Week14="September 14, 2020",
              Week15="September 28, 2020",
              Week16="October 12, 2020",
              Week17="October 26, 2020",
              Week18="November 9, 2020",
              Week19="November 23, 2020",
              Week20="December 7, 2020",
              Week21="December 21, 2020",
              Week22="January 18, 2021",
              Week23="February 1, 2021",
              Week24="February 15, 2021",
              Week25="March 1, 2021",
              Week26="March 15, 2021",
              Week27="March 29, 2021")

###########################################################################
#Preparation for reading

  #Read one of the data sets, examine and get needed sheet names.
xx <- read_excel_allsheets("Anxiety Data/health2a_week13.xlsx")
states <- names(xx)[1:52]

  #Prepare template to save the data for each sheet and each excel
template <- data.frame(matrix(NA, nrow=8,ncol=7))
colnames(template) <- c("Week","State","Age","Not at all",
                        "Several days","More than half the days",
                        "Nearly every day")


  #Prepare where to save the files (list format)
  save_data_here <- list()

  #As files for Week 13 and above has a different structure, order filenames
  #properly and apply different functions
  
  filenames <- list.files("Anxiety Data/")
  order_filenames <- c()
  for(string in filenames){
    order_filenames <- c(order_filenames,
                         parse_number(str_split(string,pattern="_")[[1]][2]))}
    filenames <- filenames[order(order_filenames)]

#############################################################################33
#Data reading and combining. 
  #Phase 1 files
for(filename in filenames[1:12])
  {
      #Read excel file
      file_xx <- read_excel_allsheets(filename=paste0("Anxiety Data/",filename))
      
      #Go to each needed state sheet and extract data
      save_data_here[[filename]] <- adply(states,1,function(k){
      templatexx <- template
      templatexx$Week <- filename
      templatexx$State <- k
      templatexx$Age <- c("Total","18 - 29","30 - 39",
                          "40 - 49","50 - 59","60 - 69","70 - 79",
                          "80 and above")
      templatexx$`Not at all` <- file_xx[[k]][["...2"]][7:15][-2]
      templatexx$`Several days` <- file_xx[[k]][["...3"]][7:15][-2]
      templatexx$`More than half the days` <- file_xx[[k]][["...4"]][7:15][-2]
      templatexx$`Nearly every day` <- file_xx[[k]][["...5"]][7:15][-2]
      return(templatexx)
    })
}
  #Phase 2-3 files
for(filename in filenames[13:27])
{
    #Read excel file
    file_xx <- read_excel_allsheets(filename=paste0("Anxiety Data/",filename))
  
    #Go to each needed state sheet and extract data
    save_data_here[[filename]] <- adply(states,1,function(k){
    templatexx <- template
    templatexx$Week <- filename
    templatexx$State <- k
    templatexx$Age <- c("Total","18 - 29","30 - 39","40 - 49",
                        "50 - 59","60 - 69","70 - 79",
                        "80 and above")
    templatexx$`Not at all` <- file_xx[[k]][["...2"]][8:16][-2]
    templatexx$`Several days` <- file_xx[[k]][["...3"]][8:16][-2]
    templatexx$`More than half the days` <- file_xx[[k]][["...4"]][8:16][-2]
    templatexx$`Nearly every day` <- file_xx[[k]][["...5"]][8:16][-2]
    return(templatexx)
  },.progress="text")
}

##############################################################################
#Data cleaning and combining stage
data_est <-  do.call(rbind.data.frame, save_data_here)[,-1] %>%
      #Get week number from filename
  separate(Week,c("TA","Week"),sep="_") %>%
  dplyr::mutate(Week=readr::parse_number(Week)) %>%
      
      #Filter ages that are interesting to us
  filter(Age %in% c("18 - 29","30 - 39","40 - 49")) %>%
      
      #Merge proper interval dates
  dplyr::mutate(date_start=as.Date(dates_beg[Week],format="%B %d, %Y"),
                date_end=as.Date(dates_end[Week],format="%B %d, %Y")) %>%
      
      #Fix problems with variables
  dplyr::select(everything(), -TA) %>%
  dplyr::mutate(More.than.half.the.days=as.numeric(`More than half the days`)) %>%
  dplyr::mutate(Several.days=as.numeric(`Several days`)) %>%
  dplyr::mutate(Nearly.every.day=as.numeric(`Nearly every day`)) %>% 
  dplyr::mutate(Not.at.all=as.numeric(`Not at all`)) %>% 
      
      #Calculate the response variable
  dplyr::mutate(Response=(More.than.half.the.days+Nearly.every.day)/
                  (Not.at.all+Several.days)) %>%

      
###################################################################
#Estimates of anxiety writing into the csv
  data_est_final <- data_est[,c(1:3,8:ncol(data_est))]
  write.csv(data_est_final,"anx_Data_estimates.csv")


###########################################################################
###########################################################################
###########################################################################
#Similar processes as above, but this time it reads errors of estimates
#(Needed for error bars). See more guidance above.

#Preparation for reading
  save_data_here <- list()
  filenames <- list.files("Anxiety Data Errors/")
  order_filenames <- c()
  for(string in filenames){
    order_filenames <- c(order_filenames,parse_number(str_split(string,pattern="_")[[1]][3]))
  }
  filenames <- filenames[order(order_filenames)]

###############################
#Reading 
for(filename in filenames[1:12])
{
  file_xx <- read_excel_allsheets(filename=paste0("Anxiety Data Errors/",filename))
  save_data_here[[filename]] <- adply(states,1,function(k){
    templatexx <- template
    templatexx$Week <- filename
    templatexx$State <- k
    templatexx$Age <- c("Total","18 - 29","30 - 39","40 - 49","50 - 59","60 - 69","70 - 79",
                        "80 and above")
    templatexx$`Not at all` <- file_xx[[k]][["...2"]][7:15][-2]
    templatexx$`Several days` <- file_xx[[k]][["...3"]][7:15][-2]
    templatexx$`More than half the days` <- file_xx[[k]][["...4"]][7:15][-2]
    templatexx$`Nearly every day` <- file_xx[[k]][["...5"]][7:15][-2]
    return(templatexx)
  },.progress="text")
}

for(filename in filenames[13:27])
{
  file_xx <- read_excel_allsheets(filename=paste0("Anxiety Data Errors/",filename))
  save_data_here[[filename]] <- adply(states,1,function(k){
    templatexx <- template
    templatexx$Week <- filename
    templatexx$State <- k
    templatexx$Age <- c("Total","18 - 29","30 - 39","40 - 49",
                        "50 - 59","60 - 69","70 - 79",
                        "80 and above")
    templatexx$`Not at all` <- file_xx[[k]][["...2"]][8:16][-2]
    templatexx$`Several days` <- file_xx[[k]][["...3"]][8:16][-2]
    templatexx$`More than half the days` <- file_xx[[k]][["...4"]][8:16][-2]
    templatexx$`Nearly every day` <- file_xx[[k]][["...5"]][8:16][-2]
    return(templatexx)
  },.progress="text")
}

###########################################################################
#Data cleaning
  
data_error <- do.call(rbind.data.frame, save_data_here)[,-1] %>%
  separate(Week,c("TA","TA1","Week"),sep="_") %>%
  filter(Age %in% c("18 - 29","30 - 39","40 - 49")) %>%
  dplyr::mutate(Week=readr::parse_number(Week)) %>%
  dplyr::mutate(date_start=as.Date(dates_beg[Week],format="%B %d, %Y"),
                date_end=as.Date(dates_end[Week],format="%B %d, %Y")) %>%
  dplyr::select(everything(), -TA,-TA1) %>%
  dplyr::mutate(More.than.half.the.days=as.numeric(`More than half the days`)) %>%
  dplyr::mutate(Several.days=as.numeric(`Several days`)) %>%
  dplyr::mutate(Nearly.every.day=as.numeric(`Nearly every day`)) %>% 
  dplyr::mutate(Not.at.all=as.numeric(`Not at all`))

############################################################################
#Data writing

data_error_final <- data_error[,c(1:3,8:ncol(data_error))]
write.csv(data_error_final,"anx_Data_error.csv")


