#############################################################################
####Running permutation test for correlation
####Output:
###############

#############################################################################
#Libraries 

library(tidyverse)
library(plyr)
library(readr)
library("lubridate")
library(fda)

#############################################################################

data_needed <- read.csv("diffsdf.csv") %>% na.omit()
 
# Calculate statistics value for California and Colorado

Stats <- data_needed %>%
  dplyr::group_by(State) %>%
  dplyr::filter(State==c("CA","CO")) %>% 
  dplyr::summarise(corr=cor(hitdiffs,anxdiffs)) %>%
  dplyr::summarise(mean(corr*corr)) %>%
  as.numeric()

# Prepare data frame to use for joining
anx_not_mix <- data_needed %>%
  dplyr::select(State,date_end,anxdiffs)

#Prepare data frame that gonna be mixed
to_mix_hitdiffs <- data_needed %>%
  dplyr::select(State,hitdiffs,date_end) %>%
  tidyr::spread(date_end,hitdiffs)

#Save statitics in here
N <- 1000
permut <- 1:nrow(to_mix_hitdiffs)
save_stats_permut <- rep(0,N)

#######################################################33
#Run permutation test
for(i in 1:N){
  mixing <- to_mix_hitdiffs
  mixing$State <- to_mix_hitdiffs$State[sample(permut,replace=FALSE)]
  save_stats_permut[i] <- anx_not_mix %>%
  left_join(mixing %>%
  gather("date_end","hitdiffs",-State),by = c("State", "date_end")) %>% 
    na.omit() %>%
    dplyr::filter(State==c("CA","CO")) %>% 
    dplyr::group_by(State) %>%
    dplyr::summarise(corr=(cor(hitdiffs,anxdiffs))) %>%
    dplyr::summarise(mean(corr*corr)) %>%
    as.numeric()
}

####################################
pvalue=mean(save_stats_permut > Stats)


