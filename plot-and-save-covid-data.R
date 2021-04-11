library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)

# Read covid data
covid <- read.csv("covid-data/United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")

states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
            "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
            "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH",
            "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
            "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY",
            "NYC") # Include NYC, to be combined with NY

# Process dates for sorting
covid <- data.frame(
  Date = as.Date(strptime(covid$submission_date, "%m/%d/%Y")),
  State = covid$state,
  Cases = covid$new_case
)
covid <- covid[covid$State %in% states,]
# Combine NYC with NY by taking first 2 substring
covid$State <- substr(covid$State, 1, 2)
covid <- aggregate(Cases ~ Date + State, covid, sum)
# Add week
covid$EndofWeek <- covid$Date - (wday(covid$Date) - 1) + 7 * (wday(covid$Date) != 1)


# Aggregate to weekly and calculate sequential log ratios
covid.weekly <- covid %>%
  group_by(State, EndofWeek) %>%
  summarize(Cases = mean(Cases))
covid.weekly <- covid.weekly %>%
  arrange(State, EndofWeek) %>%
  group_by(State) %>%
  mutate(LogRatio = log(Cases / lag(Cases)))
covid.weekly$LogRatio <- replace(covid.weekly$LogRatio, is.infinite(covid.weekly$LogRatio), NaN)
  
# Plot each state as a series
ggplot(covid.weekly, aes(x=EndofWeek, y=LogRatio)) + geom_line(aes(color=State))

#############################################################################
### Aggregate to the US

# Aggregate
covid.us <- aggregate(Cases ~ Date + EndofWeek, covid, sum)

# Aggregate to weekly and caluculate sequential log ratios
covid.us.weekly <- covid.us %>%
  group_by(EndofWeek) %>%
  summarize(Cases = mean(Cases))
covid.us.weekly <- covid.us.weekly %>%
  arrange(EndofWeek) %>%
  mutate(LogRatio = log(Cases / lag(Cases)))
covid.us.weekly$LogRatio <- replace(covid.us.weekly$LogRatio, is.infinite(covid.us.weekly$LogRatio), NaN)


ggplot(covid.us.weekly, aes(x=EndofWeek, y=LogRatio)) + geom_line()

##############################################################################

# output files after processing

write.csv(covid.weekly, "covid-data/weekly-states.csv")

write.csv(covid.us.weekly, "covid-data/weekly-us.csv")
