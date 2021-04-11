# This file runs a time series model for anxiety of 18-29 year olds with
# covid cases as a predictor

dplyr
library(plm)

# Read in anxiety data, covid data (whole US) and covid data (by state)
anxiety <- read.csv("anx_rate_ext.csv")
covid.us <- read.csv("covid-data/weekly-us.csv")
covid <- read.csv("covid-data/weekly-states.csv")
search <- read.csv("statetrends.csv")

########## Add diffs to search data
`%>%` <- dplyr::`%>%`
search <- search %>%
  dplyr::arrange(location, date_end) %>%
  dplyr::group_by(location) %>%
  dplyr::mutate(
    diff = hits - dplyr::lag(hits),
    logratio = log(hits / dplyr::lag(hits))
  )

# Join together data sources into data frame for linear model
dates <- anx$date_end

modeldata <- data.frame(
  Week = dates,
  anx = anxiety$US
)
for (i in 1:nrow(modeldata)) {
  d <- modeldata[i, "Week"]
  d.lag1 <- as.character(as.Date(d) - 7)
  d.lag2 <- as.character(as.Date(d) - 14)
  
  # Anxiety from the previous week(s)
  if (d.lag1 %in% anxiety$date_end) { 
    modeldata[i, "anx.lag1"] <- anxiety[anxiety$date_end == d.lag1, "US"]
  }
  if (d.lag2 %in% anxiety$date_end) { 
    modeldata[i, "anx.lag2"] <- anxiety[anxiety$date_end == d.lag2, "US"]
  }
  
  # Covid cases (log ratio change) in current week and previous week(s)
  if (d %in% covid.us$EndofWeek) {
    modeldata[i, "covid"] <- covid.us[covid.us$EndofWeek == d, "LogRatio"]
  }
  if (d.lag1 %in% covid.us$EndofWeek) {
    modeldata[i, "covid.lag1"] <- covid.us[covid.us$EndofWeek == d.lag1, "LogRatio"]
  }
  if (d.lag1 %in% covid.us$EndofWeek) {
    modeldata[i, "covid.lag2"] <- covid.us[covid.us$EndofWeek == d.lag2, "LogRatio"]
  }
}
modeldata$Election <- modeldata$Week %in% c("2020-11-01", "2020-11-08")


################################################################################
# Run linear models
################################################################################

# Model 1 - AR(1) of anxiety nationally, no covid
fit.ar1anx <- lm(anx ~ anx.lag1, modeldata)
summary(fit.ar1anx)

# Model 2 - AR(2) of anxiety nationally, no covid
fit.ar2anx <- lm(anx ~ anx.lag1 + anx.lag2, modeldata)
summary(fit.ar2anx)

# Model 3 - AR(1) of anxiety plus an indicator on the 2020 election week
fit.ar1anx.election <- lm(anx ~ anx.lag1 + Election, modeldata)
summary(fit.ar1anx.election)

# Model 4 - AR(1) of anxiety plus covid and lagged covid cases nationally
fit.ar1anx.covid <- lm(anx ~ anx.lag1 + covid + covid.lag1 + covid.lag2, modeldata)
summary(fit.ar1anx.covid)


################################################################################
## Panel models
################################################################################

# Create logitudinal long form data frame
paneldata <- data.frame(
  State = covid$State,
  Date = covid$EndofWeek,
  CasesPerDay = covid$Cases,
  LogCasesRatio = covid$LogRatio
)
for (i in 1:nrow(paneldata)) {
  s <- paneldata[i, "State"]
  d <- paneldata[i, "Date"]
  
  if (d %in% anxiety$date_end) {
    paneldata[i,"anx"] <- anxiety[anxiety$date_end == d, s]
  }
  if (d %in% search$date_end) {
    paneldata[i,"search"] <- search[
      (search$State == s) & (search$date_end == d),
      "hits"
    ]
    paneldata[i,"searchdiff"] <- search[
      (search$State == s) & (search$date_end == d),
      "diff"
    ]
    paneldata[i,"searchlogratio"] <- search[
      (search$State == s) & (search$date_end == d),
      "logratio"
    ]
  }
}

paneldata <- paneldata[paneldata$Date %in% dates,] # Only dates with an anxiety measurement

# Model: panel model by state with anxiety as response regressed on covid
fit.states.fixed <- plm(anx ~ LogCasesRatio + lag(anx), paneldata, 
                        index=c("State", "Date"), model="within")
summary(fit.states.fixed)

fit.states.random <- plm(anx ~ LogCasesRatio + lag(anx), paneldata, 
                         index=c("State", "Date"), model="random")
summary(fit.states.random)

phtest(fit.states.fixed, fit.states.random)


# Model: panel model by state with anxiety as response regressed on covid SEARCHES
fit.states.search.fixed <- plm(anx ~ searchlogratio + lag(anx), paneldata, 
                        index=c("State", "Date"), model="within")
summary(fit.states.search.fixed)

fit.states.search.random <- plm(anx ~ searchlogratio + lag(anx), paneldata, 
                         index=c("State", "Date"), model="random")
summary(fit.states.search.random)

phtest(fit.states.search.fixed, fit.states.search.random)
