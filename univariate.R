
library(tidyverse)
library(lubridate)
library(forecast)
library(forecastHybrid)

df <- readxl::read_excel("data.xlsx") %>% janitor::clean_names()
df$date <- ymd(df$date)



# -------------------------------------------------------------------------

# df_train <- head(df, -6)
# df_test <- tail(df, 6)

# df_train <- df %>% filter(year(date) <= 2007)
# df_test <- df %>% filter(year(date) > 2008)


# train_ts <- ts(df_train$ibudarhusnaedi_alls,
#                start = 1994,
#                frequency = 12)
# 
# test_ts <- ts(df_test$ibudarhusnaedi_alls,
#               start = 2008,
#               frequency = 12)
# 
# fit_arima <- auto.arima(train_ts,
#                   stepwise = FALSE,
#                   approximation = FALSE)
# 
# fit_arima %>% 
#   forecast(h = 12) %>% 
#   autoplot(include = 36) +
#   autolayer(head(test_ts, 12))
# 
# 
# fit_ets <- ets(train_ts)
# 
# fit_ets %>% forecast(h = 12) %>% 
#   autoplot(include = 36) + 
#   autolayer(head(test_ts, 12))
# 
# 
# 
# # hybrid
# 
# fit_hybr <- hybridModel(train_ts,
#                         weights = "cv.errors",
#                         cvHorizon = 12,
#                         horizonAverage = TRUE,
#                         parallel = TRUE,
#                         num.cores = 3)
# 
# fit_hybr %>% 
#   forecast(h = 12) %>% 
#   autoplot(include = 48) + 
#   autolayer(head(test_ts, 12))




# Out of sample -----------------------------------------------------------

df <- df %>% 
  mutate(delta = ibudarhusnaedi_alls/lag(ibudarhusnaedi_alls, 12) - 1) %>% 
  na.omit()

total_ts <- ts(df$delta,
               start = 1995,
               frequency = 12)


fit_arima_full <- auto.arima(total_ts,
                             stepwise = FALSE,
                             approximation = FALSE)

fit_arima_full %>% 
  forecast(h = 12) %>% 
  autoplot(include = 48)

fit_ets_full <- ets(total_ts)

fit_ets_full %>% 
  forecast(h = 12) %>% 
  autoplot(include = 48)


fit_hybr_full <- hybridModel(total_ts,
                             weights = "cv.errors",
                             cvHorizon = 12,
                             horizonAverage = TRUE,
                             parallel = TRUE,
                             num.cores = 8)


fit_hybr_full %>% 
  forecast(h = 12) %>% 
  autoplot(includ = 48)

fc_hybr_full <- forecast(fit_hybr_full,
                         h = 12)

fc_hybr_full$mean


spa <- c(tail(df$ibudarhusnaedi_alls, 12), fc_hybr_full$mean)
plot(spa/lag(spa, 12) - 1, type = "l")
