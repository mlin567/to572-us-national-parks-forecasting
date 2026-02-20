data <- read.csv("US-National-Parks_Use_1979-2023_By-Month.csv")
head(data)

# Dataset Overview
# - 33395 entries
# - 63 national parks in 30 states across 6 regions, Great Smoky Mountains NP as the most visited park.
# - spanning 45 years from 1979 to 2023, an overall upward trend in visits
# - 4.2 billion (4,218,318,992) total visits, 76.1% recreation, 23.9% non-recreation
# - Of recreation visits, only 7.95% involve camping (255 million (255,143,341)). Among campers, Tent 46.45%; RV 32.79%; Backcountry 20.76%.

library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)


# Great Smoky Mountains NP Tent camping visits forecasting

gsm <- data %>%
  filter(ParkName == "Great Smoky Mountains NP") %>%
  arrange(Year, Month)

ts_tent <- ts(gsm$TentCampers,
              start = c(min(gsm$Year), min(gsm$Month)),
              frequency = 12)

# Clean series
ts_tent_clean <- tsclean(ts_tent)
plot(ts_tent, main="Tent Camping Visits: Original vs Cleaned")
lines(ts_tent_clean, col="red")

# STL decomposition
camp_decomp_add <- stl(ts_tent_clean, s.window="periodic")
plot(camp_decomp_add, main="STL Additive Decomposition: Tent Camping")

# ts_tent_log <- log(ts_tent_clean)
# camp_decomp_mul <- stl(ts_tent_log, s.window="periodic")
# plot(camp_decomp_mul, main="STL Multiplicative Decomposition: Tent Camping")

# Seasonal plots
seasonplot(ts_tent_clean, main="Tent Camping Seasonality", col=rainbow(12))

# ACF and PACF
acf(ts_tent_clean, main="Tent Camping ACF")
pacf(ts_tent_clean, main="Tent Camping PACF")

# Train/test split: 1979–2019 training, 2020–2023 testing
train <- window(ts_tent_clean, end=c(2019,12))
test  <- window(ts_tent_clean, start=c(2020,1))

# Benchmark forecating
mean_fc  <- meanf(train, h=length(test))
naive_fc <- naive(train, h=length(test))
drift_fc <- rwf(train, drift=TRUE, h=length(test))

# Compare
accuracy(mean_fc, test)
accuracy(naive_fc, test)
accuracy(drift_fc, test)

# Visualization
autoplot(ts_tent_clean) +
  autolayer(mean_fc$mean, series="Mean Forecast", PI=FALSE, color="blue") +
  autolayer(naive_fc$mean, series="Naive Forecast", PI=FALSE, color="green") +
  autolayer(drift_fc$mean, series="Drift Forecast", PI=FALSE, color="red") +
  ggtitle("Tent Camping Benchmark Forecasts") +
  xlab("Year") + ylab("Tent Camping Visits")
