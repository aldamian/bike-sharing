library(prophet)
library(lubridate)
library(ggplot2)

# Bike sharing data
#data <- read.csv(file.choose(), header = T)
data <- readr::read_csv("day.csv")
data$dteday <- ymd(data$dteday)

# By first plotting the data, it is noticeable that it has both trend and seasonality.
# During winter months, bike rentals are lower, and during the summer months there is a significant increase in the number of bike rentals.
# There are also fluctuations up and down both during winter as well as summer months.

# Plot
qplot(dteday, cnt, data = data,
      main = 'Bike Rentals in Washington DC')

# Data
ds <- data$dteday
y <- data$cnt
df <- data.frame(ds, y)
df$temp <- data$temp
df$hum <- data$hum
df$windspeed <- data$windspeed

# Forecasting model
m <- prophet()
m <- add_country_holidays(m, country_name = 'US')
m <- add_regressor(m, 'temp')
m <- add_regressor(m, 'hum')
m <- add_regressor(m, 'windspeed')
m <- fit.prophet(m, df)

# Since the accuracy of the prediction drops significantly as the predicted period is larger, and as we only 
# have 2 year data that our model can learn from, we decided that it is sensible to only predict 1 month into 
# the future. 
# Data ends on January 10'th 2013, do we do the prediction for 21 more days in order to have the full month.

# Prediction
future <- make_future_dataframe(m, periods = 21)
x <- data.frame(df$temp)
colnames(x) <- 'temp'
# generate temperatures for 21 days in the future
y <- data.frame(runif(21, 0.1, 0.3))
colnames(y) <- 'temp'
future$temp <- rbind(x, y)

x <- data.frame(df$hum)
colnames(x) <- 'hum'
# generate humidity for 21 days in the future
# should get real numbers from weather forecast
y <- data.frame(runif(21, 0.4, 0.8))
colnames(y) <- 'hum'
future$hum <- rbind(x, y)

x <- data.frame(df$windspeed)
colnames(x) <- 'windspeed'
y <- data.frame(runif(21, 1, 3))
colnames(y) <- 'windspeed'
future$windspeed <- rbind(x, y)

future <- as.matrix(future)
colnames(future) <- NULL
colnames(future) <- c('ds', 'temp', 'hum', 'windspeed')
future <- data.frame(future)
future$temp <- as.numeric(future$temp)
future$hum <- as.numeric(future$hum)
future$ds <- ymd(future$ds)
future$windspeed <- as.numeric(future$windspeed)

forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
dyplot.prophet(m, forecast)

# Forecast components
prophet_plot_components(m, forecast)

# Model performance
pred <- forecast$yhat[1:731]
actual <- df[,2]
plot(actual, pred)
abline(lm(pred~actual), col='red')
summary(lm(pred~actual))

# df.cv <- cross_validation(m, initial = 365.25, period = 180, horizon = 364, units = 'days')
# head(df.cv)
# df.p <- performance_metrics(df.cv) 
# (head(df.p))
