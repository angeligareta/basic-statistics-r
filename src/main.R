# Library imports ----
library("tseries")
library("ggplot2")
library("forecast")
library("fpp")
library("fpp2")
library("gridExtra")
library("car")
library("astsa")
library("xlsx")
library("fUnitRoots")
library("MTS")

# Read data and convert to time series ----
## Data => Monthly based recording of Subsidised housing approvals as
## % of totals from January 1990 to December 2007.
data <- read.xlsx(file = "./dataset/data_g15.xlsx", sheetIndex = 1)
subsidised_house_approvals <- data[, 2]
time_series <- ts(subsidised_house_approvals, frequency = 12, start = c(1990, 1))
log_time_series <- log(time_series)

# 1. Plot the series and briefly comment on the characteristics you observe
# (stationarity, trend, seasonality, ...) ----

## Plot observed time series
autoplot(time_series, xlab = "Time", ylab = "Approval Rate (%)")

## With respect of the trend: There is a decrease from 1994 to 2000 and an increase
## from 2000 to 2004. The global trend is to decrease: from 20% to 13.8%
### Decompose time series. Show (trend and seasonality)
plot(decompose(time_series))

## With respect of the seasonality the data is strongly seasonal according to the ggmonthplot.
## Besides, looking to the gglagplot we see the data is strongly correlated to the previous season
### Plot seasonality per month and year
ggmonthplot(time_series)
ggseasonplot(time_series)

### Exploring correlations of lagged observations
gglagplot(time_series, lag = 12, do.lines = FALSE)
ggAcf(time_series)

## Stationarity: We can see that it is not stationary because of the trend, making the mean and the
## variance change over time. However, to statiistically prove it, we performed the Augmented
## Dickey-Fuller Test and the resulting p-value was of 0.41, confirming the time series is not
## stationary
### Augmented Dickey-Fuller Test => test if x is a non-stationary time series
adf.test(time_series)

# With respect of the randomm part it does not seem as white noise.
# We can see that variance is high during 1992 and 1994 and then it is reduced
# for the rest of the time, so it doesnt match with the white nose properties
# of constant variance and mean.
plot(decompose(time_series))

## ?
spec.pgram(time_series)

# 2. Obtain a plot of the decomposition of the series, using stl(). Use an additive decomposition
# or a multiplicative one, depending on your data. Use the function forecast() to forecast
# future values.
#
# Multiplicative decomposition is useful when the logarithmic transformation
# makes a difference in the appearance of your series and shows more constant variance.
## Decompose the series
stl1 <- stl(time_series, s.window = "periodic", robust = TRUE)
stl1_multiplicative <- stl(log_time_series, s.window = "periodic", robust = TRUE)

plot(stl1)
## We use log to be able to show multiplicative decomposition: https://otexts.com/fpp2/components.html
## The remainder is much better! It seems to have constant variance and mean
## Now back-transform the data with exp

## Compare individual plots
autoplot(stl1$time.series[, "seasonal"])
autoplot(exp(stl1_multiplicative$time.series[, "seasonal"]))

autoplot(stl1$time.series[, "trend"])
autoplot(exp(stl1_multiplicative$time.series[, "trend"]))

autoplot(stl1$time.series[, "remainder"])
autoplot(exp(stl1_multiplicative$time.series[, "remainder"]))

## Check variance for residuals for both decompositions. Both are stationary
adf.test(stl1$time.series[, "remainder"])
adf.test(stl1_multiplicative$time.series[, "remainder"])

## Make a forecast with both decompositions and compare the errors
# We split our model in 2: Train and Test ( Approx 80-20%, 17 years (13, 4) )
train_end <- time(time_series)[length(time_series) - 48]   #train data end
test_start <- time(time_series)[length(time_series) - 48 + 1]  #test data start
time_series_train_split <- window(time_series, end = train_end)
time_series_test_split <- window(time_series, start = test_start)

### Using hew
# Lets check if the model is multiplicative or additive
ts1_mult <- hw(time_series_train_split, seasonal = "multiplicative")
ts1_add <- hw(time_series_train_split, seasonal = "additive")
# Using hw, the additive time series has the lowest rms, but if we chhange the training
# percentage the multiplicative is higher in other ones
accuracy(ts1_mult, time_series_test_split)
accuracy(ts1_add, time_series_test_split)

# Forecasting with stl(): different methods: naive, ets, arima, rwdrift
stl1_forecast <- forecast(stl1, method = "arima", h = 48) # arima is the one selcted
plot(stl1_forecast)

stl1_forecast_multiplicative <- forecast(stl1_multiplicative, method = "arima", h = 48) # arima is the one selcted
plot(stl1_forecast_multiplicative)

## ?
# ggAcf(residuals(ts1_mult)) #Here we can see that resuduals are much higher for mult serie
# ggAcf(residuals(ts1_add))

## ?
## ETS => Exponential Smoothing State Space Model
## If you don't specify anything, the ets() function looks for the model with lowest AIC (Akaike's
## information criteria).
# ts1_ets <- ets(ts1_train)
# summary(ts1_ets)
# autoplot(ts1_ets)

# Does the remainder look like a white noise to you? White noise is just a group of independent,
# identically distributed variables, with zero mean and constant variance. Answer to this
# point just visually or plot the ACF and PACF of the remainder part.

## The remainder using additive decomposition does not look like white noise visually
## and we will check neither statistically
remainder <- stl1$time.series[, "remainder"]
autoplot(remainder)

## The remainder using multiplicative decomposition look like white noise visually
## and statistically
remainder_multiplicative <- exp(stl1_multiplicative$time.series[, "remainder"])
autoplot(remainder_multiplicative)

## Compare variance
var(remainder) # 0.75
var(remainder_multiplicative) # Almost 0

## Compare acf and pacf
acf(remainder, 30)
pacf(remainder, 30)

## We can see that the multiplicative have lower correlation in acf
acf(remainder_multiplicative, 30)
pacf(remainder_multiplicative, 30)

archTest(remainder)
archTest(remainder_multiplicative)

# 3. Fit an ARIMA model to your time series. Some steps to follow:
# a) Decide on whether to work with your original variable or with the log transform
# one (use plot and tsdisplay of both variables, the transformed one and the original
# one). Remember that logarithmic transformation is useful to stabilize the variance.
# If both plots look almost the same to you, just use the original data.
plot(time_series)
plot(log_time_series)

# Both models look almost the same, there is no significative difference in ACF
# nor PACF
tsdisplay(time_series)
tsdisplay(log_time_series)

acf(time_series)
acf(log_time_series)

# model_1 <- Arima(time_series, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))

# b) Are you going to consider a seasonal component? If the answer is yes, identify s. You
# can use the periodogram (function tsdisplay with parameter "plot.type=spectrum"),
# ACF (with significant correlations on lag s and its multiples), seasonplot() and
# monthplot(). For montlhy data, s = 12; for quarterly data s = 4. Declare this
# frequency when you define your time-series in R, regardless of including or not a
# seasonal term in your model. This will permit you to use the functions seasonplot
# (package forecast) or monthplot (built-in function).

# The answer is yes, because diagrams showed us a great seasonal component.
tsdisplay(time_series, plot.type = "spectrum") # We can see that the signal has periodic components
# from the peaks in the espectrum at 2.5, 3.5, 4.5, etc.
acf(time_series) # this doesn't help a lot...
seasonplot(time_series) #Here is hard to see significative months...
monthplot(time_series) #Here we can clearly see the cycles within months

time_series_quaterly <- ts(data[, 2], frequency = 4, start = c(1990, 1)) #we build this for quarterly data
seasonplot(time_series_quaterly)
monthplot(time_series_quaterly) # Here we can see that a pattern repeats all quarter

# We should try with s = 4 and 12

# c) Decide on the values of d and D to make your series stationary. D values are not
# usually greater than 1. You can use the standard deviation procedure and stationary
# tests (adf.test, kpss.test). Also, functions like ndiffs and nsdiffs may be useful. Don't
# be surprised if that group of tools give you contradictory results. Just, make decision
# to keep going. Plots of the ACF and PACF also help. If you can draw a pattern out
# of the ACF and PACF function, then stop differencing and start modelling. If that
# pattern doesn't work, then consider rethinking the part where you took differences.

acf(time_series)
pacf(time_series)

ndiffs(time_series) # only 1 difference needed to make the time serie stationary
# We have to say that this is not true empirically... When tested an arima model,
# we obtained better results for predictions using 0 as d

nsdiffs(time_series) # no difference required fo a seasonally stationary serie (because it is!)

adf.test(x = time_series) # Null hypothesis non stationary, obtianed p-value >0.05
kpss.test(x = time_series) # p-value < 0.05, null hypothesis is Stationary

time_series_stationary <- diff(time_series, differences = 1)

# With one diff pass both test
adf.test(x = time_series_stationary) # Null hypothesis non stationary, obtianed p-value >0.05
kpss.test(x = time_series_stationary) # p-value < 0.05, null hypothesis is Stationary

# ACF improved a lot
acf(time_series_stationary)
plot(time_series_stationary)

# With d = 1, we obtain a stationary data, so we do not need a higher d.

## For seasonal differences D, we will use D = 0, as the sd is lower with only the d = 1.
## Besides the acf are very alike
sd(time_series_stationary)
sd(diff(diff(time_series, 12)))

acf(time_series_stationary)
acf(diff(diff(time_series, 12)))

## ? I would not select which d to choose based on arima models. We have to demonstrate it statistically
arima_model_1 <- Arima(time_series_train_split, order = c(2, 1, 0), seasonal = list(order = c(1, 0, 0), period = 12))
forecast_arima_model_1 <- forecast(arima_model_1, h = 24)
plot(forecast_arima_model_1)
accuracy(forecast_arima_model_1, time_series_test_split) #RMSE: Training = 0.5393376 Test = 1.8357907 Better!

arima_model_2 <- Arima(time_series_train_split, order = c(2, 0.0000001, 0), seasonal = list(order = c(1, 0, 0), period = 12))
forecast_arima_model_2 <- forecast(arima_model_2, h = 24)
plot(forecast_arima_model_2)
accuracy(forecast_arima_model_2, time_series_test_split) #RMSE: Training = 0.5505118 Test = 2.1285549
# We tried combinations for both models and the best one is d=1 D=0

# order is composed of p,d,q, seasonal is P,D,Q
# seasonal 
#p = the number of autoregressive terms
#d = the number of non-seasonal differences
#q = the number of moving-average terms

# d) Identify values for p and q for the regular part and P and Q for the seasonal part.
# Start with low values and then increase them, one at a time. Fit different models
# and compare them using AICc and checking the residuals. Check also the correlation
# between the coefficients of the model.

arima_model_3 <- Arima(time_series_train_split, order = c(3, 1, 0), seasonal = list(order = c(0.5, 0, 1), period = 12))
forecast_arima_model_3 <- forecast(arima_model_3, h = 24)
plot(forecast_arima_model_3)
accuracy(forecast_arima_model_3, time_series_test_split) #RMSE: Training = 0.545 Test = 1.047 Better!

# optimal (p, P) empirically found are 3 and 0.5

arima_model_4 <- Arima(time_series_train_split, order = c(3, 1, 10), seasonal = list(order = c(0.5, 0, 1), period = 12))
forecast_arima_model_4 <- forecast(arima_model_4, h = 24)
plot(forecast_arima_model_4)
accuracy(forecast_arima_model_4, time_series_test_split) #RMSE: Training = 0.48447 Test = 2.2173172 Better!

# optimal (q, Q) empirically found are 10 and 1 respectively

## Made a function to automatically permutate and obtained best configuration:
getrmse <- function(x, h, ...)
  {
  train_end <- time(x)[length(x) - h]   #train data end
  test_start <- time(x)[length(x) - h + 1]  #test data start
  train <- window(x, end = train_end) #extract train data
  test <- window(x, start = test_start)  #extract test data
  fit <- Arima(train, ...) # fit model with train data
  fc <- forecast(fit, h = h) # forecast with model
  return(accuracy(fc, test)[2, "RMSE"]) #compare forecast with test data, extract the rmse
}

# We have done this before
best_combination <- ""
best_combination_rmse <- 100000

permutate_get_rmse <- function(data, p_arr, P_arr, q_arr, Q_arr) {
  for (p in p_arr) {
    for (P in P_arr) {
      for (q in q_arr) {
        for (Q in Q_arr) {
          combination <- paste("RMSE of p = ", p, " P = ", P, " q = ", q, "Q = ", Q)
          rmse <- getrmse(data, h = 12, order = c(p, 1, q), seasonal = list(order = c(P, 0, Q), period = 12))
          print(paste(combination, " = ", rmse))

          ## Save best combination
          if (rmse < best_combination_rmse) {
            assign("best_combination", combination, envir = .GlobalEnv)
            assign("best_combination_rmse", rmse, envir = .GlobalEnv)
          }
        }
      }
    }
  }

  print(paste("BEST COMBINATION => ", best_combination, " = ", best_combination_rmse))
  best_combination_rmse
}

## Permutate values p, q, P and Q
best_rmse <- permutate_get_rmse(data = time_series, p_arr = seq(0, 3), q_arr = seq(0, 3), P_arr = seq(0, 3), Q_arr = seq(0, 3))
## Best combination for raw time series => "RMSE of p =  1  P =  3  q =  0 Q =  1 " => 0.201915547276868

best_rmse_log <- permutate_get_rmse(data = log(time_series), p_arr = seq(0, 3), q_arr = seq(0, 3), P_arr = seq(0, 3), Q_arr = seq(0, 3))
## Best combination: "RMSE of p =  1  P =  1  q =  1 Q =  0" => 0.01644351

final_model <- Arima(time_series_train_split, order = c(1, 1, 0), seasonal = list(order = c(3, 0, 1), period = 12))
final_model_log <- Arima(log(time_series_train_split), order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12))

forecast_final_model <- forecast(final_model, h = 12)
forecast_final_model_log <- forecast(final_model_log, h = 12)

accuracy(forecast_final_model, time_series_test_split)
accuracy(forecast_final_model_log, log(time_series_test_split))

forecast_final_model_rmse <- getrmse(time_series, h = 12, order = c(1, 1, 0), seasonal = list(order = c(3, 0, 1), period = 12))
forecast_final_model_log_rmse <- getrmse(log(time_series), h = 12, order = c(1, 1, 1), seasonal = list(order = c(1, 0, 0), period = 12))

forecast_final_model_rmse
forecast_final_model_log_rmse

# e) Make diagnostic of the residuals for the final model chosen (autocorrelations, zero meanm normality)
#  Use plots and tests.
final_model_log_residuals_transformed <- exp(final_model_log$residuals)
plot(final_model$residuals)
plot(final_model_log_residuals_transformed)

t.test(final_model$residuals) # p-value is <>> 0.05 => true mean != 0
t.test(final_model_log_residuals_transformed) # p-value is > 0.05 =>  true mean == 0

Box.test(final_model$residuals, lag = 20, fitdf = 5, type = "L")
Box.test(final_model_log_residuals_transformed, lag = 20, fitdf = 5, type = "L")

checkresiduals(final_model) # Here we can see nicely that residuals are mean 0 and normal variance

final_model_log_temp <- final_model_log
final_model_log_temp$residuals <- exp(final_model_log_temp$residuals)
checkresiduals(final_model_log_temp)

acf2(final_model$residuals)
acf2(final_model_log_residuals_transformed) # Much less correlation in acf

# f ) Once you have found a suitable model, repeating the fitting model process several
# times if necessary, use it to make forecasts. Plot them.

plot(forecast_final_model)
plot(ts(subsidised_house_approvals, frequency = 12, start = c(1990, 1), end = c(2005, 1)))

## Real log
plot(forecast_final_model_log)
plot(ts(log(subsidised_house_approvals), frequency = 12, start = c(1990, 1), end = c(2005, 1)))

# g) Use the function getrmse to compute the test set RMSE of some of the models you
# have already fiited. Which is the one minimizing it? Use the last year of observations
# (12 observations for monthly data, 4 observations for quarterly data) as the test set.
## Util taken from teacher

## DONE UP

# h) You can also use the auto.arima() function with some of its parameters fixed, to
# see if it suggests a better model that the one you have found. Don't trust blindly
# its output. Automatic found models aren't based on an analysis of residuals but in
# comparing some other measures like AIC. Depending on how complex the data set
# is, they may find models with high values for p, q, P or Q (greater than 2).

automatic_arima_model <- auto.arima(time_series_train_split)
automatic_arima_model

tsdiag(automatic_arima_model)

forecast_automatic_arima_model <- forecast(automatic_arima_model, h = 24)
plot(forecast_automatic_arima_model)
accuracy(forecast_automatic_arima_model, time_series_test_split)
# This automated model is worse in the RMSE than the one manually tuned, since
# It is tuned based on other parameters like Akaike's information cryterion,
# instead on the residuals

Box.test(automatic_arima_model$residuals, 12, fitdf = 1)
t.test(automatic_arima_model$residuals)
jarque.bera.test(automatic_arima_model$residuals[-c(49, 72, 73, 37, 77)])

# The spectrum of the residuals:s
tsdisplay(automatic_arima_model$residuals, plot.type = "spectrum")