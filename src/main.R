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

# Read data and convert to time series ----
## Data => Monthly based recording of Subsidised housing approvals as
## % of totals from January 1990 to December 2007.
data <- read.xlsx(file = "./dataset/data_g15.xlsx", sheetIndex = 1)
subsidised_house_approvals <- data[, 2]
time_series <- ts(subsidised_house_approvals, frequency = 12, start = c(1990, 1))

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
# future values. Multiplicative decomposition is useful when the logarithmic transformation
# makes a difference in the appearance of your series and shows more constant variance. Does
# the remainder look like a white noise to you? White noise is just a group of independent,
# identically distributed variables, with zero mean and constant variance. Answer to this
# point just visually or plot the ACF and PACF of the remainder part.

## ndiffs() is used to determine the number of first differences required to make the time series non-seasonal
ndiffs(time_series)

stl1 <- stl(time_series, s.window = "periodic", robust = TRUE)
plot(stl1)

#We split our model in 2: Train and Test
ts1.train <- window(time_series, end = 2003 - 0.001)
ts1.test <- window(time_series, start = 2003)

#Lets check if the model is multiplicative or additive
ts1.mult = hw(ts1.train, seasonal = "multiplicative")
ts1.add = hw(ts1.train, seasonal = "additive")

accuracy(ts1.mult, ts1.test)
accuracy(ts1.add, ts1.test) # Bingo! Our time serie is additive (The RMS is the lowest)
ggAcf(residuals(ts1.add))
ggAcf(residuals(ts1.mult)) #Here we can see that resuduals are much higher for mult serie

ts1.ets = ets(ts1.train)
# If you don't specify anything, the ets() function looks for the model with lowest AIC (Akaike's
# information criteria).
summary(ts1.ets)
autoplot(ts1.ets)

trend = stl1$time.series[, 2]
autoplot(trend) #Here we can see the trend
seasonal = stl1$time.series[, 1]
autoplot(seasonal) #Here we can see the seasonal component
remainder = stl1$time.series[, 3]
autoplot(remainder) #Here we can see the remainder
mean(remainder) #Its mean is close to 0, but we can visually check that variance is not
acf(remainder)
pacf(remainder)
#forecasting with stl(): different methods: naive, ets, arima, rwdrift
stl1.f = forecast(stl1, method = "arima", h = 48) #arima is the one selcted
plot(stl1.f)

# 3. Fit an ARIMA model to your time series. Some steps to follow:
#   a) Decide on whether to work with your original variable or with the log transform
# one (use plot and tsdisplay of both variables, the transformed one and the original
#      one). Remember that logarithmic transformation is useful to stabilize the variance.
# If both plots look almost the same to you, just use the original data.

logUniData <- log(uni_data)
plot(logUniData)
plot(uni_data)
tsdisplay(logUniData)
tsdisplay(uni_data)
# Both models look almost the same, there is no significative difference in ACF
# nor PACF
model.1 = Arima(unemp.ts, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))


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

tsQ <- ts(data[, 2], frequency = 4, start = c(1990, 1)) #we build this for quarterly data
seasonplot(tsQ)
monthplot(tsQ) # Here we can see that a pattern repeats all quarter
# we should add a monthly component and a quarter component

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
kpssTesting <- kpss.test(x = time_series) # p-value < 0.05, null hypothesis is Stationary
Arima1model <- Arima(ts1.train, order = c(2, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12))
fc1 = forecast(Arima1model, h = 24)
plot(fc1)
accuracy(fc1, ts1.test) #RMSE: Training = 0.6591 Test = 3.6543

Arima2model <- Arima(ts1.train, order = c(2, 0.001, 0), seasonal = list(order = c(0, 0.001, 1), period = 12))
fc2 = forecast(Arima2model, h = 24)
plot(fc2)
accuracy(fc2, ts1.test) #RMSE: Training = 0.56737 Test = 1.0609 Better!
# We tried combinations for both models and the best one is d=0 D=0

# order is composed of p,d,q, seasonal is P,D,Q
# seasonal 
#p = the number of autoregressive terms
#d = the number of non-seasonal differences
#q = the number of moving-average terms

# d) Identify values for p and q for the regular part and P and Q for the seasonal part.
# Start with low values and then increase them, one at a time. Fit different models
# and compare them using AICc and checking the residuals. Check also the correlation
# between the coefficients of the model.

Arima3model <- Arima(ts1.train, order = c(3, 0.001, 0), seasonal = list(order = c(0.5, 0.001, 1), period = 12))
fc3 = forecast(Arima3model, h = 24)
plot(fc3)
accuracy(fc3, ts1.test) #RMSE: Training = 0.545 Test = 1.047 Better!

#optimal (p,P) empirically found are 3 and 0.5

Arima4model <- Arima(ts1.train, order = c(3, 0.001, 10), seasonal = list(order = c(0.5, 0.001, 1), period = 12))
fc4 = forecast(Arima4model, h = 24)
plot(fc4)
accuracy(fc4, ts1.test) #RMSE: Training = 0.48447 Test = 0.3567 Better!

#optimal (q,Q) empirically found are 10 and 1 respectively

# e) Make diagnostic of the residuals for the final model chosen (autocorrelations, zero meanm normality)
#  Use plots and tests.

t.test(residuals(Arima4model)) # p-value is > 0.05 then true mean ==0
checkresiduals(Arima4model) # Here we can see nicely that residuals are mean 0 and normal variance
acf2(Arima4model$residuals)

# f ) Once you have found a suitable model, repeating the fitting model process several
# times if necessary, use it to make forecasts. Plot them.

# We have done so previously! Lets try some blind predicion!
Arima5model <- Arima(time_series, order = c(3, 0.001, 10), seasonal = list(order = c(0.5, 0.001, 1), period = 12))
fc5 = forecast(Arima5model, h = 24)
plot(fc5)
# these results seems reasonable, still there are no way to check accuracy from our dataset.
# g) Use the function getrmse to compute the test set RMSE of some of the models you
# have already fiited. Which is the one minimizing it? Use the last year of observations
# (12 observations for monthly data, 4 observations for quarterly data) as the test set.

# We have done this before
getrmse(Arima5model)
getrmse(Arima4model)
# Not working
# h) You can also use the auto.arima() function with some of its parameters fixed, to
# see if it suggests a better model that the one you have found. Don't trust blindly
# its output. Automatic found models aren't based on an analysis of residuals but in
# comparing some other measures like AIC. Depending on how complex the data set
# is, they may find models with high values for p, q, P or Q (greater than 2).

ts1.auto = auto.arima(ts1.train)
ts1.auto
tsdiag(ts1.auto)
fc6 = forecast(ts1.auto, h = 24)
plot(fc6)
accuracy(fc6, ts1.test)
# This automated model is worse in the RMSE than the one manually tuned, since
# It is tuned based on other parameters like Akaike's information cryterion,
# instead on the residuals

Box.test(ts1.auto$residuals, 12, fitdf = 1)
t.test(ts1.auto$residuals)
jarque.bera.test(ts1.auto$residuals[-c(49, 72, 73, 37, 77)])

# The spectrum of the residuals:s
tsdisplay(ts1.auto$residuals, plot.type = "spectrum")
