library("tseries")
library("ggplot2")
library("forecast")
library("fpp")
library("fpp2")
library("gridExtra")
library("car")
library("astsa")
library("xlsx")
# Be sure to be in the right Directory
data <-read.xlsx(file="data_g15.xlsx",sheetIndex = 1)
uniData <-data$X..Viviendas
# We can see it is a monthly based recording of Subsidised housing approvals as
# % of totals

#1. Plot the series and briefly comment on the characteristics you observe 
#(stationarity, trend,seasonality, ...).
ts1 <-ts(data[,2],frequency=12,start=c(1990,1))

autoplot(ts1, xlab="Time", ylab="Approval Rate (%)")
spec.pgram(ts1) 
ggmonthplot(ts1)
ggseasonplot(ts1)

# Exploring correlations of lagged observations
gglagplot(ts1, lag=12, do.lines=FALSE)
ggAcf(ts1)

# We can extract the following conclusions:
# Data is strongly seasonal, There is a decreasing in the trend
# And Remainder is not white noise. We can see that variance is high during
# 1992 and 1994 and then the variance is reduced for the rest of the time 

#Arima1 <- Arima(ts1,order=c(2,1,0),seasonal=list(order=c(0,1,1), period=12))
# We can see here the seasonality: There was a great decrease in approvals from
# 1995 to 2000s The global trend is to decrease: from 20% to 13.8%

# 2. Obtain a plot of the decomposition of the series, using stl(). Use an additive decomposition
# or a multiplicative one, depending on your data. Use the function forecast() to forecast
# future values. Multiplicative decomposition is useful when the logarithmic transformation
# makes a difference in the appearance of your series and shows more constant variance. Does
# the remainder look like a white noise to you? White noise is just a group of independent,
# identically distributed variables, with zero mean and constant variance. Answer to this
# point just visually or plot the ACF and PACF of the remainder part.

stl1 <-stl(ts1, s.window="periodic", robust=TRUE)
plot(stl1)

#We split our model in 2: Train and Test
ts1.train<-window(ts1, end=2003-0.001)
ts1.test<-window(ts1,start=2003)

#Lets check if the model is multiplicative or additive
ts1.mult=hw(ts1.train,seasonal="multiplicative")
ts1.add=hw(ts1.train,seasonal="additive")

accuracy(ts1.mult, ts1.test)
accuracy(ts1.add, ts1.test) # Bingo! Our time serie is additive (The RMS is the lowest)
ggAcf(residuals(ts1.add))
ggAcf(residuals(ts1.mult)) #Here we can see that resuduals are much higher for mult serie

ts1.ets=ets(ts1.train) 
# If you don't specify anything, the ets() function looks for the model with lowest AIC (Akaike's
# information criteria).
summary(ts1.ets)
autoplot(ts1.ets)

trend=stl1$time.series[,2]
autoplot(trend) #Here we can see the trend
seasonal=stl1$time.series[,1]
autoplot(seasonal) #Here we can see the seasonal component
remainder=stl1$time.series[,3]
autoplot(remainder) #Here we can see the remainder
mean(remainder) #Its mean is close to 0, but we can visually check that variance is not
acf(remainder)
pacf(remainder)
#forecasting with stl(): different methods: naive, ets, arima, rwdrift
stl1.f=forecast(stl1, method="arima", h=48) #arima is the one selcted
plot(stl1.f)

# 3. Fit an ARIMA model to your time series. Some steps to follow:
#   a) Decide on whether to work with your original variable or with the log transform
# one (use plot and tsdisplay of both variables, the transformed one and the original
#      one). Remember that logarithmic transformation is useful to stabilize the variance.
# If both plots look almost the same to you, just use the original data.

logUniData<-log(uniData)
plot(logUniData)
plot(uniData)
tsdisplay(logUniData)
tsdisplay(uniData)
# Both models look almost the same, there is no significative difference in ACF
# nor PACF
model.1=Arima(unemp.ts,order=c(2,1,0),seasonal=list(order=c(0,1,1), period=12))


# b) Are you going to consider a seasonal component? If the answer is yes, identify s. You
# can use the periodogram (function tsdisplay with parameter "plot.type=spectrum"),
# ACF (with significant correlations on lag s and its multiples), seasonplot() and
# monthplot(). For montlhy data, s = 12; for quarterly data s = 4. Declare this
# frequency when you define your time-series in R, regardless of including or not a
# seasonal term in your model. This will permit you to use the functions seasonplot
# (package forecast) or monthplot (built-in function).

# The answer is yes, because diagrams showed us a great seasonal component.
tsdisplay(ts1,plot.type ="spectrum") # We can see that the signal has periodic components
# from the peaks in the espectrum at 2.5, 3.5, 4.5, etc.
acf(ts1) # this doesn't help a lot...
seasonplot(ts1) #Here is hard to see significative months...
monthplot(ts1) #Here we can clearly see the cycles within months

tsQ <-ts(data[,2],frequency=4,start=c(1990,1)) #we build this for quarterly data
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



# d) Identify values for p and q for the regular part and P and Q for the seasonal part.
# Start with low values and then increase them, one at a time. Fit different models
# and compare them using AICc and checking the residuals. Check also the correlation
# between the coefficients of the model.
# e) Make diagnostic of the residuals for the final model chosen (autocorrelations, zero
#                                                                 mean, normality). Use plots and tests.
# f ) Once you have found a suitable model, repeating the fitting model process several
# times if necessary, use it to make forecasts. Plot them.
# g) Use the function getrmse to compute the test set RMSE of some of the models you
# have already fiited. Which is the one minimizing it? Use the last year of observations
# (12 observations for monthly data, 4 observations for quarterly data) as the test set.
# 1
# h) You can also use the auto.arima() function with some of its parameters fixed, to
# see if it suggests a better model that the one you have found. Don't trust blindly
# its output. Automatic found models aren't based on an analysis of residuals but in
# comparing some other measures like AIC. Depending on how complex the data set
# is, they may find models with high values for p, q, P or Q (greater than 2).
