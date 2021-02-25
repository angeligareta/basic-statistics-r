library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(tsoutliers)

# Read data with strings as factors
data <-
  read.table(
    "./data/diamonds.txt",
    quote = "\"",
    comment.char = "",
    stringsAsFactors = T
  )

# Rename headers from data
names(data) <-
  c("Caratage", "ColourPurity", "Clarity", "InstCert", "Price_SGD")

# Reorder the colour purity factors.
data <-
  data %>% mutate(ColourPurity = relevel(ColourPurity, ref = "I"))

# Reorder the clarity factors.
data <- data %>% mutate(Clarity = relevel(Clarity, ref = "VS2"))

# Reorder the clarity Certification institution.
data <- data %>% mutate(InstCert = relevel(InstCert, ref = "HRD"))

# Look for correlation between variables
data %>% ggplot(aes(x = Caratage, y = Price_SGD, color = ColourPurity, shape = Clarity)) + geom_point()

# Plot price vs caratage
data %>% ggplot(aes(x = Caratage, y = Price_SGD)) + geom_point(color="brown")

# Plot log price vs caratage (As it is a transformation, we avoid heterocedasticity)
data %>% ggplot(aes(x = Caratage, y = log(Price_SGD))) + geom_point(color="brown")

# We can correct the heterocedasticity
data %>% ggplot(aes(x = Price_SGD)) + geom_histogram(fill="brown")
data %>% ggplot(aes(x = log(Price_SGD))) + geom_histogram(fill="brown")

# We are going to use log price since the explanatory variable is better
data <- data %>% mutate("Log_Price_SGD" = log(Price_SGD))

# Calculate the first linear model taking all the variables except the normal price.
lm1 = lm(Log_Price_SGD ~ Caratage + ColourPurity + Clarity + InstCert, data = data)
summary(lm1)

lm1_pred = predict(lm1)

data %>% ggplot(aes(x = Caratage, y = Log_Price_SGD)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Plot the linear regression models separated by color purity
data %>% ggplot(aes(x = Caratage, y = Log_Price_SGD, color = ColourPurity)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Plot the linear regression models separated by clarity
data %>% ggplot(aes(x = Caratage, y = Log_Price_SGD, color = Clarity)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Plot the linear regression models separated by InstCert
data %>% ggplot(aes(x = Caratage, y = Log_Price_SGD, color = InstCert)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Despite one of the InstCert GIA or HRD is not signifcant, the InstCertIGI is,
# if we try to do a model removing InstCert the R^2 will inevitable go down.
lm2 = lm(Log_Price_SGD ~ Caratage + ColourPurity + Clarity, data = data)
summary(lm2)

# However if we do an anova with the models it will result that InstCert model is signficant.
anova(lm2, lm1)

## TESTS
# They show a visible pattern in the distriubtion along indexes, showing that the model can be improved.
residualPlot(lm1)

# As we can se, residuals have no mean 0 on every element and variance is not the same.
plot(lm1$residuals)

# No stdres with Bonferroni p<0.05 was found in the dataset
outlierTest(lm1)

# For constant variance -> https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
bptest(lm1)
# we can see that the p value is very low, implying heterocedasticity:
# https://en.wikipedia.org/wiki/Breusch%E2%80%93Pagan_test ref here for p value interpret

# For independence
dwtest(lm1, alternative = "two.sided")
# DW is >0 and <2 meaning positive autocorrelation
# And the test tells us exists autocorr since pvalue is low

Box.test(lm1$residuals)
# exists dependency in the residuals
#https://stat.ethz.ch/pipermail/r-help/2004-April/049548.html ref for p value interp

# For normality
JarqueBera.test(lm1$residuals)
# as we can see, pvalues are very high, all > 0.05, meaning normality must ve discarded
#https://stats.stackexchange.com/questions/130368/why-do-i-get-this-p-value-doing-the-jarque-bera-test-in-r ref for interpret

# Question 3
# 3a) First, Create a new categorical variable to segregate the stones according to caratage:
# let's say less than 0.5 carats small, 0.5 to less than 1 carat (medium) and 1 carat
# and over (large)
caratcut <- cut(data$Caratage, c(0, 0.5, 1, max(data$Caratage)))

# small is ref by default
data <-
  data %>% mutate("CaratageSize" = cut(
    data$Caratage,
    c(0, 0.4999999, 0.9999999, max(data$Caratage)),
    labels = c("Small", "Medium", "Large")
  ))

lm3 <-
  lm(Log_Price_SGD ~ Caratage * CaratageSize + ColourPurity + Clarity + InstCert,
     data = data)
summary(lm3) 

# Is this regression model satisfactory? Yes, it has increased the R^2 and all 
#the variables are significant except the InstCert as before

# Assumptions of linear regression
# Linearity: The mean of the response E at each value is a linear function of the Xi.
# It can be checked through scatter plots, Linearity: Yes
plot(lm3$residuals)

# Independence: The residuals are independent? No
# For independence Durbin-Watson test
# As the DW statistic is 1, it means it has a positive autocorrelation, so the residuals are not independent.
dwtest(lm3, alternative = "two.sided")

# As p-value is low, it means no independence Box-Ljung
Box.test(lm1$residuals)

# Normality: The residuals, for each value of the predictor Xi are
# normally distribuited? No 
# JarqueBera: https://stats.stackexchange.com/questions/130368/why-do-i-get-this-p-value-doing-the-jarque-bera-test-in-r
JarqueBera.test(lm3$residuals)
   
# Equal variance? No 
# P-value low, reject constant variance Ho -> https://stats.stackexchange.com/questions/239060/interpretation-of-breusch-pagan-test-bptest-in-r
bptest(lm3)

# Are the numerical estimates sensible?
# All of them are significant except InstCert
summary(lm3)

# Interpret the interaction parameter med*carat.
## First of all the interaction is significant. Besides, it contibutes to the increment of the R square
## compared to the previous model. This could because as interacting a variable with 
## the same variable but segregated in 3 cluster, helps capturing the effect of all the prices variation
## in that cluster, so it reduces the effect of outliers, as the interaction estimate is negative
## in the medium and large cluster. Maybe the interaction term is doing the average between the caratage in the cluster.


# What can we infer on the incremental pricing of caratage in the 3 clusters?
## ?

# Which is more highly valued: colour or clarity?
# To see which variable affects more to the price of the diamond, we should
## make an average of the estimates for each variable, as the estimate remains the same for both variables.
## For ColourPurity, the mean would be: 
ColourPurityMean <- 
  (summary(lm3)$coefficients[5] + 
  summary(lm3)$coefficients[6] +
  summary(lm3)$coefficients[7] + 
  summary(lm3)$coefficients[8] +
  summary(lm3)$coefficients[9]) / 5

ClarityMean <- 
  (summary(lm3)$coefficients[10] + 
     summary(lm3)$coefficients[11] +
     summary(lm3)$coefficients[12] + 
     summary(lm3)$coefficients[13]) / 4

# As we can check, the color has higher value than clarity
ColourPurityMeanHigher <- ColourPurityMean > ClarityMean

# All other things being equal, what is the average price diference between a
# grade D diamond and another one graded (a) I (b) E?
# a) The average price difference between D and I would be the estimate of D, as I is the reference variable
# and it is already present in the intercept of 0.433562.
# b) The average price difference between D and E would be the absolute difference between the estimates of D and E,
# which is of 0.08488306.
abs(summary(lm3)$coefficients[5] - summary(lm3)$coefficients[6])  

# All other things being equal, are there price diferences amongst the stones
# appraised by the GIA, IGI and HRD?
## The price difference amongs the Certificate Institution are very low and besides they are
## not significant, so as we stated before, it would be better to do not include this variable in the model.

# 3b)  Include the square of carat as a new explanatory variable. It avoids the subjectivity
# of clusters definition.
lm4 <-
  lm(Log_Price_SGD ~ Caratage + I(Caratage ^ 2) + ColourPurity + Clarity + InstCert,
     data = data)
summary(lm4)

# Assumptions of linear regression
# Linearity: Yes
plot(lm4$residuals)

# Independence: The residuals are independent? No
# For independence Durbin-Watson test
# As the DW statistic is 0.98 and not 2, it means it has a positive autocorrelation, so the residuals are not independent.
dwtest(lm4, alternative = "two.sided")

# As p-value is low, it means no independence Box-Ljung
Box.test(lm4$residuals)

# Normality: The probability of the residuals being normal in this model is much lower than the previous one,
## but the p-value is still being lo 0.143, so we can not ssay the model normality is good, as the skewness is very significant.
# JarqueBera: https://stats.stackexchange.com/questions/130368/why-do-i-get-this-p-value-doing-the-jarque-bera-test-in-r
JarqueBera.test(lm4$residuals)

# Equal variance? Yes 
# P-value low, reject constant variance Ho -> https://stats.stackexchange.com/questions/239060/interpretation-of-breusch-pagan-test-bptest-in-r
bptest(lm4)

## The second model in terms of validity is better as the equal variance rule is satisfied and th
## normality probability is much higher than the previous model, but in both models the independence assumptiuon is not valid.
## With respect of the interpretability, as in the second model an interaction term is not being added, is much interpretable,
## as it could be interpreted as the price increases exponentially with the square of carat.
plot(Log_Price_SGD~ I(Caratage ^2), data = data)
abline(lm4)

