# Analyzing time-series
library(forecast)
library(tseries)

# Otter fur sales, Hudson's Bay company
load("otter.Rdata")
class(otter)

otter
attributes(otter)

# Plot the time-series
plot(otter, main = "Otter fur sales", xlab = "Year", ylab = "Sales")
# Not constant mean/variance  

# Look at autocorrelation and partial autocorrelation of data
acf(otter)
pacf(otter)

# Test for stationarity. Augmented Dickey-Fuller test.
# Test 
adf.test(otter, alternative="stationary")

# Not stationary because the mean varies over time. Take the first difference
diff(otter)
diff(otter,2)
plot(diff(otter)) #Note its flat now 
acf(diff(otter)) 
#Less autocorrelation. Significance at 1 only (negative)


# Looks much better after first differencing. Test for stationarity. Augmented Dickey-Fuller test
adf.test(diff(otter), alternative="stationary")

# Automatically determine the best model with a combination of AR and MA terms and differencing
auto.arima(otter, trace = T)

# Best model is an ARIMA model with no AR term, a differencing of 1, and an MA term of 1
m1 <- arima(otter, order = c(0,1,1)) 
summary(m1)
# Strong negative coefficient


# Plot the results
plot(otter, pch= 19, col = "red", type = "p", xlab = "Year", ylab = "Sales", main = "Otter fur sales, Hudson Bay company")
lines(fitted(m1))
# arima following the time series model

# To show why it is important to use ARIMA models, use a LM instead
time = 1850:1911
m3 <- lm(otter ~ time)
summary(m3)

# Now use Ljung-box test for significant autocorrelation in the residuals
Box.test(residuals(m3))
acf(residuals(m3))

Box.test(residuals(m1))
acf(residuals(m1))

# Always check autocorrelation. With autocorrelation, you're more likely to find
# significant resluts

