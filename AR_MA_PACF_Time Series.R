############# Passenger data
passenger <- read.csv("passenger.csv", head = T)
passenger <- ts(passenger, start = c(1949,1), deltat = 1/12)
plot(passenger, xlab = "Year", ylab = "Number of Passengers in thousands")

passenger.log <- log(passenger)
plot(passenger.log, xlab = "Year", ylab = "Log Number of Passengers")


##### Trend removal
# fit a linear model
pass.time <- time(passenger)
fit.trend <- lm(passenger.log ~ pass.time)
pass.trend <- ts(fit.trend$fitted.values, start = c(1949,1), deltat = 1/12)
# linear trend removed
fit.res <- fit.trend$residuals
# format it into time series
fit.res <- ts(fit.res, start = c(1949,1), deltat = 1/12)
plot(fit.res)

##### Seasonality removal
pass.month <- factor(cycle(fit.res))
fit.season <- lm(fit.res ~ pass.month)
pass.season <- ts(fit.season$fitted, start = c(1949,1), deltat = 1/12)


####With both linear trend and seasonality removed
pass.rand <- ts(fit.res - pass.season, start = c(1949,1), deltat = 1/12)

### plot the decomposition
par(mfrow = c(2, 2))
plot(passenger.log, xlab = "Year", ylab = "log(# of Passengers)")
plot(pass.trend, xlab = "Year", ylab = "Trend")
plot(pass.season, xlab = "Year", ylab = "Seasonality")
plot(pass.rand, xlab = "Year", ylab = "Random")


# check acf and pacf of the residual series
par(mfrow = c(1, 2))
acf(pass.rand, main = "ACF of the residual series",lag.max=36)
pacf(pass.rand, main = "PACF of the residual series",lag.max=36)



# Fit different models for the residual series: AR(1), AR(2), AR(3), ARMA(1,1)
fit.ar1 <- arima(pass.rand, order = c(p = 1, d = 0, q = 0), method = "ML", include.mean = F)
fit.ar1

fit.ar2 <- arima(pass.rand, order = c(p = 2, d = 0, q = 0), method = "ML", include.mean = F)
fit.ar2

fit.ar3 <- arima(pass.rand, order = c(p = 3, d = 0, q = 0), method = "ML", include.mean = F)
fit.ar3

fit.arma11 <- arima(pass.rand, order = c(p = 1, d = 0, q = 1), method = "ML", include.mean = F)
fit.arma11


#Pick AR(2) and check the fitting residuals
par(mfrow = c(1, 2))
res <- fit.ar2$residuals
acf(res, main = "ACF for residuals")
pacf(res, main = "PACF for residuals")

#Diagnosis
par(mfrow = c(1, 1))
tsdiag(fit.ar2)


# check normality
qqnorm(res)
qqline(res)

# predict the ransom part
rand.pred <- predict(fit.ar2, n.ahead = 12)

# predict the trend
pred.time <- seq(1961, by = 1/12, length = 12)
trend.pred <- predict(fit.trend, newdata = data.frame(pass.time = pred.time))

# predict the seasonality
season.pred <- fit.season$fitted[1:12]

# sum up the predictions
pred <- ts(rand.pred$pred + trend.pred+ season.pred, start = c(1961,1), deltat = 1/12)

# number for log prediction
plot(passenger.log, xlab = "Year", ylab = "log(# of Passengers)",
     xlim = c(1949, 1963), ylim = c(4.7, 7.2))
lines(pred, col = 2)


#To forecast the passenger numbers, we take exponential transformation of the log passenger numbers.

plot(passenger, xlab = "Year", ylab = "Number of Passengers",
     xlim = c(1949, 1963), ylim = exp(c(4.7, 6.9)))
lines(exp(pred), col = 2)
