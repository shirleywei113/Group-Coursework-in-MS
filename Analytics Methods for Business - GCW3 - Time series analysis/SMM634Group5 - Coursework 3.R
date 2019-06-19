#################################################
############## Q1 ###############################
#################################################
timesim <- read.table("timesim.dat", header = TRUE)
# plot column 1
t1.timesim <- ts(timesim[, 1])
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(t1.timesim)
acf(t1.timesim, main = "ACF1")
pacf(t1.timesim, main = "PACF1")
# plot column 2
t2.timesim <- ts(timesim[, 2])
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(t2.timesim)
acf(t2.timesim, main = "ACF2")
pacf(t2.timesim, main = "PACF2")
# make it stationary
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(diff(t2.timesim))
acf(diff(t2.timesim), main = "ACF2")
pacf(diff(t2.timesim), main = "PACF2")
# plot column 3
t3.timesim <- ts(timesim[, 3])
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(t3.timesim)
acf(t3.timesim, main = "ACF3")
pacf(t3.timesim, main = "PACF3")
# plot column 4
t4.timesim <- ts(timesim[, 4])
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(t4.timesim)
acf(t4.timesim, main = "ACF4")
pacf(t4.timesim, main = "PACF4")
# make it stationary
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(diff(t4.timesim))
acf(diff(t4.timesim), main = "ACF4")
pacf(diff(t4.timesim), main = "PACF4")

#################################################
############## Q2 ###############################
#################################################
## (a) ##
set.seed(100)
ts.ar1 <- arima.sim(list(ar = c(0.5, -0.4, 0.6)), n = 100)
par(mfrow = c(1, 1))
plot(ts.ar1, main = "simulation of a AR(3) model with length 100", ylab = "value")
## (b) ##
# ACF and PACF of ts.ar1
par(mfrow = c(1, 2))
acf(ts.ar1, main = "Estimated ACF")
pacf(ts.ar1, main = "Estimated PACF")
# true ACF and PACF 
acf.true <- ARMAacf(ar = c(0.5, -0.4, 0.6), lag.max = 20)
pacf.true <- ARMAacf(ar = c(0.5, -0.4, 0.6), pacf = TRUE, lag.max = 20)
par(mfrow = c(1, 2))
plot(acf.true, type = "h", xlab = "Lag", main = "True ACF")
plot(pacf.true, type = "h", xlab = "Lag", main = "True PACF")
## (c) ##
# simulations of AR(3) model with length 50 and 150
set.seed(100)
ts.ar2 <- arima.sim(list(ar = c(0.5, -0.4, 0.6)), n = 50)
set.seed(100)
ts.ar3 <- arima.sim(list(ar = c(0.5, -0.4, 0.6)), n = 150)
# ACF and PACF of ts.ar2, ts.ar3 and the true ACF and PACF
par(mfrow = c(3, 2))
acf(ts.ar2, main = "Estimated ACF of length 50")
pacf(ts.ar2, main = "Estimated PACF of length 50")
acf(ts.ar3, main = "Estimated ACF of length 150")
pacf(ts.ar3, main = "Estimated PACF of length 150")
plot(acf.true, type = "h", xlab = "Lag", main = "True ACF")
plot(pacf.true, type = "h", xlab = "Lag", main = "True PACF")

#################################################
############## Q3 ###############################
#################################################
# read the ARIMAsim2 data
ARIMAsim2 <- read.table("ARIMAsim2.dat", header = TRUE)
# remove the trend of ts1 and ts3 by differencing the data
dARIMAsim2.ts1 = ts(diff(ARIMAsim2[,1]))
ARIMAsim2.ts2 = ts(ARIMAsim2[,2])
dARIMAsim2.ts3 = ts(diff(ARIMAsim2[,3]))
# create autocorrelation and partial autocorrelation plots for each time series
par(mfrow = c(3, 2))
acf(dARIMAsim2.ts1, main = "ACF of differenced ts1", lag.max = 24)
pacf(dARIMAsim2.ts1, main = "PACF of differenced ts1", lag.max = 24)
acf(ARIMAsim2.ts2, main = "ACF of ts2", lag.max = 24)
pacf(ARIMAsim2.ts2, main = "PACF of ts2", lag.max = 24)
acf(dARIMAsim2.ts3, main = "ACF of differenced ts3", lag.max = 24)
pacf(dARIMAsim2.ts3, main = "PACF of differenced ts3", lag.max = 24)
######## fit the model for ts1
m1.ts1 = arima(dARIMAsim2.ts1, order = c(1,0,3), include.mean = FALSE) 
m2.ts1 = arima(dARIMAsim2.ts1, order = c(3,0,1), include.mean = FALSE)
m3.ts1 = arima(dARIMAsim2.ts1, order = c(2,0,1), include.mean = FALSE)
# choose the best model with the lowest AIC——m1.ts1
AIC(m1.ts1, m2.ts1, m3.ts1)
# produce the time series of residuals for m1.ts1 model
par(mfrow = c(2, 2))
plot(resid(m1.ts1))
qqnorm(resid(m1.ts1)); qqline(resid(m1.ts1))
acf(resid(m1.ts1)); pacf(resid(m1.ts1))
# use Ljung-Box test to test serial correlation of residuals
Box.test(resid(m1.ts1), type = "Ljung-Box", lag = 1)
Box.test(resid(m1.ts1), type = "Ljung-Box", lag = 10)
######### fit the model for ts2
m1.ts2 = arima(ARIMAsim2.ts2, order = c(2,0,1), include.mean = FALSE) 
m2.ts2 = arima(ARIMAsim2.ts2, order = c(1,0,2), include.mean = FALSE) 
m3.ts2 = arima(ARIMAsim2.ts2, order = c(2,0,2), include.mean = FALSE)
## choose the best model with the lowest AIC——m2.ts2
AIC(m1.ts2, m2.ts2, m3.ts2)
# produce the time series of residuals for m2.ts2 model
par(mfrow = c(2, 2))
plot(resid(m2.ts2))
qqnorm(resid(m2.ts2)); qqline(resid(m2.ts2))
acf(resid(m2.ts2)); pacf(resid(m2.ts2))
# use Ljung-Box test to test serial correlation of residuals
Box.test(resid(m2.ts2), type = "Ljung-Box", lag = 1)
Box.test(resid(m2.ts2), type = "Ljung-Box", lag = 10)
######### fit the model for ts3
m1.ts3 = arima(dARIMAsim2.ts3, order = c(2,0,0), include.mean = FALSE)
m2.ts3 = arima(dARIMAsim2.ts3, order = c(0,0,2), include.mean = FALSE)
# choose the best model with the lowest AIC——m1.ts3
AIC(m1.ts3, m2.ts3)
# produce the time series of residuals for m2.ts3 model
par(mfrow = c(2, 2))
plot(resid(m1.ts3))
qqnorm(resid(m1.ts3)); qqline(resid(m1.ts3))
acf(resid(m1.ts3)); pacf(resid(m1.ts3))
# use Box-Pierce test to test serial correlation of residuals
Box.test(resid(m1.ts3), type = "Box-Pierce", lag = 1)
Box.test(resid(m1.ts3), type = "Box-Pierce", lag = 10)

#################################################
############## Q4 ###############################
#################################################
## (a) ##
set.seed(100)
X <- rnorm(1000, mean = 2, sd = sqrt(2))
Y <- X - 2*X^2 + X^3 + rnorm(length(X), mean = 0, sd = 1) 
simulation <- as.data.frame(cbind(X, Y), row.names = 1:1000)
head(simulation)
tail(simulation)
## (b) ##
library(boot)
# LOOCV error of model i
glm.fit1 <- glm(Y ~ X, data = simulation)
cv.err1 <- cv.glm(simulation, glm.fit1)
cv.err1$delta
# LOOCV error of model ii
glm.fit2 <- glm(Y ~ poly(X, 3), data = simulation)
cv.err2 <- cv.glm(simulation, glm.fit2)
cv.err2$delta
## (c) ##
CV.fun <- function(n, data, model) {
  CV <- rep(0, n)
  for (i in 1:n) {
    y_i <- data[i, 2]
    y_i.head <- model$fitted.values[i]
    h_ii <- hatvalues(model)[i]
    CV[i] <- ((y_i - y_i.head) / (1 - h_ii))^2
  }
  CV.err <- sum(CV) / n
}
## (d) ##
cv.err1.fun <- CV.fun(n = 1000, data = simulation, model = glm.fit1)
cv.err1.fun
cv.err2.fun <- CV.fun(n = 1000, data = simulation, model = glm.fit2)
cv.err2.fun