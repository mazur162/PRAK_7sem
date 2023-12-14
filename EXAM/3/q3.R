data <- read.csv(file = file.choose(), header = TRUE)
head(data)
names(data) <- c('num', 'val')
series <- data$val
plot(series, type = "l", col = "blue",lwd = 2,main = "time_series")
series_diff = diff(series)
plot(series_diff, type = "l", col = "blue",lwd = 2,main = "time_series_diff")
#n <- length(series) # другой вариант
#series_diff = series[2:n] / series[1:(n-1)] - 1
#plot(series_diff, type = "l", col = "blue",lwd = 2,main = "time_series_diff")
series_acf = acf(series_diff, lwd = 3, main = "ACF", col = "blue", lag.max=100)
series_pacf = pacf(series_diff, lwd = 3, main = "PACF", col = "blue", lag.max=100)

# По acf и pacf видно, что MA = 1, SMA = 1, SAR = 2, период = 12, AR - непонятно, прогоним от 0 до 4 по критерию акаике
for (p in 0:4)
{
  tryCatch({
    model <- arima(series_diff,order = c(p,0,1),seasonal = list(order= c(2,0,1),period = 12),method = "ML")
    print(paste(p,model$aic))
  },
  error=function(error_message) {
  }
  )
}

# AR = 3

model <- arima(series_diff,order = c(3,0,1),seasonal = list(order= c(2,0,1),period = 12),method = "ML")

# acf остатков, тесты и QQ plot
acf(model$residuals, lwd = 3, main = "Residuals ACF", col = "blue", lag.max=100)

library(lmtest)
coeftest(model)

Box.test(model$residuals, lag = 20, type = "Ljung-Box", fitdf = 2) # Насчет степеней свободы ваще хз

qqnorm(model$residuals)
qqline(model$residuals)

# acf остатков почти не выходит за дов-ый интервал, тест Льюинг-Бокса гипотезу не отвергает, квантили остатков почти лежат на прямой
# модель, вероятно, адекватна

k3 <- kernel('daniell',c(5,5,5))
sp = spec(series_diff, kernel=k3,log='no',sub='', xlab='Frequency', col = "blue", lwd = 2, main='Smoothed Periodogram')
sp_ARMA = spec(model$residuals, kernel=k3,log='no',sub='', xlab='Frequency', col = "green", lwd = 2, main='Smoothed Periodogram', plot=F)
lines(sp_ARMA$freq,sp_ARMA$spec,col='green',lwd=2)
legend("topleft",c("Original data","Model resid."),lwd = 2, col=c("blue","green"))

# Периодограмма остатков лежит вдоль прямой и близка к ней, значит похожа на белый шум