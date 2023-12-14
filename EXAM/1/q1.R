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

library(TSA)
eacf(series_diff)

# По рандомной pacf и большой acf до 2го порядка включительно можно сделать вывод о том, что модель MA(2), eacf говорит о том же
# (левый верхний угол устойчивой границы на MA_2 и AR_0)