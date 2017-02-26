setwd("~/Desktop/MOODIES/")
source("temperature.R")
setwd("~/Desktop/MOODIES/")
source("wildfire.R")
setwd("~/Desktop/MOODIES/")
source("hurricane.R")
setwd("~/Desktop/MOODIES/")
source("airquality.R")

Metric <- function(t) {
  
  return(TemperatureMetric(t)+FireMetric(t, fire.acadia)+HurricaneMetric(t, hurricane.acadia)+AirQualityMetric(t, air.acadia))
}

MetricPlot <- function() {
  
  metric <- c()
  fire <- c()
  hurricane <- c()
  air <- c()
  for (i in 1996:2016) {
    metric <- c(metric, Metric(i))
    fire <- c(fire, FireMetric(i, fire.acadia))
    hurricane <- c(hurricane, HurricaneMetric(i, hurricane.acadia))
    air <- c(air, AirQualityMetric(i, air.acadia))
  }
  qplot(x=1996:2016,y=,metric) + geom_line() + 
    geom_point(aes(x=1996:2016,y=fire)) + geom_line(aes(x=1996:2016,y=fire), col="red") +
    geom_point(aes(x=1996:2016,y=hurricane)) + geom_line(aes(x=1996:2016,y=hurricane), col="blue") + 
    geom_point(aes(x=1996:2016, y=air)) + geom_line(aes(x=1996:2016,y=air), col="green")
}