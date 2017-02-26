rm(list=ls())
setwd("~/Desktop/MOODIES/")
air.quality <- read.csv("data/airquality/airquality.csv")

AirQualityMetric <- function(t) {
  
  range <- (1+12*(t-1997)):(12+12*(t-1997))
  a_avg <- mean(air.quality$average.AQI.value[range], na.rm=TRUE)
  a_max <- max(air.quality$max.AQI[range], na.rm=TRUE)
  a_min <- min(air.quality$min.AQI[range], na.rm=TRUE)
  return(a_avg * (a_max - a_min))
}