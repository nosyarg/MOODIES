library(ggplot2)

rm(list=ls())
setwd("~/Desktop/MOODIES/")

air.acadia <- read.csv("data/airquality/airquality.acadia.csv")
air.hatteras <- read.csv("data/airquality/airquality.hatteras.csv")
air.kenai <- read.csv("data/airquality/airquality.kenai.csv")
air.olympic <- read.csv("data/airquality/airquality.olympic.csv")
air.padre <- read.csv("data/airquality/airquality.padre.csv")

AirQualityMetric <- function(t, air.data) {
  
  range <- (1+12*(t-1996)):(12+12*(t-1996))
  a_avg <- mean(air.data$average.AQI.value[range], na.rm=TRUE)
  a_max <- max(air.data$max.AQI[range], na.rm=TRUE)
  a_min <- min(air.data$min.AQI[range], na.rm=TRUE)
  return(a_avg * (a_max - a_min))
}

AirQualityPlot <- function(air.source) {

  air.data = c()
  for (i in 1996:2016) {
    air.data = c(air.data, AirQualityMetric(i, air.source))
  }
  qplot(x=1996:2016,y=air.data) + geom_line()
}