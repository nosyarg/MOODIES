setwd("~/Desktop/MOODIES/")
source("temperature.R")
setwd("~/Desktop/MOODIES/")
source("wildfire.R")
setwd("~/Desktop/MOODIES/")
source("hurricane.R")
setwd("~/Desktop/MOODIES/")
source("airquality.R")

# Padre - Dead
# Olympic - Mid
# Kenai - Good
# Cape Hatteras - Good
# Acadia - Good

Metric.Acadia <- function(t) {
  
  return(TemperatureMetric(t,Acadia.Temp,Acadia.AVG)+
           FireMetric(t, fire.acadia)+
           HurricaneMetric(t, hurricane.acadia)+
           AirQualityMetric(t, air.acadia))
}

Metric.Hatteras <- function(t) {
  
  return(TemperatureMetric(t,Hatteras.Temp,Hatteras.AVG)+
           FireMetric(t, fire.hatteras)+
           HurricaneMetric(t, hurricane.hatteras)+
           AirQualityMetric(t, air.hatteras))
}

Metric.Kenai <- function(t) {
  
  return(TemperatureMetric(t,Kenai.Temp,Kenai.AVG)+
           #FireMetric(t, fire.kenai)+
           #HurricaneMetric(t, hurricane.kenai)+
           AirQualityMetric(t, air.kenai))
}

Metric.Olympic <- function(t) {
  
  return(TemperatureMetric(t,Olympic.Temp,Olympic.AVG)+
           FireMetric(t, fire.olympic)+
           #HurricaneMetric(t, hurricane.kenai)+
           AirQualityMetric(t, air.olympic))
}

Metric.Padre <- function(t) {
  
  return(TemperatureMetric(t,Padre.Temp,Padre.AVG)+
           FireMetric(t, fire.padre)+
           HurricaneMetric(t, hurricane.padre)+
           AirQualityMetric(t, air.padre))
}

MetricPlot.Acadia <- function() {
  
  metric <- c()
  fire <- c()
  hurricane <- c()
  air <- c()
  temp <- c()
  for (i in 1996:2016) {
    metric <- c(metric, Metric.Acadia(i))
    fire <- c(fire, FireMetric(i, fire.acadia))
    hurricane <- c(hurricane, HurricaneMetric(i, hurricane.acadia))
    air <- c(air, AirQualityMetric(i, air.acadia))
    temp <- c(temp, TemperatureMetric(i,Acadia.Temp,Acadia.AVG))
  }
  qplot(x=1996:2016,y=,metric) + geom_line() + 
    geom_point(aes(x=1996:2016,y=fire)) + geom_line(aes(x=1996:2016,y=fire), col="red") +
    geom_point(aes(x=1996:2016,y=hurricane)) + geom_line(aes(x=1996:2016,y=hurricane), col="blue") + 
    geom_point(aes(x=1996:2016,y=air)) + geom_line(aes(x=1996:2016,y=air), col="green") +
    geom_point(aes(x=1996:2016,y=temp)) + geom_line(aes(x=1996:2016,y=temp), col="orange")
}

MetricPlot.Hatteras <- function() {
  
  metric <- c()
  fire <- c()
  hurricane <- c()
  air <- c()
  temp <- c()
  for (i in 1996:2016) {
    metric <- c(metric, Metric.Hatteras(i))
    fire <- c(fire, FireMetric(i, fire.hatteras))
    hurricane <- c(hurricane, HurricaneMetric(i, hurricane.hatteras))
    air <- c(air, AirQualityMetric(i, air.hatteras))
    temp <- c(temp, TemperatureMetric(i,Hatteras.Temp,Hatteras.AVG))
  }
  qplot(x=1996:2016,y=,metric) + geom_line() + 
    geom_point(aes(x=1996:2016,y=fire)) + geom_line(aes(x=1996:2016,y=fire), col="red") +
    geom_point(aes(x=1996:2016,y=hurricane)) + geom_line(aes(x=1996:2016,y=hurricane), col="blue") + 
    geom_point(aes(x=1996:2016,y=air)) + geom_line(aes(x=1996:2016,y=air), col="green") +
    geom_point(aes(x=1996:2016,y=temp)) + geom_line(aes(x=1996:2016,y=temp), col="orange")
}

MetricPlot.Kenai <- function() {
  
  metric <- c()
  fire <- c()
  hurricane <- c()
  air <- c()
  temp <- c()
  for (i in 1996:2016) {
    metric <- c(metric, Metric.Kenai(i))
    #fire <- c(fire, FireMetric(i, fire.kenai))
    #hurricane <- c(hurricane, HurricaneMetric(i, hurricane.kenai))
    air <- c(air, AirQualityMetric(i, air.kenai))
    temp <- c(temp, TemperatureMetric(i,Kenai.Temp,Kenai.AVG))
  }
  qplot(x=1996:2016,y=metric) + geom_line() + 
    #geom_point(aes(x=1996:2016,y=fire)) + geom_line(aes(x=1996:2016,y=fire), col="red") +
    #geom_point(aes(x=1996:2016,y=hurricane)) + geom_line(aes(x=1996:2016,y=hurricane), col="blue") + 
    geom_point(aes(x=1996:2016,y=air)) + geom_line(aes(x=1996:2016,y=air), col="green") +
    geom_point(aes(x=1996:2016,y=temp)) + geom_line(aes(x=1996:2016,y=temp), col="orange")
}

MetricPlot.Olympic <- function() {
  
  metric <- c()
  fire <- c()
  hurricane <- c()
  air <- c()
  temp <- c()
  for (i in 1996:2016) {
    metric <- c(metric, Metric.Olympic(i))
    fire <- c(fire, FireMetric(i, fire.olympic))
    #hurricane <- c(hurricane, HurricaneMetric(i, hurricane.kenai))
    air <- c(air, AirQualityMetric(i, air.olympic))
    temp <- c(temp, TemperatureMetric(i,Olympic.Temp,Olympic.AVG))
  }
  qplot(x=1996:2016,y=metric) + geom_line() + 
    geom_point(aes(x=1996:2016,y=fire)) + geom_line(aes(x=1996:2016,y=fire), col="red") +
    #geom_point(aes(x=1996:2016,y=hurricane)) + geom_line(aes(x=1996:2016,y=hurricane), col="blue") + 
    geom_point(aes(x=1996:2016,y=air)) + geom_line(aes(x=1996:2016,y=air), col="green") +
    geom_point(aes(x=1996:2016,y=temp)) + geom_line(aes(x=1996:2016,y=temp), col="orange")
}

MetricPlot.Padre <- function() {
  
  metric <- c()
  fire <- c()
  hurricane <- c()
  air <- c()
  temp <- c()
  for (i in 1996:2016) {
    metric <- c(metric, Metric.Padre(i))
    fire <- c(fire, FireMetric(i, fire.padre))
    hurricane <- c(hurricane, HurricaneMetric(i, hurricane.padre))
    air <- c(air, AirQualityMetric(i, air.padre))
    temp <- c(temp, TemperatureMetric(i,Padre.Temp,Padre.AVG))
  }
  qplot(x=1996:2016,y=metric,main="Padre Island National Seashore", xlab="Year", ylab="Metric") + geom_line() + 
    geom_point(aes(x=1996:2016,y=fire)) + geom_line(aes(x=1996:2016,y=fire), col="red") +
    geom_point(aes(x=1996:2016,y=hurricane)) + geom_line(aes(x=1996:2016,y=hurricane), col="blue") + 
    geom_point(aes(x=1996:2016,y=air)) + geom_line(aes(x=1996:2016,y=air), col="green") +
    geom_point(aes(x=1996:2016,y=temp)) + geom_line(aes(x=1996:2016,y=temp), col="orange")
}

metric <- c()
for (i in 1996:2016) {
  metric <- c(metric, Metric.Acadia(i))
}
acadia.frame <- data.frame(
  0:20,
  sin(0:20),
  metric
)
names(acadia.frame) <- c("Year", "sinYear", "Metric")
acadia.fit <- glm(formula=Metric~Year+sinYear, data=acadia.frame[1:13,])
summary(acadia.fit)

metric <- c()
for (i in 1996:2016) {
  metric <- c(metric, Metric.Hatteras(i))
}
hatteras.frame <- data.frame(
  0:20,
  sin(0:20),
  metric
)
names(hatteras.frame) <- c("Year", "sinYear", "Metric")
hatteras.fit <- glm(formula=Metric~Year+sinYear, data=hatteras.frame)
summary(hatteras.fit)

metric <- c()
for (i in 1996:2016) {
  metric <- c(metric, Metric.Kenai(i))
}
kenai.frame <- data.frame(
  0:20,
  sin(0:20),
  metric
)
names(kenai.frame) <- c("Year", "sinYear", "Metric")
kenai.fit <- glm(formula=Metric~Year+sinYear, data=kenai.frame[1:15,])
summary(kenai.fit)

metric <- c()
for (i in 1996:2016) {
  metric <- c(metric, Metric.Olympic(i))
}
olympic.frame <- data.frame(
  0:20,
  sin(0:20),
  metric
)
names(olympic.frame) <- c("Year", "sinYear", "Metric")
olympic.fit <- glm(formula=Metric~Year+sinYear, data=olympic.frame)
summary(olympic.fit)

metric <- c()
for (i in 1996:2016) {
  metric <- c(metric, Metric.Padre(i))
}
padre.frame <- data.frame(
  0:20,
  sin(0:20),
  metric
)
names(padre.frame) <- c("Year", "sinYear", "Metric")
padre.fit <- glm(formula=Metric~Year+sinYear, data=padre.frame)
summary(padre.fit)
