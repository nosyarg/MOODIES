#Temperature
rm(list = ls())
library(ggplot2)

setwd("~/Desktop/MOODIES/data/alltemps")

Acadia.Temp <- read.csv("ACADIA.csv")
Hatteras.Temp <- read.csv("HATTERAS.csv")
Kenai.Temp <- read.csv("KENAI.csv")
Olympic.Temp <- read.csv("OLYMPIC.csv")
Padre.Temp <- read.csv("PADRE.csv")

Acadia.Month = round(12*(Acadia.Temp$DATE - floor(Acadia.Temp$DATE)))
Hatteras.Month = round(12*(Hatteras.Temp$DATE - floor(Hatteras.Temp$DATE)))
Kenai.Month = round(12*(Kenai.Temp$DATE - floor(Kenai.Temp$DATE)))
Olympic.Month = round(12*(Olympic.Temp$DATE - floor(Olympic.Temp$DATE)))
Padre.Month = round(12*(Padre.Temp$DATE - floor(Padre.Temp$DATE)))

Acadia.Year = round(floor(Acadia.Temp$DATE))
Hatteras.Year = round(floor(Hatteras.Temp$DATE))
Kenai.Year = round(floor(Kenai.Temp$DATE))
Olympic.Year = round(floor(Olympic.Temp$DATE))
Padre.Year = round(floor(Padre.Temp$DATE))

Acadia.AVG <- read.csv("ACADIAAVG.csv")
Hatteras.AVG <- read.csv("HATTERASAVG.csv")
Kenai.AVG <- read.csv("KENAIAVG.csv")
Olympic.AVG <- read.csv("OLYMPICAVG.csv")
Padre.AVG <- read.csv("PADREAVG.csv")

Acadia.Normtemp <- Acadia.AVG$AVG[Acadia.Month+1]
Hatteras.Normtemp <- Hatteras.AVG$AVG[Hatteras.Month+1]
Kenai.Normtemp <- Kenai.AVG$AVG[Kenai.Month+1]
Olympic.Normtemp <- Olympic.AVG$AVG[Olympic.Month+1]
Padre.Normtemp <- Padre.AVG$AVG[Padre.Month+1]

plot(x = Kenai.Temp$DATE[!is.na(Kenai.Temp$TAVG)], y= Kenai.Temp$TAVG[!is.na(Kenai.Temp$TAVG)]-Kenai.Normtemp[!is.na(Kenai.Temp$TAVG)])
plot(x = Padre.Temp$DATE[!is.na(Padre.Temp$TAVG)], y= Padre.Temp$TAVG[!is.na(Padre.Temp$TAVG)]-Padre.Normtemp[!is.na(Padre.Temp$TAVG)])
plot(x = Acadia.Temp$DATE[!is.na(Acadia.Temp$TAVG)], y= Acadia.Temp$TAVG[!is.na(Acadia.Temp$TAVG)]-Acadia.Normtemp[!is.na(Acadia.Temp$TAVG)])
plot(x = Olympic.Temp$DATE[!is.na(Olympic.Temp$TAVG)], y= Olympic.Temp$TAVG[!is.na(Olympic.Temp$TAVG)]-Olympic.Normtemp[!is.na(Olympic.Temp$TAVG)])
plot(x = Hatteras.Temp$DATE[!is.na(Hatteras.Temp$TAVG)], y= Hatteras.Temp$TAVG[!is.na(Hatteras.Temp$TAVG)]-Hatteras.Normtemp[!is.na(Hatteras.Temp$TAVG)])

Acadia.Yearlist <- unique(Acadia.Year[!is.na(Acadia.Temp$TAVG)])
Hatteras.Yearlist <- unique(Hatteras.Year[!is.na(Hatteras.Temp$TAVG)])
Kenai.Yearlist <- unique(Kenai.Year[!is.na(Kenai.Temp$TAVG)])
Olympic.Yearlist <- unique(Olympic.Year[!is.na(Olympic.Temp$TAVG)])
Padre.Yearlist <- unique(Padre.Year[!is.na(Padre.Temp$TAVG)])

Acadia.Yearavg <- rep(0,times = length(Acadia.Yearlist))
Hatteras.Yearavg <- rep(0,times = length(Hatteras.Yearlist))
Kenai.Yearavg <- rep(0,times = length(Kenai.Yearlist))
Olympic.Yearavg <- rep(0,times = length(Olympic.Yearlist))
Padre.Yearavg <- rep(0,times = length(Padre.Yearlist))

for (i in 1:length(Acadia.Yearlist))
{
  Acadia.Yearavg[i] <- mean(Acadia.Temp$TAVG[!is.na(Acadia.Temp$TAVG) & (Acadia.Year == Acadia.Yearlist[i])] - Acadia.Normtemp[!is.na(Acadia.Temp$TAVG) & (Acadia.Year == Acadia.Yearlist[i])])
  Hatteras.Yearavg[i] <- mean(Hatteras.Temp$TAVG[!is.na(Hatteras.Temp$TAVG) & (Hatteras.Year == Hatteras.Yearlist[i])]- Hatteras.Normtemp[!is.na(Hatteras.Temp$TAVG) & (Hatteras.Year == Hatteras.Yearlist[i])])
  Kenai.Yearavg[i] <- mean(Kenai.Temp$TAVG[!is.na(Kenai.Temp$TAVG) & (Kenai.Year == Kenai.Yearlist[i])]- Kenai.Normtemp[!is.na(Kenai.Temp$TAVG) & (Kenai.Year == Kenai.Yearlist[i])])
  Olympic.Yearavg[i] <- mean(Olympic.Temp$TAVG[!is.na(Olympic.Temp$TAVG) & (Olympic.Year == Olympic.Yearlist[i])]- Olympic.Normtemp[!is.na(Olympic.Temp$TAVG) & (Olympic.Year == Olympic.Yearlist[i])])
  Padre.Yearavg[i] <- mean(Padre.Temp$TAVG[!is.na(Padre.Temp$TAVG) & (Padre.Year == Padre.Yearlist[i])]- Padre.Normtemp[!is.na(Padre.Temp$TAVG) & (Padre.Year == Padre.Yearlist[i])])
}
plot(x=Acadia.Yearlist, y=Acadia.Yearavg)
plot(x=Hatteras.Yearlist, y=Hatteras.Yearavg)
plot(x=Kenai.Yearlist, y=Kenai.Yearavg[!is.na(Kenai.Yearavg)])
plot(x=Olympic.Yearlist, y=Olympic.Yearavg)
plot(x=Padre.Yearlist, y=Padre.Yearavg)

TemperatureMetric <- function(t) {
  
  return(23042094);
}
