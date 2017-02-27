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
  Acadia.Yearavg[i] <- mean(abs(Acadia.Temp$TAVG[!is.na(Acadia.Temp$TAVG) & (Acadia.Year == Acadia.Yearlist[i])] - Acadia.Normtemp[!is.na(Acadia.Temp$TAVG) & (Acadia.Year == Acadia.Yearlist[i])]))
  Hatteras.Yearavg[i] <- mean(abs(Hatteras.Temp$TAVG[!is.na(Hatteras.Temp$TAVG) & (Hatteras.Year == Hatteras.Yearlist[i])]- Hatteras.Normtemp[!is.na(Hatteras.Temp$TAVG) & (Hatteras.Year == Hatteras.Yearlist[i])]))
  Kenai.Yearavg[i] <- mean(abs(Kenai.Temp$TAVG[!is.na(Kenai.Temp$TAVG) & (Kenai.Year == Kenai.Yearlist[i])]- Kenai.Normtemp[!is.na(Kenai.Temp$TAVG) & (Kenai.Year == Kenai.Yearlist[i])]))
  Olympic.Yearavg[i] <- mean(abs(Olympic.Temp$TAVG[!is.na(Olympic.Temp$TAVG) & (Olympic.Year == Olympic.Yearlist[i])]- Olympic.Normtemp[!is.na(Olympic.Temp$TAVG) & (Olympic.Year == Olympic.Yearlist[i])]))
  Padre.Yearavg[i] <- mean(abs(Padre.Temp$TAVG[!is.na(Padre.Temp$TAVG) & (Padre.Year == Padre.Yearlist[i])]- Padre.Normtemp[!is.na(Padre.Temp$TAVG) & (Padre.Year == Padre.Yearlist[i])]))
}

Acadia.maxtemp <- max(Acadia.AVG$MAX,na.rm = T)
Acadia.mintemp <- min(Acadia.AVG$MIN,na.rm = T)
Hatteras.maxtemp <- max(Hatteras.AVG$MAX,na.rm = T)
Hatteras.mintemp <- min(Hatteras.AVG$MIN,na.rm = T)
Kenai.maxtemp <- max(Kenai.AVG$MAX,na.rm = T)
Kenai.mintemp <- min(Kenai.AVG$MIN,na.rm = T)
Olympic.maxtemp <- max(Olympic.AVG$MAX,na.rm = T)
Olympic.mintemp <- min(Olympic.AVG$MIN,na.rm = T)
Padre.maxtemp <- max(Padre.AVG$MAX,na.rm = T)
Padre.mintemp <- min(Padre.AVG$MIN,na.rm = T)

Acadia.DeltaTnorm <- Acadia.maxtemp - Acadia.mintemp
Hatteras.DeltaTnorm <- Hatteras.maxtemp - Hatteras.mintemp
Kenai.DeltaTnorm <- Kenai.maxtemp - Kenai.mintemp
Olympic.DeltaTnorm <- Olympic.maxtemp - Olympic.mintemp
Padre.DeltaTnorm <- Padre.maxtemp - Padre.mintemp

Acadia.Yearmax <- rep(0,times = length(Acadia.Yearlist))
Hatteras.Yearmax <- rep(0,times = length(Hatteras.Yearlist))
Kenai.Yearmax <- rep(0,times = length(Kenai.Yearlist))
Olympic.Yearmax <- rep(0,times = length(Olympic.Yearlist))
Padre.Yearmax <- rep(0,times = length(Padre.Yearlist))
Acadia.Yearmin <- rep(0,times = length(Acadia.Yearlist))
Hatteras.Yearmin <- rep(0,times = length(Hatteras.Yearlist))
Kenai.Yearmin <- rep(0,times = length(Kenai.Yearlist))
Olympic.Yearmin <- rep(0,times = length(Olympic.Yearlist))
Padre.Yearmin <- rep(0,times = length(Padre.Yearlist))

for (i in 1:length(Acadia.Yearlist))
{
  Acadia.Yearmax[i] <- max(Acadia.Temp$EMXT[(Acadia.Year == Acadia.Yearlist[i])],na.rm=T)
  Hatteras.Yearmax[i] <- max(Hatteras.Temp$EMXT[(Hatteras.Year == Hatteras.Yearlist[i])],na.rm=T)
  Kenai.Yearmax[i] <- max(Kenai.Temp$EMXT[(Kenai.Year == Kenai.Yearlist[i])],na.rm=T)
  Olympic.Yearmax[i] <- max(Olympic.Temp$EMXT[(Olympic.Year == Olympic.Yearlist[i])],na.rm=T)
  Padre.Yearmax[i] <- max(Padre.Temp$EMXT[(Padre.Year == Padre.Yearlist[i])],na.rm=T)
  Acadia.Yearmin[i] <- min(Acadia.Temp$EMNT[(Acadia.Year == Acadia.Yearlist[i])],na.rm=T)
  Hatteras.Yearmin[i] <- min(Hatteras.Temp$EMNT[(Hatteras.Year == Hatteras.Yearlist[i])],na.rm=T)
  Olympic.Yearmin[i] <- min(Olympic.Temp$EMNT[(Olympic.Year == Olympic.Yearlist[i])],na.rm=T)
  Padre.Yearmin[i] <- min(Padre.Temp$EMNT[(Padre.Year == Padre.Yearlist[i])],na.rm=T)
}
for(i in 1:length(Kenai.Yearlist))
{
  Kenai.Yearmax[i] <- max(Kenai.Temp$EMXT[(Kenai.Year == Kenai.Yearlist[i])],na.rm=T)
  Kenai.Yearmin[i] <- min(Kenai.Temp$EMNT[(Kenai.Year == Kenai.Yearlist[i])],na.rm=T)
}

Acadia.Yeardif = (Acadia.Yearmax-Acadia.Yearmin) - Acadia.DeltaTnorm
Hatteras.Yeardif = (Hatteras.Yearmax-Hatteras.Yearmin) - Hatteras.DeltaTnorm
Kenai.Yeardif = (Kenai.Yearmax-Kenai.Yearmin) - Kenai.DeltaTnorm
Olympic.Yeardif = (Olympic.Yearmax-Olympic.Yearmin) - Olympic.DeltaTnorm
Padre.Yeardif = (Padre.Yearmax-Padre.Yearmin) - Padre.DeltaTnorm

qplot(x=Acadia.Yearlist, y=abs(Acadia.Yearavg*Acadia.Yeardif))
qplot(x=Hatteras.Yearlist, y=abs(Hatteras.Yearavg*Hatteras.Yeardif))
qplot(x=Kenai.Yearlist, y=abs(head(Kenai.Yearavg*Kenai.Yeardif,-3)))
qplot(x=Olympic.Yearlist, y=abs(Olympic.Yearavg*Olympic.Yeardif))
qplot(x=Padre.Yearlist, y=abs(Padre.Yearavg*Padre.Yeardif))

TemperatureMetric <- function(t,fulldata,avgdata) {
  month <- round(12*(fulldata$DATE - floor(fulldata$DATE)))
  year = round(floor(fulldata$DATE))
  Normtemp <- avgdata$AVG[month+1]
  Yearlist <- unique(year[!is.na(fulldata$TAVG)])
  Yearavg <- rep(0,times = length(Yearlist))
  for (i in 1:length(Yearlist))
  {
    Yearavg[i] <- mean(abs(fulldata$TAVG[!is.na(fulldata$TAVG) & (year == Yearlist[i])] - Normtemp[!is.na(fulldata$TAVG) & (year == Yearlist[i])]))
  }
  maxtemp <- max(avgdata$MAX,na.rm = T)
  mintemp <- min(avgdata$MIN,na.rm = T)
  DeltaTnorm <- maxtemp - mintemp
  Yearmax <- rep(0,times = length(Yearlist))
  Yearmin <- rep(0,times = length(Yearlist))
  for (i in 1:length(Acadia.Yearlist))
  {
    Yearmax[i] <- max(fulldata$EMXT[(year == Yearlist[i])],na.rm=T)
    Yearmin[i] <- min(fulldata$EMNT[(year == Yearlist[i])],na.rm=T)
  }
  Yeardif = abs((Yearmax-Yearmin) - DeltaTnorm)
  if (length(Yeardif[Yearlist == t]) != 0) {
    if (Yeardif[Yearlist == t][1] == Inf) {
      return (0)
    }
    return(Yeardif[Yearlist == t][1] / 2)
  }
  return(0)
}
  
  