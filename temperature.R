#Temperature

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


