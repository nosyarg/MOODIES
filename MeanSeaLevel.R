# Mean Sea Level

library(ggplot2)

setwd("~/Desktop/MOODIES")

mean.sea.level <- read.csv("data/MeanSeaLevel.csv")

totalsealevelAcadia = cumsum(y=mean.sea.level$Acadia)

qplot(
  x=mean.sea.level$Year + mean.sea.level$Month / 12,
  y=mean.sea.level$Acadia
)

qplot(
  x=mean.sea.level$Year + mean.sea.level$Month / 12,
  y=mean.sea.level$Cape.Hatteras
)

qplot(
  x=mean.sea.level$Year + mean.sea.level$Month / 12,
  y=mean.sea.level$Kenai.Fjords
)

qplot(
  x=mean.sea.level$Year + mean.sea.level$Month / 12,
  y=mean.sea.level$Olympic
)

qplot(
  x=mean.sea.level$Year + mean.sea.level$Month / 12,
  y=mean.sea.level$Padre.Island
)

nonaacadia = mean.sea.level$Acadia[!is.na(mean.sea.level$Acadia)]
totalsealevelAcadia = cumsum(nonaacadia)

qplot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Acadia)] + mean.sea.level$Month[!is.na(mean.sea.level$Acadia)] / 12,
  y= totalsealevelAcadia,
  xlab="Time",
  ylab="Cumulative MSL",
  main="Acadia Cumulative MSL Function"
) + geom_line()

acadia <- data.frame(
  totalsealevelAcadia, 
  mean.sea.level$Year[!is.na(mean.sea.level$Acadia)] + mean.sea.level$Month[!is.na(mean.sea.level$Acadia)] / 12 - 1997,
  (mean.sea.level$Year[!is.na(mean.sea.level$Acadia)] + mean.sea.level$Month[!is.na(mean.sea.level$Acadia)] / 12 - 1997)^2,
  (mean.sea.level$Year[!is.na(mean.sea.level$Acadia)] + mean.sea.level$Month[!is.na(mean.sea.level$Acadia)] / 12 - 1997)^3,
  (mean.sea.level$Year[!is.na(mean.sea.level$Acadia)] + mean.sea.level$Month[!is.na(mean.sea.level$Acadia)] / 12 - 1997)^4,
  sin(mean.sea.level$Year[!is.na(mean.sea.level$Acadia)] + mean.sea.level$Month[!is.na(mean.sea.level$Acadia)] / 12 - 1997))
names(acadia) <- c("Acadia", "Time", "Time2", "Time3", "Time4", "sinTime")
acadia.fit <- glm(Acadia~Time+Time2+Time3+sinTime, data=acadia[1:157,])
summary(acadia.fit)
                    
nonahatteras = mean.sea.level$Cape.Hatteras[!is.na(mean.sea.level$Cape.Hatteras)]
totalsealevelCape.Hatteras = cumsum(nonahatteras)

qplot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Cape.Hatteras)] + mean.sea.level$Month[!is.na(mean.sea.level$Cape.Hatteras)] / 12,
  y= totalsealevelCape.Hatteras,
  xlab="Time",
  ylab="Cumulative MSL",
  main="Cape Hatteras Cumulative MSL Function"
) + geom_line()

hatteras <- data.frame(
  totalsealevelCape.Hatteras, 
  mean.sea.level$Year[!is.na(mean.sea.level$Cape.Hatteras)] + mean.sea.level$Month[!is.na(mean.sea.level$Cape.Hatteras)] / 12 - 1997,
  (mean.sea.level$Year[!is.na(mean.sea.level$Cape.Hatteras)] + mean.sea.level$Month[!is.na(mean.sea.level$Cape.Hatteras)] / 12 - 1997)^2,
  (mean.sea.level$Year[!is.na(mean.sea.level$Cape.Hatteras)] + mean.sea.level$Month[!is.na(mean.sea.level$Cape.Hatteras)] / 12 - 1997)^3
)
names(hatteras) <- c("Hatteras", "Time", "Time2", "Time3")
hatteras.fit <- glm(Hatteras~Time+Time2+Time3, data=hatteras)
summary(hatteras.fit)

nonakenai = mean.sea.level$Kenai.Fjords[!is.na(mean.sea.level$Kenai.Fjords)]
totalsealevelKenai.Fjords = cumsum(nonakenai)

qplot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Kenai.Fjords)] + mean.sea.level$Month[!is.na(mean.sea.level$Kenai.Fjords)] / 12,
  y= totalsealevelKenai.Fjords,
  xlab="Time",
  ylab="Cumulative MSL",
  main="Kenai Fjords Cumulative MSL Function"
) + geom_line()

kenai <- data.frame(
  totalsealevelKenai.Fjords, 
  mean.sea.level$Year[!is.na(mean.sea.level$Kenai.Fjords)] + mean.sea.level$Month[!is.na(mean.sea.level$Kenai.Fjords)] / 12 - 1997, 
  (mean.sea.level$Year[!is.na(mean.sea.level$Kenai.Fjords)] + mean.sea.level$Month[!is.na(mean.sea.level$Kenai.Fjords)] / 12 - 1997)^2,
  (mean.sea.level$Year[!is.na(mean.sea.level$Kenai.Fjords)] + mean.sea.level$Month[!is.na(mean.sea.level$Kenai.Fjords)] / 12 - 1997)^3)
names(kenai) <- c("Kenai", "Time", "Time2", "Time3")
kenai.fit <- lm(Kenai~Time+Time2+Time3, data=kenai)
summary(kenai.fit)

nonaolympic = mean.sea.level$Olympic[!is.na(mean.sea.level$Olympic)]
totalsealevelOlympic= cumsum(nonaolympic)

qplot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Olympic)] + mean.sea.level$Month[!is.na(mean.sea.level$Olympic)] / 12,
  y= totalsealevelOlympic,
  xlab="Time",
  ylab="Cumulative MSL",
  main="Olympic Cumulative MSL Function"
) + geom_line()

olympic <- data.frame(
  totalsealevelOlympic, 
  sin(mean.sea.level$Year[!is.na(mean.sea.level$Olympic)] + mean.sea.level$Month[!is.na(mean.sea.level$Olympic)] / 12 - 1997),
  mean.sea.level$Year[!is.na(mean.sea.level$Olympic)] + mean.sea.level$Month[!is.na(mean.sea.level$Olympic)] / 12 - 1997,
  (mean.sea.level$Year[!is.na(mean.sea.level$Olympic)] + mean.sea.level$Month[!is.na(mean.sea.level$Olympic)] / 12 - 1997)^2,
  (mean.sea.level$Year[!is.na(mean.sea.level$Olympic)] + mean.sea.level$Month[!is.na(mean.sea.level$Olympic)] / 12 - 1997)^3)
names(olympic) <- c("Olympic", "sinTime", "Time", "Time2", "Time3")
olympic.fit <- glm(Olympic~Time+Time2+Time3, data=olympic)
summary(olympic.fit)

nonapadre = mean.sea.level$Padre.Island[!is.na(mean.sea.level$Padre.Island)]
totalsealevelPadre.Island= cumsum(nonapadre)

qplot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12,
  y= totalsealevelPadre.Island,
  xlab="Time",
  ylab="Cumulative MSL",
  main="Padre Island Cumulative MSL Function"
) + geom_line()

padre <- data.frame(
  totalsealevelPadre.Island, 
  mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12 - 1997, 
  sin(mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12 - 1997),
  (mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12 - 1997)^2,
  (mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12 - 1997)^3,
  (mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12 - 1997)^4,
  (mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12 - 1997)^5
)
names(padre) <- c("Padre", "Time", "sinTime", "Time2", "Time3", "Time4", "Time5")
padre.fit <- glm(Padre~Time+Time2, data=padre)
summary(padre.fit)
