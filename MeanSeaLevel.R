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

plot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Acadia)] + mean.sea.level$Month[!is.na(mean.sea.level$Acadia)] / 12,
  y= totalsealevelAcadia
)

nonahatteras = mean.sea.level$Cape.Hatteras[!is.na(mean.sea.level$Cape.Hatteras)]
totalsealevelCape.Hatteras = cumsum(nonahatteras)

plot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Cape.Hatteras)] + mean.sea.level$Month[!is.na(mean.sea.level$Cape.Hatteras)] / 12,
  y= totalsealevelCape.Hatteras
)

nonakenai = mean.sea.level$Kenai.Fjords[!is.na(mean.sea.level$Kenai.Fjords)]
totalsealevelKenai.Fjords = cumsum(nonakenai)

plot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Kenai.Fjords)] + mean.sea.level$Month[!is.na(mean.sea.level$Kenai.Fjords)] / 12,
  y= totalsealevelKenai.Fjords
)

nonaolympic = mean.sea.level$Olympic[!is.na(mean.sea.level$Olympic)]
totalsealevelOlympic= cumsum(nonaolympic)

plot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Olympic)] + mean.sea.level$Month[!is.na(mean.sea.level$Olympic)] / 12,
  y= totalsealevelOlympic
)

nonapadre = mean.sea.level$Padre.Island[!is.na(mean.sea.level$Padre.Island)]
totalsealevelPadre.Island= cumsum(nonapadre)

plot(
  x=mean.sea.level$Year[!is.na(mean.sea.level$Padre.Island)] + mean.sea.level$Month[!is.na(mean.sea.level$Padre.Island)] / 12,
  y= totalsealevelPadre.Island
)