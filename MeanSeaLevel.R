# Mean Sea Level

library(ggplot2)

setwd("~/Desktop/MOODIES")

mean.sea.level <- read.csv("data/MeanSeaLevel.csv")

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
