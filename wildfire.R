library(ggplot2)

setwd("~/Desktop/MOODIES/")

fire.acadia <- read.csv("data/allfires/ACADIA.csv")
fire.hatteras <- read.csv("data/allfires/HATTERAS.csv")
fire.olympic <- read.csv("data/allfires/OLYMPIC.csv")
fire.padre <- read.csv("data/allfires/PADRE.csv")[1:41,]

FireMetric <- function(t, fire.data) {
  
  D <- 0
  for (i in 1:nrow(fire.data)) {
    if (fire.data[i,"CalendarYear"] == t) {
      D <- D + fire.data[i,"Statistics.1.Acres"]
    }
  }
  return(sqrt(D))
}

FirePlot <- function(fire.data) {
  
  fire.d <- c()
  for (i in 1996:2016) {
    fire.d <- c(fire.d, FireMetric(i, fire.data))
  }
  qplot(x=1996:2016,y=fire.d) + geom_line()
}