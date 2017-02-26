library(ggplot2)

setwd("~/Desktop/MOODIES/")

map <- c("ET"=15, "TD"=15, "TS"=56, "H1"=84.5, "H2"=103, "H3"=120, "H4"=143, "H5"=179)

hurricane.acadia <- read.csv("data/hurricanes/hurricane.acadia.csv")
hurricane.acadia$Storm.category <- map[as.character(hurricane.acadia$Storm.category)]

hurricane.hatteras <- read.csv("data/hurricanes/hurricane.hatteras.csv")
hurricane.hatteras$Storm.category <- map[as.character(hurricane.hatteras$Storm.category)]

hurricane.padre <- read.csv("data/hurricanes/hurricane.padre.csv")
hurricane.padre$Storm.category <- map[as.character(hurricane.padre$Storm.category)]

HurricaneMetric <- function(t, hurricane.data) {
  
  H <- 0
  for (i in 1:nrow(hurricane.data)) {
    if (hurricane.data[i,"Year"] == t) {
      H <- H + exp(hurricane.data[i,"Storm.category"] / 100)
    }
  }
  return(H)
}

HurricanePlot <- function(hurricane.data) {
  
  hurricane.d <- c()
  for (i in 1996:2016) {
    hurricane.d <- c(hurricane.d, HurricaneMetric(i, hurricane.data))
  }
  qplot(x=1996:2016,y=hurricane.d) + geom_line()
}