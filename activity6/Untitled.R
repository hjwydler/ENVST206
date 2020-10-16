install.packages(c("sp", "rgdal", "dplyr"))

library(sp)
library(rgdal)
library(dplyr)

g1996 <- readOGR("/Users/ginnytw/Documents/a06/GNPglaciers/GNPglaciers_1966.shp")

plot(g1966, col="skyblue")

g1966@data

g1966@data$GLACNAME

g1966@data$GLACNAME

g1966@data$Area1966

g2015@data$Area2015

plot(g1966, col="skyblue")
plot(g2015, col="tomato3", add=TRUE)

exp1 <- data.frame(NAME=as.factor("a", "b", "c"),
                   per.chagne = c(50,40,65))