install.packages(c("sp", "rgdal", "dplyr"))

#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)
#ggplot
library(ggplot2)

#read in shapefiles, read OGR in rgdal does thihs
#glaciers in 1966
g1966 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a06/GNPglaciers/GNPglaciers_1966.shp")



g2015 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a06/GNPglaciers/GNPglaciers_2015.shp")

#lets investigate the format of this data
str(g2015) 

#map the glaciers filling in the polygons with light blue and making the borders grey
plot(g1966, col = "lightblue2", border="grey50")

#data stores all accompanying info/measurements for each spatial object
#preview the first 6 lines and columns of data table using head fxn
head(g2015@data)

g1966@proj4string
help("proj4string")

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#lets combine area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name

gAll <- full_join(gdf66,gdf15, by="GLACNAME")
gAll
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

                  ####QUESTION 7####
#scatterplot of glacier area in 1966 vs % change in area

gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

plot(gAll$area66, gAll$gdiff, 
     pch = 19, 
     ylab = "% Change in Area",
     xlab = "Glacier Area in 1966 (m2)",
     main = "Glacier Area in 1966 vs %Change in Area")


#join data with the spatial data table and overwrite into spatial data table 
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")
#use spplot to shade polygons based on the % change of labels
#first argument is the spatial object
#second is the column in of data to display with the different colors
#add a title using main
#col changes the color of the borders. This argument sets them to transparent
spplot(g1966, "gdiff", main="% change in area", col="transparent")







                  ####QUESTION 8####
#calculate mean and stdev of % loss
#find glacier wit the largest and smallest percent loss
#find the glaciers in 1966 with the smallest and largest area
#describe glacial loss in GNP using spplot and numerical calculations
sd(gAll$gdiff) #20.71525
mean(gAll$gdiff)#39.02311
max(gAll$area66) #2059377, Harrison Glacier
which.max(gAll$area66) #13, = HARRISON GLACIER

min(gAll$area66) #29140.12, Gem Glacier

mean(gAll$area66) #554909.6
mean(gAll$area15) #363557.2

                          ####QUESTION 9####
#map showing glacier footprints in 1966 and 2015 for glacier w/ largest percent loss
#legend designating footprint year and descriptive title

#look at Harrison Glacier in 1966
harrison66 <- g1966[g1966@data$GLACNAME == "Harrison Glacier",]
harrison15 <- g2015[g2015@data$GLACNAME == "Harrison Glacier",]

plot(harrison66, main = "Harrison Glacier Area Cover 1966 vs 2015", col = "brown1", border="grey50")
plot(harrison15, col = "lightblue2", border = "grey50", add=TRUE)

legend("bottomright", 
       c("1966", "2015"),
       col=c("brown1", "lightblue2"),
       pch=19,
       lwd=1,
       bty="n")























