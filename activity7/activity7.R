#package for vector data
library(sp)
#package for reading in spatial data
library(rgdal)
#data manangement package
library(dplyr)

#read in shapefiles
#readOGR in rgdal does this
#ice in 1966
icetotal <- readOGR("Users/ginnytw/Desktop/EnvstData/a07/sea_ice")
ice1979 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_197909_polygon_v3.0")
ice2019 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_201909_polygon_v3.0")

                            ##############################
                            ####SUMMARY STATISTICS (5)####
                            ##############################
seaiceAll <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice_all")
head(seaiceAll@data)
plot(seaiceAll) #looking from the perspective of the north pole
plot(seaiceAll@data$year == 1979,])


     
aggregate(seaiceAll@data$area, by=list(seaiceAll@data$year), FUN="mean", na.rm=TRUE)



    seaiceaAll@proj4string
seaiceAll

sum(area[seaiceAll@data$year == 1979,])
    
    
seaiceYears <- seaiceAll@data$year
seaiceAreas <- seaiceAll@data$area
seaiceAreas <- (area(seaiceAll@data))
                            ##############################
                            ##########PLOT (5)############
                            ##############################
#map showing sea ice extent in 1979 and 2019 
#legend designating ice extent year and colors
plot(ice1979, main = "Arctic Sea Ice Extent 1979 vs 2019", col = "brown1", border="grey50")
plot(ice2019, col = "lightblue2", border = "grey50", add=TRUE)

legend("bottomright", 
       c("1979", "2019"),
       col=c("brown1", "lightblue2"),
       pch=19,
       lwd=1,
       bty="n")





