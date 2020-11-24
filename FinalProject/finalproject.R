#Hans Wydler
#Final Project
#Environmental Data Science
#Professor Kropp
#Statistical analysis and visualizations of declines in
  #arctic sea ice extent 

  #Use library() to bring in packages required for this project
#package for vector data (sp)
library(sp)
#package for reading in spatial data (rgdal)
library(rgdal)
#data management package (dplyr)
library(dplyr)
#raster package (raster)
library(raster)
#package to make graphs (ggplot2)
library(ggplot2)

#read in shapefiles using readOGR from rgdal

#seaiceAll includes ALL polygons of arctic sea ice data
seaiceAll <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice_all/sea_ice_all.shp")
#ice1979 includes data for the year 1979 (oldest spatial ice data)
ice1979 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_197909_polygon_v3.0")
#ice2019 includes the data for the year 2019 (newest spatial ice data)
ice2019 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_201909_polygon_v3.0")
#countries reads in a shape file with country borders for whole world, used in map
countries <- readOGR("/Users/ginnytw/Desktop/WORLD/CNTRY92.SHP")
#ice2007 = spatial data for that year
ice2007 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_200709_polygon_v3.0")
#ice2011 = spatial data for that year
ice2011 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_201109_polygon_v3.0")
#ice2012 = spatial data for that year
ice2012 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_201209_polygon_v3.0")
#ice2015 = = spatial data for that year
ice2015 <- readOGR("/Users/ginnytw/Desktop/EnvstData/a07/sea_ice/extent_N_201509_polygon_v3.0")

                            


                ##PART ONE##
                #Statistical analysis of ice extent data
                #Checks assumptions of linear regression, runs linear regression
                #makes plot with ggplot2 for linear regression

#Sum all ice polygons in each year using aggregate function
iceArea <- aggregate(seaiceAll@data$Area, by=list(seaiceAll$year), FUN="sum", na.rm=TRUE)

#rename the columns in iceArea vector
colnames(iceArea) <- c("Year", "Total Area")

#Scatter plot to visualize data
#looks like sea ice is DECREASING  since 1979...
plot(iceArea$Year, iceArea$`Total Area`, #year on x axis, area on y axis
     pch = 19,
     col = "royalblue4", #color
     ylab = "Total Area", #y axis title
     xlab = "Year") #x axis title

#Set up a regression using lm argument to specify model
ice.mod <- lm(iceArea$`Total Area` ~iceArea$Year) 
#get standardized results
ice.res <- rstandard(ice.mod)

#Next, check the assumptions of the regression

#First, set up QQ Plot
qqnorm(ice.res) #get qqnorm
#add qq line
qqline(ice.res) #get qqresidual

#Run those two lines of code...
#Points mostly follow line, last couple deviate more from line 
  #Fits assumptions

#Do a Shapiro-Wilks test to see if it non-normal
shapiro.test(ice.res)
#Shapiro-Wilk normality test results
    #data:  ice.res
    #W = 0.97529, p-value = 0.5038

#Make a residual plot to see if there are any trends 
#Can use the plot function
plot(iceArea$'Total Area', ice.res,
     xlab = "year",
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)
#looks good! seems as though there are no trends in the residual

#get summary of regression through regression table
  #results included below
summary(ice.mod)
            #Call:
            #  lm(formula = iceArea$`Total Area` ~ iceArea$Year)
            
            #Residuals:
            #  Min         1Q     Median         3Q        Max 
            #-1.399e+12 -3.134e+11  3.601e+10  3.172e+11  1.427e+12 
            
            #Coefficients:
            #  Estimate Std. Error t value Pr(>|t|)    
            #(Intercept)   1.719e+14  1.379e+13   12.47 3.48e-15 ***
            #  iceArea$Year -8.305e+10  6.898e+09  -12.04 1.04e-14 ***
             # ---
             # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
            
            #Residual standard error: 5.226e+11 on 39 degrees of freedom
            #Multiple R-squared:  0.788,	Adjusted R-squared:  0.7826 
            #F-statistic:   145 on 1 and 39 DF,  p-value: 1.038e-14


#First, make a residual plot with plot function to visualize the data
plot(iceArea$Year, iceArea$`Total Area`, #data for plot (x/y axis)
     pch = 19, #point size
     col = "royalblue4", #color of points
     ylab = "Surface ice area (m^2)", #y axis
     xlab =  "Year") #x axis
#add regression line
#make line width thicker
abline(ice.mod, lwd=2)

#next, can make a slightly better looking regression plot with ggplot2
#this is the figure that I included in my project
ggplot(iceArea, aes(x=iceArea$Year, iceArea$`Total Area`))+ #change to total.area
  geom_point()+ #scatter plot
  labs(x="Year", y= expression(paste("Sea Ice Extent (m"^2, ")")), title = 'Sea Ice Extent Linear Regression')+ #y axis title
  geom_smooth(method = 'lm', se = FALSE) #linear model, get rid of error grey area



            ##PART TWO##
            #Visualize spatial data for sea ice extent difference bt 1979/2019
            #Make a map showing this info

#Change projection of countries so that it matches seaiceAll
    #has to show the arctic
spTransform(countries, seaiceAll@proj4string)

#cropping countries, making it fit ice data
countriesC <- crop(countries, extent(-180,180,36,90)) 
countriesP <- spTransform(countriesC, seaiceAll@proj4string)

#Now, map it!
#first layer, ice 1979
plot(ice1979, main = "Arctic Sea Ice Extent 1979 vs 2019", axes=FALSE, border="grey50") 
#second layer, blue ocean
polygon(c(-5000000,-5000000,5000000,5000000),c(-5000000,5000000,5000000,-5000000), border=NA, col=rgb(114/255,207/255,252/255,.6))
#third layer, ice1979 again so it isn't behind ocean
plot(ice1979, col = "#00b294", border="grey50", lwd = .50, add=TRUE) 
#third layer, ice 2019 to show how it has gotten smaller
plot(ice2019, col = "#c28897", border = "grey50", add=TRUE)
#final layer, country borders (in grey, with grey borders)
plot(countriesP, col = "#bfbdbd", border="grey50", add=TRUE)
#next, add legend to map
legend("bottomright", #position
       c("1979", "2019"), #labels
       col= c("#00b294", "#c28897"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       #bg="#CAC9C8",
       bty="y") #always use this argument otherwise an ugly box is drawn




              ##PART THREE##
                #Map showing four smallest extents on map (2007, 2011, 2012, 2019)
 
#first layer, ice 2007
plot(ice2007, main = "Smallest Arctic Sea Ice Extents", axes=FALSE, border="grey50") 
#second layer, blue ocean
polygon(c(-5000000,-5000000,5000000,5000000),c(-5000000,5000000,5000000,-5000000), border=NA, col=rgb(114/255,207/255,252/255,.6))
#third layer, ice2007 again so it isn't behind ocean
plot(ice2007, col = "#00b294", border="grey50", lwd = .50, add=TRUE) #green
#fourth layer, ice 2011 
plot(ice2011, col = "#d893f5", border = "grey50", add=TRUE) #purple
#fifth layer, ice 2012
plot(ice2012, col = "#e89f73", border = "grey50", add=TRUE) #orange
#sixth layer, ice 2019
plot(ice2019, col = "#c28897", border = "grey50", add=TRUE) #pink
#final layer, country borders (in grey, with grey borders)
plot(countriesP, col = "#bfbdbd", border="grey50", add=TRUE)

legend("bottomright", #position
       c("2007", "2011", "2012", "2019"), #labels
       col= c("#00b294", "#d893f5", "#e89f73", "#c28897"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       #bg="#CAC9C8", #white background in legend
       bty="y") #gotta use the box otherwise the text is hard to read
       











