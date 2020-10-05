#read in weather station file from the data folder
datW <- read.csv("/Users/ginnytw/Desktop/EnvstData/a05/noaa2011124.csv")
#specify that the name column should be a factor
datW$NAME<- as.factor(datW$NAME)
#set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS 

#make a dataframe with just precipitation, year, and site name
#remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME,#gets rid of any NAs in dataframe
                           year=datW$year,
                           PRCP=datW$PRCP))
#total annual precipitation (mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME,datW$year), FUN="sum", na.rm=TRUE)
#use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="sum", na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME","year","totalP")
#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME,datP$year), FUN="length")$x

#make a new dataframe
pr <- precip[precip$ncount >= 364,]


#LOADING A PACKAGE INTO R
install.packages("ggplot2")#don't have to do this more than once
library(ggplot2) #load package into current R session
plot(precip$year, precip$totalP)

ggplot(data = precip,
       aes(x= year,
           y = totalP,
           color = NAME) )+
  geom_point()+
  geom_path()+
  labs(x = "year", y = "Annual precipitation (mm)")+
  theme_classic()+
  scale_color_manual(values = c("#7FB3D5","#34495E", "#E7B800", "#FC4E07","#26A69A"))

#look at only livermore california and morrissville ny precipitation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]

#lets start by looking at the basic plot in R using the California data first
plot(ca$year, ca$totalP)

#make a plot of california precipitation
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year")

#make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
#add y axis
#arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
#las = 2 changes the labels to be read in horizontal direction
axis(2, seq(200,800, by=200), las=2 )

#add new york for referance. points function adds points to existing plot
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n")
#add y axis
axis(2, seq(200,800, by=200), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3") 
# ^axes range is bad
#xlim and ylim arguments change the range of the axes
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")

#need to add a legend to properly label the plot
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

                            ####QUESTION 3####
#make a dataframe with just max temperature, year, and site name

datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year, 
                           TMAX=datW$TMAX))

#mean annual temp
temp <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN="mean", na.rm=TRUE)

#adding a new column for mean of TMAX
colnames(temp) <- c("NAME", "year", "TMAX Mean")

#new column for counts of data for TMAX
temp$ncount <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN="length")$x

#make a new dataframe
tmp <- temp[temp$ncount>= 364,]

#add the x column from mean looking at length of observations in each year
nd <- tmp[tmp$NAME == nameS[3], ] #mandan north dakota
ny <- tmp[tmp$NAME == nameS[5], ] #ny station

plot(nd$year, nd$TMAX,
     type = "b",
     pch = 19,
     ylab = "Annual Max Temperature (c)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(7, 15))
#add y axis
axis(2, seq(5,15, by=1), las=2 )
#add ny
points(ny$year, ny$TMAX,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend
legend("bottomright", #position
       c("North Dakota", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=.5, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn


                ####PLOTTING IN GGPLOT 2####
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation") #make axis labels

#GET RIDE OF GREY GRID LINES
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
  geom_point()+ #make points at data point
  geom_path()+ #use lines to connect data points
  labs(x="year", y="Annual Precipitation")+ #make axis labels
  theme_classic() #change plot theme
                        ####QUESTION 5####
#change colors, make them semi-transparent
ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
  geom_point(alpha=0.5)+
  geom_path(alpha=0.5)+
  labs(x="year", y="Annual Precipitation")+
  theme_classic()+
  scale_color_manual(values = c("#000080","#d15fee", "#008000", "#FF8C00","#ff0000"))
#Navy Blue, coral1, 

              ####DIFFERENT VISUALIZATIONS IN GGPLOT 2####
ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
  geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
  geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
  theme_classic() #git rid of ugly gridlines

sub <- datW[datW$NAME == nameS[4] & datW$ year == 1974,]
#specify date format
#%Y means a four number year 
#- indicates that the date uses dashes to seperate
#%m means month
#%d means day
sub$DATE <- as.Date(sub$DATE,"%Y-%m-%d")

ggplot(data=sub, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

#BARPLOT
ggplot(data=sub, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")


                              ####QUESTION 8####
new <- datW[datW$NAME == nameS[2] & datW$ year == 1974,]
#Livermore, CA
new$DATE <- as.Date(new$DATE,"%Y-%m-%d")

ggplot(data=new, aes(x=DATE, y=TMAX))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Maximimum temperature (C)")

ggplot(data=new, aes(x=DATE, y=PRCP))+
  geom_col(fill="royalblue3")+
  theme_classic()+
  labs(x="year", y="Daily precipitation (mm)")


                              ####QUESTION 9####
#compare the distribution of daily min. temp 
#over the past 2 decades (since 2000) at a site of your choice w sufficient data
datT2 <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year, 
                           TMIN=datW$TMIN))

#use aggregate function to get mean of daily tmin each year
temp2 <- aggregate(datT2$TMIN, by=list(datT2$NAME, datT2$year), FUN="mean", na.rm=TRUE)

#adding a new column for mean of tmin
colnames(temp2) <- c("NAME", "year", "TMIN")

#only look at years above 2000
newM <- temp2[temp2$NAME == nameS[4] & temp2$year >= 2000,]

#turn into Dates
newM$DATE <- as.Date(newM$DATE,"%Y")

#plot it
ggplot(data=newM, aes(x=year, y=TMIN))+
  geom_point()+
  geom_path()+
  theme_classic()+
  labs(x="year", y="Minimum temperature (C)")+
  
  
                           
  





