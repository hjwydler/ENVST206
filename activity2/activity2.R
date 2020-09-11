                    
                            ####ACTIVITY 2#####

#make a vector of tree heights in meters
heights <- c(30,41,20, 22)
#convert to centimeters
heights_cm <- heights*100
heights_cm 
## [1] 3000 4100 2000 2200
help("matrix") #help function helps find more information about
#specific functions 

#look at the first tree height
heights[1] #[1] 30

#look at the 2nd and 3rd tree heights heights 
heights [2:3] #[1] 41 20

                             ##MATRICES

#Matrice = multiple columns of vectors
#FUNCTIONS are built into are, have () after fnxn name
#stuff inside parentheses = ARGUMENT
help (matrix) #gives info about matrix function

                          ##EXAMPLE MATRICES

##matrix with 2 columns, filling in by ROWS
##first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#       [,1][,2]
#[1,]    1    2
#[2,]    3    4
#[3,]    5    6

##matrix with 2 columns, filling in by COLUMNS
##first argument is the vector of numbers to fill in the matrix

Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol
#       [,1] [,2] #fills in going down first instead of L to R
#[1,]    1    4
#[2,]    2    5
#[3,]    3    6

#subset the matric to look at row 1, column 2
Mat.bycol[1,2] #[1] 4

#look at all values in row 1
Mat.bycol[1,] #[1] 1 4, comma goes AFTER [COLUMN left blank]

#look at all values in column 2
Mat.bycol [,2] #[1] 4 5 6, comma goes BEFORE, [ROW left blank]

                              ##DATAFRAMES##

#DATAFRAMES are matrices where columns have names and rows contain
#descriptions for the same entity
  #works like a spreadsheet

#read in weather station file from the data folder
#you will need to adjust the directory to mathc your folder

datW <- read.csv("/Users/ginnytw/Desktop/EnvstData/Noaa2011124.csv")

#get information about the dataframe
str(datW)
#Two different types of data: numeric/character
#treat characters as FACTOR
#FACTOR data types automatically assigned to repeating character strings

#Convert data type into factor
datW$NAME <- as.factor(datW$NAME)


                      ######QUESTION 2#########

#Create an example vector of each data type with five objects in it

#Character
character_example <- c("a", "b", "c", "d", "e")
is.character(character_example) #returns TRUE

#Integer
integer_example <- c(1,2,3,4,5)
integer_example <- as.integer(integer_example)
is.integer(integer_example) #returns TRUE

#Numeric
numeric_example <- c(1.5, 2.5, 3.5, 4.5, 5.5)
is.numeric(numeric_example) #returns TRUE

#Factor
factor_example <- c("cheese1, cheese2, cheese3, cheese4, cheese5")
factor_example <- as.factor(factor_example)
is.factor(factor_example) #returns TRUE


                ##DESCRIPTIVE STATISTICS AND HISTOGRAMS##
levels(datW$NAME)#find all unique site names 
#                    [1] "ABERDEEN, WA US"                 
#                    [2] "LIVERMORE, CA US"                
#                    [3] "MANDAN EXPERIMENT STATION, ND US"
#                    [4] "MORMON FLAT, AZ US"              
#                    [5] "MORRISVILLE 6 SW, NY US"  

#look at the mean maximum temperature for Aberdeen 
#with na.rm argument set to true to ignore NA 

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE) # [1] NA
#NA because missing data in this data set
#allows us to ignore NAs
#[1] 14.68515

#also can look at standard deviation
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#[1] 5.834647

#calculate the average daily temperature
#will be halfway between the minimum and maximum temperature

datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#single calculation applied to all rows of TMIN and TMAX in datW
#created a new column in datW called TAVE
#convention for referring to a column in dataframe is always
#dataframe$column

#we can use AGGREGATE function to calculate means across indexing value

#get the mean across all sites
#the by function is a list of one or more variables to index over
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to
averageTemp <- aggregate (datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
                          #Group.1       x 
#1                  ABERDEEN, WA US 17.77689
#2                 LIVERMORE, CA US 26.88993
#3 MANDAN EXPERIMENT STATION, ND US 11.56586
#4               MORMON FLAT, AZ US 36.51020
#5          MORRISVILLE 6 SW, NY US 12.80847


#change the automatic output of column manes to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp
                            #NAME     MAAT
#1                  ABERDEEN, WA US 17.77689
#2                 LIVERMORE, CA US 26.88993
#3 MANDAN EXPERIMENT STATION, ND US 11.56586
#4               MORMON FLAT, AZ US 36.51020
#5          MORRISVILLE 6 SW, NY US 12.80847

#convert level to number for factor data type 
#we will have to reference the level output or look at row of data to see the character designation
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title main argument
#Here you want to paste the actual name of the factor not the num. index
#since that will be more meaningful
hist(datW$TAVE[datW$siteN == 1],
  freq=FALSE, 
  main = paste(levels(datW$NAME)[1]),
  xlab = "Average daily temperature (degrees C)",
  ylab = "Relative frequency",
  col = "grey45",
  border = "white")

help(hist)


                            ####QUESTION 3####

hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey45",
     border = "white")
#Builds a histogram with site Number 2: Livermore, CA US
#Appears to be normally distributed 

help(dnorm)


#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,#zero because we are only focusing on temperatures to the left of freezing (0c)
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5, #temperatures between 0-5 degrees
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#you can subtract the probability for observing values below 0 from 
#your first probability, and you will get the probability 
#of temperatures in the range of 0-5.

pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,#subtracting values - 0
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnorm of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20, 
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm gives me the value at which all values and below equal the probability in my argument
#Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


                       ####QUESTION 5####
1 - pnorm(20,
          (mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE))+4, #mean temp increases by 4
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#[1]0.128458. We would expect extreme high temps to occur 12.8 percent of time

                        ####QUESTION 6####
#make a histogram of daily precipitation for Aberdeen. 
#NOT normally distributed

hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation (Centimeters)",
     ylab = "Relative frequency",
     col = "grey45",
     border = "white")




                            ####QUESTION 7####
#use the sum and aggregate function to get precipitation for each
#year and site in the data
help(aggregate)

TotalPrecip <- aggregate(datW$PRCP, by=list(datW$siteN, datW$year), FUN="sum",na.rm=TRUE)
TotalPrecip
colnames(TotalPrecip) <- c("NAME",  "Year", "PRCP")


                            ####QUESTION 8####
#Make a histogram of annual precipitation for Aberdeen and Mandan Station ND
#Describe the general shape of the data and whether you think it n. dist.

#histogram for annual precipitation in Aberdeen, WA US
hist(TotalPrecip$PRCP[TotalPrecip$NAME == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual Precipitation (mm)",
     ylab = "Relative Frequency",
     col = "grey25",
     border = "white")

#histogram for annual precipitation in Mandan Station ND
hist(TotalPrecip$PRCP[TotalPrecip$NAME == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Annual Precipitation (mm)",
     ylab = "Relative Frequency",
     col = "grey25",
     border = "white")


                                  ####QUESTION 9####
#How likely is a year with 700 mm of precipitation or less in Mandan ND
#vs Aberdeen WA? Show your answer.

#For Aberdeen WA
pnorm(700,
      mean(TotalPrecip$PRCP[TotalPrecip$NAME == 1],na.rm=TRUE),
      sd(TotalPrecip$PRCP[TotalPrecip$NAME == 1],na.rm=TRUE))
#[1] 5.690109e-05

#For Mandan Experiment Station, ND
pnorm(700,
      mean(TotalPrecip$PRCP[TotalPrecip$NAME == 3],na.rm=TRUE),
      sd(TotalPrecip$PRCP[TotalPrecip$NAME == 3],na.rm=TRUE))
#1] 0.9969318



