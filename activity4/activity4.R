datB <- read.csv("/Users/ginnytw/Desktop/EnvstData/a04/beaver_dam.csv")
head(datB)
##   year dams.n area.ha
## 1 2002      2     594
## 2 2007      6     610
## 3 2008      7     623
## 4 2009     10     600
## 5 2010     15     618
## 6 2011     31     625

#are beavers flooding the arctic?
#SURFACE WATER is response variable

plot(datB$dams.n, datB$area.ha, 
     pch = 19,
     col = "royalblue4",
     ylab="Surface waster area (ha)",
     xlab = "Number of beaver dams")

#set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)

#get standarized residuals
dam.res <- rstandard(dam.mod)#pulling out standardized residuals

                    #ASSUMPTIONS 
#FIRST WE NEED TO CHECK IF THE RESIDUALS ARE NORMALLY DISTRIBUTED
#set up qq plot
qqnorm(dam.res) #running qqplot, all you need to do is enter the residual
#add qq line
#Shapiro-Wilks test tends to not work for data sets that have over 1k observations
qqline(dam.res)
shapiro.test(dam.res)

plot(datB$dams.n, dam.res, pch=19)
abline(h=0) #pretty randomly distributed, no patterns
summary(dam.mod)
#what does the intercept tell you?
#when you have zero dams, you can expect that surface water value
#p value is less than .05
#R-squared is .57 which shows that there is a fair amt of variability that is explained
#are beavers flooding the tundra? YES 
  #data fits all of our assumptions, and is statistically significant

#set up the regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
#get standardized residuals
dam.res <- rstandard(dam.mod)

#assessing the assumptions of the regression before interpreting results
#set up qq plot
qqnorm(dam.res)
#add qq line
qqline(dam.res)
#POINTS ON THE QQ PLOT MOSTLY FOLLOW LINE
  #last couple of points deviate more from the line
#can use the Shapiro-Wilks test if unsure about whether this is significant
#enough to be normal

shapiro.test(dam.res)
#Shapiro-Wilk normality test
#data:  dam.res
#W = 0.92993, p-value = 0.3793


#check the residual plot. 
#abline fxn adds a line to a graph, useful for visualizing patterns in residuals
#line at a residual value fo zero used to assess if there are any trends in the res.

#make residual plot
plot(datB$dams.n, dam.res,
     xlab = "beaver damns",
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)
#no major concerns about the regression assumptions around the residuals
  #can interpret the results

#can use summary function to print out regression table
summary(dam.mod)

#Call:
#  lm(formula = datB$area.ha ~ datB$dams.n)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-12.639  -7.191  -1.006   6.954  14.772 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 606.00410    4.13835 146.436  < 2e-16 ***
#  datB$dams.n   0.31769    0.08037   3.953  0.00272 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 9.251 on 10 degrees of freedom
#Multiple R-squared:  0.6097,	Adjusted R-squared:  0.5707 
#F-statistic: 15.62 on 1 and 10 DF,  p-value: 0.002718

#make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.h, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab =  "Number of beaver dams")
#add regression line
#make line width thicker
abline(dam.mod, lwd=2)


#READING IN NEW DATA
pheno <- read.csv("/Users/ginnytw/Desktop/EnvstData/a04/red_maple_pheno.csv")
#contains the day of the year (doy) that leaf out occured and year (yr) of occurence
#par function helps look at plots side by side
#par specifies parameters around plotting 
#and you can use arguments like mfrow to specify showing multiple plots in a panel

#set up panel of plots with one row and two columns
par(mfrow=c(1,2))
pheno$siteDesc <- as.factor(pheno$siteDesc) #turning siteDesc into factor data

plot(pheno$Tmax,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")

plot(pheno$siteDesc,pheno$doy, 
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Site Description")
dev.off()

#covariance plot
#set up a matrx of plots and compares all possible combinations of variables
plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)
#no real patterns bt temp and prcp values

#multiple regression that investigates max temp, prcp, elevation, urban/rural
#urban/rural designation is a character
 #want to code this as a zero and one for the regression

pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)

help(ifelse) #for question #5
#setting up multiple regression
mlr <- lm(pheno$doy ~  pheno$Tmax  + pheno$Prcp + pheno$elev + pheno$urID)
#plus sign for each new variable 

#use fitted values for regression
mlFitted <- fitted(mlr)

####QUESTION 6#### 
#are assumptions of the regression met?
mlr.res <-rstandard(mlr) #get standardized residuals
qqnorm(mlr.res) #set up qqplot
qqline(mlr.res) #add qqline

#make residual plot
plot(mlFitted, mlr.res, 
     xlab = "Day of leaf out", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

#mlFitted
summary(mlr)
## 
## Call:
## lm(formula = pheno$doy ~ pheno$Tmax + pheno$Prcp + pheno$elev + 
##     pheno$urID)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -54.497  -9.974   0.372   8.234  51.464 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 158.527450   2.313020  68.537  < 2e-16 ***
## pheno$Tmax   -2.848841   0.160166 -17.787  < 2e-16 ***
## pheno$Prcp    0.001735   0.004227   0.410    0.682    
## pheno$elev   -0.026963   0.001059 -25.465  < 2e-16 ***
## pheno$urID   -6.037373   1.317410  -4.583 5.22e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 15.54 on 924 degrees of freedom
##   (315 observations deleted due to missingness)
## Multiple R-squared:  0.6769, Adjusted R-squared:  0.6755 
## F-statistic: 483.9 on 4 and 924 DF,  p-value: < 2.2e-16


