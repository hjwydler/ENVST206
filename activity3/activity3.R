                        ####ACTIVITY 3####

ch4 <- read.csv("/Users/ginnytw/documents/EnvstData/lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory) #to make herbivory coljumn a factor

plot(ch4$CH4_Flux ~ ch4$herbivory)
#makes a boxplot to get a basic idea of the data
#Positive values indicate that methane is being emmitted from the plot
#negative value indicates that ther is methan uptake occuring over the plot surface


shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
#Shapiro-Wilk normality test

#data:  ch4$CH4_Flux[ch4$herbivory == "Ex"]
#W = 0.93325, p-value = 0.4158
#can't reject null hypothesis, doesn't significantly deviate from 
#normal distribution

shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])
#Shapiro-Wilk normality test

#data:  ch4$CH4_Flux[ch4$herbivory == "Ctl"]
#W = 0.87763, p-value = 0.08173

#dependent variable ~ independent variable
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)
#Bartlett test of homogeneity of variances
#use bartlett test since we're testing for equal variances

#data:  ch4$CH4_Flux by ch4$herbivory
#Bartlett's K-squared = 0.21236, df = 1, p-value = 0.6449

#DEPENDENT VARIABLE ~ INDEPENDENT VARIABLE
t.test(ch4$CH4_Flux ~ ch4$herbivory)
  #data:  ch4$CH4_Flux by ch4$herbivory
  #t = 1.5328, df = 21.569, p-value = 0.1399
  #alternative hypothesis: true difference in means is not equal to 0
  #95 percent confidence interval:
  #  -4.644609 30.844370
  #sample estimates:
  #  mean in group Ctl  mean in group Ex 
  #18.814645          5.714765

#if zero is in confidence interval, we also can't reject null hypothesis

#Ctl indicates that it was a control plot with no exclosure that was open to grazing
#Ex indicates that enclosure was applied preventing lemmings from grazing

help(t.test) #for question #2


                          ####QUESTION 3####
#read in insect data
datI <- read.csv("/Users/ginnytw/documents/EnvstData/insect_richness.csv")
#urbanName gives the type of urban environment the species richness was 
#measured in
#urbanType provides an identifying number originally used in by the authors
datI$urbanName <- as.factor(datI$urbanName)

                          ####QUESTION 4####
#does the insect data meet the assumptions of an ANOVA test?
  #normal population distribution, same variance
#first need to test groups for normality using shapiro test

shapiro.test(datI$Richness[datI$urbanName == "Natural"]) 
#data:  datI$Richness[datI$urbanName == "Natural"]
#W = 0.91559, p-value = 0.2514

shapiro.test(datI$Richness[datI$urbanName == "Suburban"])
#data:  datI$Richness[datI$urbanName == "Suburban"]
#W = 0.98087, p-value = 0.2461

shapiro.test(datI$Richness[datI$urbanName == "Developed"])
#data:  datI$Richness[datI$urbanName == "Developed"]
#W = 0.96155, p-value = 0.05604

shapiro.test(datI$Richness[datI$urbanName == "Dense"])
#data:  datI$Richness[datI$urbanName == "Dense"]
#W = 0.97508, p-value = 0.102

#NEXT we need to see if the groups have equal variances
#going to use the bartlett test
bartlett.test(datI$Richness ~ datI$urbanName)
#data:  datI$Richness by datI$urbanName
#Bartlett's K-squared = 1.2091, df = 3, p-value = 0.7508



#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run the ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table
summary(in.aov)

##                 Df Sum Sq Mean Sq F value  Pr(>F)   
## datI$urbanName   3   1944   647.9   4.898 0.00254 **
## Residuals      236  31216   132.3                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#dat1$urbanName = among group variability
  #sum of squares for group means compared to overall mean
  #residuals row is the WITHIN group variability


#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT

##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = in.mod)
## 
## $`datI$urbanName`
##                         diff         lwr        upr     p adj
## Developed-Dense     1.433333  -3.5966663  6.4633329 0.8819569
## Natural-Dense      12.583333   3.3998525 21.7668141 0.0026479
## Suburban-Dense      3.785714  -0.8060261  8.3774547 0.1456297
## Natural-Developed  11.150000   1.7397324 20.5602676 0.0128693
## Suburban-Developed  2.352381  -2.6776186  7.3823805 0.6210733
## Suburban-Natural   -8.797619 -17.9810999  0.3858618 0.0658910
  
  #a significant difference between groups means that the confidence interval
  #in the difference between the means will not overlap with zero and
  #will have a p value below our confidence level threshold of .05

#can also use the PLOT function and input the entire test variable to gen.
#a plot that shows the confidence levels for the mean comparisons

#make a plot
#make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)

#there is a fxn. to calculate means across factor data simultaneously in R
tapply(datI$Richness, datI$urbanName, "mean")
##     Dense Developed   Natural  Suburban 
##  19.83333  21.26667  32.41667  23.61905


                  ####PLANT PROTECTION DATA####

#set up contingency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")

#make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")
#visually looks like there may be some difference in pop status that depends
#on legal protections
#can test whether frequency of observations in each category meets the 
#expectation under a null hypothesis
species
#                      Not protected Protected
#Declining                  18         8
#Stable/Increase            15        32

#conduct a chi-squared test
chisq.test(species)
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  species
## X-squared = 7.9642, df = 1, p-value = 0.004771


