### Week 6 - 18 October 2017
# Meghan Blumstein, Cat Chamberlain, and Dave Matthews 

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load Libraries
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
require(plyr)


###########
# 13.6 #3 #
###########

### A ###

n80Perc <- function(p){
  #Find 80% power sample size
  # n=(2.8*.5/(P-P0))^2
  #Or n=P(1-P)(2.8/(P-P0))^2
  nRough <- (2.8*.5/(p-0))^2
  nExact <- p*(1-p)*(2.8/(p-0))^2
  
  #Use conservative estimate, SE=.5/sqrt(n)
  #So n = (.5/SE)^2
  n <- c(nRough,nExact)
  return(n)
}

# 1 unit dose, .01% risk. True proportion is .0001
    n1 <- n80Perc(.0001)
    print(n1)
    
    #We need a sample size of at least 78392 to get a positive result 80% of the time.
    
# 100 unit dose, 1% risk. True proportion .01
    n2 <- n80Perc(.01)
    print(n2)
    
    #We need a sample size of at least 776 to get a positive result 80% of the time.
        
# 1 unit dose, 100% risk. True proportion is 1
    n3 <- n80Perc(1)
    print(n3)
    
    #We need a sample size of at least 1 (? clearly not) to get a positive result 80% of the time.
    
    
### B ###
  #Now log scaling of drug effect
# 1 unit dose, .01% risk. True proportion is .0001
    n4 <- n80Perc(.0001)
    print(n4)
    
    #We need a sample size of at least 78392 to get a positive result 80% of the time.
    
# 100 unit dose, logarithmic scaling of effect. .03% risk. True proportion .0003
    n5 <- n80Perc(.0003)
    print(n5)
    
    #We need a sample size of at least 26125 to get a positive result 80% of the time.
    
# 1 unit dose, logarithmic scaling of effect. .05% risk. True proportion is .0005
    n6 <- n80Perc(.0005)
    print(n6)
    
    #We need a sample size of at least 15672 to get a positive result 80% of the time.
    
    
# When the drug has a linearly increasing effect, each increase in dosing brought a reduction in
    #the necessary sample size of several orders of magnitude. By giving more of the toxin (easy change)
    #we were able to greatly reduce the sampling effort.
#When the toxin effects were logarithmic, a huge increase in drug dosage lowered the necessary
    #sample size, but didn't change the order of magnitude of the sampling effort.
    
    
###########
# 14.8 #2 #
###########
    
### A ###
library(haven)
sesame <- read_dta("C:/Users/USER/Documents/School/Classes/2017 Fall/OEB 201 R Stats/Data/ARM_Data/sesame/sesame.dta")

fullData <- data.frame(sesame$age, sesame$prebody)
sum(is.na(fullData))
  #no missing data
#rename variables
fullData <- rename(fullData,c("sesame.age"="age","sesame.prebody"="BMI"))
#delete some data
fullData$age <- as.numeric(fullData$age)
fullData$BMI <- as.numeric(fullData$BMI)


indVect <- vector()
for(i in 1:nrow(fullData)) {
  indicator <- fullData$age[i]*runif(1,min=0,max=1)  
  indVect <- c(indVect,indicator)
}

meanInd <- mean(indVect)
availableData <- fullData

for(i in 1:nrow(fullData)) {
  if(indVect[i]>meanInd) {
    availableData$BMI[i] <- NA
  }
}

sum(is.na(availableData))
#121 NA values in my test run, this is approximately half


### B ###

#Full data regression
lmFull <- lm(BMI~age, data=fullData)
print(lmFull)
plot(BMI~age, data=fullData)
abline(lmFull)

#Partial data regression
lmPart <- lm(BMI~age, data=availableData)
print(lmPart)
plot(BMI~age, data=availableData)
abline(lmFull)
abline(lmPart, col="red")

#The regression coefficient with all the data was:
  #BMI = 4.26 + age*.333
#With complete data analysis on data frame with missing entries:
  #BMI = 4.52 + age*.326

#Both regression coefficient estimates had errors large enough that they overlapped with the other estimate
#Between this analysis and the graph with both regression lines, we can see that the two regressions are very similar.


### C ###

#Full data regression
lmFull2 <- lm(age~BMI, data=fullData)
print(lmFull2)
plot(age~BMI, data=fullData)
abline(lmFull2)

#Partial data regression
lmPart2 <- lm(age~BMI, data=availableData)
print(lmPart2)
plot(age~BMI, data=availableData)
abline(lmFull2)
abline(lmPart2, col="red")

#The regression coefficient with all the data was:
#BMI = 44.6 + age*.32
#With complete data analysis on data frame with missing entries:
#BMI = 42.6 + age*.35

#Based on this, the slopes look to be similar, but the intercepts are different.
  #The std. error of the sloped were 1.34 and 1.96 respectively. In either case, 2*std.error 
  #will still include the other one. This makes it difficult to believe that they are totally different.


### D ###
impute <- function(a, aImpute) {
  ifelse(is.na(a), aImpute, a)
}

lmPart3 <- stan_glm(age~BMI, data=availableData)
