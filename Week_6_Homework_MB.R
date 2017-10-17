#######################################################################################
#######################################################################################
## Written by: 
##
## Date written: 10/15/2017
##
## OEB 201 Homeowrk Week 6; ROS: 13.6: # 3 and 14.8: # 2
##
#######################################################################################
#######################################################################################

## User Input
require(foreign)
require(arm)

wd <- "/Users/Meghs/Documents/BioStats/ARM_Data/child.iq/"
setwd(wd)
full_data <- read.dta("kidiq.dta")

##############################
## 13.6 #3
##############################
## Effect size and sample size: consider a toxin that can be tested on animals at different doses. 
## Suppose a typical exposure level for humans is 1 (in some units), and at this level the toxin is
## hypothesized to introduce a risk of 0.01% of death per person. 

## (a) Consider different animal studies, each time assuming a linerarity in the dose-response relation 
##     (that is, 0.01% risk of death per animal per unit of the toxin), with the doses of 1, 100, and 10,000.
##     At each of these exposure levels, what sample size is needed to have 80% power of detecting the effect?

doses <- c(1, 100, 10000)
responses <- (doses * .01)/100
plot(responses ~ doses , pch = 16, ylab = ("Response - P(Dying|Dosage)"))
abline(a = 0, b = 0.01/100, col = "cornflowerblue")

# dose = 1
p <- responses[1]
p_control <- 0
n <- (p*(1-p)) * (2.8/(p-p_control))^2
## n = 78393 animals 

# dose = 100
p <- responses[2]
p_control <- 0
n <- (p*(1-p)) * (2.8/(p-p_control))^2
## n = 777 animals

# dose = 10000
p <- responses[3]
p_control <- 0
n <- (p*(1-p)) * (2.8/(p-p_control))^2
## n = 0, but I would say n = 3 because you would want to test at least 3 animals


## (b) This time assume that response is a logged funciton of dose and redo the calculations in (a)
doses <- c(1, 100, 10000)
responses <- log(doses) * .0001
plot(responses ~ doses , pch = 16, ylab = ("Response - P(Dying|Dosage)"))
curve(log(x)*.0001, add = T, col = "cornflowerblue")


# dose = 1
p <- responses[1]
p_control <- 0
n <- (p*(1-p)) * (2.8/(p-p_control))^2
## Probability is 0, so there is no amount of test subjects that will prove this 

# dose = 100
p <- responses[2]
p_control <- 0
n <- (p*(1-p)) * (2.8/(p-p_control))^2
## n = 17017 subjects 

# dose = 10000
p <- responses[3]
p_control <- 0
n <- (p*(1-p)) * (2.8/(p-p_control))^2
## n = 8505 (about 1/2 of the previous treatment's)


##############################
## 14.8 #2
##############################
## Take a complete dataset (with no missingness) of interest to you with two variables, x and y. Call this the
## "Full Data". 

## (a) Write a program in R to cause approximately half of the values of x to be missing. Design this missingness
##     mechanism to be at random but not completely at random; that is, the probablity that x is missing should 
##     depend on y. Call this new dataset, with misingness in x, the "available data"

## The older the mom, the more likely they were to respond to the survey

full_data$Response_Probability <- (full_data$mom_age/max(full_data$mom_age)) * 0.65 ## Pick probabilties that increase with mom's age and scale so half data is missing
full_data$Response <- sapply(full_data$Response_Probability,function(x) rbinom(1, 1, prob = x) ) ## Random generate which respond and which don't
available_data <- full_data[full_data$Response == 1,] ## keep only responders

## (b) Perform the regression of x on y (that is, with y as predictor and x as outcome), using complete-case analysis
##    (that is, using only the data fro which both variables are observed) and show that it is consistent with the 
##    regression on the full data. 

m1 <- lm(mom_age ~ kid_score, data = available_data)
display(m1)
m2 <- lm(mom_age ~ kid_score, data = full_data)
display(m2)

## Full data and subsetted data produce roughly the same results


## (c) Perform the complete-case regression of y on x and show that is is not consistent with the corresponding regression
##     on the full data 

m1 <- lm(kid_score ~ mom_age, data = available_data)
display(m1)
m2 <- lm(kid_score ~ mom_age, data = full_data)
display(m2)

## In this case, the slope of mom_age is almost halved, indicating that for every year older a mom is, a child performs ~0.48 
## points better on their IQ test on average than in the complete model, whereas the slope is 0.7. The intercepts differ
## by ~5Pts as well. 

## (d) Using just the available data, fit a model in R for x given y, and use this model to randomly impute the missing x data, 
##     Perform the regression of y on x, using this imputed dataset and compare your results to (c). 

predict_model <- lm(mom_age~kid_score, data = available_data)

full_data$Predicted_Mom_Age[full_data$Response == 0] <- predict(predict_model, data.frame(kid_score = full_data$kid_score[full_data$Response == 0]))
full_data$Predicted_Mom_Age[full_data$Response == 0] <-  sapply(full_data$Predicted_Mom_Age[full_data$Response == 0], function(x) x + rnorm(1, mean = 0, sd = sd(available_data$mom_age)) ) ## Add normal skew for random imputation
full_data$Predicted_Mom_Age[full_data$Response == 1] <- full_data$mom_age[full_data$Response == 1]

print("Available Data")
m1 <- lm(kid_score ~ mom_age, data = available_data)
display(m1)

print("Full Data")
m2 <- lm(kid_score ~ mom_age, data = full_data)
display(m2)

print("Predicted Data")
m3 <- lm(kid_score ~ Predicted_Mom_Age, data = full_data)
display(m3)

par(mfrow = c(1,2))
plot(kid_score ~ mom_age, data = full_data, pch = 16)
abline(m1, col = "cornflowerblue")


plot(kid_score ~ Predicted_Mom_Age, data = full_data, pch = 16)
abline(m3, col = "cornflowerblue")

## The original model fit is terrible (R2 = 0), as can be seen in the left-hand figure. Thus when we predict mom's age from 
## child's test scores, all the ages fall between 22 1/2 and 23 1/2. However, when we add random noise that matches the mom_age
## distribution, we have a fairly similar output to the full dataset. 
## When we impute the missing data, we come much closer to the predictions from the full dataset.  









