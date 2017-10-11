require(arm)
require(rstanarm)
require(dplyr)


rm(list=ls())

library(haven)
nesRawData <- read_dta("C:/Users/USER/Documents/School/Classes/2017 Fall/OEB 201 R Stats/Data/ARM_Data/nes/nes5200_processed_voters_realideo.dta")

################
#Clean the data#
################

#Check all the variable names
names(nesRawData)

#Take the relevant subset of the data
nesDataLong <- dplyr::filter(nesRawData, nesRawData$year==1992)
nesData <- dplyr::select(nesDataLong, presvote_2party,female,race,educ1,ideo,partyid7)
#For political spectrum categories, delete intermediate values.
nesData <-  nesData %>% mutate(partyid7 = replace(partyid7, partyid7==4, NA))
nesData <-  nesData %>% mutate(ideo = replace(ideo, ideo==3, NA))
#remove data points with missing data
nesData <- nesData[complete.cases(nesData),]

#First, I examine my response variable. Since it is binary, I want to make it between 0 and 1 instead of 1 and 2.
levels(as.factor(nesData$presvote_2party))
nesData$presvote_2party <- nesData$presvote_2party-1


#Check the possible levels of all my factors. If non-binary, convert to binary
levels(as.factor(nesData$female))

levels(as.factor(nesData$race))
  nesData$race <- ifelse(nesData$race == 1, 0, 1)

levels(as.factor(nesData$educ1))
  nesData$educ1 <- ifelse(nesData$educ1 == 1, 0, 1)

levels(as.factor(nesData$ideo))
  nesData$ideo <- ifelse(nesData$ideo == 1, 0, 1)

levels(as.factor(nesData$partyid7))
  nesData$partyid7 <- ifelse(nesData$partyid7 <= 3, 0, 1)

#Convert all of my variables to factor so R doesn't think they are continuous.
nesData$female <- as.numeric(as.factor(nesData$female))
nesData$race <- as.numeric(as.factor(nesData$race))
nesData$educ1 <- as.numeric(as.factor(nesData$educ1))
nesData$ideo <- as.numeric(as.factor(nesData$ideo))
nesData$partyid3 <- as.numeric(as.factor(nesData$partyid7))

#####
# A #
#####
#Make the model
fit1 <- glm(presvote_2party ~ female+race+educ1+ideo+partyid7, family=binomial(link="logit"), data=nesData)
display(fit1)
# coef.est coef.se
# (Intercept) -5.62     1.27  
# female       0.19     0.23  
# race        -0.53     0.29  
# educ1        0.47     0.54  
# ideo         1.73     0.24  
# partyid7     3.98     0.23  
# ---
#   n = 1044, k = 6

#It seems like ideology and partyID have the biggest effect here. After this, race and education level both seem to be important(but far less so)

#Since both ideology and partyID had strong effects on the model, and since they seem like they should be highly correlated, I will now include just one (I'll go with partyID since it had a bigger effect)
fit2 <- glm(presvote_2party ~ female+race+educ1+partyid7, family=binomial(link="logit"), data=nesData)
display(fit2)
# coef.est coef.se
# (Intercept) -2.68     1.22  
# female       0.18     0.22  
# race        -0.41     0.29  
# educ1        0.25     0.55  
# partyid7     4.50     0.22  
# ---
#   n = 1044, k = 5

#Now partyID has an even stronger effect, and the other effects have decreased. Race still has a bit larger effect than the rest, although the error is too high to really say what the effect is.

#It is possible that there would be an interaction effect between race and education (since higher education might affect one races political views more than another), so I will try including it as an interaction effect
fit3 <- glm(presvote_2party ~ female+race:educ1+partyid7, family=binomial(link="logit"), data=nesData)
display(fit3)
# coef.est coef.se
# (Intercept) -2.34     0.51  
# female       0.19     0.22  
# partyid7     4.54     0.22  
# race:educ1  -0.16     0.14  
# ---
#   n = 1044, k = 4

#This didn't create any more predictive power. I think the second model is best.



#####
# B #
#####

#The only predictors that ever had an effect size larger than the error were ideology and partyID. However, these two are largely measuring the same thing,
  #so we are probably just measuring the same affect twice, and getting colinearity. When we removed the less powerful predictor here, the predictive power of partyID went up
  #and the error essentially stayed the same. Trying to include interaction effects in other variables didn't give them any more predictive power.
  #So we conclude that given the data, partyID has the only independent, detectable effect on which candidate a person votes for.

#####
# C #
#####

#Our chosen model is presvote_2party ~ female+race+educ1+partyid7
  #The only predictor variable that carried any predictive power above the level of the noise was partyID. This had a very strong effect.
  #If we use the rule of 4, we can estimate that people who identified as republican were over 100% more likely to vote for bush. Which of course doesn't make any sense.....