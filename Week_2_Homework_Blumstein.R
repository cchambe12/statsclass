# (ARM 3.9: #4)
#   The child.iq folder contains a subset of the children and mother data 
#  discussed earlier in the chapter. You have access to children’s test scores at age 3, mother’s education, and the mother’s age at the time she
#  gave birth for a sample of 400 children. The data are a Stata file which you can read into R by saving in your working directory and then typing 
# the following:
#   
#   library ("foreign")
#   iq.data <- read.dta ("child.iq.dta")
#   
#   (a) Fit a regression of child test scores on mother’s age, display the data and fitted model, check assumptions, and interpret the slope coefficient. When do you recommend mothers should give birth? What are you assuming in making these recommendations?
#   (b) Repeat this for a regression that further includes mother’s education, inter-preting both slope coefficients in this model. Have your conclusions about the timing of birth changed?
#   (c) Now create an indicator variable reflecting whether the mother has completed high school or not. Consider interactions between the high school completion and mother’s age in family. Also, create a plot that shows the separate regres- sion lines for each high school completion status group.
#   (d) Finally, fit a regression of child test scores on mother’s age and education level for the first 200 children and use this model to predict test scores for the next 200. Graphically display comparisons of the predicted and actual scores for the final 200 children.
#   
#   As a bonus you can do #2 from ROS chapter 7 (I recommend on your own, but in your group is fine too).

## Set working directories and load necessary packages
require(foreign)
wd <- "/Users/Meghs/Documents/BioStats/"
setwd(wd)

## Plotting Colors
kid_col <- rgb(238, 211, 99, alpha = 100, max = 255)
mom_col <- rgb(161, 205, 202, alpha = 100, max = 255)

## Load Requisite Data
kidiq <- read.dta("ARM_Data/child.iq/child.iq.dta")
attach(kidiq)

########################
## Part (a)
########################
par(mfrow = c(2,2))

## Linear regression of child iq vs. mom's age
m1 <- lm(ppvt ~ momage)

## Plot Data and Model Fit
plot(ppvt ~ momage, col = "gray50", pch = 16, ylab = "Child IQ", xlab = "Mom's Age")
abline(a = coefficients(m1)[1], b = coefficients(m1)[2], main = "Child's IQ vs. Mom's Age")

## Check Assumptions of Model
hist(ppvt, col = kid_col, border = "white", xlab = "Child's IQ", xlim = c(15, 140), main = "")
hist(momage, col = mom_col, border = "white", xlab = "Mom's Age", xlim = c(15, 30), main = "")


## The Slope coefficeint is ~0.8, suggesting that, given two children, the one with the mother that is 1 year older is on average likely
## to have an IQ that is 0.8 points higher. Based on the model alone, you would suggest that the older a mother is, the higher her child's
## IQ will be. However, the model is not a very good one. The residual SE is 20.34, so the model can predict Child's 
## IQ to within an accuracy of 20.34 IQ Points, with a R-squared of .009 - indicating that 
## the proportion of the variation that is explained by the model is extremely low (only explains 0.9% of variation in data). 

########################
## Part (b)
########################
par(mfrow = c(2,2))

## Linear regression of child iq vs. mom's age + education
m2 <- lm(ppvt ~ momage + educ_cat)

## Plot Data and Model Fit
boxplot(ppvt ~ as.factor(educ_cat), col = kid_col, xlab = "Educational Category", ylab = "Child IQ", pch = 16)
plot(ppvt ~ educ_cat, col = colorRampPalette(c("gray90", "gray10"))(7)[as.factor(momage)], pch = 16)
abline(a = coefficients(m2)[1], b = coefficients(m2)[2], main = "Child's IQ vs. Mom's Age + Education Level")
legend("bottomright", bty = "n", "*Age light --> Dark")

## Check Assumptions of Model
hist(ppvt, col = kid_col, border = "white", main = "Child's IQ")
hist(momage, col = mom_col, border = "white", main = "Mom's Age")
boxplot(ppvt ~ mom_hs, col = kid_col, names = c("Didn't Attend \n HS", "Attended \n HS"))


## The Slope coeefficeint for mom age is now ~0.3, suggesting that, given two children, the one with the mother that is 1 year older is on average likely
## to have an IQ that is 0.3 points higher. This is lower than our previous model estimates, suggestign that there is a relationship between
## age and education level, which accounts for some of the variation originally explained by age alone. The slope coefficient for education is
## 4.7, suggesting that for all else being equal, a mom who went to highschool is likely to have a child that has 4.7 higher IQ points on average
## than the child of a mother who did not attend High School. Based on the model, you could still suggest that the older a mother is, the higher her child's
## IQ will be, but only slightly. The bigger factor in the model is whether or not the mom went to high school, so I would suggest waiting until at least high
## school is completed. However, it's still not a very explantive model. The residual SE is 20.05 with a R-squared of .04 - indicating that 
## the proportion of the variation that is explained by the model is still quitelow (only explains 4% of variation in data). 

########################
## Part (c)
########################

# Now create an indicator variable reflecting whether the mother has completed high school or not. 
# Consider interactions between the high school completion and mother’s age in family. Also, create a p
# lot that shows the separate regression lines for each high school completion status group

par(mfrow = c(1,1))
detach(kidiq)
kidiq$mom_hs <- kidiq$educ_cat
kidiq$mom_hs[kidiq$mom_hs < 2] <- 0
kidiq$mom_hs[kidiq$mom_hs != 0] <- 1
attach(kidiq)


## Linear regression of child iq vs. highschool (1) or not (2,3,4)
m3 <- lm(ppvt ~ as.factor(mom_hs) + momage)

## Plot Data and Model Fit
plot(ppvt ~ momage, col = colorRampPalette(c("gray80", "gray30"))(2)[as.factor(mom_hs)], pch = 16, main = "Child's IQ vs. Mom's Age", 
     ylab = "Child IQ", xlab = "Mom's Age")
legend("topright", c("No HS", "HS"), col = c("gray80", "gray30"), pch = 16)
legend("bottomright", c("HS Fit", "No HS Fit"), lwd = 3, col = c("forestgreen", "coral"))
abline(a = coefficients(m3)[1] , b = coefficients(m3)[3], lwd = 3, col = "coral")
abline(a = coefficients(m3)[1] + coef(m3)[2], b = coefficients(m3)[3], lwd = 3, col = "forestgreen")



detach(kidiq)
########################
## Part (d)
########################

# Finally, fit a regression of child test scores on mother’s age and education level f
# or the first 200 children and use this model to predict test scores for the next 200. 
# Graphically display comparisons of the predicted and actual scores for the final 200 children.

## Split data frame into first 200 children and last 200 children
f200 <- kidiq[1:200,]
l200 <- kidiq[201:400,]

## Create regression with first 200
m4 <- lm(f200$ppvt ~ f200$momage + f200$educ_cat)

## Predict IQ using the last 200 children and the model built on the first 200
IQ_predicted <- coefficients(m4)[1] + coefficients(m4)[2] * l200$momage + coefficients(m4)[3] * l200$educ_cat

## Display data
plot(IQ_predicted ~ f200$ppvt, pch = 16, xlim = c(20, 120), ylim = c(20, 120), col = mom_col, ylab = "Predicted IQ", xlab = "First 200 IQ", 
     main = "Child's IQ vs. Mother's Age \n & Education Level")

