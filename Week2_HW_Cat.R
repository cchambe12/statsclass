### ARM - Chapter 3 Question 4
## Cat - 18 September 2017

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
library(haven)

# Set working directory and load data
setwd("~/Documents/git/statsclass/ARM_Data")
d<- read.dta("child.iq/child.iq.dta")

## 3.4: part A
mod<-lm(ppvt~momage, data=d)
display(mod)
# Model output
#lm(formula = ppvt ~ momage, data = d)
#coef.est coef.se
#(Intercept) 67.78     8.69  
#momage       0.84     0.38  
#---
#  n = 400, k = 2
#residual sd = 20.34, R-Squared = 0.01

## According to the linear model, mother's should give birth later, however the residual sd
## is high and the R-squared value is low. The intercept claims a child's IQ is 67.78 when the mother's is 0 years
# of age when they give birth. There are many flaws in this model.

# 3.4: part B
mod2<- lm(ppvt~momage + educ_cat, data=d)
display(mod2)
# Model output
#lm(formula = ppvt ~ momage + educ_cat, data = d)
#coef.est coef.se
#(Intercept) 69.16     8.57  
#momage       0.34     0.40  
#educ_cat     4.71     1.32  
#---
#  n = 400, k = 3
#residual sd = 20.05, R-Squared = 0.04

## According to the model, mother's education is a greater predictor than mother's age at birth.
# The residual sd is high again and the r-squared value is low again but the mother's education predictor
# is significant. Yes, conclusions about timing of birth have changed. 
