### Week 4 - 04 October 2017
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

# Set working directory and load data
setwd("~/Documents/git/statsclass/ARM_Data")
d<- read.dta("child.iq/child.iq.dta")

## Chapter 8 Question 2
## Part A
var1<-rnorm(1000, 0, 1)
var2<-rnorm(1000, 0, 1)
mod<-lm(var1~var2)
display(mod)
## lm(formula = var1 ~ var2)
#coef.est coef.se
#(Intercept) 0.02     0.03   
#var2        0.03     0.03   
#---
#  n = 1000, k = 2
#residual sd = 0.98, R-Squared = 0.00

## No, the slope coefficient is not statistically significant, most likely due to nonidentifiable and cannot be estimated uniquely.

## Part B & C
z_scores<-rep(NA, 100)
for(k in 1:100) {
  var1<- rnorm(1000, 0, 1)
  var2<- rnorm(1000, 0, 1)
  fake<- data.frame(var1, var2)
  fit<- lm(var2~var1, data=fake)
  z_scores[k]<-coef(fit)[2]/se.coef(fit)[2]
  
}

signif<-ifelse(abs(unique(z_scores))>=2, 1, 0)
signif.count<-as.data.frame(signif)
table(signif.count$signif)
## Only 2 out of 100 were "statistically significant"


## Chapter 9 Question 4
# Part A
log(10) - (2*log(50))
# intercept is -5.522
# log(weight) = -5.522 + 2*log(height) + error
# Factor of 1.1 means that 95% of the animal weights on upper bound would fall within 10% of the expected value
# therefore, the residual standard deviation would have that, which is 5% or 0.05
## 
r_sq<-1-((0.05^2)/(.2^2))
# r_sq is 0.9375


