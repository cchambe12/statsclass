### Week 5 - 11 October 2017
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
library(rstanarm)

# Set working directory and load data
setwd("~/Documents/git/statsclass/ARM_Data")


## Chapter 13 Question 3 - hmmmm not super confident
## Part A
n1 = ((2.8*0.5)/(0.01))^2
n100 = ((2.8*0.5)/(1))^2
n10000 = ((2.8*0.5)/(100))^2

## Part B
nl1 = ((2.8*0.5)/(log(0.01)))^2
nl100 = ((2.8*0.5)/(log(1)))^2
nl10000 = ((2.8*0.5)/(log(100)))^2


## Chapter 14 Question 2
## Part A
d<-read.dta("child.iq/kidiq.dta")
dx<-subset(d, mom_iq>=92)
p<-nrow(dx)/nrow(d)
dx<-dplyr::select(dx, mom_iq)
missing<-as.data.frame(lapply(dx, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.26, 0.74), size = length(cc), replace = TRUE) ]))
d.add<-subset(d, mom_iq<92)
d.add<-dplyr::select(d.add, mom_iq)
missing<-rbind(missing, d.add)
missing<-dplyr::rename(missing, miss=mom_iq)
df<-cbind(d, missing)
## Part B
d.miss<-df[!is.na(df$miss),]
mod<-stan_glm(miss~kid_score +mom_age+mom_hs, data=d.miss)
mod1<-stan_glm(mom_iq~kid_score +mom_age+mom_hs, data=df)
print(mod);print(mod1)
#stan_glm(formula = miss ~ kid_score + mom_age + mom_hs, data = d.miss)

#Estimates:
#  Median MAD_SD
#(Intercept) 83.3    7.3  
#kid_score    0.1    0.0  
#mom_age     -0.1    0.3  
#mom_hs       1.0    1.8  
#sigma       11.3    0.5  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 90.8    1.1  
#stan_glm(formula = mom_iq ~ kid_score + mom_age + mom_hs, data = df)

#Estimates:
#  Median MAD_SD
#(Intercept) 67.1    5.8  
#kid_score    0.3    0.0  
#mom_age      0.1    0.2  
#mom_hs       6.8    1.6  
#sigma       13.2    0.4  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 100.0    0.9 
### They are consistent, however the mom_hs has a bigger effect in the dataframe with no missing data
## Part C
mod2<-stan_glm(kid_score~miss +mom_age+mom_hs, data=d.miss)
mod3<-stan_glm(kid_score~mom_iq +mom_age+mom_hs, data=df)
print(mod2);print(mod3)
#stan_glm(formula = kid_score ~ miss + mom_age + mom_hs, data = d.miss)

#Estimates:
#  Median MAD_SD
#(Intercept) 37.6   15.2  
#miss         0.4    0.1  
#mom_age      0.1    0.5  
#mom_hs       9.4    3.1  
#sigma       19.7    0.9  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 80.9    1.9  
#stan_glm(formula = kid_score ~ mom_iq + mom_age + mom_hs, data = df)

#Estimates:
#  Median MAD_SD
#(Intercept) 21.2    9.3  
#mom_iq       0.6    0.1  
#mom_age      0.2    0.3  
#mom_hs       5.7    2.2  
#sigma       18.1    0.6  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 86.8    1.2  
## They are slightly less consistent but similar to the above.
## Part D
pred_1<- colMeans(posterior_linpred(mod2, newdata=d.miss))
impute<-function(a, a_impute){
  ifelse(is.na(a), a_impute, a)
}
mom_iq_imp<-impute(df$miss, pred_1)
final.fit<-stan_glm(df$kid_score~mom_iq_imp+df$mom_age+df$mom_hs)
print(final.fit);print(mod3)
#stan_glm(formula = df$kid_score ~ mom_iq_imp + df$mom_age + df$mom_hs)

#Estimates:
#  Median MAD_SD
#(Intercept) 78.7   11.4  
#mom_iq_imp  -0.1    0.1  
#df$mom_age   0.3    0.4  
#df$mom_hs   11.5    2.3  
#sigma       19.9    0.7  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 86.8    1.3  
#stan_glm(formula = kid_score ~ mom_iq + mom_age + mom_hs, data = df)

#Estimates:
#  Median MAD_SD
#(Intercept) 21.2    9.3  
#mom_iq       0.6    0.1  
#mom_age      0.2    0.3  
#mom_hs       5.7    2.2  
#sigma       18.1    0.6  

#Sample avg. posterior predictive 
#distribution of y (X = xbar):
#  Median MAD_SD
#mean_PPD 86.8    1.2  

### Anddddddd, now they're super different...

