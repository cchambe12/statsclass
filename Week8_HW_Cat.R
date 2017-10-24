### Week 8 - 25 October 2017
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
d<-read.dta("child.iq/kidiq.dta")

## Model from last week... 
fit1<-stan_glm(kid_score~mom_hs, data=d)

### Simulate replications
sims<-as.matrix(fit1)
n_sims<-nrow(sims)

n<- length(d$kid_score)
score_rep<-array(NA, c(n_sims, n))
for(i in 1:n_sims){
  score_rep[i,]<-rnorm(n, sims[i,1], sims[i,2])
}

## Check the model fit...
checks<- function(x) {
  min(x)
}
checks_rep<-rep(NA, n_sims)
for(i in 1:n_sims){
  checks_rep[i]<-checks(score_rep[i,])
}

# Let's plot it out now!
hist(checks_rep, xlim=range(checks(score_rep), checks_rep))
lines(rep(checks(score_rep), 2), c(0,n))
## The minimum from the original model is much smaller than the smallest observations of the replications
# The model, therefore, does not capture the variation. 