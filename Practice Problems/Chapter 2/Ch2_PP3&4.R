## 11 September 2017 - Cat, Dave and Meghan
# Chapter 2 Practice Problems

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)


# Set Working Directory
setwd("~/Documents/git/statsclass/ARM_Data")
d<-read.table("arsenic/wells.dat", header=TRUE)


## 2.7 Exercise: Question 3
hist(d$arsenic) #histogram of raw data
ggplot(d, aes(x=dist, y=arsenic)) + geom_point()
ggplot(d, aes(x=dist, y=arsenic)) + geom_point(aes(color=as.factor(switch)))

## 2.7 Exercise: Question 4
ggplot(d, aes(x=dist, y=arsenic, color=as.factor(switch))) + geom_point(alpha=1/5) +
  geom_smooth(method=lm)

dev.new()
mod<-lm(arsenic~dist, data=d, subset=d$switch==1)
mod2<-lm(arsenic~dist, data=d, subset=d$switch==0)
plot(d$dist, d$arsenic)
abline(mod, col="blue")
abline(mod2, col="red")

par(mfrow=c(1, 2))
hist(d$arsenic[which(d$switch==1)])
abline(v=mean(d$arsenic),col="red")
hist(d$arsenic[which(d$switch==0)])
abline(v=mean(d$arsenic),col="red")
