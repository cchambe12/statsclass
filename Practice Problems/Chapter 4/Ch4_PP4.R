## 11 September 2017 - Cat, Dave and Meghan
# Chapter 4 Practice Problems

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)

## Question 4.4
d<-data.frame(matrix(NA, nrow = 100, ncol = 2))
d<-d%>%rename(x=X1)%>%rename(y=X2)
for(i in 1:100){
  men<-rnorm(100, 69.1, 2.9)
  women<-rnorm(100, 63.7, 2.7)
  d$x[i]<-mean(men)
  d$y[i]<-mean(women)
}

x<-rnorm(1000, mean(d$x), sd(d$x))
y<-rnorm(1000, mean(d$y), sd(d$y))
height_diff<-x-y
print(mean(height_diff)) # 5.37 vs. exact -> 5.4
print(sd(height_diff)) # 0.418 vs. exact -> 0.4
hist(height_diff)



