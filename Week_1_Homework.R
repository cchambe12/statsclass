#########################################################################################
#######################################################################################
## Written by: Meghan Blumstein (blumsteinm@gmail.com)
## Cat and Dave
##
## Date written: 09/11/17
##
## Description: Homework for the first week of biostats
## 
## Inputs: Datasets from Gelman and Hill Book
##
## Outputs: Figures representing data in various ways
##
#######################################################################################
#######################################################################################
rm(list = ls()) ## clears all objects from workspace


#######################################################################################
##                               Excercise 2.7
#######################################################################################

## 1 & 2 See Papers


## 3.
## Load requisite packages and set directories
library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
library(haven)

wd <- "/Users/Meghs/Documents/BioStats/"
setwd("~/Documents/git/statsclass/ARM_Data")

## Load Requisite Data
kidiq <- read.dta("child.iq/kidiq.dta")

## Add Age Groups
kidiq$mom_age_group <- kidiq$mom_age
kidiq$mom_age_group[kidiq$mom_age_group <= 26] <- 0
kidiq$mom_age_group[kidiq$mom_age_group > 26] <- 1

## colors & parameters
attach(kidiq)
kid_col <- rgb(238, 211, 99, alpha = 100, max = 255)
mom_col <- rgb(161, 205, 202, alpha = 100, max = 255)
par(mfrow = c(2,2), mar = c(4.5, 4.5, 0.5, 0.5))
## Plot 1
plot(kid_score ~ mom_iq, pch = 16, col = "steelblue", xlab = "Mom IQ", ylab = "Child IQ")
text(70, 140, "A.")

## Plot 2
boxplot(kid_score ~ mom_hs, col = kid_col, xlab = "Mom High School", ylab = "Child IQ", names = c("No", "Yes"))
text(0.5, 140, "B.")

## Plot 3
hist(mom_iq, breaks = 15, freq = F, border = "white", col = mom_col, ylim = c(0, 0.03), xlim = c(0, 150), xlab = "IQ", main ="")
hist(kid_score, breaks = 15, freq = F, border = "white", col = kid_col, add = T)
legend("topleft", c("Mom", "Kid"), fill = c(mom_col, kid_col), bty = "n")
abline(v = mean(mom_iq), lwd = 2)
abline(v = mean(kid_score), lwd = 2, lty = 2)
text(105, .026, 100)
text(92, .028, 86.8)
text(0.5, 140, "C.")

## Plot 4
boxplot(kid_score ~ mom_age_group, col = kid_col, xlab = "Mom Age", ylab = "Child IQ", names = c("< 26", "> 26"))
text(5, .025, "D.")

detach(kidiq)

######## Cat's version
## 2.7 Exercise: Question 3
d<-read.table("arsenic/wells.dat", header=TRUE)
hist(d$arsenic) #histogram of raw data
ggplot(d, aes(x=dist, y=arsenic)) + geom_point()
ggplot(d, aes(x=dist, y=arsenic)) + geom_point(aes(color=as.factor(switch)))

### Dave's Version
#Problem 2.7: 3
electric_Show <- read.table(header=T, "electric.company/electric.dat")

#Fix data so that columns don't have so much info
e_S2 <- electric_Show %>% gather(Test, Score, treated.Pretest:control.Posttest)
e_S3 <- e_S2 %>%
  separate(Test, c("Treatment","Timing"))

#Make plot
ggplot(data=e_S3, aes(x=Timing, y=Score)) +
  theme_few()+
  geom_boxplot(aes(col=Treatment))+
  facet_wrap(~Grade)


## 4. 

## Set-up plots
par(mfrow = c(2,2), mar = c(4.5, 4.5, 0.5, 0.5))

## Model
m1 <- lm(kid_score ~ mom_hs + mom_iq, data = kidiq)
plot(kid_score ~ mom_iq, pch = 16, col = kid_col, xlab = "Mom IQ", ylab = "Child IQ", data = kidiq)
abline(m1$coefficients[1], m1$coefficients[3], lwd = 2)
abline(m1$coefficients[1] + m1$coefficients[2], m1$coefficients[3], lwd = 2)


### Cat's Version
## 2.7 Exercise: Question 4
ggplot(d, aes(x=dist, y=arsenic, color=as.factor(switch))) + geom_point(alpha=1/5) +
  geom_smooth(method=lm)

ggplot(d, aes(x=dist, y=arsenic, color=as.factor(switch))) +
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

### Dave's version
kidiq <- read_dta("child.iq/kidiq.dta")
attach(kidiq)
#Here we care about test scores vs. mothers age
plot(kid_score~mom_age)
lm1 <- lm(kid_score~mom_age)
abline(lm1)

plot(kid_score~mom_age)
lm2 <- lm(kid_score~mom_age*mom_hs)
coeflm2 <- coef(lm2)
coef(lm2)
abline(coeflm2[1]+coeflm2[3], coeflm2[2]+coeflm2[4], col="red") #mom did finish high school
abline(coeflm2[1], coeflm2[2]) #mom didn't finish high school



## Section 4.5

##4. 
x_minus_y <- c()
for(i in 1:1000){
  
  x_all <- rnorm(100, mean = 69.1, sd = 2.9)
  y_all <- rnorm(100, mean = 63.7, sd = 2.7)
  
  x <- mean(x_all)
  y <- mean(y_all)
  
  x_minus_y <- c(x_minus_y, x-y)
}
hist(x_minus_y, col = "forestgreen", border = "white", xlim = c(4, 7), ylim = c(0, 200), main = "1,000 Draws from a Normal Distribution (x-y)")
text(4.25, 150, paste("Mean = ", round(mean(x_minus_y), 1) ))
text(4.25, 140, paste("SD = ", round(sd(x_minus_y), 1) ))

text(4.25, 180, paste("True Mean = ", round(69.1 - 63.7,1) ))
text(4.25, 170, paste("True SD = ", round(69.1 - 63.7,1) ))

#### Cat's version
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


### Dave's version
#4.5: 4
difVect <- vector()
for (i in 1:1000){
  distMen <- rnorm(100,69.1,2.9)
  distWom <- rnorm(100, 63.7,2.7)
  
  meanMen <- mean(distMen)
  meanWom <- mean(distWom)
  diffMW <- meanMen-meanWom
  
  difVect <- c(difVect,diffMW)
}

meanDiff <- mean(difVect)
sdDiff <- sd(difVect)


















