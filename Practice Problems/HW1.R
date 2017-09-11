require(ggplot2)
require(ggthemes)
require(dplyr)
require(tidyr)
require(effects)
rm(list=ls())


#Problem 2.7: 3
electric_Show <- read.table(header=T, "C:/Users/USER/Documents/School/Classes/2017 Fall/OEB 201 R Stats/Data/ARM_Data/electric.company/electric.dat")

#Fix data so that columns don't have so much info
e_S2 <- electric_Show %>% gather(Test, Score, treated.Pretest:control.Posttest)
e_S3 <- e_S2 %>%
          separate(Test, c("Treatment","Timing"))

#Make plot
ggplot(data=e_S3, aes(x=Timing, y=Score)) +
  theme_few()+
  geom_boxplot(aes(col=Treatment))+
  facet_wrap(~Grade)


#Problem 2.7: 4
  #Using childrens test scores as seen in 8.4
library(haven)
kidiq <- read_dta("C:/Users/USER/Documents/School/Classes/2017 Fall/OEB 201 R Stats/Data/ARM_Data/child.iq/kidiq.dta")
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
