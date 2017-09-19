require(arm)
require(rstanarm)
require(foreign)
require(ggplot2)
require(ggthemes)
require(dplyr)

rm(list=ls())

setwd("C:/Users/USER/Documents/School/Classes/2017 Fall/OEB 201 R Stats/Data/ARM_Data/child.iq/")

iq.data <- read.dta("child.iq.dta")

#(a) Fit a regression of child test scores on mother's age, display the data and fitted model, check assumptions, and
  #interpret the slope coefficient. When do you recommend mothers should give birth? What are you assuming in making
  #these recommendations?
attach(iq.data)
ggplot(data=iq.data, aes(x=momage, y=ppvt))+
  geom_point()+
  stat_smooth(method=lm)+
  labs(x="mom age", y="Child Test Score")+
  theme_few()

  lm1 <- lm(ppvt~momage)
  summary(lm1)
  
  
#(b)	Repeat this for a regression that further includes mother's education, interpreting both slope coefficients in
    #this model. Have your conclusions about the timing of birth changed?
  require(scatterplot3d)
  
  lm2 <- lm(ppvt~momage+educ_cat)
  summary(lm2)
  
  colFun <- colorRampPalette(c("red","blue"))
  colors <- colFun(length(ppvt))
  colors <- colors[as.numeric(iq.data$ppvt)]
  iq.data <- arrange(iq.data, ppvt)
  plot3d <- scatterplot3d(ppvt~momage+educ_cat, pch=16,color=colors)
  plot3d$plane3d(lm2)
  
  
  #(c) Now create an indicator variable reflecting whether the mother has completed high school or not. Consider
    #interactions between the high school completion and mother's age in family. Also, create a plot that shows the
    #separate regression lines for each high school completion status group.
  
  factorVect <- as.factor(educ_cat)
  tempDF <- model.matrix(~factorVect-1)
  isHS <- tempDF[,2]+tempDF[,3]+tempDF[,4]
  
  lm3 <- lm(momage~isHS)
  summary(lm3)
  lm4 <- lm(ppvt~isHS*momage)
  summary(lm4)
  
  iq.data <- cbind(iq.data,isHS)
  dataHS <- iq.data[iq.data$isHS==1,]
  dataNoHS <- iq.data[iq.data$isHS==0,]
  
  ggplot(data=iq.data, aes(x=momage, y=ppvt))+
    geom_point()+
    stat_smooth(data=dataHS, method=lm, se=F)+
    stat_smooth(data=dataNoHS, method=lm, col="red",se=F)+
    labs(x="mom age", y="Child Test Score")+
    theme_few()
  
  
  
  #(d) Finally, fit a regression of child test scores on mother's age and education level for the first 200 children
    #and use this model to predict test scores for the next 200. Graphically display comparisons of the predicted and
    #actual scores for the final 200 children.
  
  #The real data
  iq.data.short <- iq.data[1:200,]
  attach(iq.data.short)
  lm5 <- lm(ppvt~momage, data=iq.data.short)
  lm5Coefs <- coef(lm5)
  summary(lm5)
  #Find stdev of the data from the model
  stdevlm5 <- sigma(lm5)
  
  #Use real mom age values for the fake data
  xFake <- iq.data$momage[201:400]
  #Create fake data based on linear model and its error
  y <- lm5Coefs[1]+lm5Coefs[2]*xFake+rnorm(length(xFake),0,stdevlm5)
  #Make data frame with fake data
  fake <- rep(1,200)
  notFake <- rep(0,200)
  isFake <- as.factor(c(notFake,fake))
  allX <- c(iq.data.short$momage,xFake)
  allY <- c(iq.data.short$ppvt,y)
  allDataIQ <- data.frame(allX,allY,isFake)
  
  
  ggplot(data=allDataIQ, aes(x=allX, y=allY))+
    geom_point(pch=16,size=3,aes(color=isFake))+
    labs(x="mom age", y="Child Test Score", color="data")+
    scale_color_manual(values=c("red","blue"), labels=c("Real Data","Fake Data"))+
    stat_smooth(data=allDataIQ[allDataIQ$isFake==1,], method=lm, se=F)+
    stat_smooth(data=allDataIQ[allDataIQ$isFake==0,], method=lm, col="red",se=F)+
    theme_few()
  #hello    