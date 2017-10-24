require(arm)
require(rstanarm)
require(foreign)
require(ggplot2)
require(ggthemes)
require(dplyr)

rm(list=ls())

setwd("C:/Users/USER/Documents/School/Classes/2017 Fall/OEB 201 R Stats/Data/ARM_Data/child.iq/")

iq.data <- read.dta("child.iq.dta")

### PPC ###

###build the model
lm1 <- stan_glm(ppvt~momage, data=iq.data)

###simulate replications
sims <- as.matrix(lm1)
n_sims <- nrow(sims)

n <- length(iq.data$ppvt)
y_rep <- array(NA, c(n_sims,n))
for (i in 1:n_sims) {
  y_rep[i,] <- rnorm(n, sims[i,1], sims[i,2])
}


###Visualize data
#original
hist(iq.data$ppvt)
#Simulation
par(mfrow=c(5,4))
for (i in 1:15) {
  hist(y_rep[i,])
}

#The original data seems to have a much wider range of observed values than do any of the simulations.
Test <- function(y) {
  max(y)-min(y)
}
test_rep <- rep(NA, n_sims)
for(i in 1:n_sims) {
  test_rep[i] <- Test(y_rep[i,])
}

###Visualize numerical comparison
hist(test_rep,xlim=c(0,Test(iq.data$ppvt)+5))
abline(v=Test(iq.data$ppvt))


#From this graph we can see that the variability in the original data is much higher than the variability in the simulated data. It is not a good model for this data.