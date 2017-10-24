#######################################################################################
#######################################################################################
## Written by: 
##
## Date written: 10/24/2017
##
## OEB 201 Homeowrk Week 8; Evaluate models vis PCC from previous assignments
##
#######################################################################################
#######################################################################################

## Import packages
require(rstanarm)

## Read in Data
data(kidiq)

## Color Pallette
alpha <- 150
castle <- c(rgb(113, 42, 59, max = 255, alpha = alpha), rgb(229, 118, 97, max = 255, alpha = alpha), 
            rgb(248, 197, 140, max = 255, alpha = alpha), rgb(248, 231, 162, max = 255, alpha = alpha), 
            rgb(134, 221, 178, max = 255, alpha = alpha))

## Set up Models - Regressing if a child's iq is related to his mother's iq
m1 <- stan_glm(kid_score ~ mom_iq, data = kidiq, family = gaussian(link = "identity"))


## Plot Data with all simulated lines from model
draws <- as.data.frame(m1)
plot(kid_score ~ mom_iq, data = kidiq, pch = 16, xlab = "Mom IQ", ylab = "Kid Score")
for(i in 1:nrow(draws)){abline(draws$`(Intercept)`[i], draws$mom_iq[i], lwd = 1, col = castle[3])}

## Check data parameters predictions versus our own data - we can see the paramters predict a distribution that is centered
## around the mean, while our data has a longer tail off to the left. 
par(mfrow = c(3,2), mar = c(0, 0, 0 ,0), oma = c(5, 5, 0, 0))
hist(kidiq$kid_score, col = "darkblue", border = "white", breaks = 30, xlim = c(20, 140),ylim = c(0,80), axes = F, main = "")
axis(2, at = seq(0, 80, by = 20))
for(i in 1:5){
  b <- hist(rnorm(434, mean = mean(m1$y), sd = sd(m1$residuals)), col = "lightblue", border = "white", 
                   breaks = 30, xlim = c(20, 140), ylim = c(0,80), axes = F, main = "")
  if(i == 4 | i == 5){axis(1, at = seq(20, 140, by = 20))}
  if(i == 2 | i == 4){axis(2, at = seq(0, 80, by = 20))}
}
legend("topright", col = c("darkblue", "lightblue"), legend = c("y", "y{rep}"), lwd = 3)

## Check the estimated means versus the actual
pp_check(m1, plotfun = "stat", stat = "mean")












