### Design and Sample Size Demonstration
## Simulating the difference between effect size and sample size in a power analysis


### The power of Sunny Delight
## Kids who drink sunny d at a young age are more likely to be happier adults
## In our example, if a child drinks 1 bottle of sunny d a week at the age of 5, 
# then the amount of dopamine in the brain increases in adulthood. 
## The average adult has 45+/-5 ng/ml of dopamine, but kids who drink sunny d 
# increase that level to 55 ng/ml 
## The first study tests 1000 kids

library(display)

## What is the effect size in this example?
ES = (55-45)/5
SE = 5/(sqrt(1000))
### Andrew states that " it is generally better to double the effect size [theta] than to double the sample size [n]"
## here we can see that doubling the sample size decreases the standard error at a slower rate than doubling the effect size
# The SE: effect size influences the numerator, sample size influences the denominator     
    
nsunny = 2
rep = 500
ntot = nsunny*rep
sunny = gl(nsunny, rep, length=ntot)

sunnydiff = 10
sunnydiff.sd = 5
suns<-rnorm(ntot, 55, 2)

base <- 45
child <- base + c(suns)-mean(suns)
mm <- model.matrix(~(sunny)^2, data.frame=(sunny))

fake <- vector()
for (i in 1:length(ntot)){
  coeff <- c(child[i], 
             rnorm(1, sunnydiff, sunnydiff.sd)
  )
  
  dp <- rnorm(n = length(sunny), mean = mm %*% coeff, sd = 2)
  
  fake <- data.frame(dp=dp, sunny=sunny)  
}
hist(fake$dp[sunny==1])        
mean(fake$dp) # 45.6
sd(fake$dp) # 2.36
display(lm(dp~sunny, data=fake))
#lm(formula = dp ~ sunny, data = fake)
#coef.est coef.se
#(Intercept) 44.37     0.09  
#sunny2       2.56     0.13  
#---
#  n = 1000, k = 2
#residual sd = 1.98, R-Squared = 0.29     


### Alright, now let's double the effect size...
### Sunny D increases the level of dopamine to 90 ng/ml - we'll keep the sample size the same
nsunny = 2
rep = 500
ntot = nsunny*rep
sunny = gl(nsunny, rep, length=ntot)

sunnydiff = 20
sunnydiff.sd = 5
suns<-rnorm(ntot, 90, 2)

base <- 45
child <- base + c(suns)-mean(suns)
mm <- model.matrix(~(sunny)^2, data.frame=(sunny))

fake <- vector()
for (i in 1:length(ntot)){
  coeff <- c(child[i], 
             rnorm(1, sunnydiff, sunnydiff.sd)
  )
  
  dp <- rnorm(n = length(sunny), mean = mm %*% coeff, sd = 2)
  
  fake <- data.frame(dp=dp, sunny=sunny)  
}
hist(fake$dp)        
mean(fake$dp) # 51.6
sd(fake$dp) # 8.72
display(lm(dp~sunny, data=fake))
#lm(formula = dp ~ sunny, data = fake)
#coef.est coef.se
#(Intercept) 43.12     0.09  
#sunny2      16.94     0.13  
#---
#  n = 1000, k = 2
#residual sd = 2.03, R-Squared = 0.95


## And now, we double the sample size...
nsunny = 2
rep = 1000
ntot = nsunny*rep
sunny = gl(nsunny, rep, length=ntot)

sunnydiff = 10
sunnydiff.sd = 5
suns<-rnorm(ntot, 55, 2)

base <- 45
child <- base + c(suns)-mean(suns)
mm <- model.matrix(~(sunny)^2, data.frame=(sunny))

fake <- vector()
for (i in 1:length(ntot)){
  coeff <- c(child[i], 
             rnorm(1, sunnydiff, sunnydiff.sd)
  )
  
  dp <- rnorm(n = length(sunny), mean = mm %*% coeff, sd = 2)
  
  fake <- data.frame(dp=dp, sunny=sunny)  
}
hist(fake$dp)        
mean(fake$dp) # 49.7
sd(fake$dp) # 3.9
display(lm(dp~sunny, data=fake))
#lm(formula = dp ~ sunny, data = fake)
#coef.est coef.se
#(Intercept) 46.36     0.07  
#sunny2       6.60     0.09  
#---
#  n = 2000, k = 2
#residual sd = 2.08, R-Squared = 0.72


        