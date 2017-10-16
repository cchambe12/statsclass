##############################################################################################################
##############################################################################################################
## The Folder NES contains the survey data of presidential preference and income for the 1992
## election analyzed in section 10.1, along with other variables including sex, ethnicity, educatin,party 
## identification, and political ideaology. 
##############################################################################################################
##############################################################################################################

## User Input
wd <- "/Users/Meghs/Documents/BioStats/ARM_Data/nes/"
setwd(wd)

## Requisite Packages
require(arm)
require(foreign)
require(plyr)

##################################################################################################
## (a) Fit a logistic regression predicitng support for Bush given all these inputs. Consider
##     how to include these as regression predictors and also consider possible interactions. 
##################################################################################################

dat <- read.dta("nes5200_processed_voters_realideo.dta")



















