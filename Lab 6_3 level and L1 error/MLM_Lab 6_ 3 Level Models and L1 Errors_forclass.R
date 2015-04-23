##### This lab reviews growth models, and demonstrates different L1 error structures.
##### It also introduces 3-level models.
##### Value-added bonus: reading .txt files is included in the 3-level model portion.
##### The data files are wgt.dat and hw3a.txt


#### LOAD PACKAGES ####

library(foreign) 	# reads data
library(ggplot2)	# plotting
library(psych)		# descriptives by group
library(nlme)		# run models; 
					# nlme offers a lot of flexibility with error structures 
					# lme4 does not allow you to change L1 error structures


##### GROWTH MODELS 2.0 #####
##### How does weight change with age in babies? #####

#### GET DATA ####

# These analyses uses the "wgt.dta" data file, which has "5" timepoints 

wgt<-read.dta("wgt.dta", convert.factors=T)
head(wgt)



#### CODING TIME ####

## Notice that there are two variables that code for time:
##	"occ" is a fixed value from 1:5 that codes the measurement occassion
##  "age" is the participant's actual age in years (all are ~ 6wks old @ T1, so these are fractional values)

# Examine the variability in age at each timepoint:

describeBy(wgt$age,wgt$occ)

# There are only 3 participants who have 5 timepoints. Let's get rid of them.

wgt<-wgt[wgt$occ<5,] # keep all the rows of wgt in which the occ value is less than 5

# Examine means again to make sure your subsetting worked:

describeBy(wgt$age,wgt$occ)

## Center Time:

# There's quite a bit of variability in the starting ages. 
# So, in real life, we'd want to use age as our time variable (instead of occasion), 
# since we're interested in changes associated with age.
# But! We'll eventually be estimating unstructured and heterogenous L1 error structures,
# which would try to give us a variance for EVERY age in our dataset.
# Those models won't converge, so we'll use measurement occassion as our time variable.

wgt$occ_c<-wgt$occ-1

## Compute Polynomials:

# Because we only have 4 timepoints, we'll just look at the quadratic effect.

wgt$occ_c2<-wgt$occ_c^2



#### DEFAULT (HOMOGENOUS) POLYNOMIAL GROWTH MODELS ####

## Unconditonal Means Model:

mod1<-lme(weight~1, random=~1|id, na.action=na.exclude, data=wgt)
summary(mod1)

## Unconditional Linear Growth:

mod2<-lme(weight~occ_c, random=~occ_c|id, na.action=na.exclude, data=wgt, control=list(opt="optim"))
summary(mod2)

## Compare:
anova(mod1,mod2)

# this will give you output, but will yell about REML comparisons.
# if you want to re-run the models with ML, add the command method="ML" to your equation:
mod1a<-update(mod1,method="ML")
mod2a<-update(mod2,method="ML")
anova(mod1a,mod2a)



## Unconditional Linear + Quadratic Growth:

mod3<-lme(weight~occ_c+occ_c2, random=~occ_c|id, na.action=na.exclude, data=wgt, control=list(opt="optim"))
summary(mod3)

## Compare:
# Note that we've only added a fixed quadratic effect
# We haven't changed the random effects
anova(mod2,mod3)



## Unconditional Linear + Quadratic Growth w/ Random Slopes:

mod4<-lme(weight~occ_c+occ_c2, random=~occ_c+occ_c2|id, na.action=na.exclude, data=wgt, control=list(opt="optim"))
summary(mod4)

## Compare:

anova(mod3,mod4)


## Plot the final model:

mod4est<-fixef(mod4)

# mod4est[1] = intercept
# mod4est[2] = occ_c
# mod4est[3] = occ_c2

# define the function of the curve based on the fixed effects:
linquad= function(x){
	mod4est[1] + mod4est[2]*(x)  + mod4est[3]*(x^2)
	}


# Dataset to define the x-axis [time= 0, 1, 2, 3]
tmp <- data.frame(x= min(wgt$occ_c) : max(wgt$occ_c) )

# set the base model:
fit <- ggplot(data=tmp, aes(x))

fit +  
	stat_function(fun=linquad) +     # plot the function
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks=c(0, 1, 2, 3), 
    	  labels=c("1", "2", "3", "4"), "Measurement Occassion") +  # define and label the x axis
	scale_y_continuous(limits = c(min(wgt$weight), max(wgt$weight)), # define and label the y axis to be within range
	                   "Weight") +
	theme(legend.position="none")





#### CHANGE L1 ERROR STRUCTURE ####

### The default in nlme/lme4 is to estimate a single variance @ L1 and no L1 covariances.
### Because we have the same people (L2) providing measurements at different times (L1),
### this assumption of homogeneity is likely to be violated.
### nlme has you mess with the covariances (correlation=) and the variances (varFunc) separately.

## Unstructured L1:

# This structure freely estimates all of the covariances and variances.

mod4.unst<-lme(weight~occ_c+occ_c2, random=~occ_c+occ_c2|id, na.action=na.exclude, data=wgt, control=list(opt="optim"), 
correlation=corSymm(),  # specify unrestricted covariances
weights=varIdent(form=~1|occ_c)) # estimate a separate variance for each occassion 

# Alternate specification using update():

mod4.unst<-update(mod4,correlation=corSymm(), weights=varIdent(form=~1|occ_c))

summary(mod4.unst)

## AR1 L1 Structure:

# This structure estimates a single variance (so we don't need to change the default),
# but estimates the covariances as a function of phi, which accounts for the 
# possibility that measurements closer together in time will be more highly correlated
# than measurements farther apart in time.

# For this to work properly, the data must be sorted by age.
wgt<-wgt[order(wgt$occ_c),]

mod4.AR<- mod5<-lme(weight~occ_c+occ_c2, random=~occ_c+occ_c2|id, na.action=na.exclude, data=wgt, control=list(opt="optim"), 
correlation=corAR1()) # this says the covariances are governed by an autoregressive structure

summary(mod4.AR)


mod3.AR<-lme(weight~occ_c+occ_c2, random=~occ_c|id, na.action=na.exclude, data=wgt, control=list(opt="optim"), 
correlation=corAR1()) # this says the covariances are governed by an autoregressive structure

summary(mod3.AR)

## Heterogenous L1 Structure:

# This structure allows for each variance to be estimated separately,
# but, like the default structure, the covariances are fixed to zero.
# NOTE: I removed the random effect of acceleration (occ_c2) to make this converge.

mod3.hetero<-lme(weight~occ_c+occ_c2, random=~occ_c|id, na.action=na.exclude, data=wgt, control=list(opt="optim"), 
weights=varIdent(form=~1|occ_c)) # estimate a separate variance for each occassion 

summary(mod3.hetero)


## Compare nested models:

# AR1 vs. homogenous:
anova(mod4, mod4.AR)
anova(mod3, mod3.AR)
# heterogenous vs. homogenous:
anova(mod3, mod3.hetero)


#######################################################

#### 3-LEVEL MODELS ####

### Read in the data:

data<-read.table("hw3a.txt",header=FALSE,
col.names=c("district","respondant","year","attitude","gender","age83","religpref"))


# check number of timepoints
summary(data$year)

# center time @ year 1
data$year_c<-data$year - 1.


### COMPARE 2- and 3-LEVEL MODELS 

## Unconditional Linear Growth: 2 level model

mod.2level<-lme(attitude~year_c, random=~ year_c |respondant, na.action=na.exclude, data=data)
summary(mod.2level)

## Unconditional Linear Growth: 3 level model

# the "/" in the random portion designates the nesting of the two factors
# the highest-level factor is listed first, so "district/respondant" specifies two random effects:
# respondant (L2) nested in district (L3) (time is L1)
# this is also what you'd do for a 3-level model in lme4

mod.3level<-lme(attitude~year_c, random=~ year_c |district/respondant, na.action=na.exclude, data=data)

# note that you now get two sets of random effects: one for district and one for respondent in district
# the fixed effects should be very close to what they were in the 2-level model
summary(mod.3level)

