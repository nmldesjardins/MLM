#### This document gives an introduction to running unit-specific and population-average binomial logistic multilevel models.
#### It re-creates the HLM example from Lecture 10 (starts p. 41)
#### For a lot more information about running unit-specific models, including plotting options and 
#### obtaining bootstrapped SEs, see http://www.ats.ucla.edu/stat/r/dae/melogit.htm




### Load packages:

library(foreign) 	# read SPSS files
library(lme4) 		# run unit-specific binary logistic mixed model
# install.packages("gee",dependencies=T)
library(gee) 		# run population-average binary logistic model 
						# NOTE: I chose gee because I found it first and it suits our needs. Other options include:
						# geepack - advantage: better handling of missing data if you're fitting a logistic growth model
						#	disadvantage: FREAKS OUT if you try running an unstructured model
						# multgee - advantage: handles multinomial DVs
						# for a functional comparison of gee and geepack, see:
						# http://www.unc.edu/courses/2010spring/ecol/562/001/docs/lectures/lecture14.htm




### Read data:

## First, read it in as usual:
atlas<-read.spss("ATLAS.sav", to.data.frame=T)
head(atlas)

## Then, read it in without the value labels:

# This will become important later, when we're using gee. It doesn't like
# that the DV (here, use0) has character labels, and it won't run if those labels are attached to the variable
atlas2<-read.spss("ATLAS.sav", to.data.frame=T, use.value.labels=F)
head(atlas2)





### Reproduce the HLM example from Lecture 10 (starts on p. 41):
### Prior to the intervention (intervention), do the schools differ in steroid use (use0)?

## Unit-Specific Model ##

# We'll estimate the unit-specific model in lme4.
# Instead of using lmer(), we'll use glmer().
# glmer() uses the same specification for fixed and random effects as lmer().
# family = binomial tells R that the DV is binomial and we want it to use a logit link function
# control = glmerControl(optimizer="bobyqa") is optional and specifies a different optimizer that will help the model converge
# nAGQ = 10 is also optional and increases the precision of estimation by increasing the number of "integration points" to 10

# First, estimate a null model in order to get the ICC:

null.mod<-glmer(use0~1+(1|schoolid), family=binomial, data=atlas)

# Recall that L1 variance (eij) is not estimated for these models. Instead, use the "known variance", pi^2/3:

icc.use<- .4759 / (.4759+(pi^2/3))



# Then, estimate the full model with no L1 predictors, 1 L2 predictor (intervention) and a random intercept:

us.mod<-glmer(use0~intervention+(1|schoolid), family=binomial, data=atlas)

summary(us.mod)


# Convert logits to odds by exponentiating (exp()) the fixed effects (fixef()):

exp(fixef(us.mod))



## Population-Average Model ##

# We'll estimate the population-average model using gee() in the gee package.
# The fixed effects specification is the same as ever.
# Instead of a random effect, we specify the nesting structure by including id=
# corstr = defines the correlation structure within each group. The options are "independent" - members of each group are uncorrelated with each other;
# 	"exchangeable" - every pairwise correlation within a group is held equal; and "unstructured" - all correlations are freely estimated.
#	 We'll run "exchangeable" and "unstructured" models. "Unstructured" are the most flexible, but that also makes it harder for them to converge
# scale.fix=T tells R to fix the scaling parameter to 1, which is what you want when you have binary data (if this isn't included or is FALSE, R estimates this parameter)
# NOTE: While the gee function is running, it will print estimates that are somewhere beween the unit-specific and final pop-average estimates - these are NOT the final model estimates.


# Try running the unstructured model with the original atlas data.frame (with value labels):

popav.mod1a<-gee(use0~intervention, id=schoolid, family=binomial, data=atlas, corstr="unstructured", scale.fix=T)

# Now try with atlas2 (no value labels):

popav.mod1b<-gee(use0~intervention, id=schoolid, family=binomial, data=atlas2, corstr="unstructured", scale.fix=T)

# Since the unstructured model didn't converge, try an exchangeable model:

popav.mod2<-gee(use0~intervention, id=schoolid, family=binomial, data=atlas2, corstr="exchangeable", scale.fix=T)

# For reasons I cannot explain, the full summary of a gee object gives you a lot of useful output AND a MASSIVE, useless correlation matrix:

summary(popav.mod2)

# To just get the coefficients and the SEs, use this:

(summary(popav.mod2)$coefficients)

# Convert the logits to odds (the logits -- the estimates of the model -- are the first column [,1] of the coefficients table)

exp(summary(popav.mod2)$coefficients[,1])



## Overdispersion ##

# I haven't found a way to deal with overdispersion that's similar to what HLM will give you (i.e., an estimate of overdispersion).
# Supposedly setting scale.fix=F in gee() will estimate a model that corrects for overdispersion.
# John also found this (search in text for "overdispersion"): http://glmm.wikidot.com/faq
# It gives a few options for testing for and dealing with overdispersion.





##

us.mod1<-glmer(use0~intervention+(1|schoolid), family=binomial, data=atlas)
popav.mod1<-gee(use0~intervention, id=schoolid, family=binomial, data=atlas, corstr="exchangeable", scale.fix=T)

exp(fixef(us.mod1))
exp(summary(popav.mod1)$coefficients[,1])


## ICC: use variance estimate from glmer model (only one with rdn effect + "known" estimate 3.29)

## Compute for 1-unit difference: just compute (like simple slopes)-use logits to compute, then exponentiatoe; odds ratio between the two expected odds should be the same as estimate of the odds ratio of the coefficient for that variable


#unit-specific.
us.mod2<-glmer(use0~coachtol0+reasons0+(1|schoolid), family=binomial, data=atlas)
summary(us.mod2)

# pop average.
popav.mod2<-gee(use0~coachtol0+reasons0, id=schoolid, family=binomial, data=atlas, corstr="exchangeable", scale.fix=T)


