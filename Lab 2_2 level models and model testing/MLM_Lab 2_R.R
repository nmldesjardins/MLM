
#### This lab provides an overview of 2-level multilevel models.
#### It goes through a number of different model specifications (e.g., with and without L2 predictors; with different random components)
#### and model comparisons of nested models.
#### It also introduces the nmle package, an alternative to lme4.

#### dataset: ATLAS.sav from Blackboard.
#### required packages: foreign, lme4, lmerTest, nlme



### Load the packages for this session: ###

library(foreign) # lets you read in SPSS files
library(lme4) # runs MLMs
library(lmerTest) # gets significance tests for lme4 models
library(nlme) # runs MLMs



### Read in the datafile: ###

## Remember to set your working directory first!

atlas<-read.spss("ATLAS.sav", to.data.frame=TRUE)
head(atlas)



### Perceptions of coach tolerance ###

## For these problems, we'll be trying to predict differences in students' perceptions of their coaches' tolerance for steroid use.
## First, we'll run the null/empty/unconditional model to see whether there is substantial variability in these perceptions across schools.
## Then, we'll see whether (a) the student's steroid use and (b) the school's intervention status explain that variability.

## Run the null model and obtain the ICC:

# First, run the model using lme4:
null<-lmer(coachtol0~1+(1|schoolid), data=atlas)
summary(null)

# Compute the ICC based on the variances:
icc<- .1138/(.1138+1.3345)
icc

# Next, let's run it with nlme:
null2<-lme(coachtol0~1,~1|schoolid,data=atlas, na.action=na.exclude)
summary(null2)

# Compute the ICC based on the VARIANCES:
icc2<-.337^2/((.337^2)+(1.155^2))
icc2


#### Notable differences between lme4 and nlme ####

### Now seems like as good a time as any to go over the differences between these two packages.
### They do the same thing, but in somewhat different ways.
### Specification:
##		Fixed effects: Both packages use the same specification for the fixed effects: dv~IV1+IV2...
##		Random effects: The basic specification of the random effects is the same: rdneffect | grouping variable
##			but the random effect is added, in parentheses, to the fixed effects formula in lme4
##			whereas it's specified as its own formula (separated from the fixed effects by a comma and starting with a ~) in nlme
##		Missing data: nmle demands to be told what to do with the missing data it encounters. The default is "na.fail", which means the model will 
##			fail to run and you'll get an error if missing data is encountered. "na.exclude" tells nmle to drop the cases that have missing data.
##			"na.omit" could also be used here, but "na.exclude" is more elegant if you find yourself needing to save residuals or create new dataframes.
### Output:
##		Model fit: lme gives you the -2LL ("REML criterion at convergence"), whereas nlme gives you the log likelihood ("logLik").
##			In order to do a deviance change test with nlme, you need to multiply the log liklihood by -2.
##			nlme also provides two additional fit statistics, AIC and BIC, which can be used to compare nested models.
##			More information about different types of fit statistics: http://davidakenny.net/cm/fit.htm
##		Random effects: lme reports both the variance estimates and the standard deviations; nlme only reports standard deviations.
##		Fixed effects: both packages should give you the same fixed effects, but the degrees of freedom (and thus the signficiance) will differ.



### Back to perceptions of coach tolerance ###

## We saw above that about 7% of the variance in perceptions of tolerance was attributable to schools.
## This means that most of the variance is attributable to students.
## Let's see if a student-level variable helps explain some of that variance.


## Are perceptions of tolerance related to steroid use?

# Maybe it's the case that we only think the intercepts vary randomly 
# i.e., the relationship between use and perceptions is the same across schools, but schools have different mean levels of tolerance
# So, let's start with a model that just has random intercepts:

model1<-lmer(coachtol0~use0+(1|schoolid), data=atlas)
summary(model1)

model1n<-lme(coachtol0~use0,~1|schoolid,data=atlas,na.action=na.exclude)
summary(model1n)


# Next, let's add in the random slopes (and covariance between intercepts and slopes):

model2<-lmer(coachtol0~use0+(use0|schoolid), data=atlas)
summary(model2)

model2n<-lme(coachtol0~use0,~use0|schoolid,data=atlas,na.action=na.exclude)
summary(model2n)


## Does adding the random slope (and the covariance) significantly improve the fit of our model?

# We could do that manually, but that's boring. Thanks, Jason, for this handy function!
# The function, called "deviance", takes two lme4 or nlme model (a and b) as input.
# It then calculates the difference between the -2LLs for each model (by pulling the loglikihood and multiplying it by -2), the df for the test, and the p value
# Then it retruns the results to you, in English.
# To work the function, first paste the whole thing, as is, into your console and hit enter (i.e., run the function)
# Nothing should happen.
# Then, whenever you want to compare models, just enter "deviance(model1, model2)", and voila!

deviance <- function(a, b) {
  diffneg2LL <- (-2*as.numeric(logLik(a))) - (-2*as.numeric(logLik(b)))
  dfneg2LL <- (attr(logLik(b), "df") - attr(logLik(a), "df"))
  p<-(1 - pchisq(diffneg2LL, dfneg2LL))
return(print(paste("The -2LL difference is ", round(diffneg2LL, digits=3), "with ", dfneg2LL, "df, p = ", round(p, digits=3))))
}



deviance(model1,model2)
deviance(model1n,model2n)


anova(model1,model2)
## Does intervention affect perceptions of steroid tolerance?

## We know that there is very little variation between schools, but let's look at a school-level predictor anyway.
## We'll also go back to the model that estimates all of the random components (so this is the same as model 2, but with L2 predictors)

model3<-lmer(coachtol0~use0*intervention+(use0|schoolid), data=atlas)
summary(model3)

model3n<-lme(coachtol0~use0*intervention,~use0|schoolid,data=atlas,na.action=na.exclude)
summary(model3n)

## We can compare this to the model that didn't have L2 predictors:
deviance(model2,model3)
deviance(model2n,model3n)





## Does self-esteem affect perceptions of steroid tolerance?

## We didn't get much action out of the intervention variable (and shouldn't have...).
## Maybe another student-level variable, self-esteem (SE0) will help explain perceptions:

# Start with the full model, estimating all possible random effects with an unstructured covariance matrix 
# Because SE0 is a student level variable, it can have its own random effect, as can the use*SE interaction
# By putting "use*SE0" in the random portion of our formula, we're telling R to estimate a random slope for the interaction and all lower-order (i.e., main) effects

model4<-lmer(coachtol0~use0*SE0+(use0*SE0|schoolid), data=atlas)
summary(model4)

model4n<-lme(coachtol0~use0*SE0,~use0*SE0|schoolid, data=atlas, na.action = na.exclude)
summary(model4n)



##### CHOOSE YOUR OWN ADVENTURE! ####
## The full model with 2 L1 predictors and all possible random components (model 4) didn't converge.
## That's sad. Make a model that (a) fits the data well and (b) converges!

### OPTION 1: INCREASE ITERATIONS

## Maximum liklihood is an iterative function - it tests a bunch of model solutions, and lands on the one that best fits the data (i.e., the model that is most likely, given the data)
## Simpler models will converge in fewer iterations than more complex models, and the default number of iterations is usually sufficient to get a reasonable solution.
## If the model doesn't converge, though, you may want to increase the number of iterations (i.e., force R to try more solutions before giving up)

# lme4:
# Change iterations to 20000
# For more information, see ?lmerControl
model4<-lmer(coachtol0~use0*SE0+(use0*SE0|schoolid), data=atlas, control=lmerControl(optCtrl=list(maxfun=20000)))

# nlme:
# Change the number of iterations for each iterative step:

# for more information about the controls you can change, see ?nlmeControl
model4n<-lme(coachtol0~use0*SE0,~use0*SE0|schoolid, data=atlas, na.action = na.exclude, control=nlmeControl(maxIter = 100, pnlsMAxIter = 50, mxMaxIter = 100, niterEM= 50))



### OPTION 2: REMOVE RANDOM EFFECTS.
## Rather than estimating the random intercept + 3 random slopes, you can remove one (or more).
## Here are a few options:

## No random slope for the interaction:
model5<-lmer(coachtol0~use0*SE0+(use0+SE0|schoolid), data=atlas)
summary(model5)

model5n<-lme(coachtol0~use0*SE0,~use0+SE0|schoolid, data=atlas, na.action = na.exclude)
summary(model5n)

## Only one random slope and random intercept:
# random slope for use:
model6<-lmer(coachtol0~use0*SE0+(use0|schoolid), data=atlas)
summary(model6)

model6n<-lme(coachtol0~use0*SE0,~use0|schoolid, data=atlas, na.action = na.exclude)
summary(model6n)

# random slope for self esteem:
model7<-lmer(coachtol0~use0*SE0+(SE0|schoolid), data=atlas)
summary(model7)

model7n<-lme(coachtol0~use0*SE0,~SE0|schoolid, data=atlas, na.action = na.exclude)
summary(model7n)

## All random slopes, but no random intercept:
model8<-lmer(coachtol0~use0*SE0+(0+use0*SE0|schoolid), data=atlas)
summary(model8)

model8n<-lme(coachtol0~use0*SE0,~0+use0*SE0|schoolid, data=atlas, na.action = na.exclude)
summary(model8n)

### OPTION 3: Remove the L1 Interaction (so you just have main effects)
model9<-lmer(coachtol0~use0+SE0+(use0+SE0|schoolid), data=atlas)
summary(model9)

model9n<-lme(coachtol0~use0+SE0,~use0+SE0|schoolid, data=atlas, na.action = na.exclude)
summary(model9n)