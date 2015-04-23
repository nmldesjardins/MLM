### This lab goes through cross-classified models and 
### basic diagnostics.

###--------------------------------------------------------------------###
###						CROSS-CLASSIFIED MODELS						   ###	
###--------------------------------------------------------------------###

#### This part of lab goes through a basic cross-classified model.
#### The dataset is mostly fake, and is not NELS or ATLAS!
#### In it, perceivers (the participants) rated the emotions of a bunch of targets.
#### There are a total of 24 targets and 100 perceivers.
#### Every perceiver rated 1 of 4 sets of 6 targets.
#### The sets of targets are consistent across perceivers; each set
#### was rated by 25 perceivers.

#### We want to see how much of the variance in the emotion perceptions
#### is attributable to perceivers vs. targets.
#### In other words, we're answering two questions:
#### 	(1) Do people show a lot of bias or idiosyncrancies when they rate targets?
####		(i.e., is perceiver variance high?)
####	(2) Do people agree about their ratings of targets? (i.e., is target variance high?)
#### We can then try to explain the perceiver and target variance with L2 variables.


##### LOAD PACKAGES.  #####
library(foreign)
library(lme4)



##### GET DATA. #####

data<-read.spss("perception data.sav", to.data.frame=T, use.value.labels = F)
head(data)
summary(data)

## about the variables:
# percid: perceiver (participant) id
# targid: target id
# perc_emo: the perceiver's rating of the target's emotion
# actual_emo: the target's actual emotion
# targ_gender: target gender; 0 = male; 1 = female
# arousal/dominance/valence: perceiver emotions

## about the data:
# each row is a perception of one target from one perceiver
# perception is the L1 variable
# there are NO L1 predictors
# perceptions are nested in perceivers crossed with targets

## full vs partial crossing:
# Because each perceiver only saw a subset of the targets,
# these data are partially crossed. If each perceiver saw
# all 24 targets, it would be fully crossed. Both types
# of models are specified in the same way.
# We can see the crossing structure here (we can 
# also see that we have missing data):

xtabs(~percid + targid, data=data)



##### CROSS-CLASSIFIED MODEL #####

## Again, we want to know how much of the variance in the 
## perceptions is attributable to perceivers vs. targets.
## We have no L1 predictors, so we can only have an intercept + random effects.

model1<- lmer(perc_emo~1 + (1|percid) + (1|targid), data=data)
summary(model1)

### Get ICCs:
as.data.frame(VarCorr(model1))
p_var<-as.data.frame(VarCorr(model1))[1,4]
t_var<-as.data.frame(VarCorr(model1))[2,4]
err_var<-as.data.frame(VarCorr(model1))[3,4]

tot_var<-p_var+t_var+err_var

p_ICC<-p_var/tot_var
t_ICC<-t_var/tot_var

p_ICC*100
t_ICC*100


### Add a perceiver predictor:
## 	Does the perceiver's emotion explain some of the variance in their ratings?

model2<- lmer(perc_emo ~ valence + (1|percid) + (1|targid), data=data)
summary(model2)

### Add a target predictor:
## 	Does the target's gender influence perceptions of their emotions?

model3<- lmer(perc_emo ~ actual_emo + (1|percid) + (1|targid), data=data)
summary(model3)




###--------------------------------------------------------------------###
###								DIAGNOSTICS							   ###	
###--------------------------------------------------------------------###

## This goes through the Lecture 16 diagnostics (starting at p. 11).

##### LOAD PACKAGES.  #####
library(foreign)
library(ggplot2)
library(HLMdiag)
library(nlme)	# I'm switching to nlme here because the EB estimates
				# are output in a format that's easier to deal with




##### LOAD DATA. #####

## For these plots, we'll be using the NELS88 dataset.

nels<-read.spss("NELS88.sav", to.data.frame=T)

head(nels)
summary(nels)


##### FIT THE MODEL. #####

## compute mean SES by school

nels$meanSES<-ave(nels$ses,nels$Schoolid)


mod1<- lme(mathscore ~ timeonmath + meanSES, ~timeonmath|Schoolid, data=nels, na.action = na.exclude)

##### RESIDUAL PLOTS. #####

### L1 PLOTS.

## boxplot of residuals by school (p. 14)
boxplot(resid(mod1)~nels$Schoolid)

## plot of residuals vs predicted (fitted()) values
# GET LINE AT 0?
plot(resid(mod1, scaled=T)~fitted(mod1), xlab="Predicted Values", ylab="Standardized Residuals")

## histogram of standardized (scale()) residuals -- error distribution
hist(scale(resid(mod1)), main= "Distribution of L1 Residuals")

# OR
hist(resid(mod1, scaled=T))

## qq plot
qqnorm(resid(mod1))


## by predictor

# time on math (in the model) (p. 18)
plot(resid(mod1)~nels$timeonmath, xlab="Math Homework", ylab="L1 Residuals")

# parent education (not in the model) (p. 19)
plot(resid(mod1)~nels$parented, xlab= "Parent Education", ylab= "L1 Residuals")


### L2

# Get the EB estimates for the intercept and slope:
l2eb<-coef(mod1)
grpm<-ave(nels$ses, nels$Schoolid)
l2eb<-cbind(l2eb,unique(grpm))
colnames(l2eb)<-c("EB.Intercept","EB.timeonmath","EB.meanSES","grp.meanSES")
head(l2eb)


# intercept
plot(l2eb$"(Intercept)"~l2eb$"unique(grpm)")

# slope
plot(l2eb$timeonmath~l2eb$"unique(grpm)")