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
## We're just going through some basic, built-in plotting.
## For more flexibility AND more diagnostic tests, see Loy & Hofmann, 2014
## for information about using the HLMdiag package.


##### LOAD PACKAGES.  #####
library(foreign)
library(ggplot2)
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

## fit the model

mod1<- lme(mathscore ~ timeonmath + meanSES, ~timeonmath|Schoolid, data=nels, na.action = na.exclude)

##### RESIDUAL PLOTS #####

### L1 PLOTS. ###

## boxplot of residuals by school (p. 14)
boxplot(resid(mod1)~nels$Schoolid, ylab="Residuals", xlab="School")

## plot of residuals vs predicted (fitted()) values

plot(resid(mod1, scaled=T)~fitted(mod1), xlab="Predicted Values", ylab="Standardized Residuals")

## histogram of standardized (scale()) residuals -- error distribution
hist(scale(resid(mod1)), main= "Distribution of L1 Residuals", xlab="Standardlized L2 Residuals")

# OR
hist(resid(mod1, scaled=T))

## qq plot
qqnorm(resid(mod1))


## by predictor

# time on math (in the model) (p. 18)
plot(resid(mod1)~nels$timeonmath, xlab="Math Homework", ylab="L1 Residuals")

# parent education (not in the model) (p. 19)
plot(resid(mod1)~nels$parented, xlab= "Parent Education", ylab= "L1 Residuals")


### L2 PLOTS. ###

## Get the EB residuals for the intercept and slope:
# recall:
l2eb<-coef(mod1) # gives you the EB estimates of the betas
colnames(l2eb)<-c("EB.Intercept.Est","EB.timeonmath.Est","meanSES.Est")
head(l2eb)

# for the residuals, we want the difference between those estimates and the model estiamtes:
l2eb.res<- ranef(mod1)

# we'll get the mean SES for each school, which will be used as a predictor:
grpm<-ave(nels$ses, nels$Schoolid)


l2eb.res<-cbind(l2eb.res,unique(grpm))
colnames(l2eb.res)<-c("EB.Intercept","EB.timeonmath","grp.meanSES")
head(l2eb.res)


## Plot residuals:

# residuals against fitted values:

plot(l2eb.res$EB.Intercept~l2eb$EB.Intercept.Est, ylab="EB Residuals - Intercept", xlab="EB Intercept")
plot(l2eb.res$EB.timeonmath~l2eb$EB.timeonmath.Est,ylab="EB Residuals - Slope", xlab="EB Slope")


# Plot the intercept residuals (the EB intercept residuals) against the mean of SES (p. 20)
plot(l2eb.res$EB.Intercept~l2eb.res$grp.meanSES, ylab= "EB Residuals - Intercept", xlab = "Mean SES")


# Plot the slope residuals (the EB residuals of timeonmath) against the mean of SES (p. 21)
plot(l2eb.res$EB.timeonmath~l2eb.res$grp.meanSES, ylab="EB Residuals - Slope", xlab="Mean SES")


## QQ-Plots:

# The built-in qqplot doesn't like to work here, so we'll use ggplot.

# QQ Plot of the intercept:
ggplot_qqnorm(x = l2eb.res[,"EB.Intercept"], line = "rlm")

# QQ Plot of the slope:
ggplot_qqnorm(x = l2eb.res[,"EB.timeonmath"], line = "rlm")








##### RESIDUAL PLOTS: USING HLMdiag #####
install.packages("HLMdiag", dependencies=T)
library(HLMdiag)
library(ggplot2)

## HLMdiag requires that models be fit with lme4:

mod2<- lmer(mathscore ~ timeonmath + meanSES + (timeonmath|Schoolid), data=nels, na.action = na.exclude)

## Get L1 residuals:

# Option 1: get EB residuals -- these are identical to using resid()
resid1a<-HLMresid(mod2, level = 1)

# Option 2: get standardized OLS residuals
# notice that you get a lot more output with this: the observed scores from the data,
# the residual, the predicted (fitted) value, and the standardized residual:

resid1b<-HLMresid(mod2, level=1, type="LS", standardize = T)
head(resid1b)

# Plot OLS residuals against fitted values:

p1<-qplot(x=resid1b$fitted, y = resid1b$LS.resid)
p1 + geom_point()+ geom_hline(yintercept=0, linetype="dotted") + 
theme_bw() + ylab("L1 OLS Residuals") + xlab("L1 Fitted Values")

# QQ plot:

ggplot_qqnorm(x = resid1b$std.resid, line="rlm")


# Double residual plot (p. 25-26)

plot(x=resid1b$LS.resid, y = resid1a, xlab="OLS residuals", ylab="EB residuals")



### L2 residuals 

# get the L2 residuals:

resid2<-HLMresid(mod2, level = "Schoolid")

# QQ plot of intercept:
ggplot_qqnorm(x = resid2[,"(Intercept)"], line = "rlm")

# QQ plot of slope:
ggplot_qqnorm(x = resid2[,"timeonmath"], line = "rlm")