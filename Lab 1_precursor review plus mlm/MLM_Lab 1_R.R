#### This file provides a primer on the precursors to Multilevel Modeling
#### and a basic introduction to MLM in lme4.
#### It corresponds to the MLM Lab 1 handout, and walks through disaggregated, aggregated and WABA
#### models, slopes and intercepts as outcomes, calculating the between-groups ICC,
#### and running a full random-coefficients regression.
#### The focus is on predicting math achievement from time spent on math homework.
#### The following packages are required: foreign, lmSupport, plyr, car, ggplot2, psych, lme4, and lmerTest.




### Opening an SPSS file ###

## First be sure that you've saved the data file in your working directory.
	# To get to the working directory:
	# R.64bit GUI: when the console is active, select File > Chg Dir
	# R Studio: Session > Change Working Directory > Set Working Directory
	# R: Misc > Change Working Directory
	# From the console: setwd(directory path)
	# Mac shortcut: command + D
	# Alternately, you can enter file.choose() on the command line to select the location.
## You can open an SPSS file (.sav) directly into R with the the foreign package.
## The "to.data.frame = TRUE" command is essential -- the default is that this is FALSE, which
## will load your data as a big ugly mess.
## You WILL get weird warning messages about "unrecognized record types." 
## These are just warnings; like all warnings in R, you can ignore them.

# Load the package

library(foreign)

# Read the data into an object called "nels"

nels<-read.spss("MLM_Lab 1_NELS88.sav", to.data.frame=TRUE)

# Check out your dataset

head(nels)

# Notice that you are already in a disaggregated dataset (where each row is an observation)

str(nels)


### Disagregated and Aggregated Models ###

## First, run a disaggregated model that predicts math achievement from time spent on homework.
## Because this is the disaggregated model, we won't account for group differences.


# Run the model, and save the results in an object called "disag".
# In R, GLM equations are written as "DV ~ IV".

disag<-lm(mathscore~timeonmath,data=nels)
summary(disag)

## Next, run the aggregated model. This model will only include the between-group effects.
## Because we want an aggregated analysis, we will first need to generate an aggregated data
## frame.

# Create the aggregated dataframe.
# This command will compute the mean (FUN=mean) of the specified variables ($variablename) in the data set (nels) 
# for each of the Schoolid (by...), will save the means and Schoolid into an object, will re-name the columns,
# and then will merge the average time and average math score variables into one dataframe (nelsagg).
# Alternately, you could just run the aggregate function on the whole dataframe, and generate a new dataframe
# in which all variables have been aggregated:
# nelsagg<-aggregate(nels, by=list(nels$Schoolid), FUN=mean, na.rm=TRUE)


timemean<-aggregate(nels$timeonmath,by=list(nels$Schoolid),FUN=mean,na.rm=TRUE)
colnames(timemean)<-c("Schoolid","time_mean")

mathmean<-aggregate(nels$mathscore,by=list(nels$Schoolid),FUN=mean,na.rm=TRUE)
colnames(mathmean)<-c("Schoolid","math_mean")

nelsagg<-merge(timemean, mathmean, by = "Schoolid")



# Look at what you made:
head(nelsagg)
# Take a more stringent look, especially noting the # of observations (i.e., rows):
str(nelsagg)

# Now, using the aggregated data, run the model and save the results as "agg"

agg<-lm(math_mean~time_mean,data=nelsagg)
summary(agg)


### Within and Between Analysis (WABA) ###

## WABA models use the disaggregated dataset,
## but they incorporate both within- and between-groups effects in the same model.
## The within-group effect is captured by the difference between each individual's score and their group mean.
## The between-group effect is captured by the difference between the group means and the grand mean.

# Add the group means to the disaggregated dataframe:

nels<-merge(nels,nelsagg,by="Schoolid")

head(nels)

# Compute the within-group effect: SCOREij - SCORE.j
nels$time_wi<-(nels$timeonmath-nels$time_mean)

# Compute the between-group effect: SCORE.j - SCORE..
nels$time_bt <-(nels$time_mean - (mean(nels$timeonmath)))

# Check your work -- the within variable should be different for each person, but the between variable should be the same for each person within a given group

head(nels)
tail(nels)

# Estimate the WABA model:

waba<-lm(mathscore~time_wi + time_bt, data=nels)
summary(waba)


### Calculating the ICC ###

## Next, we want to know how much between-group variability we have.
## To do so, we'll calculate an ICC. In order to do that, we first need the sums of squares.
## The easiest way to calculate the ICC will be to save the results of the ANOVA as an object.
## We'll then be able to use that object (instead of copying numbers) in our calculation.

# Run an ANOVA that predicts mathscores from schools.
# Save the sums of squares as an object called "ss".

icc1<-lm(mathscore~Schoolid,data=nels)
summary(icc1)
ss<-anova(icc1)
ss

## Hmmm...this doesn't match what SPSS gave us. What could be happening? 
## (Hint: when things get weird, check the df)

# Investigate the issue further by checking out the properties of the Schoolid variable.

str(nels$Schoolid)

# Now, run the ANOVA again, but specify that Schoolid is a factor.

icc1<-lm(mathscore~factor(Schoolid),data=nels)
summary(icc1)
ss<-anova(icc1)
ss

## Now we can calculate the ICC!

# ss$"Sum Sq"[1] translates to: the first entry in the variable/column called "Sum Sq" in the
# object called ss. If you look at ss, you'll see that this corresponds to the between-groups 
# sums of squares.

ICC<-(ss$"Sum Sq"[1]/(ss$"Sum Sq"[1] + ss$"Sum Sq"[2]))

# Convert the ICC to an easily intrepratable percentage.

ICC2<-print(paste("The grouping factor accounts for",round(ICC*100, digits=2),"% of the variance in math scores."))






### Does the grouping factor aid prediction? ###

## Our ICC gives us a pretty good clue, but let's find out for sure. 
## To do that, we'll run a set of hierarchical models that include the predictor (time), 
## dummy codes for each of the schools, and the dummy*time interactions.

# In order to get the R-squared change test, we need to get the package lmSupport.

library(lmSupport)

# First, generate the model with no grouping factor (i.e., the disaggregated model).

model1<-lm(mathscore~timeonmath,data=nels)

# Next, generate the model with the dummy-coded grouping factor.
# Note that, to add predictors, you just put "+ varname" into your formula.
library(psych)
model2<-lm(mathscore~timeonmath+dummy.code(Schoolid)[,1:9],data=nels)

## HOLD THE PHONE! Don't we have to create dummy codes? Nope. R is made of MAGIC. 
## The "dummy.code" command will automatically generate dummies. 
## By default, the last group is the referent group.
## By subsetting Schoolid (here, we asked it just to make dummies for the 9 schools), we ensure that our
## degrees of freedom are correct in the next step (without subsetting, it will count the referent group in your
## df); Note that this DOES NOT change your estimates.
## If you wanted the first school (instead of the last) to be the referent, you could subset it thusly:
## dummy.code(Schoolid)[,2:10]
## You do need the psych package loaded to use it, though.

## Another option for dummy coding is to use the "C" function
## C will let you specify which group is the referent (the first group is the default)
## It will also let auto-code Helmert and polynomial contrasts
## If we wanted dummy codes with the 4th school (# 24725) as the referent, it'd look like this:
## C(factor(nels$Schoolid), treatment, base=4)


# Let's see if adding the dummies aided prediction.

lm.deltaR2(model1,model2)

# Or, depending on your version of R:
modelCompare(model1,model2)

# Finally, generate the model with the interaction terms.
# By default, R will produce the main effects if you only include the interaction term
# in your formula. So, "lm(dv~iv1+iv2+iv1*iv2)" is equivalent to "lm(dv~iv1*iv2).

model3<-lm(mathscore~timeonmath*dummy.code(Schoolid)[,1:9], data=nels)

# See if the interactions aided prediction.

lm.deltaR2(model2,model3)
modelComare(model2,model3)



### Intercepts and Slopes as Outcomes ###

## Now we know that the schools vary in both their intercepts (model1 vs. model2) and their slopes (model2 vs. model3).
## Knowing that is all well and good, but let's see if we can find out why.
## To do so, we're going to predict the slopes and intercepts from a group-level variable, type of school.

# First, we need to run a regression on each group separately, 
# and save their intercepts and slopes in a dataframe.
# The most efficient way to do this is with my favorite package, plyr.

library(plyr)

# This script tells R to run our regression (model) for each school (ddply(...(Schoolid)).
# It will then save the coefficents (i.e., the slopes and the intercepts) from each regression. 
# The saved intercepts and slopes will be in the object "slopesints".

slopesints<- ddply(nels,.(Schoolid), function(nels){
	model<-lm(mathscore~timeonmath,data=nels)
	coef(model)
})

# For cosmetic purposes (so you don't have to type "(Intercept)" in quotes every time you need
# it), rename the variables in the dataframe.

colnames(slopesints)<-c("Schoolid","intercept","timeslope")

###### TANGENT! Let's say that all you wanted to do in R is to conveniently generate the slopes and intercepts, 
###### and then export them into a csv file so that you could proceed with your analyses there. 
###### Here's how you'd export the dataframe into an csv file that can be read by SPSS:
write.csv(slopesints,file="NELS88 slopes and intercepts.csv")

## The next thing we need to do is pull over the public/private school type variable from the original dataset.
## I am confident that there is a better way to do this, but this is what I got to work (let me know if you find something better!) 


# First, isolate the variables we want (schoolid and school type)

temp<-cbind(nels$Schoolid,nels$schooltype)
colnames(temp)<-c("Schoolid","schooltype")

# Then, filter it down to include only the unique cases (i.e., one row per group)

temp2 <-unique(temp)

# Finally, merge with the slopesints dataframe, matching cases based on the school id

data<-merge(slopesints,temp2, by = "Schoolid")

# Check out your handiwork

head(data)
str(data)

# R is a saucy minx, and she re-coded the school type variables without our consent. Let's change them back.
# This will make 0 = private and 1 = public.
# While we're at it, we'll make sure that R knows that school type is a categorical variable.

library(car)
data$schooltype<-factor(recode(data$schooltype,"1=0; 2=1"))


## Now, we'll finally predict those slopes and intercepts!

# Predict variability in the intercepts

int <-lm(intercept~schooltype,data=data)
summary(int)

# And now predict variability in the slopes from schooltype

slope<-lm(timeslope~schooltype,data=data)
summary(slope)




### Plotting Group Lines ###

## You may want to visualize your data to get a sense of how the groups vary from one another.
## Here are a few options; both of which utilize the dataset of intercepts and slopes that we computed above.
library(ggplot2)

# This will overlay regression lines on a scatterplot.
qplot(timeonmath,mathscore, data=nels) + geom_abline(data=slopesints,aes(intercept=intercept,slope=timeslope, colour=factor(Schoolid)))


# This plots just the lines, without the individual datapoints

ggplot() + scale_x_continuous(name="time", limits=c(0,7)) + scale_y_continuous(name="math", limits =c(30,70)) + scale_linetype(name="Schoolid") + geom_abline(data=slopesints,aes(intercept=intercept,slope=timeslope, colour=factor(Schoolid)))



### Run MLMs ###

## Like SPSS, the default estimation mode is REML.
## The basic formula structure is just like lm(): DV~IV1 + IV2...
## We add random components directly into our formula.
## Random components are defined as "(component|grouping variable)".
## Like lm, lmer will automatically estimate an intercept and fixed effects given the interaction.
## The intercept is noted as "1".


# load the MLM package + the package that will give you p values

library(lme4)
library(lmerTest)


## Random Effects ANOVA ##

# Get ICC for variance in math scores attributable to schools.

# Specify a model with a fixed intercept, a random intercept, and no predictors.
# The ICC is computed as the between-subjects (i.e., the intercept) variance / total variance (intercept + residual)
# Note that this ICC will be somewhat lower than the one you obtained using the GLM, above.
# That's because we're now using a different estimation procedure (REML vs OLS) that weights the variances by group size [if the groups were all identically sized, the results would be the same across both procedures].

empty<-lmer(mathscore~1+(1|Schoolid),data=nels)
summary(empty)


## Random Coefficient Model ##

# Estimate the full MLM, with timeonmath as the L1 predictor (X), schooltype as the L2 main effect (g01),
# and the cross-level interaction (g11) as fixed effects, and random slopes and intercepts.


# Here's how to estimate a model with random slopes and intercepts, but no slope/intercept covariance.
# This is equivalent to using covtype(vc) in SPSS.
# (1|Schoolid) is the random intercept, with school as the grouping variable.
# (0+timeonmath|Schoolid) is the random slope of timeonmath, with school as the grouping variable.
# Note: without the "0+", R will estimate an additional random intercept component (which makes no sense in this model).
 
rcr1<-lmer(mathscore~timeonmath*schooltype+(1|Schoolid)+(0+timeonmath|Schoolid), data=nels)
summary(rcr1)


# Here's how to estimate a model with random slopes and intercepts and the covariance between the slopes/intercepts.
# This is equivalent to using covtype(unr) in SPSS (the covariance is reported as a correlation). 

rcr2<-lmer(mathscore~timeonmath*schooltype+(timeonmath|Schoolid), data=nels)
summary(rcr2)


## Bonus! ##

# Random effects ANCOVA (random intercept, fixed slope, no L2 predictors)

rdnANCOVA<-lmer(mathscore~timeonmath+(1|Schoolid), data=nels)
summary(rdnANCOVA)

# RCR with no L2 predictors (random intercept and slope and slope/intercept covariance)

RCRnoL2<-lmer(mathscore~timeonmath+(timeonmath|Schoolid), data=nels)
summary(RCRnoL2)

# RCR with Main effect @ L2

RCRmeL2<-lmer(mathscore~timeonmath+schooltype+(timeonmath|Schoolid), data=nels)
summary(RCRmeL2)

# RCR with Cross-level interaction but no L2 ME
# Note that you change the * to : for the interaction, which tells R not to estimate the main effects

RCRxint<-lmer(mathscore~timeonmath+(schooltype:timeonmath)+(timeonmath|Schoolid), data=nels)
summary(RCRxint)

## Model Comparisons ##

## To compare model fit, get the difference in -2LL values.
## In lme4, this is called the "REML Criterion at Convergence".
## The difference is chi-square distributed, with df = # of additional parameters.
## You can do this manually, but why?! [This is kind of labor intensive for just one comparison, but if you are doing a lot of comparisons, you could automate it]
## Here's one way to compare the null model (empty) with the model that adds a predictor at L1 but still has a fixed slope and no L2 predictors (rdnANCOVA). These models differ by 1 parameter (the addition of the L1 fixed effect), so the difference in their -2LL is chi-square distributed with 1df.

# Save the analysis summaries as objects:
null<-summary(empty)
L1pred<-summary(rdnANCOVA)

# Get the -2LL for each model - notice that everything after the name of the summary object will always be the same ($"devcomp"$cmp[7] is where the -2LL is stored in the summary object)
nullLL<-null$"devcomp"$cmp[7]
L1predLL<-L1pred$"devcomp"$cmp[7]

# Get the difference between the -2LL values:
LLdiff<-nullLL - L1predLL

# Get the significance of the chi-square test (LLdiff is the chi-square value; 1 is the df):
1-pchisq(LLdiff,1)



## Plot the cross-level interaction ##

# quick and dirty plotting [note: I could not get this to work with the homework data]
nels$y <- predict(rcr2, nels)
with(nels, interaction.plot(timeonmath,schooltype,y))



# fancier plotting [this does work with the homework data]
# you can remove "+geom_point()" to remove the individual datapoints
gp <- ggplot(data=nels, aes(x=timeonmath, y=mathscore, colour=factor(schooltype))) 
gp + geom_point() + stat_smooth(method="lm")

# more accurate plotting:
library(effects)
plot(effect("timeonmath:schooltype",rcr2))