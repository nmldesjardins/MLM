##### This lab provides a taste of longitudinal data analysis.
##### It is heavily plagiarized from the CASE longitudinal data workshop 1 code, written by Tyler Matta.
##### The full CASE code is also available on Blackboard.

#### LOAD PACKAGES ####

library(foreign) 	# reads data
library(reshape2) 	# restructures data
library(psych)		# handy for descriptives
library(ggplot2)	# plotting
library(lme4)		# run models
library(lmerTest)	# get significance tests	



#### READ DATA ####

# Note that this is a .dta file, which is indiginous to STATA, and needs to be read by foreign.
# convert.factors = T tells R to use the value labels for factors.

ei<-read.dta("earlyint_pp.dta", convert.factors=T)
head(ei)
str(ei)



#### RESTRUCTURE DATA ####

### BASIC TERMINOLOGY: 
###	Long format: Every L1 observation (i.e., every person's values for every time point) has a row
###				 Values that vary at L2 (but not L1) are repeated
###				 Useful for growth models in MLM
###				 This is the format we're used to seeing
### Wide format: Every L2 observation (i.e., every person) has a row
###				 Values that vary at L1 get their own variables (e.g., there's a variable for T1, a separate variable for T2, etc.)
###				 Useful for getting rank-order stability

### This dataset is in long format. How do we know that?
length(ei$id)

### Convert the dataset to WIDE format:

## DCAST:
# this will drop the time column all together, because it isn't included (because it's redundant with age)


ei.wide<-dcast(ei,
	id+program				# id and program are variables that we want to keep the same (they vary at L2)
	~age,  					# we want to create new variables based on the age variable
	value.var=c("cog"))		# those new variables should contain the value of the cog column

# rename columns for cosmetic purposes:
colnames(ei.wide)<-c("id","program","cog1","cog1.5","cog2")
head(ei.wide)


## RESHAPE:

ei_wide <- reshape(ei, 
	timevar = "time", 			# identifies the variable that will be used to create new variables for the time-varying measures
	idvar = c("id", "program"), # identifies variables that vary at L2 
	direction = "wide")			# which way we're going
	
head(ei_wide)



### Conver the wide dataset to LONG format:

## MELT:

ei.long<-melt(ei.wide,
	id.vars=c("id","program"),  # variable that vary at L2 (i.e., that identify people)
	variable.name="age", 		# name of the index variable (i.e., that codes time)
	value.name="cog")			# name of the variable with the measurement in it

# change the age variable to numeric values:
levels(ei.long$age)[levels(ei.long$age)=="cog1"]<-1
levels(ei.long$age)[levels(ei.long$age)=="cog1.5"]<-1.5
levels(ei.long$age)[levels(ei.long$age)=="cog2"]<-2

# sort by id:
ei.long<-ei.long[order(ei.long$id),]


## RESHAPE:

ei_long <- reshape(ei_wide, 
	varying= list(c("cog.0", "cog.0.5", "cog.1"), 
	c("age.0", "age.0.5", "age.1")), 				# variables in the wide dataset that vary
	v.names= c("cog", "age"), 						# names of the time variant variable in the new (long) dataset
	timevar= "time", 								# variable to be created that will code for time
	times= c(0, .5, 1), 							# values of the time variable
	direction= "long") 								# change to long format


# sort by id:
ei_long<-ei_long[order(ei_long$id),]



#### DESCRIPTIVES ####
## We'll use the original long format file (ei) for all of these.

## Get descriptives of the cog variable for each timepoint
# This uses the psych() package
# It produces descriptives of the variable (cog) for each timepoint (time)

describeBy(ei$cog,ei$time)

## Rank-order stability:

# This is easier to do with the wide dataset, but you may not want to restructure JUST to get some correlations.
# Instead, we'll subset the data:

c1<-subset(ei,time==0.0)
c2<-subset(ei,time==0.5)
c3<-subset(ei,time==1.0)

# Correlation between T1 and T2:
cor(c1$cog,c2$cog,use="pair")

# Correlation between T2 and T3:
cor(c2$cog,c3$cog,use="pair")

# Correlation between T1 and T3:
cor(c1$cog,c3$cog,use="pair")

## Spaghetti plot!

# Plot the slopes/intercepts of everyone:

spaghetti<-ggplot(data=ei, aes(y = cog, x = age)) # sets up initial plot
spaghetti + stat_smooth(method=lm,  			  # estimate a linear model with x and y from above
	aes(group=id, 								  # estimate separate models for each id	
	colour=factor(program)), 					  # plot people who were and were not in the program in different colors	
	se = F) +									  # don't print confidence intervals	
	ylab("Cognitive Function") + xlab("Age")      # set axis labels




#### BASIC LINEAR GROWTH MODELS ####

### Random effects ANOVA/Unconditional Means Model: Does the average level of cognitive performance vary across people?

m1 <- lmer(cog ~ 1 + (1 | id), data = ei)
summary (m1)


## Get ICC here - between-person variance in means at starting point 

L1.var<-as.data.frame(VarCorr(m1))[2,4]
L2.var<-as.data.frame(VarCorr(m1))[1,4]

icc1<-L2.var/(L1.var+L2.var)
icc1

### Unconditional growth model: Does cognitive performance change over time?

## Coding for time:
## 	We have two variables that code for time: age and time
##	Age is uncentered (i.e., there is no 0 value)
##  Time is centered at T1 and increases in steps of .5 (i.e, 0, 0.5, 1.5)
##	That is, I think, kind of confusing, since our coefficients are interpreted in 1-unit increases
##  Let's create a new time variable that's still centered at T1 but which increase in steps of 1, 
##	so a 1-unit increase captures change over 6 months:
		
ei$time2[ei$time==0]<-0
ei$time2[ei$time==0.5]<-1
ei$time2[ei$time==1.0]<-2

## Run the model, predicting cog from our centered time variable, with a random intercept and random slope:

m2<-lmer(cog~time2 + (time2|id), data=ei)
summary(m2)

## Get 2nd ICC here - between-person variance in slopes
 
L1.res.var<-as.data.frame(VarCorr(m2))[4,4]
L2.int.var<-as.data.frame(VarCorr(m2))[1,4]
L2.slope.var<-as.data.frame(VarCorr(m2))[2,4]

icc2<-L2.slope.var/(L1.res.var+L2.int.var+L2.slope.var)


### Conditional growth model: Is change in cognitive performance conditional on program enrollment?

## Add program as a predictor of both the intercept and the slope:

m3<-lmer(cog ~ time2*program + (time2|id), data=ei)
summary(m3)


### Model Comparisons:

## We can use the anova() function to run the chi-square test on the deviances of the nested models.
## Because we have different fixed effects in the models we're comparing, it's more appropriate to compare the deviances
## obtained from models estimated with full maximum-likelihood, rather than from models estimated with restricted estimation ML (REML).
## REML is the default estimation mode in lme4, nlme, and SPSS because it does a better job of estiamting the random effects, esp. w/ small samples.
## In the CASE syntax, you'll see the command "REML = F" in the lmer equations, and this is why.
## However, anova() automatically re-runs the models with ML and produces the chi-sq test based on those deviances.
## In my opinion, this is a better option than just estimating all of your models with ML, 
## because you want the robust estimates REML provides for substantive interpretations.

anova(m2,m3)


### Plot the cross-level interaction:

# save the fixed effects from m3
m3_fe <- fixef(m3)	

# write the equations for both groups as functions
prog0= function(x){													
	m3_fe[1] + m3_fe[3]*(0) + m3_fe[2]*(x) + m3_fe[4]*(0)*(x)	
	}			
prog1= function(x){
	m3_fe[1] + m3_fe[3]*(1) + m3_fe[2]*(x) + m3_fe[4]*(1)*(x)
	}

# make a dataframe that encodes the time variable
tmp1<-data.frame(x=seq(from=0,to=2,by=1))		

# set the base plot
m3_fit <- qplot(x, data=tmp1)					

# fancy it up:

m3_fit + 
	stat_function(fun=prog1, linetype="dashed") + # make a dashed line for the people in the program based on the function 
	 geom_text(aes(label="Program \n participants", x=1.95, y= 89, size=1)) + # label the program group line
	stat_function(fun=prog0) +
	 geom_text(aes(label="Non-\nparticipants", x=1.95, y= 63, size=1)) +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), # don't print grid lines
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = c(0, 1, 2), label=c("6 months", "12 months", "18 months"), "Age") +	# label the X axis
	scale_y_continuous(limits = c(min(ei$cog), max(ei$cog)),# restrict the y axis to be within the observed range of cog
	                   "Cognitive Performance Score") +		# label the y axis
	theme(legend.position="none") # don't print a legend (since you have the line labels)



#### POLYNOMIAL GROWTH MODELS ####

## These analyses uses the "wgt.dta" data file, which has 4 timepoints (except for participant # 45, who has 5 for some reason).

wgt<-read.dta("wgt.dta", convert.factors=T)
head(wgt)

# recode gender (1: male, 2: female) to (0: male, 1: female)
wgt$female <- wgt$gender -1


### Coding Time

## Notice that there are two variables that code for time:
##	"occ" is a fixed value from 1:4 that codes the measurement occassion
##  "age" is the participant's actual age in years (all are ~ 6wks @ T1, so these are fractional values)


# center occ @ t1
wgt$occ_c<-wgt$occ-1

# center age around average age @ t1:
wgt.t1<-subset(wgt, occ==1)
wgt$age_c<-wgt$age-(mean(wgt.t1$age))


# run conditional growth models to compare the effects of centering:
mod1<-lmer(weight~occ_c + (occ_c|id), data=wgt)
mod2<-lmer(weight~age_c + (age_c|id), data=wgt)
mod3<-lmer(weight~age + (age|id), data=wgt)


### Proceed with the uncentered age variable, and include the polynomial:

wgt$age2<-(wgt$age)^2

# Try getting the random effects of age, age2 and the intercept:
mod4<-lmer(weight~ age + age2 + (age + age2 | id), data=wgt)

# Remove the random effect of age2:
mod5<-lmer(weight~ age + age2 + (age | id), data=wgt)
summary(mod5)


# Include gender (female) as an L2 predictor of the intercept and both slopes:
mod6<-lmer(weight~age*female + age2*female + (age|id), data=wgt)
summary(mod6)

 