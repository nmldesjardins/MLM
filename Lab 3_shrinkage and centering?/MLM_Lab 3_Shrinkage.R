#### This goes through procedures for getting the EB estimates of the betas.
#### For simple models without L2 predictors, R just produces these for you.
#### When you have L2 predictors, you have to do a little magic to get the betas.
#### This lab uses nlme, but the procedure is nearly identical in lme4.

library(foreign) # reads SPSS files
library(lattice) # plots
library(nlme) # runs models
library(plyr) # is awesome (runs a function on subsets of the data)


nels<-read.spss("MLM_Lab 1_NELS88.sav", to.data.frame=TRUE)
head(nels)


#### EB Estimates and Shrinkage.

### For these problems, we'll be looking at whether a student's SES (ses)
### predicts the time they spend on their math homework (timeonmath).


### Fun with plotting.

## Let's look at the intercepts and slopes of each school separately.
# Note that, here, we're plotting the OLS regression for each school-
# none of the estimates account for any of the data from other schools.

# enter IV and DV as formula; grouping factor is "as.factor"
# for type, "p" shows the datapoints, "r" draws the trendline and "g" inputs the grid in the background
# col sets the color of the datapoints; col.line sets the color of the line
# xlab and ylab give the axes names

xyplot(timeonmath~ses|as.factor(Schoolid), data=nels,type=c("p","r","g"),col="forest green", col.line="black",xlab="SES", ylab="Time on Math")



## Run the model and get the EB estimates

# First, run the model.
# the option "opt = "optim"" changes the optimizer from the current default, nlmimb, to the default from previous releases. I am not 100% sure about the differences between these, other than that "optim" tends to converge more often than "nlmimb."

model<-lme(timeonmath~ses, random=~ses|Schoolid, data=nels, na.action=na.exclude, control=list(opt="optim"))

# get the EB estimates for the gs:
coef(model)


# To get a better idea of how much shrinkage there is for each group, let's compute the differences between the fixed model estimate and the group bs:

# to do this, we'll save the summary of the model as an object:
x<-summary(model)

# then get the fixed estimate of the intercept:
# this is telling R to call the first instance [1] of the variable "fixed", which is in the table of coefficients ("coefficients") saved in the summary of the model ("x"). I know this because I used the command names(x), found something that looked like what I wanted (coefficients), got the names it had ((names(x$coefficients))), and picked the one I wanted.
int<-x$coefficients$fixed[1]
# in lme4, this is:
# int<-x$coefficients[1,1]

# and the fixed estimate of the slope:
slope<-x$coefficients$fixed[2]
# in lme4:
# slope<-x$coefficients[2,1]


# get the differences between the fixed estimates and the EB estimates for each school:

shrink<-as.data.frame(cbind(coef(model),int-(coef(model)[,1]),(slope-coef(model)[,2])))
colnames(shrink)<-c("EB intercept","EB slope", "Shrinkage_Int", "Shrinkage_Slop")

shrink

# for fun, let's add in the number of participants in each school:
n<-summary(as.factor(nels$Schoolid))
shrink<-cbind(shrink,n)

# get the means of the EB intercepts and slopes; compare to the model intercept and slope; blow your mind:
 mean(shrink$"EB intercept")
 int
 mean(shrink$"EB slope")
 slope


## for more fun, compare the EB estimates to the OLS estimates [refer to lab 1 for more info]:
slopesints<- ddply(nels,.(Schoolid), function(nels){
	model<-lm(timeonmath~ses,data=nels)
	coef(model)
})

colnames(slopesints)<-c("Schoolid","OLS intercept","OLS slope")

shrink<-cbind(shrink,slopesints)


shrink

### Here, it appears as though the school with the most students actually has the MOST shrinkage.
### Why might that happen?

### ADVENTURE TIME!
## Add the L2 predictor, schooltype, to the model as a predictor of both the intercept and the slope.
## Get the shrinkage and EB estimates for b00 and b10.
## HINT: b00 = g00 + g10TYPE; b10 = g01 + g11TYPE
