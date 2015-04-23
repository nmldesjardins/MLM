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

### UPDATE 10.30.14: Skip all of this. 
### Comparing the EB estimates to the fixed effects don't give you much value-added - you want to compare the EB estimates to the OLS estimates.
### To clarify, I've commented out this whole section.


# To get a better idea of how much shrinkage there is for each group, let's compute the differences between the fixed model estimate and the group bs:

# to do this, we'll save the summary of the model as an object:
#x<-summary(model)

# then get the fixed estimate of the intercept:
# this is telling R to call the first instance [1] of the variable "fixed", which is in the table of coefficients ("coefficients") saved in the summary of the model ("x"). I know this because I used the command names(x), found something that looked like what I wanted (coefficients), got the names it had ((names(x$coefficients))), and picked the one I wanted.
#int<-x$coefficients$fixed[1]
# in lme4, this is:
# int<-x$coefficients[1,1]

# and the fixed estimate of the slope:
#slope<-x$coefficients$fixed[2]
# in lme4:
# slope<-x$coefficients[2,1]


# get the differences between the fixed estimates and the EB estimates for each school:

#shrink<-as.data.frame(cbind(coef(model),int-(coef(model)[,1]),(slope-coef(model)[,2])))
#colnames(shrink)<-c("EB intercept","EB slope", "Shrinkage_Int", "Shrinkage_Slop")


### UPDATE 10.30.14: instead, i'll just save the EB estiamtes as a dataframe:

shrink<-coef(model)
colnames(shrink)<-c("EB intercept", "EB slope")
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
	model2<-lm(timeonmath~ses,data=nels)
	coef(model2)
})

colnames(slopesints)<-c("Schoolid","OLS intercept","OLS slope")

shrink<-cbind(shrink,slopesints)

shrink


### UPDATE 10.30.14: Now, get the difference between the OLS and EB estimates:

shrink$int_diff<-shrink$"EB intercept"- shrink$"OLS intercept"
shrink$slope_diff<-shrink$"EB slope"- shrink$"OLS slope"


## You can also sort by school size (or whatever variable makes sense to you):
shrink[order(n),]






### UPDATE: 10.30.14: All that follows is still fine, though note that you want to compare the EB estimates to the OLS estiamtes (which we didn't get here)


### ADVENTURE TIME!
## Add the L2 predictor, schooltype, to the model as a predictor of both the intercept and the slope.
## Get the shrinkage and EB estimates for b00 and b10.
## HINT: b00 = g00 + g10TYPE; b10 = g01 + g11TYPE

# run the model:
model2<-lme(timeonmath~ses*schooltype, random=~ses|Schoolid, data=nels, na.action=na.exclude, control=list(opt="optim"))

# get EB estimates for gs; save as an object called "eb"
eb<-coef(model2)

# to make it easier to keep track of things, let's give each EB estimate a name:
eb_g00 <- eb[,1]   		# g00 = the first whole column of eb ("Intercept")
eb_g01_type <-eb[,3]	# g01 = the third whole column of eb ("type")
eb_g10_ses <-eb[,2]		# g00 = the second whole column of eb ("ses")
eb_g11_sesXtype<-eb[,4] # g00 = the fourth whole column of eb ("sesXtype")

# get fixed effects
x2<-summary(model2)

# save each fixed effect as an object for easy retrieval:
int2<-x2$coefficients$fixed[1]
ses2<-x2$coefficients$fixed[2]
type2<-x2$coefficients$fixed[3]
sesXtype2<-x2$coefficients$fixed[4]

# compute shrinkage estimates for each parameter + betas and combine


# this puts together, in a single dataframe:
	# n for each group: (summary(as.factor(nels$Schoolid)))
	# the EB estimates for each parameter: (eb)
	# the EB estimates for the betas: (eb_g00+eb_g01_type), (eb_g10_ses+eb_g11_sesXtype)
		# remember: because we have an L2 predictor, b00 = g00 + g01Type and b10 = g10 + g10Type
	# the shrinkage estiamtes for each parameter: (int2-eb_g00), (ses2-eb_g10_ses),(type2-eb_g01_type),(sesXtype2-eb_g11_sesXtype)
		# these are just the differences between the fixed parameter (which we saved as variables, above) and the corresponding EB estimate for that parameter	(which we also saved)
	# the shrinkage estimates for the betas (this is what you're actually interested in):((int2+type2)-(eb_g00+eb_g01_type)), ((ses2+sesXtype2)-(eb_g10_ses+eb_g11_sesXtype2))
		# again, because the betas are composed of g00+g01 and g10+g11, we need to add the corresponding fixed effects together to get the fixed estimate of the betas rather than the fixed estimates of the gs. 
		
		
shrink2<-as.data.frame(cbind((summary(as.factor(nels$Schoolid))),(coef(model2)),(eb_g00+eb_g01_type), (eb_g10_ses+eb_g11_sesXtype),(int2-eb_g00), (ses2-eb_g10_ses),(type2-eb_g01_type),(sesXtype2-eb_g11_sesXtype), ((int2+type2)-(eb_g00+eb_g01_type)), ((ses2+sesXtype2)-(eb_g10_ses+eb_g11_sesXtype))))

colnames(shrink2)<-c("n","EBg00","EBg10time", "EBg01type", "EBg11timeXtype","b00","b10", "Shrinkageg00", "Shrinkageg10time", "Shrinkageg01type", "Shrinkageg11timeXtype", "Shrinkageb00", "Shrinkageb10")

