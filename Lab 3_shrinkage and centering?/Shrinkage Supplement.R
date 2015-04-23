#### This is a supplementary file that just runs through getting the EB estimates and comparing them to the OLS estimates.
#### It only estimates a random effects ANOVA.

library(foreign) # reads SPSS files
library(nlme) # runs models
library(plyr) # is awesome (runs a function on subsets of the data)


nels<-read.spss("MLM_Lab 1_NELS88.sav", to.data.frame=TRUE)
head(nels)


# Run the Random Effects ANOVA

model<-lme(timeonmath~1, random=~1|Schoolid, data=nels, na.action=na.exclude, control=list(opt="optim"))

# Save the EB estimates

shrink<-coef(model)
colnames(shrink)<-c("EB intercept")


# Get the OLS estimates

slopesints<- ddply(nels,.(Schoolid), function(nels){
	model2<-lm(timeonmath~1,data=nels)
	coef(model2)
})

colnames(slopesints)<-c("Schoolid","OLS intercept")


# Save the EB estimates and the OLS estimates together
shrink<-cbind(shrink,slopesints)


# Compute the difference between the EB estimate (the weighted estimate that accounts for the u00, eij, and group size)
# and the OLS estimate (unweighted)

shrink$ind_diff<-shrink$"EB intercept"-shrink$"OLS intercept"


# Get the group ns and add them to the dataframe
n<-summary(as.factor(nels$Schoolid))
shrink<-cbind(shrink,n)

# Look at the estimates!

shrink

# You can also sort the rows by n:

shrink[order(n),]




## Added bonus: Recreate the EB estimate for school 25456 (see Lecture 10 p. 8-9): 0.77436405

g00<-summary(model)$coefficients$fixed[1]  # the fixed intercept
mean_25456<-0.8636364  # this is obtained from the OLS regression
t00<-0.6779546^2  # variance of the intercept (remember, nlme reports SDs not variances)
s2<-1.323817^2 # error variance
n_25456<-22
lambda_25456<-t00/(t00+(s2/n_25456))

eb_25456<-lambda_25456*mean_25456+(1-lambda_25456)*g00