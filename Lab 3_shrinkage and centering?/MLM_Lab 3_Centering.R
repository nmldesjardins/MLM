##### This document walks through two centering procedures:
##### grand-mean centering, which just removes the grand mean from each raw score;
##### and group-mean centering, which removes the group mean from each group member's raw score.
##### We'll use the time on math variable from the NELS 88 dataset.


## Load packages.
library(foreign) # reads SPSS files
library(plyr)    # required for the ddply function


nels<-read.spss("MLM_Lab 1_NELS88.sav", to.data.frame=TRUE)
head(nels)


# First, let's check out the original variable:
summary(nels$timeonmath)


#### Grand-Mean Center ####

# Now, let's create a new variable that's grand-mean centered
# All we have to do is subtract the mean of the whole sample:

nels$timeonmath_c<-(nels$timeonmath-(mean(nels$timeonmath)))

summary(nels$timeonmath_c)



#### Group-Mean Center ####

# Now, let's group-mean center it
# To do this, we'll use the plyr package and write a function that separates the file by group, computes the group mean, and then subtracts that mean from the original time variable.
# Fake bonus points to whomever gets this to work as a single function!

# get the group means
groupmeans<-ddply(nels,.(Schoolid),function(nels){
	mean<-mean(nels$timeonmath)
	
	})


# merge the group means into the dataset and rename them	
nels<-merge(nels,groupmeans)
head(nels)
names(nels)[names(nels)=="V1"]<-c("time_grpmean")

# group mean center
nels$time_grpc<-(nels$timeonmath - nels$time_grpmean)


# check it
summary(nels$time_grpc)

# even better, check the means by group
# the "by" command takes the inputs: variable, grouping factor, function
by(nels$time_grpc,as.factor(nels$Schoolid),summary)