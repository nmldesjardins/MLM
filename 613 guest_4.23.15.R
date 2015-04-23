########################### PSY 613 MLM Guest Lecture ##########################
############################### NMLD Apr 22 2015 ###############################

################################################################################
# This file uses a subset of the UCLA HDP dataset to demonstrate basic         #
# multi-level models in R. It requires packages lme4, lmerTest, and ggplot2.   #
# Examples include: testing 2-level (a) null models; (b) random coefficient    #
# models with (i) one L1 predictor, (ii) one L1 predictor + one L2 predictor,  #
# (iii) cross-level interactions, (iv) random intercepts, (v) random slopes,   #
# (vi) random intercepts + random slopes; (c) calculating ICCs; and (d) chi-sq #
# tests of comparative model fit. It's pretty cool.                            #
################################################################################


################################## Set-up ######################################
## Load libraries

library(lme4) # runs MLMs
library(lmerTest) # gets significance tests for lme4 models
# library(nlme) # runs MLMs (we won't use it, but it's a good option for more
                # complex covariance structures)


## Read in the Hospital, Doctor, Patient dataset from UCLA
## Info about the dataset: http://www.ats.ucla.edu/stat/r/pages/mesimulation.htm

hdp_full<-read.csv("http://statistics.ats.ucla.edu/stat/data/hdp.csv")
head(hdp_full)
str(hdp_full)

## This is a 3-level dataset, with patients(doctors(hospitals))
## Each row is a patient; DID = doctor; HID = hospital

# We'll just work with the biggest hospital, # 4

hdp<-hdp_full[which(hdp_full$HID==4),]
hdp$DID<-as.factor(hdp$DID)
str(hdp)


######################## Super Basic Descriptives ##############################
######################## (plus a little grooming) ##############################

# L1 units per L2
docs<-table(hdp$DID)
docs
summary(docs[]) # numbers of patients per doctor

# DV/IVs

summary(hdp$mobility) # DV: Degree of patient mobility
summary(hdp$Age) # L1: Patient age (years)
summary(hdp$Lawsuits) # L2: Dr lawsuits 

# Grand-Mean Center age and experience
hdp$age_c<-hdp$Age-mean(hdp$Age)
summary(hdp$age_c)

hdp$exp_c<-hdp$Experience-mean(hdp$Experience)
summary(hdp$exp_c)


############################# Fitting Models ###################################
# You generally always want to start with the null model, which is just the DV #
# predicted by an intercept and a random intercept. The ICC from this model    #
# tells you what percentage of the total variance is due to the grouping factor#
# (for us, this is doctors), and, in turn, whether or not MLM is appropriate   #
# (though the answer to this is almost always yes).                            #
# After testing the null model, we'll add a predictor at L1 and estimate random#
# intercepts and slopes. Then we'll add the main effect of an L2 predictor.    #
# Then we'll add the cross-level interaction. Then, we'll selectively remove   #
# the random components. Then, who knows? The world is our oyster.             #
################################################################################

################################## Null Model ##################################

# tumor size = b00 + u00
null<-lmer(mobility~1+(1|DID), data=hdp)
summary(null)

# Compute the ICC from the variances:
as.data.frame(VarCorr(null))
L2var<-as.data.frame(VarCorr(null))[1,4]
L1var<-as.data.frame(VarCorr(null))[2,4]
L2var
L1var

icc<-L2var/(L2var+L1var)
icc


############################### One L1 Predictor ###############################

## with random intercept only:
# mobility = g00 + g10age_c + u00
m1<-lmer(mobility~age_c+(1|DID), data=hdp)
summary(m1)

# test for improved fit
anova(null,m1)

## with random intercept and random slope:
# mobility = g00 + g10age_c + u00 + u10
m2<-lmer(mobility~age_c+(age_c|DID), data=hdp)
summary(m2)

anova(m1,m2)

## a brief plotting excursion:

plot(hdp$age_c, hdp$mobility, col=hdp$DID)

library(ggplot2)
ggplot(hdp,aes(x=age_c, y=mobility, colour=DID))+
        geom_point()+
        facet_wrap(~DID)+
        geom_smooth(method="lm")



################### One L1 Predictor + One L2 Predictor ########################

## with random slopes + random intercepts (unstructured):
# mobility = g00 + g01Lawsuits + g10age_c + u00 + u10
m3<-lmer(mobility~age_c + Lawsuits + (age_c|DID), data=hdp)
summary(m3)

anova(m2,m3)

## with random slopes + random intercepts but no covariance (variance comps):
m3.0<-lmer(mobility~age_c + Lawsuits + (1|DID)+(0+age_c|DID), data=hdp)
summary(m3.0)


## just random intercepts:
# tumor size = g00 + g01Lawsuits + g10age_c + u00 
m3.i<-lmer(mobility~age_c + Lawsuits + (1|DID), data=hdp)
summary(m3.i)

## just just random slopes:
# tumor size = g00 + g01Lawsuits + g10age_c + u10
m3.s<-lmer(mobility~age_c + Lawsuits + (0+age_c|DID), data=hdp)
summary(m3.s)



############### L1 Predictor + L2 Predictor + L1*L2 Interaction ################

# tumor size = g00 + g01Lawsuits + g10age_c + g11age_c*Lawsuits + u00 + u10
m4<-lmer(mobility~age_c*Lawsuits+ (age_c|DID), data=hdp)
summary(m4)

anova(m3,m4)

