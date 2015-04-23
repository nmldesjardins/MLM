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

# L1 units per L2
docs<-table(hdp$DID)
docs
summary(docs[]) # numbers of patients per doctor

# DV/IVs

summary(hdp$mobility) # DV: Degree of patient mobility
summary(hdp$Age) # L1: Patient age (years)
summary(hdp$CancerStage) # L1: Cancer stage (1-4)
summary(hdp$Experience) # L2: Dr experience (years)
summary(hdp$Lawsuits) # L2: Dr lawsuits 

# Grand-Mean Center age and experience
hdp$age_c<-hdp$Age-mean(hdp$Age)
summary(hdp$age_c)

hdp$exp_c<-hdp$Experience-mean(hdp$Experience)
summary(hdp$exp_c)

## Null Model:
# tumor size = b00 + u00
null<-lmer(mobility~1+(1|DID), data=hdp)
summary(null)

# Compute the ICC based on the variances:
as.data.frame(VarCorr(null))
L2var<-as.data.frame(VarCorr(null))[1,4]
L1var<-as.data.frame(VarCorr(null))[2,4]
L2var
L1var

icc<-L2var/(L2var+L1var)
icc


## One L1 Predictor:
# mobility = g00 + g10age_c + u00
m1<-lmer(mobility~age_c+(1|DID), data=hdp)
summary(m1)

# test for improved fit
anova(null,m1)

# mobility = g00 + g10age_c + u00 + u10
m2<-lmer(mobility~age_c+(age_c|DID), data=hdp)
summary(m2)

anova(m1,m2)

## a brief plotting excursion

plot(hdp$age_c, hdp$mobility, col=hdp$DID)

library(ggplot2)
ggplot(hdp,aes(x=age_c, y=mobility, colour=DID))+
        geom_point()+
        facet_wrap(~DID)+
        geom_smooth(method="lm")




## One L1 Predictor + One L2 Predictor:

# mobility = g00 + g01Lawsuits + g10age_c + u00 + u10
m3<-lmer(mobility~age_c + Lawsuits + (age_c|DID), data=hdp)
summary(m3)

anova(m2,m3)

# tumor size = g00 + g01Lawsuits + g10age_c + g11age_c*Lawsuits + u00 + u10
m4<-lmer(mobility~age_c*Lawsuits+ (age_c|DID), data=hdp)
summary(m4)

anova(m3,m4)

## Changing random effects
# Model 3, with just random intercept:
# tumor size = g00 + g01Lawsuits + g10age_c + u00 
m3.i<-lmer(mobility~age_c + Lawsuits + (1|DID), data=hdp)
summary(m3.i)

# Model 3, with just random slope:
# tumor size = g00 + g01Lawsuits + g10age_c + u10
m3.s<-lmer(mobility~age_c + Lawsuits + (0+age_c|DID), data=hdp)
summary(m3.s)
