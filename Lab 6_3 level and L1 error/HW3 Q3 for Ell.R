library(nlme)
data<-read.table("hw3a.txt",header=FALSE,col.names=c("district","respondant","year","attitude","gender","age83","religpref"))

head(data)

data$year_c<-data$year-1
data$year_c2<-data$year_c^2


# save output.
sink(file="H3Q3.doc", split=T)

## all models predict attitudes from a linear and quadratic growth term (year, centered @ year 1) 
## and religious preference.
## all models have only a random intercept (no random slopes)
## all models are 2-level, with time nested in people
## models differ only in their L1 error structure


# default model:

mod.homo<-lme(attitude~year_c+year_c2+religpref, random=~ 1|respondant, na.action=na.exclude, data=data, control=list(opt="optim")) 

summary(mod.homo)


# AR1 model:

mod.AR<-lme(attitude~year_c+year_c2+religpref, random=~ 1|respondant, correlation=corAR1(),na.action=na.exclude, data=data, control=list(opt="optim"))

summary(mod.AR)

# unstructured model:

mod.unst<-lme(attitude~year_c+year_c2+religpref, random=~ 1|respondant, 
correlation=corSymm(),  # freely estimate covariances
weights=varIdent(form=~1|year_c), # variances for each year can be different
na.action=na.exclude, data=data, control=list(opt="optim"))

summary(mod.unst)

# heterogenous model:
# uses default correlation structure (i.e., no covariances)
mod.hetero<-lme(attitude~year_c+year_c2+religpref, random=~ 1|respondant, 
weights=varIdent(form=~1|year_c),  # freely estimate variances for each year
na.action=na.exclude, data=data, control=list(opt="optim"))

summary(mod.hetero)

## test model differences [this is kind of weird because it reports LL instead of -2LL]

# default vs. AR1
anova(mod.homo,mod.AR)

# default vs. heterogenous
anova(mod.homo,mod.hetero)

sink()