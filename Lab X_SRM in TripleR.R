###--------------------------------------------------------------------------###
##							SRM in TripleR							  		  ##
##  	Or, the greatest thing you'll ever learn if you do group studies	  ##
###--------------------------------------------------------------------------###

###--------------------------------------------------------------------------###
##								PREAMBLE								  	  ##
##  DESIGN																	  ##	
## Round-robin designs, in which each member of the group makes judgments	  ##
## about every other member of the group (usually including themselves) are	  ##
## the most common design for social relations modeling (as far as I can 	  ##	
## tell). Because each person rates a slightly different group of targets	  ##
## (e.g., A rates B, C, D; B rates A, C, D; etc), this is a cross-classified  ## 
## model in which targets and percievers are crossed. Self-reports don't go   ##
## into the SRM, because making judgments about ourselves is quite different  ##
## from making judgments about other people. They can be (and often are) used ##
## to assess accuracy and bias in subsequent analyses. Each group must have   ##
## AT LEAST 4 members in order for the model to be estimated.				  ##
##																			  ##	
##	DATA FORMAT														          ##
## TripleR likes data to be in long format. Every row will contain the        ##
## perceptions of a single target provided by a single perceiver. Groups can  ##
## be different sizes, and missing data isn't usually a problem (unless it's  ##
## excessive - if fewer than 4 group members have valid data, that group gets ##
## kicked out of the analysis)												  ##	
##																			  ##	
##	QUESTIONS																  ##
## There are two overarching goals when you run SRM: (1) determine			  ##	
## the proportions of variance attributable to the target, perciever, and	  ##	
## dyad (in some cases -- estimating dyadic or relationship variance isn't    ##
## always possible); and (2) obtain the residuals to use in subsequent        ##
## analyses. In general, you never use predictors in SRM models. Instead, you ##
## save out the "effects" -- the residuals -- and can then look at            ##
## correlations amongst them or use other variables (e.g., individual diffs)  ## 
## to predict them. For a run down of the "9 questions of interpersonal       ##
## perception," which can be answered in this way, see David Kenny's website: ##
## http://davidakenny.net/ip/laing.htm 										  ##
##																			  ##
##	TERMINOLOGY																  ##
## TARGET: the person who is being rated; also, PARTNER						  ##
## PERCEIVER: the person doing the ratings; also, ACTOR						  ##
## TARGET VARIANCE: proportion of variance in a rating that's due to the	  ##
##   target; also, CONSENSUS - it's a measure of agreement amongst			  ##
##   the group members -- when target variance is high, group members		  ##
##   generally agree with each other about their ratings of each target		  ##
## PERCEIVER VARIANCE: proportion of variance in a rating that's due to the	  ##
##    perceiver; also, BIAS (loosely defined) - it's a measure of how much 	  ##
##    idiocyncrancy there is in the ratings -- when perceiver variance is     ##
##    high, group members are bringing a lot to the table, and ratings vary	  ##
##    systematically across perceivers 										  ##	
## RELATIONSHIP VARIANCE: proportion of variance in a rating that's due to	  ##	
##    that unique target-perciever relationship; this can only be estimated   ##
##    if there are 2+ indicators of the variable (e.g., rating "outgoing" and ##
##    "sociable" to get at extraversion), otherwise the relationship variance ##
##     goes into the error term												  ##
## TARGET EFFECT: this is the residual for the target -- it's the average 	  ##
##		rating of a particular target, controlling for perceiver and 		  ##
##		relationship effects												  ##
## PERCEIVER EFFECT: residual for the perceiver -- the average rating a 	  ##
##		particular perciever gives, controlling for target and relationship   ##
##		effects																  ##
###--------------------------------------------------------------------------###

###--------------------------------------------------------------------------###
##								 SET UP						  				  ##
## This dataset is from LDJ, Srivastava, Kufner & Back (under review).        ##
## Participants engaged in a leaderless group discussion (lgd); immediately   ##
## following the lgd, they provided perceptions of themselves and their group ##
## members on a number of dimensions.										  ##
###--------------------------------------------------------------------------###

# load packages
# install.packages("TripleR", dependencies=T)
library(TripleR)

# set the output formatting - this just tells tripleR to use the target/
# perceiver terminology instead of actor/parter terminology (which is the default)

RR.style("p")


# read data
lgd<-read.csv("srm2_rrlong_LGD.csv")
head(lgd)

## Things to notice:
# Each row contains the perceptions of one perceiever of one target.
# Perceivers (perc), targets (targ), and groups (group) are all identified with variables.
# Row 1 is a self perception -- perc = targ.
# The first 5 rows are all from the same perceiver - they rated themselves +
# their 4 group members.

#  subjid group percnum perc targ targnum cond b5out b5cold b5thoro b5nerv b5imag
#1   101A   101       1 1011 1011       1  0.5     8      6       6      1      9
#2   101A   101       1 1011 1012       2  0.5     7      8       8      4      6
#3   101A   101       1 1011 1013       3  0.5     9      5       7      2      7
#4   101A   101       1 1011 1014       4  0.5     6      7       9      3      8
#5   101A   101       1 1011 1015       5  0.5    10      4      10      0     10
#6   101B   101       2 1012 1011       1  0.5     8      3       8      6      9

###--------------------------------------------------------------------------###
##							VARIANCE ESTIMATES						  		  ##
## First, we'll run a few models to get the variance estimates of the         ##
## extraversion items (b5out and b5reser)  									  ##
###--------------------------------------------------------------------------###

## UNIVARIATE ANLAYSIS (one indicator)
# predict perceptions of outgoingness (b5out) from the crossed perceiver*target;
# we indicate that we have multiple groups with | group ("group" is the group
# ID variable).

RRout<-RR(b5out~perc*targ | group, data=lgd, na.rm=TRUE)
RRout


## OUTPUT

# You get estimates of all of the variance components.
# Note that the relationship variance is actually the error + relationship variance -
# the two can't be separated because we only have 1 indicator of outgoingness.
# The perceiver-target covariance indicates how much one's target effect
# (i.e., how you are rated) covaries with one's perciever effect (i.e., how you
# rate others).
# The relationship covariance is the covariance amongst relationship effects -
# if I like you, do you like me?
# Both of these covariances account for the non-independence in the ratings.

# If you give TripleR self ratings (like we did), it will also give you
# the estimates of assumed similarity and self-other agreement.


#[1] "Round-Robin object ('RR'), calculated by TripleR"
#[1] "Univariate analysis of one round robin variable in multiple groups"
#[1] "Univariate analyses for: b5out"
#[1] "Group descriptives: n =  22 ; average group size =  4.68 ; range:  4 - 5"
#                            estimate standardized    se t.value p.value
#perceiver variance             0.527        0.128 0.122   4.322   0.000
#target variance                1.776        0.432 0.302   5.886   0.000
#relationship variance          1.808        0.440 0.194   9.327   0.000
#error variance                    NA           NA    NA      NA      NA
#perceiver-target covariance    0.265        0.273 0.200   1.321   0.201
#relationship covariance       -0.492       -0.272 0.130  -3.797   0.001
#[1] "Perceiver effect reliability: .518"
#[1] "Target effect reliability: .783"
#NULL


#Partial correlations with self ratings (controlled for group membership):
                                                       r    t     df    p   
#self rating with Perceiver effect (assumed similarity) .030 .269  8.000 .789
#self rating with Target effect (self-other agreement)  .510 5.302 8.000 .000

## We can then compute the ICCs:

out_targICC<- 1.776/(.527+1.776+1.808)
out_percICC<- .527/(.527+1.776+1.808)

# We find that 43% of the variance is attributable to targets, and 12%
# is attributable to percievers. Together, this indicates that people
# agree a lot about how outgoing someone is.


## BIVARIATE ANALYSIS (2 indicators)
# In this analysis, we'll be predicting both of the extraversion
# items together. This allows us to separate relationship and error variance.

# First, we need to reverse code the reserved item so that both items
# are positively keyed (higher = more extraversion):

lgd$b5reser_r <- 12-lgd$b5reser

## Run the analysis:

RRext<-RR(b5out/b5reser_r~perc*targ | group, data=lgd, na.rm=TRUE)
RRext

## OUTPUT
# Note that you still get all of the variance estimates,
# but now the relationship and error variance can be separated.
# You also get the reliability of the relationship effect.

#[1] "Round-Robin object ('RR'), calculated by TripleR"
#[1] "Latent construct analysis of one construct measured by two round robin variables in multiple groups"
#[1] "Univariate analyses for: b5out/b5reser_r"
#[1] "Group descriptives: n =  22 ; average group size =  4.68 ; range:  4 - 5"
#                            estimate standardized    se t.value p.value
#perceiver variance             0.045        0.007 0.199   0.227   0.411
#target variance                2.051        0.337 0.351   5.839   0.000
#relationship variance          0.927        0.152 0.216   4.291   0.000
#error variance                 3.066        0.504    NA      NA      NA
#perceiver-target covariance    0.154        0.504 0.181   0.848   0.406
#relationship covariance       -0.259       -0.280 0.172  -1.509   0.146
#[1] "Perceiver effect reliability: .039"
#[1] "Target effect reliability: .780"
#[1] "Relationship effect reliability: .498"
#NULL


#Partial correlations with self ratings (controlled for group membership):
#                                                       r    t     df    p   
#self rating with Perceiver effect (assumed similarity) .305 2.860 8.000 .005
#self rating with Target effect (self-other agreement)  .519 5.432 8.000 .000


###--------------------------------------------------------------------------###
##							EXTRACTING EFFECTS						  		  ##
## This is really the meat of SRM analyses -- you want to extract the effects ##
## so that you can use them in other models, either as IVs or DVs. There are  ##
## a couple of ways to do this: (a) you can pull the effects piecemeal or     ##
## all at once, and (b) the effects can be group-mean centered (the default)  ##
## or not. The centered effects are what you would use most often - centering ##
## around the group means removes the group dependencies, so you can use the  ##
## centered effects in regular OLS models (e.g., predicting target effects    ##
## from condition in a regs regression). If you want to model changes in the  ##
## effects over time, you want to use the uncentered effects in a MLM so that ##
## you can examine mean-level changes. 										  ##
## 																			  ##
## You get (at least) 3 things here: target effects, perciever effects, and   ##
## the self-reports. You can also optionally get a self-enhancement index     ##
## (based on Kwan et al 2004), which captures the extent to which people over-##
## or under-estimate where they stand on the variable, controlling for how    ##
## they tend to be seen (target eff), how they tend to rate others (perc eff),##
## and how people tend to see themsleves in general (group mean of self 	  ##
## ratings).																  ##	
###--------------------------------------------------------------------------###

### OPTION 1: PIECEMEAL

# Effects are automatically generated when you run the RR formula.
# So, you can just call them from the RR object:

lgd_outeff<- RRout$effects
head(lgd_outeff)

## Things to notice:
# These are all group mean centered (the default) 
# Every row is now a person.
# .p = perciever effect; .t = target effect; .s = centered self rating
# What this means: subject 1011 tends to see other people as more outgoing than
# the group mean, controlling for the target effects (.p = 1.33), but is seen
# as somewhat less outgoing than other group members, controlling for the perc
# effects (.t = - .066), and sees themselves as slightly more outgoing than the mean
#(.s = .2)

#    id group.id    b5out.p     b5out.t b5out.s
#1 1011      101  1.3333333 -0.06666667     0.2
#2 1012      101  1.2666667 -1.33333333    -0.8
#3 1013      101  0.4000000  1.20000000     0.2
#4 1014      101 -1.9333333 -2.13333333    -0.8
#5 1015      101 -1.0666667  2.33333333     1.2
#6 1021      102 -0.9333333 -0.53333333    -0.2

## This is fine if you only want effects for, say, one variable. It is a 
## collossal pain in the ass if you have multiple variables - the standard
## round robin form that I use has 50-some items on it, and pulling effects
## this way is super inefficient. Thankfully, there's a script for that:


### OPTION 2: AUTOMATED

## Learning this was a huge victory for me - it runs
## SRM on all of the variables, and then produces a single dataframe
## that contains all of the effects. It is magic.
## The only drawback is that it won't produce the variance estimates for
## every variable; but you typically don't care a whole lot about those, or 
## only care for a handful of specific variables, and can run separate 
## models for them after saving all of the effects.

# First, define your variable list:
# Here, we'll just get the effects for the Big 5 items.

vars<-colnames(lgd)[8:17]

#> vars
# [1] "b5out"   "b5cold"  "b5thoro" "b5nerv"  "b5imag"  "b5reser" "b5trust"
# [8] "b5lazy"  "b5relax" "b5art"  

# Note that the general form of the formula is the same,
# but you don't provide a DV, and you do give it a list of variables.
# adding "index = "enhance"" will get us the self-enhancement index.

b5eff<-getEffects(~perc*targ| group, index="enhance", data=lgd, varlist=vars,na.rm=T)

head(b5eff)

## TA-DA! Now all of your effects are merged, and you can use them
## in other analyses. NOTE: making sure to include the na.rm = T IS VERY IMPORTANT.
## If that isn't included but you have missing data on ANY variable, that
## whole row gets deleted.

# .enhance is the enhancement index; for subject 1011, b5out.enhance = -1.066
# indicates that that person sees themselves as less outgoing than others see them
# (i.e., they're self-effacing or underestimating their level of outgoingness)

#    id group.id    b5out.p     b5out.t b5out.s b5out.enhance    b5cold.p
#1 1011      101  1.3333333 -0.06666667     0.2   -1.06666667  1.53333333
#2 1012      101  1.2666667 -1.33333333    -0.8   -0.73333333 -1.73333333
#3 1013      101  0.4000000  1.20000000     0.2   -1.40000000 -2.06666667
#4 1014      101 -1.9333333 -2.13333333    -0.8    3.26666667  1.33333333
#5 1015      101 -1.0666667  2.33333333     1.2   -0.06666667  0.93333333
#6 1021      102 -0.9333333 -0.53333333    -0.2    1.26666667 -0.06666667

## To get the uncentered effects, just add gm=TRUE to the command:
b5eff.gm<-getEffects(~perc*targ| group, index="enhance", data=lgd, varlist=vars,gm=T, na.rm=T)

head(b5eff.gm)

# TA-DA! Now you have effects that include the group means.
# This means the .s variables are just the raw self reports.
# The enhancement index doesn't change, because it (by definition) has to
# be group-mean centered.

#    id group.id  b5out.p  b5out.t b5out.s b5out.enhance b5cold.p b5cold.t
#1 1011      101 7.983333 6.583333       8   -1.06666667 5.833333 3.633333
#2 1012      101 7.916667 5.316667       7   -0.73333333 2.566667 5.566667
#3 1013      101 7.050000 7.850000       8   -1.40000000 2.233333 4.233333
#4 1014      101 4.716667 4.516667       7    3.26666667 5.633333 4.833333
#5 1015      101 5.583333 8.983333       9   -0.06666667 5.233333 3.233333
#6 1021      102 5.616667 6.016667       7    1.26666667 3.383333 2.983333