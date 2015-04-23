# Set working directory
getwd()

#For Mac users
#setwd("~/Dropbox/CASE/CASE_1")
# For Windows users
#setwd("C:/...")

#------------------------------- Hour 1 --------------------------------------#

#-----------------------------------------------------------------------------#
#------------------ Longitudinal Data Structures -----------------------------#
#-----------------------------------------------------------------------------#

# ----------------- Read in data ---------------------------------------------#

##install.packages("foreign")
library(foreign); library(reshape); library(ggplot2)


# Read in .dta data
tol_dat <- read.dta("./data/nys.dta", convert.factors=T)
head(tol_dat)

#------------------ Reshape from wide to long --------------------------------#

tol_long <- reshape(tol_dat, 
	varying= c("tol11", "tol12", "tol13", "tol14", "tol15"),
	v.names= "tol",
	timevar= "age",
	times= c(11, 12, 13, 14, 15),
	direction= "long")

tol_long <- tol_long[order(tol_long$id),]

## create time variable:
tol_long$time <- tol_long$age-11

head(tol_long, 20)


#-----------------------------------------------------------------------------#
#------------------ Descriptive Analysis of Change ---------------------------#
#-----------------------------------------------------------------------------#

#------------------ Plot individual change -----------------------------------#

p1 <- ggplot(data = tol_long, aes(y = tol, x = age)) 

p1 + geom_line() + 
	facet_wrap(~ id, ncol = 4) + 
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
   	ylab("Tolerance") + 
    xlab("Age")

#------------------ Plot individual change trajectories ----------------------#

p2 <- ggplot(data = tol_long, aes(y = tol, x = age))  

p2 + geom_point() + 
	 stat_smooth(method=lm, se=F) +
	 facet_wrap(~ id, ncol = 4) +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
   	 ylab("Tolerance") + 
     xlab("Age")

p2 + geom_point() + 
	 stat_smooth(method=loess) +
	 facet_wrap(~ id, ncol = 4) +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
	 	   panel.grid.minor = element_blank()) +
   	 ylab("Tolerance") + 
     xlab("Age")    
    
#------------------ Plot grouped change trajectories ----------------------#

p3 <- ggplot(data = tol_long, aes(y = tol, x = age))  

p3 + stat_smooth(method=lm, aes(group= id), se=F, size=.75, color="grey50") +
	 stat_smooth(method=lm, se=F, size=1.5, color="black") +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
	 ylim(0, 4) +
     ylab("Tolerance") + 
     xlab("Age")

p3 + stat_smooth(method=loess, aes(group= id), se=F, size=.75, color="gray50") +
	 stat_smooth(method=loess, se=F, size=1.5, color="black") +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
	 ylim(0, 4) +
     ylab("Tolerance") + 
     xlab("Age")


#------------------ Plot change trajectories by factors ----------------------#

p4_male <- ggplot(data = subset(tol_long, male==1), aes(y = tol, x = age))  

p4_male + stat_smooth(method=lm, aes(group= id), se=F, size=.75, color="grey50") +
	 stat_smooth(method=lm, se=F, size=1.5, color="black") +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
	 ylim(0, 4) +
     ylab("Tolerance") + 
     xlab("Age")

p4_female <- ggplot(data = subset(tol_long, male==0), aes(y = tol, x = age))  

p4_female + stat_smooth(method=lm, aes(group= id), se=F, size=.75, color="grey50") +
	 stat_smooth(method=lm, se=F, size=1.5, color="black") +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
	 ylim(0, 4) +
     ylab("Tolerance") + 
     xlab("Age")

#------------------ Plot change trajectories by factors ----------------------#

## create a logical variable T if exposure is greater than 1.145, F otherwise
tol_long$hiexp <- tol_long$exposure > 1.145


p4_hiexp <- ggplot(data = subset(tol_long, hiexp==T), aes(y = tol, x = age))  

p4_hiexp + stat_smooth(method=lm, aes(group= id), se=F, size=.75, color="grey50") +
	 stat_smooth(method=lm, se=F, size=1.5, color="black") +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
	 ylim(0, 4) +
     ylab("Tolerance") + 
     xlab("Age")

p4_loexp <- ggplot(data = subset(tol_long, hiexp!= T), aes(y = tol, x = age))  

p4_loexp + stat_smooth(method=lm, aes(group= id), se=F, size=.75, color="grey50") +
	 stat_smooth(method=lm, se=F, size=1.5, color="black") +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
	 ylim(0, 4) +
     ylab("Tolerance") + 
     xlab("Age")


#------------------------------- Hour 2 --------------------------------------#
#-----------------------------------------------------------------------------#
#------------------ The Multilevel Model for Change --------------------------#
#-----------------------------------------------------------------------------#
# library(foreign); library(reshape); library(lme4); library(ggplot2)

# Read in .dta data from ch.3
ei_l <- read.dta("./data/earlyint_pp.dta", convert.factors=T)

head(ei_l)
tail(ei_l)

ei_w <- reshape(ei_l, 
  timevar = "time",
  idvar = c("id", "program"),
  direction = "wide")

head(ei_w)

length(ei_w$id)

## take a random sample of 8 observations for visual analysis
## keep all variables, and sample w/o replacement 
set.seed(123)
ei_samp <- ei_w[sample(1:nrow(ei_w), 16,
  	replace=FALSE),]

ei_samp

## reshape the sample dataset from wide to long
ei_samp_l <- reshape(ei_samp, 
	varying= list(c("cog.0", "cog.0.5", "cog.1"), 
				  c("age.0", "age.0.5", "age.1")),
	v.names= c("cog", "age"),
	timevar= "time",
	times= c(0, .5, 1),
	direction= "long")	
ei_samp_l <- ei_samp_l[order(ei_samp_l$id),]

#------------------ Empirical Growth Plot ------------------------------------#

ei_p1 <- ggplot(data = ei_samp_l, aes(y = cog, x = age))  

#pdf("./tex/ei_p1.pdf")
ei_p1 + geom_point() + 
	 stat_smooth(method=lm, se=F) +
	 facet_wrap(~ id, ncol = 4) +
   	 theme_bw() +
   	 theme(panel.grid.major = element_blank(), 
		   panel.grid.minor = element_blank()) +
	 scale_x_continuous(breaks = c(1, 2)) +
   	 ylab("Cognitive Performance Score") + 
     xlab("Age") 
#dev.off()

#------------------ HLM Model Output Function ---------------------------------#

hlm.output <- function(x,npar=TRUE,print=F) {

options(scipen=999)

## Fixed effects table
cc <- fixef(x)
se <- sqrt(diag(vcov(x)))     
coef.table <- round(cbind("Est."= cc, 
						  "SE"= se, 
						  "z"= cc/se, 
						  "95CIL"= cc - (se * 1.96), 
						  "95CIU"= cc + (se * 1.96)),3)
							 
## Variance Covariance Table
var.cov <- as.data.frame(VarCorr(x))[,c(1, 2, 3, 4)]

# Deviance, df and BIC
mod_dev <- -2*(as.numeric(logLik(x)))
mod_df <- as.numeric(attr(logLik(x), "df"))
mod_bic <- AIC((ll <- logLik(x)), k = log(attr(ll,"nobs")))

mod_fit <- as.data.frame(c(mod_dev, mod_df, mod_bic),
	row.names= c("Deviance", "df", "BIC"))
colnames(mod_fit) <- "Model Fit"

cat("\nFixed Effects\n"); print(coef.table)
cat("\nVarinace Components\n"); print(var.cov)
print(mod_fit)
}

#------------------ The unconditional means model ----------------------------#

m1 <- lmer(cog ~ 1 + (1 | id), 
	data= ei_l, 
	REML= F)
	
summary(m1)

m1.out <- hlm.output(m1)


#------------------ The unconditional growth model ---------------------------#

m2 <- lmer(cog ~ time + (time| id), 
	data= ei_l, 
	REML= F)
	
summary(m2)

m2.out <- hlm.output(m2)

#------------------ Conditional growth model ---------------------------#

m3 <- lmer(cog ~ time + time*program + (time| id), 
	data= ei_l, 
	REML= F)
	
summary(m3)

m3.out <- hlm.output(m3)


#------------------ Plot M3 Fitted trajectories ------------------------------#

m3_fe <- fixef(m3)
prog0= function(x){
	m3_fe[1] + m3_fe[3]*(0) + m3_fe[2]*(x) + m3_fe[4]*(0)*(x)
	}
prog1= function(x){
	m3_fe[1] + m3_fe[3]*(1) + m3_fe[2]*(x) + m3_fe[4]*(1)*(x)
	}

tmp1 <- data.frame(x= min(ei_l$time) : max(ei_l$time))

m3_fit <- qplot(x, data=tmp1)

#pdf("./tex/m3_fit.pdf")
m3_fit + 
	stat_function(fun=prog1, linetype="dashed") + 
	 geom_text(aes(label="Program \n participants", x=1.95, y= 89, size=1)) +
	stat_function(fun=prog0) +
	 geom_text(aes(label="Non-\nparticipants", x=1.95, y= 63, size=1)) +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = c(0, 1), label=c(1, 2), "Age") +
	scale_y_continuous(limits = c(min(ei_l$cog), max(ei_l$cog)),
	                   "Cognitive Performance Score") +
	theme(legend.position="none")
#dev.off()

#-----------------------------------------------------------------------------#
#------------------------------- Hour 3 --------------------------------------#
#-----------------------------------------------------------------------------#
#-------------- Extending the Multilevel Model for Change --------------------#
#-----------------------------------------------------------------------------#

# library(foreign); library(reshape) ;library(ggplot2); library(lme4)
# library(HLMdiag)


# Read in .dta data from ch.3
alc_l <- read.dta("./data/alc_pp.dta", convert.factors=T)

head(alc_l)
tail(alc_l)

#------------------------ Preparing IVs -------------------------------------#

alc_w <- reshape(alc_l, 
  timevar = c("age"),
  idvar = c("id", "coa", "male", "peer"),
  direction = "wide")

## Visual of IVs
ggplot(data = melt(alc_w[, -c(1, 5, 6, 7)]), aes(x = factor(value))) +
	geom_histogram(position = "identity") + 
	facet_wrap(~ variable, scales = "free", as.table = TRUE, nrow = 2) +
	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) 

## centering variables:

alc_l$age_14 <- alc_l$age-14
alc_l$cpeer <- alc_l$peer-mean(alc_l$peer)
alc_l$ccoa <- alc_l$coa-mean(alc_l$coa)

#-----------------------------------------------------------------------------#
#------------------ HLM Model Output Function --------------------------------#

hlm.output <- function(x,npar=TRUE,print=F) {

options(scipen=999)

## Fixed effects table
cc <- fixef(x)
se <- sqrt(diag(vcov(x)))     
coef.table <- round(cbind("Est."= cc, 
						  "SE"= se, 
						  "z"= cc/se, 
						  "95CIL"= cc - (se * 1.96), 
						  "95CIU"= cc + (se * 1.96)),3)
							 
## Variance Covariance Table
var.cov <- as.data.frame(VarCorr(x))[,c(1, 2, 3, 4)]

# Deviance, df and BIC
mod_dev <- -2*(as.numeric(logLik(x)))
mod_df <- as.numeric(attr(logLik(x), "df"))
mod_bic <- AIC((ll <- logLik(x)), k = log(attr(ll,"nobs")))

mod_fit <- as.data.frame(c(mod_dev, mod_df, mod_bic),
	row.names= c("Deviance", "df", "BIC"))
colnames(mod_fit) <- "Model Fit"

cat("\nFixed Effects\n"); print(coef.table)
cat("\nVarinace Components\n"); print(var.cov)
print(mod_fit)
}
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
#--------------------- HLM R2 Output Function --------------------------------#

# A function to calculate R2 at level 1 and 2
# x= pervious model y= new model

rsq.output <- function(x, y, npar=TRUE,print=F, na.encode = F) {

options(scipen=999)

## R2_e
m1_var_e <- as.data.frame(VarCorr(x))[which(as.data.frame(VarCorr(x))=="Residual"), 4]
m2_var_e <- as.data.frame(VarCorr(y))[which(as.data.frame(VarCorr(y))=="Residual"), 4]
m2_r2_e <- (m1_var_e - m2_var_e) / m1_var_e

## R2_0
m1_int_var<- as.data.frame(VarCorr(x))
m1_var_0 <- m1_int_var$vcov[which(m1_int_var[,"var1"]=="(Intercept)" & is.na(m1_int_var[,"var2"]))]

m2_int_var<- as.data.frame(VarCorr(y))
m2_var_0 <- m2_int_var$vcov[which(m2_int_var[,"var1"]=="(Intercept)" & is.na(m2_int_var[,"var2"]))]
m2_r2_0 <- (m1_var_0 - m2_var_0) / m1_var_0


## R2_1
m1_lin_var <- as.data.frame(VarCorr(x))
m1_var_1 <- m1_lin_var$vcov[which(m1_lin_var[,"var1"]!="(Intercept)" & is.na(m1_lin_var[,"var2"]))]

m2_lin_var<- as.data.frame(VarCorr(y))
m2_var_1 <- m2_lin_var$vcov[which(m2_lin_var[,"var1"]!="(Intercept)" & is.na(m2_lin_var[,"var2"]))]
m2_r2_1 <- (m1_var_1 - m2_var_1) / m1_var_1

## Final table to print
mod_r2 <- as.data.frame(c(m2_r2_e, m2_r2_0, m2_r2_1),
	row.names= c("lvl-1", "lvl-2 Intercept", "lvl-2 Slope"))
colnames(mod_r2) <- "R-Square"					
print(mod_r2)
}
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#


#------------------ Model M1: Unconditional Means Model  ---------------------#
	
alc_m1 <- lmer(alcuse ~ 1 + (1|id),
	data=alc_l,
	REML=F)
	
summary(alc_m1)
alc_m1.out <- hlm.output(alc_m1)

## ICC
lvl2.var <- as.data.frame(VarCorr(alc_m1))[1,4] # Level 2 Variance
lvl1.var <- as.data.frame(VarCorr(alc_m1))[2,4] # Level 1 Variance
icc <- lvl2.var / (lvl2.var + lvl1.var)

#------------------ Model M2: Unconditional Growth Model  -----------------------#

alc_m2 <- lmer(alcuse ~ age_14 + (age_14|id),
	data=alc_l,
	REML=F)
	
summary(alc_m2)
alc_m2.out <- hlm.output(alc_m2)

## R2 comparing m2 to m1
rsq.output(alc_m1, alc_m2)


#------------------ Model M3: COA at INT and LIN  ----------------------------#

alc_m3 <- lmer(alcuse ~ age_14*coa + (age_14|id),
	data=alc_l,
	REML=F)
	
summary(alc_m3)
alc_m3.out <- hlm.output(alc_m3)

## R2 comparing m3 to m2
rsq.output(alc_m2, alc_m3)

#------------------ Model M4: COA & PEER at INT and LIN  ---------------------#

alc_m4 <- lmer(alcuse ~ age_14*coa + age_14*peer + (age_14|id),
	data=alc_l,
	REML=F)
	
summary(alc_m4)
alc_m4.out <- hlm.output(alc_m4)

## R2 comparing m3 to m4
rsq.output(alc_m3, alc_m4)

#------------------ Model M5: COA & PEER at INT and PEER at LIN  -------------#
alc_m5 <- lmer(alcuse ~ age_14 + coa + age_14*peer + (age_14|id),
	data=alc_l,
	REML=F)
	
summary(alc_m5)
alc_m5.out <- hlm.output(alc_m5)


## R2 comparing m3 to m4
rsq.output(alc_m4, alc_m5)



#-----------------------------------------------------------------------------#
#------------------------- Deviance statistic  -------------------------------#
# function to calculate deviance statistic and return p-value
# a = nested model object, b = bigger model object

dev <- function(a, b){
	return(1 - pchisq(
	         m1_dev <- -2*(as.numeric(logLik(a))) - 
      	     -2*(as.numeric(logLik(b))), 
		     as.numeric(attr(logLik(b), "df"))-
		     as.numeric(attr(logLik(a), "df"))))
}
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

## Compare alc_m2 to alc_m3
dev(alc_m2, alc_m3)

## Compare alc_m3 to alc_m4
dev(alc_m3, alc_m4)

## Compare alc_m5 to alc_m4
dev(alc_m5, alc_m4)



#------------------------------ Graph alc_m2 ---------------------------------#

## Table of fixed effects
alc_m2_fe <- fixef(alc_m2)

## alc_m2_fe[1] = Intercept 
## alc_m2_fe[2] = age_14 


## Functions to be plotted
uncond_growth= function(x){
	alc_m2_fe[1] + alc_m2_fe[2]*(x) 
	}

	
## Dataset to define the x-axis [time= 0, 1, 2]
tmp <- data.frame(x= min(alc_l$age_14) : max(alc_l$age_14) )


alc_m2_fit <- ggplot(data=tmp, aes(x))

#pdf("./tex/alc_m2_fit.pdf")
alc_m2_fit +  
	stat_function(fun=uncond_growth) +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks=c(0, 1, 2), 
    	  labels=c("14", "15", "16"), "Age") +
	scale_y_continuous(limits = c(0, 2),
	                   "Alcohol Use") +
	theme(legend.position="none")
#dev.off()

#------------------------------ Graph alc_m3 ---------------------------------#

## Table of fixed effects
alc_m3_fe <- fixef(alc_m3)

## alc_m3_fe[1] = Intercept 
## alc_m3_fe[2] = age_14 
## alc_m3_fe[3] = coa 
## alc_m3_fe[4] = age_14*coa 

## Functions to be plotted
coa0= function(x){
	alc_m3_fe[1] + alc_m3_fe[3]*(0) + alc_m3_fe[2]*(x) + alc_m3_fe[4]*(0)*(x)
	}
coa1= function(x){
	alc_m3_fe[1] + alc_m3_fe[3]*(1) + alc_m3_fe[2]*(x) + alc_m3_fe[4]*(1)*(x)
	}
	
## Dataset to define the x-axis [time= 0, 1, 2]
tmp <- data.frame(x= min(alc_l$age_14) : max(alc_l$age_14) )


alc_m3_fit <- ggplot(data=tmp, aes(x))

#pdf("./tex/alc_m3_fit.pdf")
alc_m3_fit + 
	stat_function(fun=coa0, linetype="dashed") +
			 geom_text(aes(label="COA = 0", x=1.92, y=0.8, size=1)) +
	stat_function(fun=coa1) +
			 geom_text(aes(label="COA = 1", x=1.92, y=1.6, size=1)) +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks=c(0, 1, 2), 
    	  labels=c("14", "15", "16"), "Age") +
	scale_y_continuous(limits = c(0, 2),
	                   "Alcohol Use") +
	theme(legend.position="none")
#dev.off()

#------------------------------ Graph alc_m5 ---------------------------------#

## Table of fixed effects
m5_fe <- fixef(alc_m5)

## Functions to be plotted
##COA=0, PEER=low
coan_peerlo= function(x){
	m5_fe[1] + m5_fe[3]*(0) + m5_fe[4]*(peerlo) + m5_fe[2]*(x) + m5_fe[5]*(x)*(peerlo) 
	}
##COA=0, PEER=hi
coan_peerhi= function(x){
	m5_fe[1] + m5_fe[3]*(0) + m5_fe[4]*(peerhi) + m5_fe[2]*(x) + m5_fe[5]*(x)*(peerhi) 
	}
##COA=1, PEER=low
coay_peerlo= function(x){
	m5_fe[1] + m5_fe[3]*(1) + m5_fe[4]*(peerlo) + m5_fe[2]*(x) + m5_fe[5]*(x)*(peerlo) 
	}
##COA=1, PEER=hi
coay_peerhi= function(x){
	m5_fe[1] + m5_fe[3]*(1) + m5_fe[4]*(peerhi) + m5_fe[2]*(x) + m5_fe[5]*(x)*(peerhi) 
	}

## Dataset to define the x-axis [time]
tmp <- data.frame(x= min(alc_l$age_14) : max(alc_l$age_14) )

## Using +/- .5 SD from mean
peerlo <- mean(alc_l$peer)-(.5*sd(alc_l$peer))
peerhi <- mean(alc_l$peer)+(.5*sd(alc_l$peer))

## using 2nd and 4th quantiles (25% and 75%)
#peerlo2 <- quantile(alc_l$peer)[2]
#peerhi2 <- quantile(alc_l$peer)[4]


alc_m5_fit <- ggplot(data=tmp, aes(x))

#pdf("./tex/alc_m5_fit.pdf")
alc_m5_fit + 
	stat_function(fun=coan_peerlo, linetype="dashed") + 
		 geom_text(aes(label="COA = 0\nPEER = Low", x=1.92, y=0.67, size=1)) +
	stat_function(fun=coan_peerhi, linetype="dashed", size=1.1) +
		 geom_text(aes(label="COA = 0\nPEER = High", x=1.92, y=0.97, size=1)) +
   	stat_function(fun=coay_peerlo) +	
   		 geom_text(aes(label="COA = 1\nPEER = Low", x=1.92, y=1.24, size=1)) +
   	stat_function(fun=coay_peerhi, , size=1.1) +
   		 geom_text(aes(label="COA = 1\nPEER = High", x=1.92, y=1.54, size=1)) +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks=c(0, 1, 2), 
    	  labels=c("14", "15", "16"), "Age") +
	scale_y_continuous(limits = c(0, 2),
	                   "Alcohol Use") +
	theme(legend.position="none")
#dev.off()



#-----------------------------------------------------------------------------#
#------------------------------ Assumptions ----------------------------------#
#-----------------------------------------------------------------------------#


#-------------------------- lvl-1 Heteroscedasticity  ------------------------#

resid_lvl1_m2 <- HLMresid(alc_m2, level = 1, type = "LS", standardize = "semi")
head(resid_lvl1_m2)

## level 1 residual plotted against alcuse--the outcome variable  

ls_outcome <- qplot(data=resid_lvl1_m2, x=age_14, y=LS.resid)

#pdf("./tex/assumption1.pdf")
ls_outcome+	geom_point() +
	stat_smooth(method=loess)+
	geom_hline(yintercept=0, linetype="dotted") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
	ylab("Level-1 residuals") +
	scale_x_continuous("AGE") 
#dev.off()


#-------------------------- lvl-1 qqnorm plots -------------------------------#

## Normal quantile plot of the semi-standardized level-1 residuals 
ss_resid <- na.omit(resid_lvl1_m2$semi.std.resid)

#pdf("./tex/assumptions2.pdf")
ggplot_qqnorm(x = ss_resid, line = "rlm") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank())
#dev.off()

#-------------------------- lvl-1 residual vs ID plot ------------------------#

sdres_e <- ggplot(data=resid_lvl1_m2, aes(x=id, y=semi.std.resid))

#pdf("./tex/assumptions3.pdf")
sdres_e + geom_point() +
	geom_hline(yintercept=2, linetype="dashed") +
	geom_hline(yintercept=-2, linetype="dashed") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
	scale_x_continuous( "ID") +
	ylab("Semi-Std. Raw Residual") 
#dev.off()

#-------------------------- lvl-2 qqnorm plots -------------------------------#


resid_lvl2_m2 <- HLMresid(object = alc_m2, level = "id")

colnames(resid_lvl2_m2)[1] <- "int_eb"					
resid_lvl2_m2$stuid <- seq(1:nrow(resid_lvl2_m2))
head(resid_lvl2_m2)

#-------------------------- Intercept qqnorm plots ---------------------------#


#pdf("./tex/assumptions4.pdf")
ggplot_qqnorm(x = resid_lvl2_m2[, "int_eb"], line = "rlm") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank())
#dev.off()

#-------------------------- Slope qqnorm plots -------------------------------#


#pdf("./tex/assumptions5.pdf")
ggplot_qqnorm(x = resid_lvl2_m2[, "age_14"], line = "rlm") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank())
#dev.off()


#-------------------------- Raw residual plots -------------------------------#


resid_lvl2_m2$sdeb_int <- resid_lvl2_m2[, "int_eb"] / sd(resid_lvl2_m2[, "int_eb"])
resid_lvl2_m2$sdeb_age14 <- (resid_lvl2_m2[, "age_14"] / sd(resid_lvl2_m2[, "age_14"]))


#-------------------- Intercept std. residual vs ID plots --------------------#

sdres_int <- ggplot(data=resid_lvl2_m2, aes(x=stuid, y=sdeb_int))

#pdf("./tex/assumptions6.pdf")
sdres_int + geom_point() +
	geom_hline(yintercept=2, linetype="dashed") +
	geom_hline(yintercept=-2, linetype="dashed") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
	scale_x_continuous( "ID") +
	ylab("Std. Fitted Intercept Variance") 
#dev.off()

#-------------------- Slope std. residual vs ID plots --------------------#


sdres_lin <- ggplot(data=resid_lvl2_m2, aes(x=stuid, y=sdeb_age14))

#pdf("./tex/assumptions7.pdf")
sdres_lin + geom_point() +
	geom_hline(yintercept=2, linetype="dashed") +
	geom_hline(yintercept=-2, linetype="dashed") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
	scale_x_continuous( "ID") +
	ylab("Std. Fitted Linear Growth Variance") 
#dev.off()


#------------------------------- Hour 4 --------------------------------------#
#-----------------------------------------------------------------------------#
#--------------------------- Dealing with time -------------------------------#
#-----------------------------------------------------------------------------#


#------------------- Structured vs Unstructured ------------------------------#

# Read in .dta data

read_l <- read.dta("./data/reading_pp.dta", convert.factors=T)

centered <- read_l[ ,3:4] - min(read_l$agegrp)
dimnames(centered)[[2]] <- c("agegrp.c", "age.c")  
read_l <- cbind(read_l, centered)

head(read_l)


# forcing structure on data
agegrp <- lmer(piat ~ agegrp.c + (agegrp.c | id), 
	read_l, 
	REML = F)
	
summary(agegrp)
hlm.output(agegrp)

# using unstructured data
age <- lmer(piat ~ age.c + (age.c | id), 
	read_l, 
	REML = F)
	
summary(age)
hlm.output(age)


 #--------------------- Time-varying Covariates ------------------------------#

unemp_l <- read.dta("./data/unemploy.dta", convert.factors=T)
head(unemp_l)

# Unconditional Growth Model (A)
unemp_m1 <- lmer(cesd ~ months + (months | id), 
                 unemp_l, 
                 REML = FALSE)
                 
summary(unemp_m1)
hlm.output(unemp_m1)

# Time-varying predictor UEMP (B)
unemp_m2 <- lmer(cesd ~ months + unemp + (months | id), 
                 unemp_l, 
                 REML = FALSE)
summary(unemp_m2)
hlm.output(unemp_m2)


# allow effect of time-varying predictor (unemp) to vary over time
unemp_m3 <- lmer(cesd ~ months + unemp*months + (months | id), 
                 unemp_l, 
                 REML = FALSE)
summary(unemp_m3) 
hlm.output(unemp_m3)

             
# constant slope for unemp=0, changing slope for unemp=1
unemp_m4 <- lmer(cesd ~ unemp + unemp:months + (unemp:months | id), 
                 unemp_l, 
                 REML = FALSE)
summary(unemp_m4)
hlm.output(unemp_m4)
             
# constant slope for unemp=0, changing slope for unemp=1
unemp_m5 <- lmer(cesd ~ 1 + unemp + unemp*months + 
				(1| id) + (0 + unemp| id) + (0 + unemp*months | id), 
                 unemp_l, 
                 REML = FALSE)

summary(unemp_m5)
hlm.output(unemp_m5)




#------------------ Plot Fitted trajectories ------------------------------#

tmp1 <- data.frame(x= seq(0, 15, by=5))

unemp2_fe <- fixef(unemp_m2)

unemp2= function(x){	
	ifelse(x <= 5 ,
	unemp2_fe[1] + unemp2_fe[3]*(1) + unemp2_fe[2]*(x) ,
	NA)	
	}
unemp3= function(x){	
	ifelse(x >= 5 ,
	unemp2_fe[1] + unemp2_fe[3]*(0) + unemp2_fe[2]*(x) ,
	NA)
	}

unemp1_fit <- ggplot(data=tmp1, aes(x))

pdf("./tex/unemp1.pdf")
unemp1_fit + 
	stat_function(fun=unemp2) + 
	stat_function(fun=unemp3) +
	geom_segment(aes(x= 5, y= 11.65568 , xend = 5, yend =  16.76699), linetype="dotted") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(limits=c(0, 15), "Months") +
    scale_y_continuous(limits=c(5, 20), "CES-D Score") +
	theme(legend.position="none")
dev.off()



#------------------ Model 4 Graph ------------------------------#

tmp2 <- data.frame(x= seq(0, 15, by=5))
unemp_fe <- fixef(unemp_m4)


unemp= function(x){	
	unemp_fe[1] + unemp_fe[2]*(1) + unemp_fe[3]*(1)*(x)
	}
emp1= function(x){
	ifelse(x >= 5, 
	unemp_fe[1] + unemp_fe[2]*(0) + unemp_fe[3]*(0)*(x),
	NA)
	}
emp2= function(x){
	ifelse(x < 5, 
	unemp_fe[1] + unemp_fe[2]*(0) + unemp_fe[3]*(0)*(x),
	NA)
	}	
unemp_fit <- ggplot(data=tmp2, aes(x))

pdf("./tex/unemp2.pdf")
unemp_fit + 
	stat_function(fun=unemp, size=1.5) + 
		 geom_text(aes(label="Unemployed", x=14.5, y= 14.75, size=1)) +
	stat_function(fun=emp1) +
	stat_function(fun=emp2, linetype="dashed") +
		geom_text(aes(label="Employed", x=14.5, y= 10.5, size=1)) +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous(limits=c(0, 15), "Months") +
    scale_y_continuous(limits=c(5, 20), "CES-D Score") +
	theme(legend.position="none")
dev.off()



 #------------------------ Re-centering TIME ---------------------------------#

med_l <- read.dta("./data/med.dta", convert.factors=T)
med_l$timeofday <- round(med_l$timeofday, 2)
med_l$time <- round(med_l$time, 2)

med_l$time333 <- med_l$time-median(med_l$time)
med_l$time667 <- med_l$time-max(med_l$time)
head(med_l, 20)

#------------------------------ Initial Status ---------------------------------#

# constant slope for unemp=0, changing slope for unemp=1
med_m1 <- lmer(pos ~ treat + time*treat + (time | id), 
                 med_l, 
                 REML = FALSE)
summary(med_m1)
hlm.output(med_m1)

#--------------------------- Midpoint Status ---------------------------------#

med_m2 <- lmer(pos ~ treat + time333*treat + (time333 | id), 
                 med_l, 
                 REML = FALSE)
summary(med_m2)
hlm.output(med_m2)

#---------------------------- Final Status -----------------------------------#

med_m3 <- lmer(pos ~ treat + time667*treat + (time667 | id), 
                 med_l, 
                 REML = FALSE)
summary(med_m3)
hlm.output(med_m3)




#------------------ Plot Fitted trajectories ------------------------------#


med_fe <- fixef(med_m1)
cont= function(x){
	med_fe[1] + med_fe[2]*(0) + med_fe[3]*(x) + med_fe[4]*(0)*(x)
	}
treat= function(x){
	med_fe[1] + med_fe[2]*(1) + med_fe[3]*(x) + med_fe[4]*(1)*(x)
	}

tmp1 <- data.frame(x= seq(min(med_l$time) : max(med_l$time)))


med_fit <- ggplot(data=tmp1, aes(x))

pdf("./tex/med_fit.pdf")
med_fit + 
	stat_function(fun=treat, size=1.1) + 
	 geom_text(aes(label="Treatment", x=6.85, y= 190, size=1)) +
	stat_function(fun=cont) +
	 geom_text(aes(label="Control", x=6.85, y= 147, size=1)) +
	 geom_segment(aes(x= 0, y= 164.3585 , xend = 0, yend = 167.4608), linetype="dotted") +
	geom_segment(aes(x= 6.667, y= 185.1423, xend = 6.667, yend = 151.3457), linetype="dotted") +
	geom_segment(aes(x= 3.333, y= 174.7489, xend = 3.333, yend = 159.4044), linetype="dotted") +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    xlab("Days") +
    scale_y_continuous(limits = c(140, 200),
	                   "Positive Mood Score") +
	theme(legend.position="none")
dev.off()

#-----------------------------------------------------------------------------#
#---------------------- Polynomial Growth Models -----------------------------#
#-----------------------------------------------------------------------------#

# ----------------- Read in data ---------------------------------------------#

##install.packages("foreign")
library(foreign); library(reshape); library(ggplot2); library(lme4)


# Read in .dta data
wgt_l <- read.dta("./data/wgt.dta", convert.factors=T)
head(wgt_l)

# recode gender (1: male, 2: female) to (0: male, 1: female)
wgt_l$female <- wgt_l$gender -1


#------------------- Structured vs Unstructured ------------------------------#

p <- ggplot(data = wgt_l, aes(x = age, y = weight, group = id))
p + geom_point() + facet_grid(. ~ female)

p + geom_line() + facet_grid(. ~ female)

#------------------- Model 2: Unconditional Growth (lin) ---------------------#

wgt_m1 <- lmer(weight ~ age + (age|id),
	data=wgt_l,
	REML=F)

hlm.output(wgt_m1)

#------------------- Model 2: Unconditional Growth (quad) --------------------#

wgt_m2 <- lmer(weight ~ age + I(age^2) + (age|id),
	data=wgt_l,
	REML=F)

hlm.output(wgt_m2)
rsq.output(wgt_m1, wgt_m2)
dev(wgt_m1, wgt_m2)

#---------------------------- Model 3  ---------------------------------------#


wgt_m3 <- lmer(weight ~ age*female + I(age^2)*female + (age|id),
	data=wgt_l,
	REML=F)
	
hlm.output(wgt_m3)
rsq.output(wgt_m2, wgt_m3)
dev(wgt_m2, wgt_m3)

#---------------------------- Model 4  ---------------------------------------#


wgt_m4 <- lmer(weight ~ age*female + I(age^2) + (age|id),
	data=wgt_l,
	REML=F)

hlm.output(wgt_m4)
rsq.output(wgt_m3, wgt_m4)
dev(wgt_m4, wgt_m3)

#---------------------------- Model 5  ---------------------------------------#

wgt_m5 <- lmer(weight ~ age + female + I(age^2) + (age|id),
	data=wgt_l,
	REML=F)
	
hlm.output(wgt_m5)
rsq.output(wgt_m4, wgt_m5)
dev(wgt_m5, wgt_m3)
dev(wgt_m2, wgt_m5)

#------------------------------ Graph asian_m5 ---------------------------------#

## Table of fixed effects
wgt_fe <- fixef(wgt_m5)

## Functions to be plotted
male= function(x){
	wgt_fe[1] + wgt_fe[2]*(x) + wgt_fe[3]*(0) + wgt_fe[4]*(x)^2
	}
female= function(x){
	wgt_fe[1] + wgt_fe[2]*(x) + wgt_fe[3]*(1) + wgt_fe[4]*(x)^2
	}
	
## Dataset to define the x-axis [time= 0, 1, 2]
tmp <- data.frame(x= seq(0 , max(wgt_l$age), by=0.5 ))


wgt_m5_fit <- ggplot(data=tmp, aes(x))

wgt_m5_fit + 
	stat_function(fun=male, linetype="dashed") +
			 geom_text(aes(label="Male", x=2.25, y=13.25, size=1)) +
	stat_function(fun=female) +
			 geom_text(aes(label="Female", x=2.25, y=11.5, size=1)) +
   	theme_bw() +
   	theme(panel.grid.major = element_blank(), 
		  panel.grid.minor = element_blank()) +
    scale_x_continuous("Age (in years)") +
	scale_y_continuous(limits = c(0, 20),
	                   "Weight (in kg)") +
	theme(legend.position="none")

