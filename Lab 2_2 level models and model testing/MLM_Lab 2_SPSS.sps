** This syntax goes along with the R code, but in SPSS (naturally).
** It reviews 2-level models using the ATLAS data from Blackbord.
** We'll predict students' perceptions of their coaches' tolerance of steroid use.

** Run the null model and obtain the ICC:
* For the null model, we have no predictors.
* Students (our L1 unit) are nested within schools (L2); we specify this structure with subject(schoolid) on the random line.

mixed coachtol0 
/fixed = intercept
/random = intercept | subject(schoolid) covtype(un)
/print testcov solution.

** Predict tolerance from student use of steroids. 
* Only specify a random intercept.
* Note that use is a dichotomous variable, and SPSS handles categorical variables VERY STRANGELY.
* Use caution when interpreting the fixed effects estimates (make sure you're interpreting the correct level of use).

mixed coachtol0 BY use0
/fixed = intercept use0
/random = intercept | subject(schoolid) covtype(un)
/print testcov solution.

* Now specify the random slope.
* Using covtype(un) will also get you the covariance between the slope and intercept. 
* If you don't want the covariance, change it to covtype(vc).

mixed coachtol0 BY use0
/fixed = intercept use0
/random = intercept use0| subject(schoolid) covtype(un)
/print testcov solution.

* Like I said, SPSS gets wild with categorical variables (it will try to estimate random effects for each level of the slope, plus all of the covariances). 
*Try pretending that use is continuous:.
mixed coachtol0 WITH use0
/fixed = intercept use0
/random = intercept use0| subject(schoolid) covtype(un)
/print testcov solution.


** Add in the L2 variable, intervention, to predict both the slope and the intercept.

mixed coachtol0 WITH use0 intervention
/fixed = intercept use0 intervention use0*intervention
/random = intercept use0| subject(schoolid) covtype(un)
/print testcov solution.



** Add a second L1 variable:.
mixed coachtol0 WITH use0 SE0
/fixed = intercept use0 SE0 use0*SE0
/random = intercept use0 SE0 use0*SE0| subject(schoolid) covtype(un)
/print testcov solution.


mixed coachtol0 WITH use0 se0
/fixed = intercept use0 se0 use0*se0
/random = intercept use0 se0 use0*se0| subject(schoolid) covtype(un)
/print testcov solution.

mixed coachtol0 WITH use0 se0
/fixed = intercept use0 se0 use0*se0
/random = intercept use0 se0 use0*se0| subject(schoolid) covtype(vc)
/print testcov solution.


mixed coachtol0 WITH stse0 se0
/fixed = intercept stse0 se0 stse0*se0
/random = intercept stse0 se0 stse0*se0| subject(schoolid) covtype(un)
/print testcov solution.

mixed coachtol0 WITH stse0 se0
/fixed = intercept stse0 se0 stse0*se0
/random = intercept stse0 se0 stse0*se0| subject(schoolid) covtype(vc)
/print testcov solution.

