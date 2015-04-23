**Disaggreagated model.

*runs a regular, OLS regression that ignores grouping.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CI(95)
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mathscore
  /METHOD=ENTER timeonmath.



**Aggregated model.
* runs a regular, OLS regression that ignores within-group variation (i.e., only group means are examined).

*Create aggreagated dataset that only includes the group means for the DV and IV.
DATASET DECLARE aggregated.
AGGREGATE
  /OUTFILE='aggregated'
  /BREAK=Schoolid
  /timeonmath_mean=MEAN(timeonmath) 
  /mathscore_mean=MEAN(mathscore).


*Run regression in the aggregated dataset.
DATASET ACTIVATE aggregated.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CI(95)
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mathscore_mean
  /METHOD=ENTER timeonmath_mean.



***WABA***.
**This analysis incorportates both within- and between-group effects in the same model.
**It utilizes the disaggregated dataset (i.e., where each row is an individual).

*first, add the group means to the dataset (these will be repeated for each individual in the group).
AGGREGATE
  /OUTFILE=* MODE=ADDVARIABLES OVERWRITEVARS=YES
  /BREAK=schoolid 
  /time_mean=MEAN(timeonmath).

*then, compute the within-group effect (i.e., the deviation of each individual from their group mean).
COMPUTE time_wi = timeonmath - time_mean.
EXECUTE.

*then, compute the between-group effect (i.e., the deviation of each group mean from the grand mean).

*find the grand mean.
MEANS timeonmath.

*compute the between-group variable.
COMPUTE time_bt = time_mean - 2.023077.
EXECUTE.

* run the model, predicting the DV from the within- and between-group effects.
REGRESSION
  /DESCRIPTIVES MEAN STDDEV CORR SIG N
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mathscore
  /METHOD=ENTER time_wi time_bt.


**Computing the ICC (be sure to use the disaggregated dataset - we want to separate the variability due to within- and between-group factors in order to
determine what percentage of the variability is due to the grouping factor. ).
GLM mathscore BY schoolid
  /PRINT = DESCRIPTIVE PARAMETER HOMOGENEITY
  /DESIGN = schoolid .

ONEWAY mathscore BY Schoolid
  /MISSING ANALYSIS.


**Plot the slopes all together.
GRAPH
  /SCATTERPLOT(BIVAR)=timeonmath WITH mathscore BY Schoolid
  /MISSING=LISTWISE.

*then, in the chart editor, select "add fit line at subgroups"
*then, double click on the lines to get the line properties, and un-select "attach label to line"
*then, in properties window, delete the "element type: marker" element.

***Slopes and Intercepts as Outcomes***.
*This analyses use the disaggregated dataset to begin with.
*Create dummy codes for schools.
Compute d1 = 0.
if (schoolid = 7472) d1=1.
exe.

Compute d2 = 0.
if (schoolid = 7829) d2=1.
exe.

Compute d3 = 0.
if (schoolid = 7930) d3=1.
exe.

Compute d4 = 0.
if (schoolid = 24725) d4=1.
exe.

Compute d5 = 0.
if (schoolid = 25456) d5=1.
exe.

Compute d6 = 0.
if (schoolid = 25642) d6=1.
exe.

Compute d7 = 0.
if (schoolid = 62821) d7=1.
exe.

Compute d8 = 0.
if (schoolid = 68448) d8=1.
exe.

Compute d9 = 0.
if (schoolid = 68493) d9=1.
exe.

*Double check that you did what you thought you did.

CROSSTABS
  /TABLES=Schoolid BY d1 d2 d3 d4 d5 d6 d7 d8 d9
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


*See if grouping variable contributes significantly to the model.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mathscore
  /METHOD=ENTER timeonmath
  /METHOD=ENTER d1 d2 d3 d4 d5 d6 d7 d8 d9.

*Compute interaction terms.

Compute d1hw = d1*timeonmath.
Compute d2hw = d2*timeonmath.
Compute d3hw = d3*timeonmath.
Compute d4hw = d4*timeonmath.
Compute d5hw = d5*timeonmath.
Compute d6hw = d6*timeonmath.
Compute d7hw = d7*timeonmath.
Compute d8hw = d8*timeonmath.
Compute d9hw = d9*timeonmath.
exe.

*See if slopes vary between groups.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mathscore
  /METHOD=ENTER timeonmath
  /METHOD=ENTER d1 d2 d3 d4 d5 d6 d7 d8 d9
  /METHOD=ENTER d1hw d2hw d3hw d4hw d5hw d6hw d7hw d8hw d9hw.

*Get unique intercepts and slopes.
SORT CASES  BY schoolid.
SPLIT FILE LAYERED BY schoolid.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT mathscore
  /METHOD=ENTER timeonmath.

*Predict intercepts and slopes from school types.
* Note that this requires creating a new dataset from the previous analysis (or, for this example, opening the Nels88 L2 file).

*Intercept.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT intercept
  /METHOD=ENTER schtyp.


*Slope.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT slope
  /METHOD=ENTER schtyp.


***Full Multilevel Model/Random Coefficient Regression.
*always use the disaggregated dataset (where each row is an observation and group-level variables are repeated for all of the members of the group) for these models.
MIXED mathscore WITH timeonmath BY schooltype
/PRINT TESTCOV SOLUTION
/FIXED INTERCEPT timeonmath schooltype timeonmath*schooltype
/RANDOM INTERCEPT timeonmath | SUBJECT(schoolid) COVTYPE(UN).

* plot the interaction.
GRAPH
  /SCATTERPLOT(BIVAR)=timeonmath WITH mathscore BY schooltype
  /MISSING=LISTWISE.

*then, in the chart editor, select "add fit line at subgroups"
*then, double click on the lines to get the line properties, and un-select "attach label to line"
*then, in properties window, delete the "element type: marker" element.
