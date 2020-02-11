# ProactiveAttritionManagementCaseStudyInR
## AIM 
This case requires us to develop a model for predicting customer churn at “Cell2Cell,” a
fictitious wireless telecom company, and use insights from the model to develop an incentive plan
for enticing would-be churners to remain with Cell2Cell.
The data are a scaled down version of the full database generously donated by an anonymous wireless 
telephone company.There are still 71,047 customers the database, and 75 potential predictors.

## Implementation
### Understanding the problem statement 
Firstly we understand the problem statement and the data dictionary (i.e. what each variable means) 
and what is the question we are trying to answer. 

### Loading and munging the dataset
Then we load the dataset into R using readxl package's read_excel() 
function. And then doing some explicit datatype changes that have been wrongly interpreted by read_excel(). 
Then removing variables with 0 standard deviation. And the redundant variables.
We are told that the values are missing at random so no need to worry much just remove the variables which 
have high percentage of missing values and for other variables imputing them with mean.So I created a user 
defined function to check the descriptive statistics, different quariles, number of missing values and an 
indicator if a variable has outliers using a formula.

### Fitting a logistic regression model.
Then we do Factor Analysis to select numerical data and anova for categorical variables to select important variables.
Then we fit the step-wise Logistic regression model and use Vif on subsequent models to check for Multicollinearity.
Then we use different methods to check model accuracy like Lift chart, Concordance, confusion matrix, decile analysis,etc.
**The process of model fitting one is an Iterative process.**


## Prerequisites
1) R Installed
2) Knowledge of Data Munging.
3) Knowledge of Logistic Regression.


## Author
**Abhinav Gera**
