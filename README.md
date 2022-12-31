# Job-Change-of-Data-Scientists

# Project Overview 
Analysing the dataset which have information of data scientists in a big data company to understand the factors that lead a person to leave current job for other opportunities. 
Create a predictive model to predict whether employee will leave the organization or stay and understand the factors which influence his decision.

# EXPLORATORY DATA ANALYSIS
The train dataset has 19158 observations and 14 variables. The test dataset has 2129 observations and 13 variables. 
We find that few columns have missing data. For the column LastNewJob we calculate the mean of the column and replace the missing values with it. 
For the columns with categorical data we first conduct mode imputation but on evaluating it we realize that lot os bias is being introduced.
So we conduct MICE imputation on Train, Validate and Test data. The MICE algorithm can impute mixes of continuous, binary, unordered categorical and ordered categorical data.

METHODOLOGY
Using 5 algorithms - LDA, Logistic Regression, Recursive Partioning and Regression Trees, KNN and Random Forest to evaluate which model works best.
We use the best performing model on Validate dataset and based on accuracy on validate dataset use the model to make predictions on test dataset

RESULTS
We find that out of the 5 models Recursive Partioning and Regression Trees perform the best with the accuracy of 78%. 
We use the model on the validate dataset and get accuracy of 97%. We use the model to predict our target variable in test dataset. Output of the files is 
stored in DSJobChange_finaloutput.csv



