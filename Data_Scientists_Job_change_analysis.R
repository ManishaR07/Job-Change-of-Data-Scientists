## Loading the required libraries
library(dplyr)
library(ggplot2)
library(car)
library(GGally)
library(fastDummies)
library(caret)
library(DescTools)
library(rpart)
library(randomForest)
library(rpart)
library(e1071)
library(rattle)
library(VIM)
library(mice)

## Loading the data from csv file
associate_train_df <- read.csv("aug_train.csv",header=TRUE,sep=",")
associate_test_df <- read.csv("aug_test.csv",header=TRUE,sep=",")

## Checking the structure of the data
str(associate_train_df) ## 19158 observartions with 14 variables
str(associate_test_df) ## 2129 observartions with 13 variables

## Checking for missing data in the dataset and converting empty cells to NA
associate_train_df[associate_train_df==""] <- NA
colSums(is.na(associate_train_df))
associate_test_df[associate_test_df==""] <- NA
colSums(is.na(associate_test_df))

## Gender,Enrolled_university, education_level, major discipline, experience, company size, company type, last new job columns has missing data

## Removing enroll_id and city columns from the traina and test data
associate_sub_train_df <- subset(associate_train_df,select = -c(enrollee_id,city))
associate_sub_test_df <- subset(associate_test_df,select = -c(enrollee_id,city))

## EXPLORATORY DATA ANALYSIS

## Analyzing the gender distribution in dataset

ggplot(associate_sub_train_df,aes(x=gender))+
  geom_bar(fill = "green")+
  xlab("Gender")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Gender Distribution")

prop.table(table(associate_sub_train_df$gender)) ## 90% male and 8% female, 1% others

## Analysing the distibution by experience in dataset

ggplot(associate_sub_train_df,aes(x=experience))+
  geom_bar(fill = "blue")+
  xlab("Experience")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Experience Distribution")

prop.table(table(associate_sub_train_df$experience))

## Maximum number of employees has experience>20 (17%)

## Analysing Distribution by relevant experience
ggplot(associate_sub_train_df,aes(x=relevent_experience))+
  geom_bar(fill = "blue")+
  xlab("Relevant Experience")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Relevant Experience Distribution")

prop.table(table(associate_sub_train_df$relevent_experience)) ## 72% of the trainees have relevant experience vs 28% with no relevant experience

## Analysing Distribution by enrolled university

ggplot(associate_sub_train_df,aes(x=enrolled_university))+
  geom_bar(fill = "blue")+
  xlab("Enrolled University")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Distribution by Enrolled University")

prop.table(table(associate_sub_train_df$enrolled_university))
## 74% of the trainees have currently not enrolled in university, 20% are enrolled in a full time course and 6% in part time courses

##Analysing distribution by Education Level
ggplot(associate_sub_train_df,aes(x=education_level))+
  geom_bar(fill = "blue")+
  xlab("Education")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Distribution by Education Level")

prop.table(table(associate_sub_train_df$education_level))
## Graduates form the highest proportion of employees who enroll for trainings (62%) followed by trainees with 23% holding Masters degree
## 10% holding High School degree, 2% holding Phd degree and 1% holding primary school degree

##Analysing distribution by Major
ggplot(associate_sub_train_df,aes(x=major_discipline))+
  geom_bar(fill = "blue")+
  xlab("Major")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Distribution by Major")

prop.table(table(associate_sub_train_df$major_discipline))
## Maximum trainees have majors in STEM (89%), followed by Humanities (4%), Business Degree and others (2%), Arts and no major (1%)

##Analysing distribution by Company Size
ggplot(associate_sub_train_df,aes(x=company_size))+
  geom_bar(fill = "blue")+
  xlab("Company Size")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Distribution by Company size of Trainees")

prop.table(table(associate_sub_train_df$company_size))
## 23% of trainees work in company size (50-999), 19% in (10-500), 15% in (10000+), 11% in (10-49)

##Analysing distribution by Company Type
ggplot(associate_sub_train_df,aes(x=company_type))+
  geom_bar(fill = "blue")+
  xlab("Company Type")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Distribution by Company type of Trainees")

prop.table(table(associate_sub_train_df$company_type))
## 75% of the trainees are from Pvt Ltd, 7% each from Public and Funded startup, 4% each from NGO and Early Stage Startup

##Analysing distribution by Last New Job
ggplot(associate_sub_train_df,aes(x=last_new_job))+
  geom_bar(fill = "blue")+
  xlab("Last New Job")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+
  ggtitle("Distribution by Last New Job of Trainees")

prop.table(table(associate_sub_train_df$last_new_job))
## 43% of the trainees have changed job in last 1 year followed by 18% who have changes last job >4 years back and 13% who have never chnaged a job

##Analysing distribution by Education_Level and Relevant Experience
ggplot(associate_sub_train_df,aes(x=major_discipline,fill=relevent_experience))+
  geom_bar()+
  facet_wrap("gender")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Major")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Distribution by College Major and Relevant Experience")

##Analysing distribution by City Development Index and Relevant Experience
ggplot(associate_sub_train_df,aes(x=city_development_index,fill=relevent_experience))+
  geom_bar()+
  xlab("City Development Index")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Distribution by City Development Index and Relevant Experience")

## More trainees have  relevant experience in cities with high city development index

## Analysing the data with respect to target

## Target vs Gender
ggplot(associate_sub_train_df,aes(x=gender,fill=gender))+
  geom_bar()+
  facet_wrap("target")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("gender")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Target by Gender")

prop.table(table(associate_sub_train_df$target,associate_sub_train_df$gender))
## 20% of males and 2% females have been looking for a job change

## Target and Experience
ggplot(associate_sub_train_df,aes(x=experience,fill=experience))+
  geom_bar()+
  facet_wrap("target")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Experience")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Target by Experience")

prop.table(table(associate_sub_train_df$target,associate_sub_train_df$experience))

## More than 2% of trainees each with experience >20 yrs, 2-7yrs looking for job change

## Target and Relevant Experience
ggplot(associate_sub_train_df,aes(x=relevent_experience,fill=education_level))+
  geom_bar()+
  facet_wrap("target")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Relevant Experience")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Target by Relevant Experience")

prop.table(table(associate_sub_train_df$target,associate_sub_train_df$education_level,associate_sub_train_df$relevent_experience))
prop.table(table(associate_sub_train_df$target,associate_sub_train_df$relevent_experience))
## 11% of the graduates,3% of the trainees with masters degree who has relevant degree looking for job change
## 16% of the trainees with STEM who has relevant experience looking for a job

## Target and Major, Relevant Experience
ggplot(associate_sub_train_df,aes(x=major_discipline,fill=relevent_experience))+
  geom_bar()+
  facet_wrap("target")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Major")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Target by Major")

prop.table(table(associate_sub_train_df$target,associate_sub_train_df$major_discipline,associate_sub_train_df$relevent_experience))
prop.table(table(associate_sub_train_df$target,associate_sub_train_df$major_discipline,associate_sub_train_df$education_level,associate_sub_train_df$relevent_experience))
## 11% of the graduates with STEM as major , having relevant experience looking for a job change
 
## Target and Last NEw job
ggplot(associate_sub_train_df,aes(x=last_new_job,fill=relevent_experience))+
  geom_bar()+
  facet_wrap("target")+
  theme(axis.text.x = element_text(angle = 90, hjust =1, vjust = 0.5))+
  xlab("Last New Job")+
  ylab("Count")+
  geom_text(stat='count', aes(label=..count..), vjust=-1,check_overlap = TRUE)+
  ggtitle("Target by LAst New job")

prop.table(table(associate_sub_train_df$target,associate_sub_train_df$last_new_job,associate_sub_train_df$relevent_experience))
## 7% of the trainees who has changed new job in last 1 year looking for a job change

## DATA PREPROCESSING

## Data Cleaning - replacing "never" and ">4" in last new job with '0" and "5"
associate_sub_test_df <-associate_sub_test_df %>% mutate(last_new_job = ifelse(last_new_job == "never", 0, last_new_job ))
associate_sub_test_df <-associate_sub_test_df %>% mutate(last_new_job = ifelse(last_new_job == ">4", 5, last_new_job ))

associate_sub_train_df <-associate_sub_train_df %>% mutate(last_new_job = ifelse(last_new_job == "never", 0, last_new_job ))
associate_sub_train_df <-associate_sub_train_df %>% mutate(last_new_job = ifelse(last_new_job == ">4", 5, last_new_job ))

##Data cleaning - replacing ">20 and <1" in experience to "21 and 0.5"
associate_sub_test_df <-associate_sub_test_df %>% mutate(experience = ifelse(experience == "<1", 0.5, experience ))
associate_sub_test_df <-associate_sub_test_df %>% mutate(experience = ifelse(experience == ">20", 21, experience ))

associate_sub_train_df <-associate_sub_train_df %>% mutate(experience = ifelse(experience == "<1", 0.5, experience ))
associate_sub_train_df <-associate_sub_train_df %>% mutate(experience = ifelse(experience == ">20", 21, experience ))

## Data Cleaning - Replacing "10/49" in company size to "10-49"
associate_sub_test_df <-associate_sub_test_df %>% mutate(company_size = ifelse(company_size == "10/49", "10-49", company_size ))
associate_sub_train_df <-associate_sub_train_df %>% mutate(company_size = ifelse(company_size == "10/49", "10-49", company_size ))

## Converting datatypes of variables in test and train dataset
associate_sub_test_df$experience <- as.numeric(associate_sub_test_df$experience)
associate_sub_test_df$last_new_job<- as.numeric(associate_sub_test_df$last_new_job)
associate_sub_test_df$gender <- as.factor(associate_sub_test_df$gender)
associate_sub_test_df$relevent_experience <- as.factor(associate_sub_test_df$relevent_experience)
associate_sub_test_df$enrolled_university <- as.factor(associate_sub_test_df$enrolled_university)
associate_sub_test_df$major_discipline  <- as.factor(associate_sub_test_df$major_discipline )
associate_sub_test_df$company_type  <- as.factor(associate_sub_test_df$company_type )
associate_sub_test_df$company_size  <- as.factor(associate_sub_test_df$company_size )
associate_sub_test_df$education_level <- factor(associate_sub_test_df$education_level, ordered = TRUE, 
                                              levels = c("Phd", "Masters", "Graduate", "High School", "Primary School"))

associate_sub_train_df$experience <- as.numeric(associate_sub_train_df$experience)
associate_sub_train_df$last_new_job<- as.numeric(associate_sub_train_df$last_new_job)
associate_sub_train_df$gender <- as.factor(associate_sub_train_df$gender)
associate_sub_train_df$relevent_experience <- as.factor(associate_sub_train_df$relevent_experience)
associate_sub_train_df$enrolled_university <- as.factor(associate_sub_train_df$enrolled_university)
associate_sub_train_df$major_discipline  <- as.factor(associate_sub_train_df$major_discipline )
associate_sub_train_df$company_type  <- as.factor(associate_sub_train_df$company_type )
associate_sub_train_df$company_size  <- as.factor(associate_sub_train_df$company_size )
associate_sub_train_df$education_level <- factor(associate_sub_train_df$education_level, ordered = TRUE, 
                                              levels = c("Phd", "Masters", "Graduate", "High School", "Primary School"))
associate_sub_train_df$target  <- as.factor(associate_sub_train_df$target)

## Checking for proportion of missing values in Train and Test data
table(is.na(associate_sub_train_df))
table(is.na(associate_sub_test_df))
table(is.na(associate_sub_train_df$last_new_job))
str(associate_sub_train_df)
str(associate_sub_test_df)

missing_data_plot_train <- aggr(associate_sub_train_df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,axes=TRUE,cex.lab=1.2,cex.axis=.7)
missing_data_plot_test <- aggr(associate_sub_test_df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,axes=TRUE,cex.lab=1.2,cex.axis=.7)

## 50% of the data is missing in both train and test dataset, maximum missing data is in company_type and size. 
## With 50% missing dataset we simply cannot remove the missing data in the dataset

## Replacing the NA in numeric columns with column mean
associate_sub_train_df$last_new_job[is.na(associate_sub_train_df$last_new_job)] <- mean(associate_sub_train_df$last_new_job, na.rm = TRUE) 
associate_sub_test_df$last_new_job[is.na(associate_sub_test_df$last_new_job)] <- mean(associate_sub_test_df$last_new_job, na.rm = TRUE) 

## Using mode imputation for missing categorical values
associate_sub_mode_traindf <- associate_sub_train_df
associate_sub_mode_testdf <- associate_sub_test_df

val <- unique(associate_sub_mode_traindf$company_type[!is.na(associate_sub_mode_traindf$company_type)])                   # Values in vec_miss
my_mode <- val[which.max(tabulate(match(associate_sub_mode_traindf$company_type, val)))] 
associate_sub_mode_traindf$company_type_imp <- associate_sub_mode_traindf$company_type                                    # Replicate vec_miss
associate_sub_mode_traindf$company_type_imp[is.na(associate_sub_mode_traindf$company_type_imp)] <- my_mode                     # Impute by mode


## Evaluating how much bias is introduced with mode imputation
missingness <- c(rep("No Missings", 6), rep("Post Imputation", 6)) # Pre/post imputation
Category <- as.factor(rep(names(table(associate_sub_mode_traindf$company_type))))                   # Categories
Count <- c(as.numeric(table(table(associate_sub_mode_traindf$company_type))), as.numeric(table(associate_sub_mode_traindf$company_type_imp)))     # Count of categories

data_barplot <- data.frame(missingness, Category, Count)           # Combine data for plot

## We see that Pvt Ltd as a category was the mode and having replaced the missing value with the mode we have increased the bias in the data
## So mode imputation will not work for this dataset


## Partitiong the train dataset into train and validation
inTrain <- createDataPartition(y=associate_sub_train_df$target, p=0.6, list=FALSE)
train_df_train <- associate_sub_train_df[inTrain, ] 
train_df_validate <- associate_sub_train_df[-inTrain, ]
dim(train_df_train)
dim(train_df_validate)


## Checking MICE imputation for missing values. The MICE algorithm can impute mixes of continuous, binary, unordered categorical and ordered categorical data.

## Finding missing data pattern with MICE package

md.pattern(associate_sub_train_df)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train_df_train,2,pMiss)
apply(train_df_validate,2,pMiss)
str(associate_sub_train_df)

## 32% of data missing in company_type, 30% in company_size, 23% in gender, 14% in major_discipline, 2% each in education level and enrolled_university

## Imputing with MICE train,validate and test dataset

train_df_train_impute <- mice(train_df_train, m=5, seed = 123)
train_df_validate_impute <- mice(train_df_validate,m=5,seed=123)

test_df_impute <- mice(associate_sub_test_df,m=5,seed=123)

Final_train_impute <- complete(train_df_train_impute)
Final_validate_impute <- complete(train_df_validate_impute)
Final_test_impute <- complete(test_df_impute)

## Cross validation using 10 fold
trcontrol <- trainControl(method="cv",number =10)
metric <- "Accuracy"

## Using 5 algorithms - LDA, Logistic Regression, Decision Trees, KNN and Random Forest to evaluate which model works best.

## 1) Linear Discriminant Analysis

set.seed(10)
fit_lda <- train(target~., data=Final_train_impute, method="lda", metric=metric, trControl=trcontrol)

## 2) Logistic Regression
set.seed(10)
fit_glm <- train(target~., data=Final_train_impute, method="glm", family="binomial",metric=metric, trControl=trcontrol)

## 3) Decision Trees
set.seed(10)
fit_rpart <- train(target~., data=Final_train_impute, method="rpart", metric=metric, trControl=trcontrol)

## 4) kNN
set.seed(10)
fit_knn <- train(target~., data=Final_train_impute, method="knn", metric=metric, trControl=trcontrol)

## 5) Random Forest
set.seed(10)
fit_rf <- train(target~., data=Final_train_impute, method="rf", metric=metric, trControl=trcontrol)

## Comparing accuracy of models
results <- resamples(list(lda=fit_lda, GLM = fit_glm, DT=fit_rpart, knn=fit_knn))
summary(results)

## Decision Trees shows the best mean accuracy of 78%

print(fit_rpart)

## Make predictions on the validate dataset using Decision Tree Model
prediction_trainee_model <- predict(fit_rpart, Final_validate_impute, na.action = na.pass)
confusionMatrix(prediction_trainee_model, Final_validate_impute$target)

## Predicting on testing dataset using Decion Tree Model
prediction_testdf <- predict(fit_rpart, Final_test_impute, type = "raw")
Final_test_impute <- Final_test_impute %>% select(everything()) %>% mutate(target = prediction_testdf)
summary(prediction_testdf)

