## MIS480 Project
## Bank Marketing Analysis

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(GGally)
library(pander)
library(caret)
library(rpart)
library(rpart.plot)
library(rminer)
library(nnet)
library(caTools)
library(dummies)
library(Amelia)

## Set random seed in order to replicate results
set.seed(100)

## Set a working directory
setwd("C:/Users/elksd/Dropbox/My PC (LAPTOP-899T4F9L)/Documents/School Stuff/19 MIS 480/MIS480_Portfolio_Project")

## Read in dataset
bank_full<-read.csv("bank-full.csv", header = TRUE)

## Create two variables that provide column and row number
row_count<-nrow(bank_full)
var_count<-ncol(bank_full)

## Structure of data
str(bank_full)

## Manipulate data for analysis

## Change int to factor
bank_full$job<-as.factor(bank_full$job)
bank_full$marital <-as.factor(bank_full$marital)
bank_full$education <-as.factor(bank_full$education)
bank_full$default <-as.factor(bank_full$default)
bank_full$housing <-as.factor(bank_full$housing)
bank_full$loan <-as.factor(bank_full$loan)
bank_full$contact <-as.factor(bank_full$contact)
bank_full$day<-as.factor(bank_full$day)
bank_full$month <-as.factor(bank_full$month)
bank_full$poutcome <-as.factor(bank_full$poutcome)
bank_full$y <-as.factor(bank_full$y)

## Structure and summary of data
str(bank_full)
summary(bank_full)

## Change day variable back to integer
bank_full$day<-as.integer(bank_full$day)

## Data validation
sum(duplicated(bank_full))
sum(!complete.cases(bank_full))

## Disecting and visualizing data

## Create variable for average age of client
age_mean<-round(mean(bank_full$age, digits = 0))
age_mean

## Graph for Age Histogram and box plot
gg <- ggplot(bank_full)
p1 <- gg + geom_histogram(aes(x=age), color = "black", fill = "white", binwidth = 5)+
  ggtitle("Age Distibution (red mean line)")+
  ylab("Count")+
  xlab("Age")+
  geom_vline(aes(xintercept = mean(age), color = "red"))+
  scale_x_continuous(breaks = seq(0,100,5))+
  theme_bw()+
  theme(legend.position = "none", 
        axis.text.x = element_text(size = rel(0.75)))

p2 <- gg + geom_boxplot(aes(x='', y=age))+
  ggtitle("Age Boxplot")+
  ylab("Age")+
  theme_bw()

grid.arrange(p1,p2, ncol = 2)

## Create Summary Table on Age variable
pander(summary(bank_full$age))

## Create Table and Prop. Table for Y variable
pander(table(bank_full$y))
pander(prop.table(table(bank_full$y)))

## Graph for Y/N breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = y, y = age, color = y, alpha = 0.01))+
  geom_point(position = position_jitter(width = 0.48, height = 0.1))+
  xlab("Term Deposit Purchased")+
  ylab("Age of Client")+
  ggtitle("Y/N breakdown with Age of Client")+
  theme_bw()+
  theme(legend.position = "none")

## Graph for Job breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x=job, y = age, color = y))+
  geom_point(position = position_jitter(width = 0.4, height = 0.1))+
  labs(title = "\nJob breakdown with Age of Client", x = "Job of Client", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45))

## Graph for Marital Status breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x=marital, y=age, color = y))+
  geom_point(position = position_jitter(width = 0.45, height = 0.1))+
  labs(title = "\nMarital Status breakdown with Age of Client", x = "Marital Status of Client", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Education breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = education, y = age, color=y))+
  geom_point(position = position_jitter(width = 0.45, height = 0.1))+
  labs(title = "\nEducation breakdown with Age of Client", x = "Education of Client", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Client in Default breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = default, y = age, color=y))+
  geom_point(position = position_jitter(width = 0.45, height = 0.1))+
  labs(title = "\nClient in default with Age breakdown", x = "Default Status of Client", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Home Loan breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = housing, y = age, color=y))+
  geom_point(position = position_jitter(width = 0.45, height = 0.1))+
  labs(title = "\nClient with a House Loan by Age breakdown", x = "Client has a home loan", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Personal Loan breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = loan, y = age, color=y))+
  geom_point(position = position_jitter(width = 0.45, height = 0.1))+
  labs(title = "\nClient with a Personal Loan by Age breakdown", x = "Client has a personal loan", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Last Contact type breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = contact, y = age, color=y))+
  geom_point(position = position_jitter(width = 0.45, height = 0.1))+
  labs(title = "\nType of Contact with Client by Age breakdown", x = "Type of Contact with Client", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Last Contact Month breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = month, y = age, color=y))+
  geom_point(position = position_jitter(width = 0.4, height = 0.1))+
  labs(title = "\nMonth with last contact by Age breakdown", x = "Month with Last Contact", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Outcome of last campaign breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = poutcome, y = age, color=y))+
  geom_point(position = position_jitter(width = 0.4, height = 0.1))+
  labs(title = "\nOutcome of previous campaign by Age breakdown", x = "Outcome of previous campaign", y = "Age of Client", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Graph for Duration of last contact breakdown with Age of Client
bank_full %>% 
  ggplot(aes(x = age, y = duration, color=y))+
  geom_point(position = position_jitter(width = 0.4, height = 0.1))+
  labs(title = "\nDuration of last contact by Age breakdown", x = "Age of Client", y = "Duration of last contact (sec)", color = "Term\nDeposit\nPurchased\n")+
  theme_bw()

## Modeling

## Steps to complete for Model 1 and 2
## Convert `y` variable to `num`
bank_full$y=ifelse(bank_full$y=='yes',1,0)

## Data Partition 75/25 Split
train_var<-createDataPartition(y=bank_full$y, p=0.75, list = FALSE)
bank_train<-bank_full[train_var,]
bank_test<-bank_full[-train_var,]

## Model 1 -- Logistic Regression
bank_model<-glm(y ~ ., family = "binomial", bank_train)
summary(bank_model)

## Model Prediction
bank_y_predict<- predict(bank_model, bank_test, type='response')

## Generate ROC Curve
bank_model_AUC<-colAUC(bank_y_predict, bank_test$y, plotROC = T)
abline(h=bank_model_AUC, col='Green')
text(.3,.6, cex=.8, labels = paste("Optimal Cutoff:", round(bank_model_AUC,4)))

## Convert probabilities to Class
y_class<- ifelse(bank_y_predict > 0.9075, 1, 0)

## Transform back to factors for comparison
y_class<-factor(y_class)
bank_test$y<-factor(bank_test$y)

#Confusion Matrix
confusionMatrix(y_class, bank_test$y)

## Model 2 -- Neural Network
bank_NNlog<-multinom(y~., data = bank_train)
summary(bank_NNlog)

## Prediction Table
bank_y_Predict2<-predict(bank_NNlog, bank_test)
prediction_table<-table(bank_y_Predict2, bank_test$y)
prediction_table

# Correct Classification
sum(diag((prediction_table))/sum(prediction_table))

#Missclassification
1-sum(diag((prediction_table))/sum(prediction_table))

## Model 3 -- Decision Tree
## Change to factor
bank_full$y <-as.factor(bank_full$y)

## Partition data differently
data.partition<- sample(2,nrow(bank_full), replace = TRUE, prob = c(0.75, 0.25))
tree_train<-bank_full[data.partition==1,]
tree_test<-bank_full[data.partition==2,]

## Build decision Tree
d_tree<-rpart(tree_train$y~tree_train$age + tree_train$job + tree_train$marital +
                tree_train$education + tree_train$default + tree_train$balance +
                tree_train$housing + tree_train$loan + tree_train$contact + 
                tree_train$day + tree_train$month + tree_train$duration + 
                tree_train$campaign + tree_train$pdays + tree_train$previous +
                tree_train$poutcome)
summary(d_tree)

## Plot  training decision tree
rpart.plot(d_tree, extra = 5)

## y prediction
tree.prediction<-predict(d_tree, tree_train, type="class")

# Check levels
levels(tree.prediction)
levels(tree_test$y)

confusionMatrix(tree.prediction, tree_train$y)

## Test data set

d_test_tree<-rpart(tree_test$y~tree_test$age + tree_test$job + tree_test$marital +
                     tree_test$education + tree_test$default + tree_test$balance + tree_test$housing +
                     tree_test$loan + tree_test$contact + tree_test$day + tree_test$month +
                     tree_test$duration + tree_test$campaign + tree_test$pdays + tree_test$previous+
                     tree_test$poutcome)
summary(d_test_tree)

## Plot  test decision tree
rpart.plot(d_test_tree, extra = 5)

## y prediction
tree_test_prediction<-predict(d_test_tree, tree_test, type="class")

# check levels
levels(tree_test_prediction)
levels(tree_test$y)

## confusion matrix
confusionMatrix(tree_test_prediction, tree_test$y)
