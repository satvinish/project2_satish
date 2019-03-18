## Import data 
## by setting Working directory
setwd("C:/Users/satish/Desktop")
Churndata <- read.csv("Churn.csv", na.strings = c(""," ","NA"))   

## Check for duplicates
Churndata<-Churndata[!duplicated(Churndata),]

############################## Missing Values #########################
## Visualize Na terms
library(Amelia)
missmap(Churndata)
sapply(Churndata,function(x) sum(is.na(x)))

#### Delete Obervations with NA values
compTrain <- na.omit(Churndata)

#### Impute mean/median/mode 
library(ggplot2)

#### churn
ggplot(Churndata, aes(1, Churn)) + geom_boxplot()
hist(Churndata$Churn)

# Impute by Median
Churndata$Churn[is.na(Churndata$Churn)]<-
  median(Churndata$Churn, na.rm = T)

## Impute using package imputeMissings
library(imputeMissings)
l<-impute(Churndata, method = "median/mode")

## Mice Package
library(mice)
d<-Churndata[,c(1:21)]
imputed_Data <- mice(d, m=5, maxit = 50, method = 'pmm', seed = 3000)

############################# Outliers Treatment ###################
## churn Variable
library(ggplot2)
ggplot(l, aes(1,Churn)) + geom_boxplot(outlier.colour = "red",
                                            outlier.shape = 2)
## Labeling Outliers 
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

library(dplyr)
l %>%
  mutate(outlier = ifelse(is_outlier(Churn), Churn, as.numeric(NA))) %>%
  ggplot(.,aes(1,Churn)) + geom_boxplot(fill = "steelblue",outlier.colour = "red",
                                             outlier.shape = 2)+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

## Churn
boxplot(l$Churn)
ggplot(l, aes(1,Churn)) + geom_boxplot(outlier.colour = "red",outlier.shape = 2)
qnt <- quantile(l$Churn, 0.75, na.rm = T)
caps <- quantile(l$Churn, 0.95, na.rm = T)
H <- 1.5 * IQR(l$Churn, na.rm = T)
l$Churn[l$Churn > (qnt +  H)] <- caps

#### Bivariate Analysis
## Continuous Variable
contVars<-c("Account.Length","VMail.Message","Day.Mins","Eve.Mins","Night.Mins","Intl.Mins","CustServ.Calls","Int'l.Plan","VMail.Plan",
"Day.Calls","Day.Charge","Eve.Calls","Eve.Charge","Night.Calls")
cont_df<-l[,names(l) %in% contVars]

## Scatter plot
pairs(cont_df)
library(corrplot)
corrplot(cor(cont_df), type = "full", "ellipse")

# 
ggplot(l, aes(Account.Length, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(VMail.Message, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Day.Mins, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Eve.Mins, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Night.Mins, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Intl.Mins, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(CustServ.Calls, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Intl.Plan, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(VMail.Plan, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Day.Calls, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Day.Charge, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Eve.Calls, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Eve.Charge, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Night.Calls, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Night.Charge, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Intl.Calls, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Intl.Charge, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(State, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Area.Code, Churn)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Phone, Churn)) + geom_boxplot(fill = "steelblue")

### Data Modelling

# creating train and test data
str(Churndata)
train_Churn <- Churndata[1:2666,] # creating train data
test_Churn <- Churndata[2667:3333,]# creating test data

#create vector
vector<- seq(0,1,0.1)
vector

## Logistic Regression
str(train_Churn)
logistic<-glm(Churn~., family = "binomial", data = train_Churn)
summary(logistic)

# making predictions
library(caret)
churn.probs<- predict(logistic, type = "response")
prediction <- predict(logistic,newdata=train_Churn,type='response')
for(i in seq(0.1, 0.9, 0.1))
{
  churn.probs<- ifelse(prediction<=i, 0, 1) 
  confusionMatrix <- confusionMatrix(churn.probs,train_Churn$Churn)
}


# Looking at the response encoding
Churn<- as.factor(Churndata$Churn)
contrasts(df$Churn)


# converting probabilities to "Yes" and "No" 
glm.pred = rep("No", length(churn.probs))
glm.pred[churn.probs > 0.5] = "Yes"
glm.pred <- as.factor(glm.pred)

# creating a confusion matrix
confusionMatrix(glm.pred, test_Churn$Churn, positive = "Yes")

#ROC curve 
library(ROCR)
# need to create prediction object from ROCR
pr <- prediction(churn.probs, test_Churn$Churn)

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


# accuarcy check
confusionMatrix <- confusionMatrix(prediction,train_Churn$Churn)

# summarize results
confusionMatrix<- confusionMatrix(predictions,test$Loan_Status)
confusionMatrix

library("Rserve")
Rserve()
