
# Reading the data
employeeData<-read.csv("dataset/338_cert_proj_datasets_v3.0.csv")

# displaying the data
employeeData

# Converting non numeric data to numeric for some calculations
employeeDataNum<-employeeData
employeeDataNum$department<-as.numeric(employeeDataNum$department)
employeeDataNum$salary<-as.numeric(employeeDataNum$salary)
employeeDataNum

# Finding and reading correlation
correlation <- cor(employeeDataNum)

correlation

install.packages('corrplot', dependencies=TRUE)
library(corrplot)

corrplot(correlation,type='lower')

# Subsetting employees who left
employee_left<-subset(employeeData, left==1)
employee_left

# Subsetting employees who have not left
employee_nonleft<-subset(employeeData, left==0)
employee_nonleft

# Displaying salart data and relation between last evalutation and satisfaction level
plot(employeeData$salary)
plot(employeeData$last_evaluation, employeeData$satisfaction_level)

# Coparing total vs emplyees left and non-left
hist(employeeData$last_evaluation)
hist(employee_left$last_evaluation)
hist(employee_nonleft$last_evaluation)

# Generating and Analyzing summaries
summary(employeeData)
summary(employee_left)
summary(employee_nonleft)

# Employees left based on department
plot(employee_left$department)

# use library dplyr
library( dplyr )

# Finding percentage of employee left over employee left count based on department
employee_left %>% 
    group_by( employee_left$department ) %>% 
    summarise( percent = 100 * n() / nrow( employee_left ) )

# Finding percantage of employee left over total employee count based on department
employeeData %>% 
    filter(employeeData$left == 1) %>%
    group_by( employee_left$department) %>% 
    summarise( percent = 100 * n() / nrow( employeeData ) )

# Using decision tree for analysis
employeeData<-read.csv("dataset/338_cert_proj_datasets_v3.0.csv")
employeeData

set.seed(3)
id<-sample(2,nrow(employeeData),prob=c(0.7,0.3),replace = T)
traindata<-employeeData[id==1,]
testdata<-employeeData[id==2,]
traindata

library(rpart)
employee_tree<-rpart(formula = left ~ .,data=traindata)
employee_tree

plot(employee_tree, margin=0.1)
text(employee_tree,pretty=T,cex=0.7)

# using decision tree with yes no factor columns for better clarity
employeeData<-read.csv("dataset/338_cert_proj_datasets_v3.0.csv")
employeeData$left<-ifelse(employeeData$left==1,"YES","NO")
employeeData$left<-as.factor(employeeData$left)
employeeData

set.seed(3)
id<-sample(2,nrow(employeeData),prob=c(0.7,0.3),replace = T)
traindata<-employeeData[id==1,]
testdata<-employeeData[id==2,]
employee_tree<-rpart(formula = left ~ .,data=traindata)
employee_tree

plot(employee_tree, margin=0.1)
text(employee_tree,pretty=T,cex=0.7)

predtree<-predict(employee_tree,testdata,type="class")
predtree

install.packages('e1071', dependencies=TRUE)

library(caret)

confusionMatrix(table(predtree,testdata$leftlibrary(randomForest)))
# accuracy 96%  

# using random forest for analysis
library(randomForest)

employee_forest<-randomForest(left~.,data=traindata)
predforest<-predict(employee_forest,testdata,type="class")
confusionMatrix(table(predforest,testdata$left))
# Accuracy 99%

# using naive bayes
library(e1071)


employee_naive<-naiveBayes(left~.,data=traindata)
pred_naive<-predict(employee_naive,testdata,type="class")
confusionMatrix(table(pred_naive,testdata$left))
# Accuracy 78%

?svm

# using svm
employee_svm<-svm(left~.,data=traindata)
pred_svm<-predict(employee_svm,testdata,type="class")
confusionMatrix(table(pred_svm,testdata$left))
#Accuracy 95%
