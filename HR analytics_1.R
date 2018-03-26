#Read csv file
hr <- read.csv("D://Project/HR.csv",sep=",")

#summary of the data set
summary(hr)

#taking left value as a factor
hr$left <- as.factor(hr$left)

#visualizing left varaible
table(hr$left)
print((prop.table(table(hr$left)))*100)
plot(hr$left)

#data cleaning
colSums(is.na(hr))
hr[is.na(hr)] <- 0

#data Visualization
library(ggplot2)
library(rpart)

#bar plot using ggplot
g1 <- ggplot(hr,aes(sales,fill=left,color=left))+geom_bar()
prop.table(table(hr$sales,hr$left))
plot(g1)



g2 <- ggplot(hr,aes(average_montly_hours,fill=left))+geom_bar()
prop.table(table(hr$average_montly_hours,hr$left))
plot(g2)

g3 <- ggplot(hr,aes(hr$last_evaluation,fill=left))+geom_bar()
prop.table(table(hr$last_evaluation,hr$left))
plot(g3)

g4 <- ggplot(hr,aes(hr$number_project,fill=left))+geom_bar()
prop.table(table(hr$number_project,hr$left))
plot(g4)


g5 <- ggplot(hr,aes(hr$time_spend_company,fill=left))+geom_bar()
prop.table(table(hr$time_spend_company,hr$left))
plot(g5)


g6 <- ggplot(hr,aes(hr$Work_accident,fill=left))+geom_bar()
prop.table(table(hr$Work_accident,hr$left))
plot(g6)


g7 <- ggplot(hr,aes(hr$promotion_last_5years,fill=left))+geom_bar()
prop.table(table(hr$promotion_last_5years,hr$left))
plot(g7)


g8 <- ggplot(hr,aes(hr$salary,fill=left))+geom_bar()
prop.table(table(hr$salary,hr$left))
plot(g8)

g9 <- ggplot(hr,aes(hr$satisfaction_level,fill=left))+geom_bar()
plot(g9)

#scatter plot
library(ggplot2)

f1<- ggplot(hr,aes(satisfaction_level,last_evaluation,fill=left,color=left))+geom_jitter()
plot(f1)

f2<- ggplot(hr,aes(satisfaction_level,average_montly_hours,fill=left,color=left))+geom_jitter()
plot(f2)

f3<- ggplot(hr,aes(salary,last_evaluation,fill=left,color=left))+geom_jitter()
plot(f3)

f4<- ggplot(hr,aes(salary,satisfaction_level,fill=left,color=left))+geom_jitter()
plot(f4)


f5<- ggplot(hr,aes(last_evaluation,average_montly_hours,fill=left,color=left))+geom_jitter()
plot(f5)

f6<- ggplot(hr,aes(last_evaluation,Work_accident,fill=left,color=left))+geom_jitter()
plot(f6)


#correlation Plot
hr1 <- hr
hr1$sales<-as.numeric(hr1$sales)

hr1$salary <- factor(hr1$salary, levels = c("low", "medium", "high"), ordered=TRUE)
hr1$left <- as.factor(hr1$left)

hr1$salary <- as.numeric(hr1$salary)
library(corrplot)
M<-cor(hr1)
corrplot(M)

#Classification Decision Tree


library(rpart)

# grow tree 
fit <- rpart(left ~ . , method="class", data = hr)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE)

#Data Slicing
smp_size <- floor(0.75 * nrow(hr))

## set the seed
set.seed(123)
train_ind <- sample(seq_len(nrow(hr)), size = smp_size)

train <- hr[train_ind, ]
test <- hr[-train_ind, ]

train

#Support Vector Machine
library(e1071)
library(caret)
svm_model <- svm(left ~ average_montly_hours+last_evaluation+number_project+time_spend_company+satisfaction_level, data=train,type="C-classification")
summary(svm_model)
pred <- predict(svm_model,test)
hr_model_svm<-cbind(test,pred)
confusionMatrix_svm<- confusionMatrix(hr_model_svm$pred,hr_model_svm$left)
confusionMatrix_svm


# cross-validation
#train_control<- trainControl(method="cv", number=5, repeats=3)
#head(train_control)

# Tree learning
library("rpart")
# train the model 

rpartmodel<- rpart(left~average_montly_hours+last_evaluation+number_project+time_spend_company+satisfaction_level, data=train, method="class")
# make predictions
predictions<- predict(rpartmodel,test,type="class")
hr_model_tree<- cbind(test,predictions)
# summarize results
confusionMatrix_tree<- confusionMatrix(hr_model_tree$predictions,hr_model_tree$left)
confusionMatrix_tree



#LinearRegression
reg_model <- glm(left ~average_montly_hours+last_evaluation+number_project+time_spend_company+satisfaction_level,family=binomial(link='logit'),data=train)
predictions<- predict(reg_model,test)
hr_model_reg<- cbind(test,predictions)

i<-1
for(i in 1:3750)
{
 if(hr_model_reg$predictions[i] > 0) 
   { 
    hr_model_reg$predictions[i] <- 1 
   }
   else 
   {
     hr_model_reg$predictions[i] <- 0
   }
}


# summarize results
confusionMatrix_logreg<- confusionMatrix(hr_model_reg$predictions,hr_model_reg$left)
confusionMatrix_logreg



