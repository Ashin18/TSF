#ASHIN JOHNSON
#jan2021@grip
#Task1:predict percentage of student using simple linear regression 
#to import data from web
library(RCurl)
marks=read.csv(text = getURL("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"))
#load caret package for machine learning algorithms
library(caret)
#viewing data use view()
View(marks)
#to see the structure of the given data frame,data types
str(marks)
#checking for missing values
sum(is.na(marks))
plot(Hours~Scores,data = marks,main="Hours vs scores",xlab="Hours",ylab = "Scores",col="blue")
#the graph shows relationship b/w x&y may be linear
cor(marks$Hours,marks$Scores)
#the correlation coefficient is very close to 1 which means a +ve trend b/w x&y.
#next is to split the data into 2 datasets;train,test with a ratio of 70%:30%
set.seed(100)
index=createDataPartition(marks$Scores,p=0.7,list = FALSE)
trainset=marks[index,]
testset=marks[-index,]
#training model
model1=train(Scores~Hours,data = trainset,
            method="lm",
            na.action = na.omit,
            preProcess=c("scale","center"),
            trcontrol=trainControl(method = "none"))
# applying model for prediction
model.train=predict(model1,trainset)
model.test=predict(model1,testset)
print(model.train)
print(model.test)
#plotting actual values vs predicted values
plot(trainset$Scores,model.train,col="blue")            
plot(testset$Scores,model.test,col="blue")
summary(model1)
#showing high R^2 value,high F statistic value and low p-value.so our model perfoms well
predict(model1,data.frame(Hours=9.25))
#solution:the percentage of score corresponding to 9.25 hr study time is 93.62852
#Thank you