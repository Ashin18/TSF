#ASHIN JOHNSON
#jan2021@grip
#Task1:predict percentage of student using simple linear regression 
#to import data from web
library(RCurl)
marks=read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv", header = T)
#viewing data use view()
View(marks)
#to see the structure of the given data frame,data types
str(marks)
#checking for missing values
which(is.na(marks))
plot(Hours~Scores,data = marks,main="Hours vs scores",xlab="Hours",ylab = "Scores",col="blue")
#the graph shows relationship b/w x&y may be linear
cor(marks$Hours,marks$Scores)
#the correlation coefficient is very close to 1 which means a +ve trend b/w x&y.
#next is to split the data into 2 datasets;train,test with a ratio of 60%:30%
index=sample(1:nrow(marks), 0.6*nrow(marks), replace = F)
trainset=marks[index,]
testset=marks[-index,]
#training model
model1=lm(Scores ~., trainset)
summary(model1) #95% of outcome variable's variability can be explained 
at = c(model1$df.residual, summary(model1)$r.squared, summary(model1)$sigma, anova(model1)["Residuals", "Sum Sq"])
at = as.data.frame(at, optional = T)
row.names(at) = c("Residual df", "Multiple-R squared", "std.dev estimate", "Residual SS")
at
model2 = predict(model1, testset)
residual = testset$Scores - model2
library(rminer)
measure_train = mmetric(trainset$Scores, model1$fitted.values, c("SSE", "RMSE", "ME"))
print(round(measure_train, 3))
mmetric(testset$Scores, model2, c("SSE", "RMSE", "ME"))
range(residual)
boxplot(residual, ylim = c(-15, 12))
predict(model1,data.frame(Hours=9.25))
#solution:the percentage of score corresponding to 9.25 hr study time is 96.83968
#Thank you
