View(mtcars)
library(caTools)
sample.split(mtcars$mpg,SplitRatio = 0.65)->mysplit
train <- subset(mtcars,mysplit==T)
test <- subset(mtcars,mysplit==F)
lm(mpg~.,data=train)->mod1
predict(mod1,test)->resulT
cbind(actual=test$mpg,predicted=resulT)->final
as.data.frame(final)->final
cbind(final,error=final$actual-final$predicted)->final
rmsel <- sqrt(mean(final$error^2))
rmsel


lm(mpg~.-gear,-carb,data = train) -> mod2
predict(mod2,test)->result1
cbind(actual=test$mpg,predicted=result1)->final1
as.data.frame(final1)->final1
cbind(final1,error1=final1$actual-final$predicted)->final1
rmse2 <- sqrt(mean((final1$error1)^2))
rmse2

#classification 
library(rpart)
read.csv("C:/Users/OWUSU/Desktop/ML n DL/kaggle/adult.csv",header=TRUE)->census_data
View(census_data)
sample.split(census_data$income,SplitRatio = 0.65)-> mysplit
train <- subset(census_data,mysplit=T)
test <- subset(census_data,mysplit=F)
rpart(income~.,data = train,method = "class")-> mod_cview_classifier
predict(mod_cview_classifier,test,type = "class") -> result
library(caret)
library(e1071)
confusionMatrix(test$income,result)


View(iris)
table(iris$Species)
iris[,-5]->iris4
kmeans(iris4,3)->k1
cbind(iris4,k1$cluster)->iris4
cbind(iris4,iris$Species)->iris4
