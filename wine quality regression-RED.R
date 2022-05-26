#Renaming dataset

data<-winequality_red    

#Data Type

mode(data) 

#Structure of dataset

str(data)

#Removing Duplicate data entries

data<-unique(data)
str(data)

#Dimension cheching of dataset

dim(data)

#Correlation of data set variables

z<-round(cor(data),digits=2)
z

#Correlation Matrix

corrplot(z)

#Skewness of data

skewness(data)

#Converting from List to Numeric

data2<-sapply(data1,as.numeric)
mode(data2)

#Checking for NULL values & Duplicacy & Dimension

sum(is.na(data))
data2<-unique(data2)
dim(data2) 

#Transforming data using 'BoxCox' transformation to remove skewness
data_process<-preProcess(data2[,1:11],method ="BoxCox")
new_data<-data.frame(trans=predict(data_process,data2))

#Checking Skewness of transformed dataframe

skewness(new_data$trans.fixed.acidity)
skewness(new_data$trans.volatile.acidity)
skewness(new_data$trans.citric.acid)
skewness(new_data$trans.residual.sugar)
skewness(new_data$trans.chlorides)
skewness(new_data$trans.free.sulfur.dioxide)
skewness(new_data$trans.total.sulfur.dioxide)
skewness(new_data$trans.density)
skewness(new_data$trans.pH)
skewness(new_data$trans.sulphates)
skewness(new_data$trans.alcohol)
skewness(new_data$trans.quality)

#Correlation plot of transformed data

corrplot(cor(new_data),type="lower")

#splitting dataset

set.seed(101)
training<-sample.split(new_data,SplitRatio=0.8)
train<-subset(new_data,training==T)
test<-subset(new_data,training==F)

#Running Linear Regression

linear<-lm(trans.quality~.,train)
summary(linear)

#Anova techinques: Variance Inflation Factor

vif(linear)

#Akaike Information Criteria

stepAIC(linear)

#logistic Regression

data$category[data$quality <=5]<- 0
data$category[data$quality>5]<- 1

logit<-glm(category~.-quality,data,family='binomial')
summary(logit)

#Splitting Dataset

set.seed(101)
spliting<-sample.split(data$category,SplitRatio = 0.8)
train_new<-subset(data,spliting==T)
test_new<-subset(data,spliting==F)

#Regression after removing variables

logik<-glm(category~.,data=train_new[,-c(1,3,4,8,9,12)],family='binomial')
summary(logik)

#Prediction

prob<-predict(logik,type='response')
trend<-ifelse(prob>0.5,"Good wine","Bad wine")

#Confusion matrix

confusion<-table(Predicted=trend,Actual=train_new$category)
confusion

#Plotting Confusion matrix

confusion1<-data.frame(confusion)
library(ggplot2)
tile<-ggplot(confusion1,aes(x=confusion1$Predicted,y=confusion1$Actual,
                            fill=confusion1$Freq))+geom_tile()
tile<-tile+geom_text(aes(label=confusion1$Freq),size=5)
tile

#Accuracy of training set

accuracy_train<-sum(diag(confusion))/sum(confusion)
accuracy_train

#test data
prob_test<-ifelse(predict(logik,newdata = test_new,type='response')>0.5,"Good wine","Bad Wine")
test_table<-table(Predicted=prob_test,Actual=test_new$category)
test_table
accuracy_test<-sum(diag(test_table))/length(test_new$category)
accuracy_test
