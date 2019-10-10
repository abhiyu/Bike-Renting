#Project Title:- Bike Renting using R

#Problem Statemnt:- The objective of this Case is to Predication of bike rental count on daily based on the environmental and seasonal settings.

#Clearing Objects:-
rm(list=ls(all=T))

#Loading Libraries:-
library(tidyverse)
library(DMwR)
library(corrgram)
library(caret)
library(rpart)
library(randomForest)
library(rpart.plot)

#Setting Directory:-
setwd("D:/Practice_R")

#Importing the training Data:-
df_bike<-read.csv("day.csv")
head(df_train)

#Creating new dataset excluding casual and registered variables due to total_count is combined of both:-
df_bike<-subset(df_bike,select=-c(casual,registered))
head(df_bike,5)

#Dimension of dataset:-
dim(df_bike)

#Summary of the dataset:-
summary(df_bike)

#Structure of dataset:-
str(df_bike)

#Renaming the columns for better understanding:-
names(df_bike)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')

#Read the data:-
head(df_bike,5)

#Typecasting the datetime and numerical attributes to category:-

df_bike$datetime<- as.Date(df_bike$datetime)
df_bike$year<-as.factor(df_bike$year)
df_bike$month<-as.factor(df_bike$month)
df_bike$season <- as.factor(df_bike$season)
df_bike$holiday<- as.factor(df_bike$holiday)
df_bike$weekday<- as.factor(df_bike$weekday)
df_bike$workingday<- as.factor(df_bike$workingday)
df_bike$weather_condition<- as.factor(df_bike$weather_condition)

#Missing values in dataset:-
missing_val<-data.frame(apply(df_bike,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val

#Monthly distribution of counts:-

#Column plot for season wise monthly distribution of counts:-
ggplot(df_bike,aes(x=month,y=total_count,fill=season))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Season wise monthly distribution of counts')
#Column plot for weekday wise monthly distribution of counts:-
ggplot(df_bike,aes(x=month,y=total_count,fill=weekday))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Weekday wise monthly distribution of counts')

#Observation:- From the above plots, we can observed that increasing the bike rental count in springer and summer season and then decreasing the bike rental count in fall and winter season.
#Here, season 1 -> spring season 2 -> summer season 3 -> fall season 4 -> winter.

#Violin plot for Yearly wise distribution of counts:-
ggplot(df_bike,aes(x=year,y=total_count,fill=year))+geom_violin()+theme_bw()+
  labs(x='Year',y='Total_Count',title='Yearly wise distribution of counts')

#Observation:- From the violin plot, we can observed that the bike rental count distribution is highest in year 2012 then the in year 2011.
#Here, year 0-> 2011, year 1-> 2012.

#Column plot for holiday wise distribution of counts:-
ggplot(df_bike,aes(x=holiday,y=total_count,fill=season))+geom_col()+theme_bw()+
  labs(x='holiday',y='Total_Count',title='Holiday wise distribution of counts')

#Observation:- From the above bar plot, we can observed that during no holiday the bike rental counts is highest compared to during holiday for different seasons.
#Here, 0->No holiday, 1-> holiday

#Column plot for workingday wise distribution of counts:-
ggplot(df_bike,aes(x=workingday,y=total_count,fill=season))+geom_col()+theme_bw()+
  labs(x='workingday',y='Total_Count',title='Workingday wise distribution of counts')

#Observation:-From the above bar plot, we can observed that during workingday the bike rental counts is quite highest compared to during no workingday for different seasons.
#Here, 0-> No workingday, 1-> workingday

#Column plot for weather_condition distribution of counts:-
ggplot(df_bike,aes(x=weather_condition,y=total_count,fill=season))+geom_col()+theme_bw()+
  labs(x='Weather_condition',y='total_count',title='Weather_condition distribution of counts')

#Observation:-From the above bar plot, we can observed that during clear,partly cloudy weather the bike rental count is highest and the second highest is during mist cloudy weather and followed by third highest during light snow and light rain weather.

#Outlier Analysis:-
#Boxplot for total_count_outliers:-
par(mfrow=c(1, 1))#divide graph area in 1 columns and 1 rows
boxplot(df_bike$total_count,main='Total_count',sub=paste(boxplot.stats(df_bike$total_count)$out))

#Observation:-From the box plot, we can observed that no outliers are present in total_count variable.


#Box plots for outliers:-
par(mfrow=c(2,2))
#Box plot for temp outliers:-
boxplot(df_bike$temp, main="Temp",sub=paste(boxplot.stats(df_bike$temp)$out))
#Box plot for humidity outliers:-
boxplot(df_bike$humidity,main="Humidity",sub=paste(boxplot.stats(df_bike$humidity)$out))
#Box plot for windspeed outliers:-
boxplot(df_bike$windspeed,main="Windspeed",sub=paste(boxplot.stats(df_bike$windspeed)$out))

#Observation:- From the box plot, we can observed that no outliers are present in normalized temp but few outliers are present in normalized windspeed and humidity variable.

#Replace and imputate the outliers:-
#create subset for windspeed and humidity variable:-
wind_hum<-subset(df_bike,select=c('windspeed','humidity'))
#column names of wind_hum:-
cnames<-colnames(wind_hum)
for(i in cnames){
  val=wind_hum[,i][wind_hum[,i] %in% boxplot.stats(wind_hum[,i])$out] #outlier values
  wind_hum[,i][wind_hum[,i] %in% val]= NA  # Replace outliers with NA 
}
#Imputating the missing values using mean imputation method
wind_hum$windspeed[is.na(wind_hum$windspeed)]<-median(wind_hum$windspeed,na.rm=T) 
wind_hum$humidity[is.na(wind_hum$humidity)]<-mean(wind_hum$humidity,na.rm=T)

#Remove the windspeed and humidity variable in order to replace imputated data:-
new_df<-subset(df_bike,select=-c(windspeed,humidity))
#Combined new_df and wind_hum data frames:-
df_bike<-cbind(new_df,wind_hum)
head(df_bike,5)

#Correlation plot
corrgram(df_bike[,10:14],order=F,upper.panel=panel.pie,text.panel=panel.txt,main='Correlation Plot')

#Observation:- From correlation plot, we can observed that some features are positively correlated and some are negatively correlated to each other. The temp and atemp are highly positively correlated to each other, it means that both are carrying same information. So, we are going to ignore atemp variable for further analysis.

#Feature Scaling:-

hist(df_bike$temp)
hist(df_bike$humidity)
hist(df_bike$windspeed)
hist(df_bike$total_count)

#Observation:- Observation:- Till here, our features are selected and they are already normalized.So, we don't need to perform feature scaling.

#Split the dataset into train and test dataset:-
#Split the dataset based on simple random resampling--
train_index<-sample(1:nrow(df_bike),0.7*nrow(df_bike))
train_data<-df_bike[train_index,]
test_data<-df_bike[-train_index,]
dim(train_data)
dim(test_data)

#Reading the train and test data:-
head(train_data,5)
head(test_data,5)

#Create a new subset for train attributes :-
train<-subset(train_data,select=c('season','year','month','holiday', 'weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
#Create a new subset for test attributes
test<-subset(test_data,select=c('season','year','month','holiday','weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
head(train,5)
head(test,5)

#create a new subset for train categorical attributes:-
train_cat_attributes<-subset(train,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for test categorical attributes:-
test_cat_attributes<-subset(test,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for train numerical attributes:-
train_num_attributes<-subset(train,select=c('weekday','month','temp','humidity','windspeed','total_count'))
#create a new subset for test numerical attributes:-
test_num_attributes<-subset(test,select=c('weekday','month','temp', 'humidity','windspeed','total_count'))

#Encoding the categorical features:-
#other variables along with target variable to get dummy variables
othervars<-c('month','weekday','temp','humidity','windspeed','total_count')
set.seed(2626)
#Categorical variables:-
vars<-setdiff(colnames(train),c(train$total_count,othervars))
#formula pass through encoder to get dummy variables:-
f <- paste('~', paste(vars, collapse = ' + '))
#encoder is encoded the categorical variables to numeric:-
encoder<-dummyVars(as.formula(f), train)
#Predicting the encode attributes:-
encode_attributes<-predict(encoder,train)
#Binding the train_num_attributes and encode_attributes:-
train_encoded_attributes<-cbind(train_num_attributes,encode_attributes)
head(train_encoded_attributes,5)


#Test_encoded_attributes:-

set.seed(5662)
#Categorical variables
vars<-setdiff(colnames(test),c(test$total_count,othervars))
#formula pass through encoder to get dummy variables
f<- paste('~',paste(vars,collapse='+'))
#Encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f),test)
#Predicting the encoder attributes
encode_attributes<-predict(encoder,test)
#Binding the test_num_attributes and encode_attributes
test_encoded_attributes<-cbind(test_num_attributes,encode_attributes)
head(test_encoded_attributes,5)

#Modelling the training dataset:-

#Linear Regression Model:-
#Set seed to reproduce the results of random sampling:-
set.seed(672)
#training the lr_model
lr_model<-lm(train_encoded_attributes$total_count~.,train_encoded_attributes[,-c(6)])
#Summary of the model
summary(lr_model)

#Cross validation prediction:-

#To ignore warning messages:-
options(warn=-1)
#Set seed to reproduce results of random sampling:-
set.seed(623)
#Cross validation resampling method:-
train.control<-trainControl(method='CV',number=3)
#Cross validation prediction:-
CV_predict<-train(total_count~.,data=train_encoded_attributes,method='lm',trControl=train.control)
#Summary of cross validation prediction:-
summary(CV_predict)

#Cross validation prediction plot:-
residuals<-resid(CV_predict)
y_train<-train_encoded_attributes$total_count
plot(y_train,residuals,ylab=('Residuals'),xlab=('Observed'),main=('Cross validation prediction plot'))
abline(0,0)

#Observation:-Cross validation prediction plot tells about finite variance between actual target value and predicted target value. In this plot, some data points are have same finite variance between them and for some are not have it.

#Model performance on test data:-

set.seed(6872)
options(warn=-1)
#predict the lr_model
lm_predict<- predict(lr_model,test_encoded_attributes[,-c(6)])
head(lm_predict,5)


#Root mean squared error:-
rmse<-RMSE(lm_predict, test_encoded_attributes$total_count)
print(rmse)
#Mean squared error:-
mae<-MAE(lm_predict, test_encoded_attributes$total_count)
print(mae)

#Residual plot:-
y_test<-test_encoded_attributes$total_count
residuals<-y_test-lm_predict
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)

#Observation:-Residual plot tells about finite variance between actual target value and predicted target value. In this plot, some data points are have same finite variance between them and for some are not have it.

#Decision Tree Regressor:-

#Training the model:-
set.seed(568)
#rpart.control to contro the performance of model:-
rpart.control<-rpart.control(minbucket = 2,cp = 0.01,maxcompete = 3, maxsurrogate = 4, usesurrogate = 2, xval = 3,surrogatestyle = 0, maxdepth = 10) 
#training the dtr model
dtr<-rpart(train_encoded_attributes$total_count~.,data=train_encoded_attributes[,-c(6)],control=rpart.control,method='anova',cp=0.01)
#Summary of dtr model
dtr

#Visualize the learned decision tree model:-
rpart.plot(dtr, box.palette="RdBu", shadow.col="gray", nn=TRUE,roundint=FALSE)

#Cross validation prediction:-
options(warn=-1)
set.seed(5769)
#cross validation resampling method:-
train.control<-trainControl(method='CV',number=3)
#cross validation pred:-
dtr_CV_predict<-train(total_count~.,data=train_encoded_attributes,method='rpart',trControl=train.control)
dtr_CV_predict

#Cross validation prediction plot:-
residuals<-resid(dtr_CV_predict)
plot(y_train,residuals,xlab='Observed',ylab='Residuals',main='Cross validation plot')
abline(0,0)

#Model performance on the test dataset:-
set.seed(7882)
#predict the trained model:-
dtr_predict<-predict(dtr,test_encoded_attributes[,-c(6)])
head(dtr_predict,5)

#Root mean squared error:-
set.seed(6889)
rmse<-RMSE(y_test,dtr_predict)
print(rmse)
#Mean absolute error:-
mae<-MAE(y_test,dtr_predict)
print(mae)

#Residual plot:-
residuals<-y_test-dtr_predict
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)

#Random Forest:-
set.seed(6788271)
#training the model
rf_model<-randomForest(total_count~.,train_encoded_attributes,importance=TRUE,ntree=200)
rf_model

#Cross validation prediction for Random Forest:-
options(warn=-1)
set.seed(6772)
#load the ranger library for random forest CV:-
library(ranger)
#Cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#Cross validation prediction
rf_CV_predict<-train(total_count~.,train_encoded_attributes,method='ranger',trControl=train.control)
rf_CV_predict

#Cross validation prediction plot:-
residuals<-resid(rf_CV_predict)
plot(y_train,residuals,xlab='Observed',ylab='Residuals',main='Cross validation prediction plot')
abline(0,0)

#Model performance on the test dataset:-
set.seed(7889)
#Predicting the model:-
rf_predict<-predict(rf_model,test_encoded_attributes[,-c(6)])
head(rf_predict,5)

#Root mean squared error:-
set.seed(667)
rmse<-RMSE(y_test,rf_predict)
print(rmse)
mae<-MAE(y_test,rf_predict)
print(mae)

#Residual plot:-
residuals<-y_test-rf_predict
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)

#Observation:-Residual plot tells about finite variance between actual target value and predicted target value. In this plot, some data points are have same finite variance between them and for some are not have it.

#Final model for predicting the bike rental count:-

#When we compare the root mean squared error and mean absolute error of all 3 models, the random forest model has less root mean squared error and mean absolute error. So, finally random forest model is best for predicting the bike rental count on daily basis.

Bike_prediction=data.frame(y_test,rf_predict)
write.csv(Bike_prediction,'Bike_Renting_R.CSV',row.names=F)
Bike_prediction
