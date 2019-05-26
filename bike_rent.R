#clearing all the contents of the environment
rm(list=ls())
#get the working directory
getwd()

#set the working directory
setwd("C:/Users/Gaurav's BEAST/Desktop/project3edwisor")

#Load Libraries
required_library <- c('ggplot2', 'corrgram', 'corrplot', 'randomForest',
                      'caret', 'class', 'e1071', 'rpart', 'mlr','grid',
                      'DMwR','usdm','dplyr','caTools','LiblineaR')

# reading csv file
bike_data = read.csv("day.csv")

# cheking datatypes of all columns
str(bike_data)

numeric_columns = c('temp', 'atemp', 'hum', 'windspeed', 'casual', 'registered', 'cnt')
cat_columns = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')

summary(bike_data)

# checking missing value for each column and storing counting in dataframe with column name
missing_val <- data.frame(lapply(bike_data, function(feat) sum(is.na(feat))))

# creating another dataset with dropping outliers i.e. bike_data_wo
bike_data_wo <- bike_data

# removing outliers from hum and windspeed columns
for (i in c('hum', 'windspeed')){
  out_value = bike_data_wo[,i] [bike_data_wo[,i] %in% boxplot.stats(bike_data_wo[,i])$out]
  bike_data_wo = bike_data_wo[which(!bike_data_wo[,i] %in% out_value),]
}

# dropping unwanted columns
drop_col=c('instant', 'dteday', 'holiday', 'atemp', 'casual', 'registered')
bike_data[,drop_col]= NULL
bike_data_wo[,drop_col] = NULL

set.seed(1)
split = sample.split(bike_data$cnt, SplitRatio = 0.80)
train_set = subset(bike_data, split == TRUE)
test_set = subset(bike_data, split == FALSE)

split = sample.split(bike_data_wo$cnt, SplitRatio = 0.80)
train_set_wo = subset(bike_data_wo, split == TRUE)
test_set_wo = subset(bike_data_wo, split == FALSE)

fit.predict.show.performance <- function(method, train_data, test_data){
  reg_fit <- caret::train(cnt~., data = train_data, method = method)
  
  y_pred <- predict(reg_fit, test_data[,-10])
  print("R2 on test dataset")
  print(caret::R2(y_pred, test_data[,10])) 
  
  y_pred <- predict(reg_fit, train_data[,-10])
  print("R2 on train dataset")
  print(caret::R2(y_pred, train_data[,10]))
  
  # creating 10 folds of data
  ten_folds = createFolds(train_data$cnt, k = 10)
  ten_cv = lapply(ten_folds, function(fold) {
    training_fold = train_data[-fold, ]
    test_fold = train_data[fold, ]
    reg_fit <- caret::train(cnt~., data = training_fold, method = method)
    
    y_pred <- predict(reg_fit, test_fold[,-10])
    return(as.numeric(caret::R2(y_pred, test_fold[,10]))) 
  })
  sum = 0
  for(i in ten_cv){
    sum = sum + as.numeric(i)
  }
  print("K-fold (K =10) explained variance")
  print(sum/10)
}

#linear regression
# building model for dataset bike_data
fit.predict.show.performance('lm', train_set, test_set)

# building model for dataset bike_data_wo i.e. without  outliers
fit.predict.show.performance('lm', train_set_wo, test_set_wo)

#k nearest neighbors
# building model for dataset bike_data
fit.predict.show.performance('knn', train_set, test_set)

# building model for dataset bike_data_wo i.e. without  outliers
fit.predict.show.performance('knn', train_set_wo, test_set_wo)

#support vector machine
# building model for dataset bike_data
fit.predict.show.performance('svmLinear3', train_set, test_set)

# building model for dataset bike_data_wo i.e. without  outliers
fit.predict.show.performance('svmLinear3', train_set_wo, test_set_wo)

#decision tree
# building model for dataset bike_data
fit.predict.show.performance('rpart2', train_set, test_set)

# building model for dataset bike_data_wo i.e. without  outliers
fit.predict.show.performance('rpart2', train_set_wo, test_set_wo)

#random forest
# building model for dataset bike_data
fit.predict.show.performance('rf', train_set, test_set)

# building model for dataset bike_data_wo i.e. without  outliers
fit.predict.show.performance('rf', train_set_wo, test_set_wo)

#xgboost 
# building model for dataset bike_data
fit.predict.show.performance('xgbTree', train_set, test_set)

# building model for dataset bike_data_wo i.e. without  outliers
fit.predict.show.performance('xgbTree', train_set_wo, test_set_wo)

