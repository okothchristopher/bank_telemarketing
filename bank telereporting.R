


#load the packages 
library(ggplot2)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(magrittr)
library(ggthemes)
set.seed(123)
##Data Loading and consolidation
Data <- read.csv(file.choose())

str(Data)
cols = whichNumerics(data = Data)
for (i in cols) {
  Data[,i] = as.numeric(Data[,i])
}
#Is there any missing values in the data?
colSums(is.na(Data))

#Is there empty data
colSums(Data == "")


##plots
plot(x = Data$MARITAL,
     y = Data$Y,
     main = "TELEMARKETING GIVEN MARITAL STATUS",
     ylab = "SUCCESS",
     xlab = "MARITAL STATUS",
     col = c("blue","red"))
##Impact of Level of Education on telemarketing success

ggplot(data = Data,aes(x = EDUCATION,fill = Y))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme_pander()+coord_flip()
###Impact of Job type on telemarketing success
ggplot(data = Data,aes(x = JOB,fill = Y)) + 
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####Impact of Personal Loan on telemarketing success
plot(x = Data$LOAN,y = Data$Y)
##Effect  of  Having a Housing loan on telemarketing success
plot(x = Data$HOUSING,y = Data$Y,col = c("blue","pink"))
##Impact of Credit history on telemarketing success
plot(x = Data$DEFAULTCREDIT,y = Data$Y,col = c("blue","pink"))

##Impact of Communication type on telemarketing success
plot(Data$CONTACT,Data$Y,col = c("grey","pink"))

##Partition the data into training and testing dataset
Data %<>% select(-DURATION)
id <- createDataPartition(Data$Y,times = 1,p = 0.7,list = FALSE)
train <- Data[id,]
test <- Data[-id,]

##Benchmark decision tree
set.seed(123)


benchmark <- rpart(formula = Y~.,
                   data = train,
                   method = "class")
fancyRpartPlot(benchmark)


##Let's predict using the benchmark model
benchmark_predict <- predict(object = benchmark,
                             newdata = train,
                             type = "class")
confusionMatrix(benchmark_predict,train$Y)

### cross validation and further modelling
set.seed(123)
model <- function(X = "DATA", n = "number of crossvalidations"){
  cv.n <- createMultiFolds(train$Y,k = n,times = n)
  #Control
  ctrl <- trainControl(method = "repeatedcv", number = n, repeats = n,
                       index = cv.n)
  
  #Train the model
  model_validate <- Model_CDT <- train(x = X[,-ncol(X)],
                                       y = X[,ncol(X)], 
                                       method = "rpart", 
                                       tuneLength = 30,
                                       trControl = ctrl)
  
  #plot the model
  plot <- rpart.plot(model_validate$finalModel,extra = 3,fallen.leaves = T)
  
  return(plot)
  return(model_validate$finalModel)
}


model_tree <- model(train,5)
prediction <- predict(object = model_tree$obj,
                      newdata = train,
                      type = "class")
confusionMatrix(prediction,train$Y)

#testing the model

model_predict <- function(X = "DATA"){
  predictions <- predict(object = model_tree$obj,
                         newdata = X,
                         type = "class")
  confusionMatrix(predictions,test$Y)
}
model_predict(test)

## Building the logistic regression model 
log_model <- glm(Y~AGE+MARITAL+EDUCATION+DEFAULTCREDIT+HOUSING+LOAN+CONTACT,family="binomial",data = train)


summary(log_model)


##predicting onn new data

pred <- predict(log_model, newdata =test, type = "response")
actual <- as.numeric(Data$Y)
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
## let us check the accuracy of the model
mean <- mean(y_pred == actual1)

