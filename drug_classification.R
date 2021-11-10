#set working directory
setwd("D:/R files/R_file/class work")
#import the libraries
library(caret)
library(tibble)

#import dataset
df <- read.csv("D:/R files/R_file/class work/drug200.csv" , stringsAsFactors = T)

#find the unique values
unique(df$Drug)
unique(df$BP)
unique(df$Cholesterol)
unique(df$Sex)

#encode the dataset for further analysis
df$Cholesterol<-ifelse(df$Cholesterol == "HIGH" ,1,0 )
df$BP<- factor(df$BP , levels = c( "HIGH" ,"LOW" ,"NORMAL") , labels = c(0,1,2))
df$Sex<-factor(df$Sex , levels = c("F","M") ,labels = c(0,1))
df$Drug<-factor(df$Drug , levels = c("DrugY", "drugC", "drugX" ,"drugA" ,"drugB") , labels = c(1,2,3,4,5))

#create train and test dataset 
set.seed(44)
index <- createDataPartition(df$Drug ,p=0.7 , list = FALSE)
train<- df[index,]
test<- df[-index,]

#create input and output data
x<-train[,1:5]
y<-train[,6]
#check any missing value 
missing_values <- function(df) {
  missing_val <- c()
  for (c in colnames(df)) {
    missing <- sum(is.na(df[[c]]))
    missing_val <- c(missing_val, missing)
  }
  tibble(colnames(df), missing_val)
}

missing_values(df)

#build the model 
trctrl <- trainControl(method="repeatedcv", number = 12 , repeats = 4)
svm<-train(Drug~., data = train , method = "svmLinear" , 
           trControl=trctrl , preProcess=c("center" , "scale") , 
           tuneLength = 10)

svm

#test the model
test_pred <- predict(svm , newdata = test)
test_pred

#predict the accuracy
confusionMatrix(table(test_pred , test$Drug))

#improve the accuracy
grid <- expand.grid(C = c(0,0.1,0.001,0.25,0.75,0.0001,1))
svm_grid<-train(Drug~., data = train , method = "svmLinear" , 
           trControl=trctrl , preProcess=c("center" , "scale") , 
           tuneLength = 10 , tuneGrid = grid)
svm_grid
#plot svm_grid
plot(svm_grid)

#test improved model 
test_pred2 <- predict(svm_grid, newdata = test)
test_pred2

#predict the accuracy 
confusionMatrix(table(test_pred2 , test$Drug))
#test model
predict(svm_grid , x)





