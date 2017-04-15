#Author: Qazi Zaahirah
require(caret)
require(rpart)
require(e1071)
require(tm)


movie<-read.table(path, sep="\t",encoding="UTF-8")
output<-read.table(outputPath,encoding="UTF-8")

trainIndex <- createDataPartition(output$V1, p=0.80, list=FALSE)
trainIndex = unlist(trainIndex)
data_train <- movie[trainIndex,]
data_test <- movie[-trainIndex,]
output_train <- as.factor(output$V1[ trainIndex])
output_test <-as.factor(output$V1[-trainIndex])

svm.model<-svm(output_train~.,data=data_train,
               kernel="radial", scale =FALSE)

svm.predtrain<-predict(svm.model,data_train)
svm.predtest<-predict(svm.model,data_test)
cf<- confusionMatrix(output_test,svm.predtest)
