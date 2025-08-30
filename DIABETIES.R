dia_data<-read.csv(file.choose(), header=TRUE, sep="\t")
str(dia_data)
head(dia_data)
library(caret)
#remove a col
dis_data$ID<-NULL
str(dis_data)
 # convert  to factor
 dis_data$Diabet<-factor(dis_data$Diabet, levels = c(0,1))
str(dis_data) 

sum(is.na(dis_data))#check missing data
#split data 
set.seed(1235)

index<-createDataPartition(dis_data$Diabet,p=0.7, list = FALSE)

train_data<-dis_data[index, ]

test_data<-dis_data[-index, ]

summary(train_data)

summary(test_data)
head(train_data)

#model
dis_model<-glm(Diabet~BMI+AGE+PGL, data = train_data, family=binomial)
summary(dis_model)
dis_predict<-predict(dis_model, newdata = test_data, type="response")
dis_predict
dis_predict_class<-ifelse(dis_predict>0.5,1,0)
str(dis_predict_class)
dis_predict_class<-factor(dis_predict_class, levels=c(0,1))
confusionMatrix(dis_predict_class, test_data$Diabet)
