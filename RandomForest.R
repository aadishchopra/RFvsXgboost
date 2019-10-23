# type of supervised learning technique where multiple models are trained on a training dataset and their 
#individual outputs are combined by some rule to derive the final output

# following RBloggers tutorial

install.packages("randomForest")
library("randomForest")

# load dataset

car<-read.csv("car.data",col.names = c("buying","maint","doors","persons",
                                       "lug_boot","safety","condition"))
head(car)
str(car)
summary(car)


# split dataset 


set.seed(100)
ind<-sample(2,nrow(car),replace = T,prob = c(0.7,0.3))

train<-car[ind==1,]
test<-car[!ind==1,]

# randomForest model

modelrF<-randomForest(condition~.,data=train,importance=TRUE)

# tuning parameters

modelrF_tune<-randomForest(condition~.,data=train,importance=TRUE,mtry=6)

pred_modelrF_tune<-predict(modelrF_tune,newdata = test,type = "response")

mean(pred_modelrF_tune==test$condition)
table(pred_modelrF_tune,test$condition)



importance(modelrF_tune)


a=c()
i=5
for (i in 3:8) {
  modelrF_tune <- randomForest(condition ~ ., data = train, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(modelrF_tune, test, type = "class")
  a[i-2] = mean(predValid == test$condition)
}

a

plot(3:8,a)


