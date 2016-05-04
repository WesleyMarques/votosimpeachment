require(GGally)
require(ggplot2)
require(corrplot)
require(dplyr)
require(caret)
library(mlbench)
library(C50)

dataAll <- read.csv2("./data/deputados_temas_e_impeachment_v1.1.csv");
dataAll <- filter(dataAll, grepl('SIM|NAO', dataAll$IMPEACHMENT)) %>% droplevels()
#dataAll <- dataAll[complete.cases(dataAll),]
split <- createDataPartition(y = dataAll$IMPEACHMENT, p = 0.75, list = F)
train <- dataAll[split,]
test <- dataAll[-split,]
names(train) = names(dataAll) #adicionando cabeçalho aos dados de treino e test
names(test) = names(dataAll)



#tree
grid = expand.grid(.winnow=c(TRUE,FALSE),.trials=c(1,5,10,20,30,40,50,60,70,80,90,100),.model="tree")
fitControl = trainControl(method="repeatedcv",number=10,repeats=10,returnResamp="all")
labels = as.factor(train$IMPEACHMENT)

model = caret::train(x=train[c(4,5)],y=labels,tuneGrid=grid,trControl=fitControl,method="C5.0",verbose=FALSE)
plot(model)
summary(model)
test_labels = as.factor(test$IMPEACHMENT)
predictions = predict(model,newdata=test[c(4,5,25)])
confusionMatrix(data = predictions, test_labels)


#logistic
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
table(test$IMPEACHMENT)
#logit <- glm(IMPEACHMENT ~ UF+partido, family = binomial(link = "logit"), data = train[c(4,5,25)])
logit <- train(IMPEACHMENT ~ UF+partido, 
               method='glm', 
               family='binomial', 
               data=train[c(4,5,25)], 
               trControl = ctrl)
test.probs <-predict(logit, test[c(4,5)], type = "prob")
test.probs <- levels(test$IMPEACHMENT)[apply(predict(logit,test[c(4,5)],type="prob"), 1, which.max)]
#pred.logit <- rep("NAO",length(test.probs))
#pred.logit[test.probs >= 0.78] <- "SIM"
#test2 <- test[complete.cases(test),]
table(as.factor(a),test$IMPEACHMENT)
confusionMatrix(as.factor(test$IMPEACHMENT), as.factor(test.probs))

#KNN
ctrl_knn <- trainControl(method = "repeatedcv", number = 10)
knnFit <- train(IMPEACHMENT ~ .-id_dep-nome-deputado , 
                data = train, 
                method = "knn", 
                trControl = ctrl_knn,
                preProcess = c("center","scale"), 
                tuneGrid = expand.grid(.k = 2:10),
                metric = "Accuracy")
test_knn <- test[complete.cases(test),]
confusionMatrix(test2$IMPEACHMENT, predict(knnFit, test_knn[-c(25)]))
