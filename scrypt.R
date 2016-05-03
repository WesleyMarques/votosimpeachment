require(GGally)
require(ggplot2)
require(corrplot)
require(dplyr)
require(caret)
library(mlbench)
library(C50)

dataAll <- read.csv2("~/workspaceR/votosImpeachment/data/deputados_temas_e_impeachment_v1.1.csv");
dataAll <- filter(dataAll, grepl('SIM|NAO', dataAll$IMPEACHMENT)) %>% droplevels()
#dataAll <- dataAll[complete.cases(dataAll),]
dim(dataAll)
str(dataAll)
plot(dataAll[,c(4:25)])
corrplot(dataAll[,c(4:25)])
dataAll[,4]
ggpairs(dataAll, columns = c(6:25))


summary(dataAll)
plot(dataAll)
str(dataAll)
plot(dataAll[c(13,25)],pch=20)
pairs(dataAll$tema_19, dataAll$IMPEACHMENT)

#logistic
split <- createDataPartition(y = dataAll$IMPEACHMENT, p = 0.75, list = F)
train <- dataAll[split,]
test <- dataAll[-split,]
names(train) = names(dataAll) #adicionando cabeçalho aos dados de treino e test
names(test) = names(dataAll)
prop.table(table(train$IMPEACHMENT))
prop.table(table(test$IMPEACHMENT))
grid = expand.grid(.winnow=c(TRUE,FALSE),.trials=c(1,5,10,20,30,40,50,60,70,80,90,100),.model="tree")
fitControl = trainControl(method="repeatedcv",number=10,repeats=10,returnResamp="all")
labels = as.factor(train$IMPEACHMENT)

model = caret::train(x=train[c(4,5)],y=labels,tuneGrid=grid,trControl=fitControl,method="C5.0",verbose=FALSE)
  plot(model)
summary(model)
test_labels = as.factor(test$IMPEACHMENT)
predictions = predict(model,newdata=test[c(4,5,25)])
confusionMatrix(data = predictions, test_labels)



test <- filter(test, grepl("SIM|NAO", test$IMPEACHMENT)) %>% droplevels()
table(test$IMPEACHMENT)
logit <- glm(IMPEACHMENT ~ partido, family = 'binomial', data = train)
summary(logit)
summary(test$partido)
summary(train$partido)
test.probs <-predict(logit, test, type = "response")
pred.logit <- rep(0,length(test.probs))
pred.logit[test.probs >= 0.5] <- 1
table(pred.logit, test$IMPEACHMENT)
table(pred.logit, test$IMPEACHMENT)
confusionMatrix(test$IMPEACHMENT, pred.logit)

table(pred.logit)
table(test$IMPEACHMENT)
test$IMPEACHMENT
pred.logit
