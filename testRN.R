require(GGally)
require(ggplot2)
require(corrplot)
require(dplyr)
require(caret)
library(mlbench)
library(C50)
require(RSNNS)
library(kohonen)



dataAll <- read.csv("./data/deputados_temas_e_impeachment_v1.1.csv");
words <- read.csv("./data/keywords.csv",header = F)
dataAll <- filter(dataAll, grepl('sim|nao', dataAll$IMPEACHMENT)) %>% droplevels()
dataAll = dataAll[sample(1:nrow(dataAll), length(1:nrow(dataAll))), 1:ncol(dataAll)]
dataTest <- dataAll[1:100,]
dataTest <- dataTest[-c(1,2,3)]

newDataFrame <- c()
for (i in 1:nrow(dataTest)) {
  for (j in 3:ncol(dataTest)) {
    newDataFrame <- rbind(newDataFrame,data.frame(dataTest[i,c(1,2)],voto = dataTest[i,j],kw1=words[j-2,1],kw2=words[j-2,2],kw3=words[j-2,3]))
  }
}
dataTest <- newDataFrame
dataTest <- dataTest[sample(1:nrow(dataTest), length(1:nrow(dataTest))), 1:ncol(dataTest)]



dadosValues <- cbind(dataTest[, -c(3)])
dadosValuesDecode <- c()
for (i in 1:ncol(dadosValues)) {
  dadosValuesDecode <- cbind(dadosValuesDecode, decodeClassLabels(dadosValues[,i]))
}
dadosTargets <- dataTest[, c(3)]

dadosTargetDecode <- decodeClassLabels(dadosTargets)

dataAll2 = splitForTrainingAndTest(dadosValuesDecode, dadosTargetDecode, ratio = 0.15)

#model <- RSNNS::som(dataAll2$inputsTrain, mapX = 3, mapY = 3, maxit = 3000, saveWinnersPerPattern = T,targets=dataAll2$targetsTrain)
model2 <- mlp(dataAll2$inputsTrain, dataAll2$targetsTrain, size = 30, learnFuncParams = 0.11, maxit = 100, inputsTest = dataAll2$inputsTest, targetsTest = dataAll2$targetsTest, linOut = FALSE)

#predictions <- predict(model,dataAll2$inputsTest)
predictions2 <- predict(model2,dataAll2$inputsTest)

#confusionMatrix(dataAll2$targetsTest,predictions)

sse = sum((model2$fittedTestValues-dataAll2$targetsTest)^2)
rmse = sqrt(mean((model2$fittedTestValues-dataAll2$targetsTest)^2))
par(mfrow=c(1,1))
plotIterativeError(model2, main="Erro Iterativo")
plotIterativeError(model2)
plotRegressionError(predictions2[,2], dataAll2$targetsTest[,2], main="Erro de Regressão")

confusionMatrix(dataAll2$targetsTrain,fitted.values(model2))
predictions2_changed <- ifelse(predictions2 > 0.1, 1, 0)
confusionMatrix(dataAll2$targetsTest,predictions2_changed)
confusionMatrix(dataAll2$targetsTest,predictions2)

plotROC(fitted.values(model2)[,2], dataAll2$targetsTrain[,2], main="ROC train")
plotROC(predictions2[,2], dataAll2$targetsTest[,2], ylim=c(0,1), main="ROC test")

#confusion matrix with 402040-method
confusionMatrix(dataAll2$targetsTrain, encodeClassLabels(fitted.values(model2),method="402040", l=0.4, h=0.6))



