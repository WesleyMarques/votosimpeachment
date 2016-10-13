require(GGally)
require(ggplot2)
require(corrplot)
require(dplyr)
require(caret)
library(mlbench)
library(C50)
require(RSNNS)

dataAll <- read.csv2("./data/deputados_temas_e_impeachment_v1.1.csv");
dataAll <- filter(dataAll, grepl('SIM|NAO', dataAll$IMPEACHMENT)) %>% droplevels()

dataTest <- dataAll[c(1:3),]
dataTest <- dataTest[-c(1)]
newTest <- c()
for (i in 1:ncol(dataTest)) {
  newTest = cbind(newTest, decodeClassLabels(dataTest[,i]))
  print(nrow(dataTest[,i]))
}
model = som(newTest, mapX = 5, mapY = 5, maxit = 3000, saveWinnersPerPattern = T)
vencedores = model$winnersPerPattern
#Criando um vetor que apresenta os atributos e o repectivo neuronio de uma instancia
marcador = 1
vetor = c()

for (i in seq(1:9)) {
  for (j in seq(1,length(vencedores))) {
    if (i == vencedores[j]) {
      vetor$neuronio[marcador] = i
      marcador = marcador + 1
    }
  }
}

vetor$neuronio = as.factor(vetor$neuronio)


decodeClassLabels(dataTest$partido)

#random data
dataAll = dataAll[sample(1:nrow(dataAll), length(1:nrow(dataAll))), 1:ncol(dataAll)]
dataAll = dataAll[-c(1,2)]
decodeClassLabels(dataAll$deputado)

dadosValues = cbind(dataAll[, -c(23)])
dadosTargets = dataAll[, 23]

dataAll2 = splitForTrainingAndTest(dadosValues, dadosTargets, ratio = 0.25)
conf.level = 0.95


ic.media <- function(x){ 
  n = length(x) 
  mu = mean(x)
  sigma = sqrt(var(x))
  if (n > 30){ 
    z <- qnorm((1+conf.level)/2) 
  }else{  
    z <- qt((1+conf.level)/2, n-1) 
  } 
  return(mu + c(-1, 1)* z * sigma/sqrt(n)) 
} 

tabela = c()
for(i in seq(1,100)) {
  model.umaCamada = mlp(dataAll2$inputsTrain, dataAll2$targetsTrain, size = 10, learnFuncParams = 0.11, maxit = 400, inputsTest = dataAll2$inputsTest, targetsTest = dataAll2$targetsTest, linOut = FALSE)
  sse = sum((model.umaCamada$fittedTestValues-dataAll2$targetsTest)^2)
  rmse = sqrt(mean((model.umaCamada$fittedTestValues-dataAll2$targetsTest)^2))
  tabela$sse[i] = sse
  tabela$rmse[i] = rmse
  tabela$camadas[i] = "uma"
}
