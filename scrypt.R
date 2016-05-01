require(GGally)
require(ggplot2)
require(corrplot)
require(dplyr)
require(caret)
dataAll = read.csv2("~/workspaceR/votosImpeachment/data/deputados_temas_e_impeachment_v1.1.csv");
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
logit <- glm(IMPEACHMENT ~ -id_dep, family = 'binomial', data = train)
summary(logit)
test.probs <-predict(logit, test, type = "response")
pred.logit <- rep(0,length(test.probs))
pred.logit[test.probs >= 0.5] <- 1
table(pred.logit, test$IMPEACHMENT)