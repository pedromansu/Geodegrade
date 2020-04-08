library(readr)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(MASS)
library(tree)
library(randomForest)
library(designGLMM)
library(rpart)
library(rpart.plot)
library(nnet)
library(xtable)
library(dplyr)


set=read_xlsx("C:/Users/Pedro/Desktop/ME_710/set.xlsx")


set=set %>% mutate_if(is.character, as.factor)

### Splitando o banco

set.seed(1)



n = nrow(set)
trainIdx = createDataPartition(y=set$EstagioDeg, p=0.7, list=FALSE)

train = set[trainIdx, ]
test = set[-trainIdx, ]


### (Modelo Regressão Log�?stica Ordinal com Odds Proporcional)


### Selecao de variáveis

satmodel=polr(EstagioDeg ~ ., data=train)


step(satmodel, direction = "both", trace=FALSE)


model=polr(EstagioDeg ~ tipo_capim + cobert_pi + alt_pasto + soma_coberturas + perc_cv, data = train)

### Coeficientes e testes de significância

(ctable = coef(summary(model)))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable = cbind(ctable, "p value" = p))


### Validação do modelo na base de teste

predicaolog=predict(model,newdata = test)
confusionmodel=table(test$EstagioDeg,predicaolog)


### (Arvore de Decisão com resposta Ordinal)

arvore=rpart(EstagioDeg ~ ., data=train)

### Visualização da Arvore

rpart.plot(arvore)

### Visualização do Parametro de complexidade

plotcp(arvore)

### Poda da arvore

arvore.podada= prune(arvore,cp=0.16)
rpart.plot(arvore.podada)

### Validação do modelo na base de teste

predicaoarv=predict(arvore.podada,newdata = test,type = "class")
confusionarv=table(test$EstagioDeg,predicaoarv)



### (Floresta Aleatoria)

m = round(sqrt(ncol(set)))
floresta=randomForest(EstagioDeg ~ .,data=train,mtry=m,importance=TRUE)

### Visualização do Erro contra o número de arvores 

plot(floresta)

### Importancia das variáveis

varImpPlot(floresta, main="")


### Validação do modelo na base de teste

predicaoflor=predict(floresta,newdata = test)
confusionflo=table(test$EstagioDeg,predicaoflor)