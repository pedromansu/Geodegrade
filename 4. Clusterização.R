
#Função para determinar o número ótimo de cluster ElBOW-METHOD atráves da soma de quedrados de cada cluster

wssplot = function(data, nc=15, seed=1){
  wss = (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

library(readr)
library(caret)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(MASS)
library(tree)
library(randomForest)
library(designGLMM)
library(rpart)
library(rpart.plot)
library(nnet)
library(xtable)
library(cluster)
library(factoextra)
library(tibble)
library(VGAM)


#Lendo o banco de dados total
dados=read_csv("C:/Users/Pedro/Downloads/Dados.csv")
dadosgeral=read_csv("C:/Users/Pedro/Downloads/Dados.csv")

dados=dados[,-c(1,4,6)]

#Particionando o banco nos tipos de capim para modelos distintos
dadosBB=subset(dados, tipo_capim=='BB')
dadosBD=subset(dados, tipo_capim=='BD')

class.true=as.vector(dados$sit_pastagem)
class.trueBB=as.vector(dadosBB$sit_pastagem)
class.trueBD=as.vector(dadosBD$sit_pastagem)

#Retirando as variáveis cont�?nuas
data=as.data.frame(dados[,-c(1:3)])
data=as.data.frame(lapply(data, as.numeric))

dataBB=as.data.frame(dadosBB[,-c(1:3)])
dataBB=as.data.frame(lapply(dataBB, as.numeric))

dataBD=as.data.frame(dadosBD[,-c(1:3)])
dataBD=as.data.frame(lapply(dataBD, as.numeric))

#Padronizando os data
data=as.data.frame(scale(data))
dataBB=as.data.frame(scale(dataBB))
dataBD=as.data.frame(scale(dataBD))

#Número otimo de clusters
wssplot(dataBB,10,1)
wssplot(dataBD,10,1)
wssplot(data,10,1)


#Agrupamento por KMeans
k.means.data=kmeans(data, 4)
k.means.dataBB=kmeans(dataBB, 4)
k.means.dataBD=kmeans(dataBD, 4)

#----------------------------------------------------------------------------#


#Agrupamento por Ward
d.data=dist(data, method = "euclidean")
d.dataBB=dist(dataBB, method = "euclidean")
d.dataBD=dist(dataBD, method = "euclidean")

h.data=hclust(d.data, method="ward")
h.dataBB=hclust(d.dataBB, method="ward")
h.dataBD=hclust(d.dataBD, method="ward")

#----------------------------------------------------------------------------#

#Visualização dos Clusters e as observações

clusplot(data, k.means.data$cluster, main='Cluster para as duas espécies',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

clusplot(dataBB, k.means.dataBB$cluster, main='Cluster para o Capim BB',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

clusplot(dataBD, k.means.dataBD$cluster, main='Cluster para o Capim BD',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#---------------------------------------------------------------------------#

#Visualização dos Clusters e as observações

plot(h.data,main="Cluster para as duas espécies")
groups.data = cutree(h.data, k=4)
rect.hclust(h.data, k=4, border="red") 

plot(h.dataBB, main='Cluster para o Capim BB')
groups.BB = cutree(h.dataBB, k=4)
rect.hclust(h.dataBB, k=4, border="red") 

plot(h.dataBD, main='Cluster para o Capim BD')
groups.BD = cutree(h.dataBD, k=4)
rect.hclust(h.dataBD, k=4, border="red")



