# Função para gerar as tabelas de contigência

marginais=function(i,j,vetor){
  somalinha=c()
  somacoluna=c()
  for(k in 1:i){
    somalinha[k]=sum(vetor[k,])
  }
  
  for(l in 1:j){
    somacoluna[l]=sum(vetor[,l])
  }
  
  TabCont=cbind(vetor,somalinha)
  TabCont=rbind(TabCont,somacoluna)
  TabCont[i+1,j+1]=sum(somalinha)
  
  
  return (TabCont)
}

library(ggplot2)
library(gridExtra)
library(xtable)
library(ggcorrplot)
library(writexl)

#Limpando o banco de dados e fazendo algumas transformações

#Lendo o banco de dados

Todos_BB = read.csv("C:/Users/Pedro/Desktop/Format/ME710_ME810/Geodegrade/Assessoria Estat�?stica/Programas R/Todos_BB.csv")
Todos_BD = read.csv("C:/Users/Pedro/Desktop/Format/ME710_ME810/Geodegrade/Assessoria Estat�?stica/Programas R/Todos_BD.csv")

#Excluindo as linhas e colunas de NA

Todos_BB=Todos_BB[,-(23:24)]
Todos_BB=na.omit(Todos_BB)

#Combinando os dos banco de dados e retirando a observacao com NA

set=na.omit(rbind(Todos_BB,Todos_BD))

#Comparando as colunas

all.equal.character(set$sit_pastagem,set$sit_pastagem_1)
all.equal(set$disp_forragem,set$df1)
all.equal(set$solo_descob,set$sd1)
all.equal(set$cobert_pi,set$pi1)
all.equal(set$perc_sp,set$cs1)
all.equal(set$perc_cv,set$cv1)

#Excluindo a variaveis duplicadas

set=set[,-c(17:22)]
set_cont=as.matrix(set[,c(6,7,8,9,10,11,13,14,15,16)])
set_cont=data.matrix(as.data.frame(set_cont[,-1]))

#Criando a variavel resposta numerica

EstagioDeg = vector('character', nrow(set))
indexNivelEscolhido = set$sit_pastagem %in% "degradada"
EstagioDeg[indexNivelEscolhido] = 2
indexNivelEscolhido = set$sit_pastagem %in% "em_degradacao"
EstagioDeg[indexNivelEscolhido] = 1
indexNivelEscolhido = set$sit_pastagem %in% "nao_degradada"
EstagioDeg[indexNivelEscolhido] = 0
set$EstagioDeg=as.factor(as.numeric(EstagioDeg))

#Substituindo os nomes das categorias
Degradacao = vector('character', nrow(set))
indexNivelEscolhido = set$sit_pastagem %in% "degradada"
Degradacao[indexNivelEscolhido] = "Degradada"
indexNivelEscolhido = set$sit_pastagem %in% "em_degradacao"
Degradacao[indexNivelEscolhido] = "Em Degradacao"
indexNivelEscolhido = set$sit_pastagem %in% "nao_degradada"
Degradacao[indexNivelEscolhido] = "Não Degradada"
set$Degradacao=as.factor(Degradacao)

#Recombinando os n�?veis das variáveis categóricas

for (i in 1:nrow(set)){
  
  if(set[i,2]=="2_2_2012"){
    set[i,2]="2_2012"
  }
  
}

for (i in 1:nrow(set)){
  if(set[i,12]=="verde_amarelado"){
    set[i,12]="verde amarelado"
  }
  if(set[i,12]=="verde_amarelo"){
    set[i,12]="verde amarelo"
  }
  if(set[i,12]=="verde_claro"){
    set[i,12]="verde claro"
  }
  if(set[i,12]=="verde_escuro"){
    set[i,12]="verde escuro"
  }
}

#Tranformando os char e variáveis categóricas

set$municipio=factor(set$municipio)
set$mes_ano=factor(set$mes_ano)
set$tipo_capim=factor(set$tipo_capim)
set$sit_pastagem=factor(set$sit_pastagem)
set$cor_pasto=factor(set$cor_pasto)

set=set[,-c(1,4,6,18)]

# Exportando a base "Limpa"
write_xlsx(set, "C:/Users/Pedro/Desktop/ME_710/set.xlsx")

# (Descritiva)

# Criando a Tabelas de Contigência e testando a associação

TabCont1=table(set$tipo_capim,set$sit_pastagem)
pvalor1=chisq.test(TabCont1)
TabCont2=table(set$mes_ano,set$sit_pastagem)
pvalor2=chisq.test(TabCont2)
TabCont3=table(set$cor_pasto,set$sit_pastagem)
pvalor3=chisq.test(TabCont3)

TabCont1=marginais(nrow(TabCont1),ncol(TabCont1),TabCont1)
TabCont2=marginais(nrow(TabCont2),ncol(TabCont2),TabCont2)
TabCont3=marginais(nrow(TabCont3),ncol(TabCont3),TabCont3)

# Criando a tabela com as medidas resumo de cada variável cont�?nua

medres=matrix(nrow = 7, ncol = 6,0)
standard=c()
for(i in 1:nrow(medres)){
  medres[i,]=round(summary(set_cont[,i]),2)
  standard[i]=sd(set_cont[,i])
}
medres=round(cbind(medres,standard),2)


# Matriz de correlação entre as cont�?nuas

corr=round(cor(set_cont), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

#Gráficos Influência na variável resposta

p1=ggplot(data=set,aes(x=EstagioDeg,y=solo_descob,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Solo Descoberto",fill="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.75), angle = 90,vjust = 0.5))
p2=ggplot(data=set,aes(x=EstagioDeg,y=cobert_capim,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Capim Coberto",fill="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.75), angle = 90,vjust = 0.5))
p3=ggplot(data=set,aes(x=EstagioDeg,y=cobert_pi,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Cobertura de Plantas Invasoras",fil="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.75), angle = 90,vjust = 0.5))
p4=ggplot(data=set,aes(x=EstagioDeg,y=capim_verde,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Capim Verde",fil="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.75), angle = 90,vjust = 0.5))



p5=ggplot(data=set,aes(x=EstagioDeg,y=perc_sp,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Capim Seco em pé",fill="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.75), angle = 90,vjust = 0.5))
p6=ggplot(data=set,aes(x=EstagioDeg,y=perc_cv,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Capim Verde",fill="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.75), angle = 90,vjust = 0.5))



g1=ggplot(data=set,aes(x=EstagioDeg,y=alt_pasto,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Altura do Pasto",fill="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.7), angle = 90,vjust = 0.5))
g2=ggplot(data=set,aes(x=EstagioDeg,y=alt_pls_inv,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Altura das Plantas Invasoras",fill="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.7), angle = 90,vjust = 0.5))
g3=ggplot(data=set,aes(x=EstagioDeg,y=disp_forragem,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Disponibilidade de Forragem",fill="")+theme_bw()+ scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.7), angle = 90,vjust = 0.5))
g4=ggplot(data=set,aes(x=EstagioDeg,y=soma_coberturas,fill=tipo_capim))+geom_boxplot()+labs(x="",y="Soma das Coberturas",fill="")+theme_bw()+scale_fill_manual(values=c("dimgray", "darkgoldenrod2", "chartreuse3"))+theme(axis.title.y = element_text(size = rel(0.7), angle = 90,vjust = 0.5))

grid.arrange(p1,p2,p3,p4,ncol=2, top="Gráficos 1")
grid.arrange(p5,p6,top="Gráficos 2")
grid.arrange(g1,g2,g3,g4,ncol=2,top="Gráficos 3")

