
#install.packages("readxl") talvez tenha que instalar esse
library(dplyr)
library(stringr)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)

casos <- read_excel("Brasil-HIV.xlsx", sheet= "casos") #abrindo uma aba especifica do Banco de dados
casos<-data.frame(casos) #transformando em data frame
names(casos)<- str_remove(names(casos),"X") #removendo o X dos títulos das colunas

mortes <- read_excel("Brasil-HIV.xlsx", sheet="obitos")
mortes<-data.frame(mortes)
names(mortes)<- str_remove(names(mortes),"X")

casos_total<-filter(casos, Casos.de.AIDS=="Total") #filtrando para pegar apenas o total
casos_filtrado<-select(casos_total,starts_with("20"))  #tirando o acumulado e deixando só os anos
casos_filtrado<- as.data.frame(t(casos_filtrado))#transformando linha em coluna
names(casos_filtrado)[1]<-"total" #renomeando linha

mortes_total<-filter(mortes, ...1=="Óbitos por AIDS")
mortes_filtrado<-select(mortes_total,starts_with("20"))
mortes_filtrado<-as.data.frame(t(mortes_filtrado))
names(mortes_filtrado)[1]<-"total"

#theme(legende.position = c(0,0))

ggplot()+
  geom_line(data=casos_filtrado, aes(x=as.numeric(rownames(casos_filtrado)), y= total), 
             color = "blue", linetype=1, group =1 )+
  geom_line(data=mortes_filtrado, aes(x=as.numeric(rownames(mortes_filtrado)), y= total),
             color= "red", linetype= 2, group=2)+
 # geom_hline(xintercept = 2015, col="black", tly="dashed")+
  geom_point(data=casos_filtrado, aes(x=as.numeric(rownames(casos_filtrado)), y= total), 
            color = "blue", group=1)+
  geom_point(data=mortes_filtrado, aes(x=as.numeric(rownames(mortes_filtrado)), y= total),
             color="red", group=2)+
  labs(title= "Mortes e Novos Casos", x= "Anos", y= "Pessoas")+
  theme_set(theme_bw())
  legend("topright", pch=c(16,17), col=c("red","blue"), legend=c("Mulheres", "Homens"))



cor <- read_excel("Brasil-HIV.xlsx", sheet="cor")
cor<-data.frame(cor)
names(cor)<- str_remove(names(cor),"X")
cor_filtrada<-select(cor,-c(names(cor)[2],names(cor)[3]))# estou apenas tirando os dados acumulados e total
cor_filtrada<-as.data.frame(t(cor_filtrada))#transpondo o dataframe para ter datas em coluna
names(cor_filtrada)<-cor_filtrada[1,]#renomeando as colunas do novo daframe
cor_filtrada["ano"]<-rownames(cor_filtrada)#fazendo uma coluna com os anos 
cor_filtrada<-slice(cor_filtrada,2:n())#removendo a primeira linha, pois ela já foi transformada nos nomes das colunas
cor_filtrada<- select(cor_filtrada,ano, Branca, Preta, Amarela, Parda, Indígena, Ignorada ) #colocando a data na frente puramente por estetica

c("red","orange","blue","pink","green","purple")
#"Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorada"
aux<-as.data.frame.matrix(cadu)
ggplot()+
  barplot(aux, legend=rownames(cadu) , xlab = "anos",
          col=1:6 )
ggplot()+
  geom_bar()

