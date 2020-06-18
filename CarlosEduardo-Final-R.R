library(stringr)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(reshape2)

casos<-read_excel("Brasil-HIV.xlsx", sheet="casos") #abrindo uma aba especifica do Banco de dados
casos<-data.frame(casos) #transformando em data frame
names(casos)<- str_remove(names(casos),"X") #removendo o X dos títulos das colunas

mortes<-read_excel("Brasil-HIV.xlsx", sheet="obitos")
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


ggplot()+
  geom_line(data=casos_filtrado, aes(x=as.numeric(rownames(casos_filtrado)), y= total), 
            color = "blue", linetype=1)+
  geom_line(data=mortes_filtrado, aes(x=as.numeric(rownames(mortes_filtrado)), y= total),
            color= "red", linetype= 2)+
  geom_point(data=casos_filtrado,aes(x=as.numeric(rownames(casos_filtrado)), y=total, 
                                     color="casos novos"), fill="red", shape=16)  + 
  geom_point(data=mortes_filtrado, aes(x=as.numeric(rownames(mortes_filtrado)), y=total, 
                                       color="mortes"), size=2, shape=16)  +
  scale_color_manual("Legenda", breaks=c("casos novos", "mortes"), values = c("blue","red"))+
  scale_x_continuous(breaks = seq(2007,2019,2), limits = c(2007,2019))+
  scale_y_continuous(breaks= seq(0,45000,5000), limits=c(0,45000))+
  labs(title= "Mortes e Novos Casos", x= "Anos", y= "Pessoas")+
  theme(plot.title = element_text(hjust=0.5))+
  geom_line(aes(x=2012,y=seq(0,50000)))+
  geom_line(aes(x=2018,y=seq(0,50000)))+
  theme_set(theme_bw())




casos_filtrado["REC"]="casos"
mortes_filtrado["REC"]="mortes"
casos_filtrado["ano"]=rownames(casos_filtrado)
mortes_filtrado["ano"]=rownames(mortes_filtrado)
teste<-full_join(casos_filtrado,mortes_filtrado)







cor<-read_excel("Brasil-HIV.xlsx", sheet = "cor")
cor<-data.frame(cor)
names(cor)<- str_remove(names(cor),"X")
cor_filtrada<-select(cor,-c(names(cor)[2],names(cor)[3]))# estou apenas tirando os dados acumulados e total

cor_novo<-melt(cor_filtrada, id.vars = c(names(cor_filtrada[1])), value.name= "proporcao")
names(cor_novo)[2]<-paste("ano")

ggplot()+
  geom_bar(data= cor_novo, aes(y=proporcao, x=ano, fill= Cor.ou.Raça), stat="identity")





expo<-read_excel("Brasil-HIV.xlsx", sheet = 'exposicao2')
exposicao<-data.frame(expo)
exposicao_total<-select(exposicao, c(names(exposicao)[1],names(exposicao)[2]))
exposicao_total


y=c()
labels=c()
for(i in 1:length(exposicao_total[,2])){
  print(i)
  y[i]=exposicao_total[i,2]
  labels[i]=paste(exposicao_total[i,1], y[i], "%", sep=" ")
}

aux1<-y[1]
aux3<-y[3]
aux4<-y[4]
aux5<-y[5]

auxi1<-labels[1]
auxi3<-labels[3]
auxi4<-labels[4]
auxi5<-labels[5]


labels[1]<-auxi4
labels[3]<-auxi5
labels[4]<-auxi1
labels[5]<-auxi3

y[1]<-aux4
y[3]<-aux5
y[4]<-aux1
y[5]<-aux3


pie(y,main="Exposição ao vírus",labels, col=rainbow(7))


br<-read_excel("Estados-HIV.xlsx", col_names = TRUE)
br<-data.frame(br)
names(br)<- str_remove(names(br),"X")
br<-select(br,c(names(br)[1],names(br)[2]))

brasil  <- url("https://sites.google.com/site/cbee2014reb/r-data/BRA_adm1.RData")
print(load(brasil))
close(brasil)


#vetor com a desocupaÃ§Ã£o por estado no 1trimestre de 2020
estados_des  <- c()
for (i in 1:27){
  estados_des[i] <- br$Total[i]
} 

estados_des<- log(estados_des)
freq <- estados_des
cutfreq  <- cut(freq, 6) 

levels(cutfreq)  <- c("<2800", "3171-7470", "8168-16616", "18928-35843", 
                      "47469-95558",">128357")
gadm$freq <- cutfreq 

col  <- brewer.pal(6, "PuRd") 

spplot(gadm, "freq", main = "Número de Casos por Estado", 
       col.regions=col, col= 'white', border="black", 
       par.settings = list(axis.line = list(col = 'purple'))) 


