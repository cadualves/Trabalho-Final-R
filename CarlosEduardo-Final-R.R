
#install.packages("readxl") talvez tenha que instalar esse
library(dplyr)
library(stringr)
library(readxl)

casos <- read_excel("Brasil-HIV.xlsx", sheet= "casos") #abrindo uma aba especifica do Banco de dados
casos<-data.frame(casos) #transformando em data frame
names(casos)<- str_remove(names(casos),"X") #removendo o X dos títulos das colunas
head(casos)
mortes <- read_excel("Brasil-HIV.xlsx", sheet="obitos")
mortes<-data.frame(mortes)
names(mortes)<- str_remove(names(mortes),"X")
head(mortes)

