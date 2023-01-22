#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(xlsx)
library(openxlsx)

#----------------------------------------#
#--------Carregar banco de dados---------#
#----------------------------------------#
#Caminho
Caminho=paste0(getwd(),"/")



#Banco de dados - Campeonato Brasileiro
BD_Amostra<-read.csv(paste0(Caminho,"BD_Amostra_Backup.csv"),
                     encoding = "UTF-8")