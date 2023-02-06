#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(openxlsx)
library(readxl)


#----------------------------------------#
#--------Carregar banco de dados---------#
#----------------------------------------#
#Caminho
Caminho=paste0(getwd(),"/")

#Carregar arquivo resultante da etapa anterior
BD_Normalizado<-read_excel(paste0(Caminho,"BD_Normalizado_Backup.xlsx"))
