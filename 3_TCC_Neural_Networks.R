#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(openxlsx)
library(readxl)
library(Hmisc)

#----------------------------------------#
#--------Carregar banco de dados---------#
#----------------------------------------#
#Caminho
Caminho=paste0(getwd(),"/")


#Carregar arquivo resultante da etapa anterior
BD_Amostra<-read_excel(paste0(Caminho,"BD_Amostra_Backup.xlsx"))

#Remoção das variáveis que não farão parte das Redes Neurais
BD_Neural_Networks<-select(BD_Amostra,everything(),
                  -Estado_man,
                  -Estado_vis,
                  -rodada,
                  -idade_media_titular_man,
                  -idade_media_titular_vis,
                  -Aprov_3_man,
                  -Aprov_5_man,
                  -Aprov_1_vis,
                  -Aprov_3_vis,
                  -Finalista_Copa_Brasil_vis,
                  -Finalista_Estadual_man,
                  -Finalista_Estadual_vis)
