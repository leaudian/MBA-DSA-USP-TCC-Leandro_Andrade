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
BD_Amostra<-read.csv(paste0(Caminho,"brasileirao_serie_a.csv"),
                     encoding = "UTF-8")%>% filter(ano_campeonato>=2008) %>% select(everything(),
                                                                                    -data,-horario,-estadio,
                                                                                    -arbitro,-publico_max,
                                                                                    -tecnico_man,-tecnico_vis,
                                                                                    -gols_1_tempo_man,-gols_1_tempo_vis,
                                                                                    -escanteios_man,-escanteios_vis,
                                                                                    -faltas_man,-faltas_vis,
                                                                                    -chutes_bola_parada_man,-chutes_bola_parada_vis,
                                                                                    -defesas_man,-defesas_vis,
                                                                                    -impedimentos_man,-impedimentos_vis,
                                                                                    -chutes_man,-chutes_vis,
                                                                                    -chutes_fora_man,-chutes_fora_vis,
                                                                                    -publico)

#Banco de dados – Historico de brasileiros
BD_Historico_Brasileiro<-read.csv(paste0(Caminho,"Campeoes_brasileiros.csv"),encoding = "ASCII",header = T,sep = ";")

#Banco de dados – Estados dos clubes
BD_Times_Estados<-read.csv(paste0(Caminho,"Times_e_Estados.csv"),encoding = "ASCII",header = T,sep = ";")

#Banco de dados – Posicao no campeonato estadual
BD_Historico_Estadual<-read.csv(paste0(Caminho,"estaduais.csv"),encoding = "ASCII",header = T,sep = ";")

#Banco de dados - Libertadores
BD_Libertadores<-read.csv(paste0(Caminho,"Libertadores.csv"),
                          encoding = "UTF-8",header = T,sep = ";")

#Banco de dados - Copa do Brasil
BD_Copa_do_Brasil<-read.csv(paste0(Caminho,"copa_do_brasil.csv"),
                            encoding = "ASCII",sep = ";")
