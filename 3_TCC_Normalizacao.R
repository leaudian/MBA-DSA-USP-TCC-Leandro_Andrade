#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(openxlsx)
library(readxl)
library(DataEditR)
library(fastDummies)


#----------------------------------------#
#--------Carregar banco de dados---------#
#----------------------------------------#
#Caminho
Caminho=paste0(getwd(),"/")

#Carregar arquivo resultante da etapa anterior
BD_Amostra<-read_excel(paste0(Caminho,"BD_Amostra_Backup.xlsx"))

#Remoção das variáveis que não farão parte das Redes Neurais
BD_Normalizado<-select(BD_Amostra,everything(),
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
                  -Finalista_Estadual_vis,
                  -Batch_Fold_Index)


#Alterando a ordem das colunas no dataset (Batch / Var de Saída / Var Cat / Var Cont)
BD_Normalizado<-BD_Normalizado %>% 
  relocate(Finalista_Brasileiro_vis,.after = time_vis) %>% 
  relocate(Finalista_Brasileiro_man,.after = time_vis) %>% 
  relocate(Finalista_Copa_Brasil_man,.after = time_vis) %>% 
  relocate(Libertadores_vis,.after = time_vis) %>% 
  relocate(Libertadores_man,.after = time_vis)

#----------------------------------------#
#--Normalização das variáveis contínuas--#
#----------------------------------------#

BD_Normalizado<-BD_Normalizado %>% 
  mutate(gols_man=(gols_man-min(gols_man))/(max(gols_man-min(gols_man)))) %>% 
  mutate(gols_vis=(gols_vis-min(gols_vis))/(max(gols_vis)-min(gols_vis))) %>% 
  mutate(ano_campeonato=(ano_campeonato-min(ano_campeonato))/(max(ano_campeonato)-min(ano_campeonato))) %>% 
  mutate(colocacao_man=(colocacao_man-min(colocacao_man))/(max(colocacao_man)-min(colocacao_man))) %>% 
  mutate(colocacao_vis=(colocacao_vis-min(colocacao_vis))/(max(colocacao_vis)-min(colocacao_vis))) %>% 
  mutate(med_acum_gols_m_man=(med_acum_gols_m_man-min(med_acum_gols_m_man))/(max(med_acum_gols_m_man)-min(med_acum_gols_m_man))) %>% 
  mutate(med_acum_gols_m_vis=(med_acum_gols_m_vis-min(med_acum_gols_m_vis))/(max(med_acum_gols_m_vis)-min(med_acum_gols_m_vis))) %>% 
  mutate(med_acum_gols_s_man=(med_acum_gols_s_man-min(med_acum_gols_s_man))/(max(med_acum_gols_s_man)-min(med_acum_gols_s_man))) %>% 
  mutate(med_acum_gols_s_vis=(med_acum_gols_s_vis-min(med_acum_gols_s_vis))/(max(med_acum_gols_s_vis)-min(med_acum_gols_s_vis)))


#----------------------------------------#
#--Normalização das variáveis binárias---#
#----------------------------------------#
#Participação do time mandante na libertadores
BD_Normalizado<- BD_Normalizado%>%
  mutate(Libertadores_man=case_when(
    Libertadores_man=="Sim"~1,
    Libertadores_man=="Nao"~0))

#Participação do time mandante na libertadores
BD_Normalizado<- BD_Normalizado%>%
  mutate(Libertadores_vis=case_when(
    Libertadores_vis=="Sim"~1,
    Libertadores_vis=="Nao"~0))

#Time mandante finalista da Copa do Brasil
BD_Normalizado<- BD_Normalizado%>%
  mutate(Finalista_Copa_Brasil_man=case_when(
    Finalista_Copa_Brasil_man=="Campeao"~1,
    Finalista_Copa_Brasil_man=="Vice"~1,
    Finalista_Copa_Brasil_man=="Nao"~0))

#Time mandante finalista do Campeonato Brasileiro
BD_Normalizado<- BD_Normalizado%>%
  mutate(Finalista_Brasileiro_man=case_when(
    Finalista_Brasileiro_man=="Campeao"~1,
    Finalista_Brasileiro_man=="Vice"~1,
    Finalista_Brasileiro_man=="Nao"~0))

#Time visitante finalista do Campeonato Brasileiro
BD_Normalizado<- BD_Normalizado%>%
  mutate(Finalista_Brasileiro_vis=case_when(
    Finalista_Brasileiro_vis=="Campeao"~1,
    Finalista_Brasileiro_vis=="Vice"~1,
    Finalista_Brasileiro_vis=="Nao"~0))



#----------------------------------------#
#----Dummy das variáveis categoricas-----#
#----------------------------------------#
#Criação das Variáveis Dummy para o nome dos clubes mandantes e visitantes
BD_Normalizado <- BD_Normalizado %>%tibble() %>%dummy_cols()

#Inclusão da coluna referente aos lotes
BD_Normalizado <- bind_cols(BD_Amostra$Batch_Fold_Index,BD_Normalizado)
names(BD_Normalizado)[1]<-"Batch_Fold_Index"

#Ajuste no nome das colunas
names(BD_Normalizado)<-make.names(names(BD_Normalizado))

#Remoção das variáveis referentes ao nome dos clubes mandantes e visitantes
#Remoção da variavel referente time do vitoria (Dummy Trap)
BD_Normalizado<-select(BD_Normalizado,everything(),
                           -time_man,
                           -time_vis,
                           -time_man_Vitoria,
                           -time_vis_Vitoria)

rm(BD_Neural_Networks)
