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

#Remoção das variáveis que não farão parte da análise de correspoindência
BD_AnaCor<-select(BD_Amostra,everything(),
                  -Batch_Fold_Index, 
                  -Estado_man,
                  -Estado_vis)

#---------------------------------------#
#----Conversão para Var Categóricas-----#
#---------------------------------------#

#valor da equipe mandante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(valor_equipe_titular_man=case_when(
    valor_equipe_titular_man<=quantile(valor_equipe_titular_man,0.25)~"val_menores",
    valor_equipe_titular_man>quantile(valor_equipe_titular_man,0.25) & 
      valor_equipe_titular_man<=quantile(valor_equipe_titular_man,0.5)~"val_int_baixos",
    valor_equipe_titular_man>quantile(valor_equipe_titular_man,0.5) &
      valor_equipe_titular_man<=quantile(valor_equipe_titular_man,0.75)~"val_int_altos",
    valor_equipe_titular_man>quantile(valor_equipe_titular_man,0.75)~"val_maiores"))

#valor da equipe visitante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(valor_equipe_titular_vis=case_when(
    valor_equipe_titular_vis<=quantile(valor_equipe_titular_vis,0.25)~"val_menores",
    valor_equipe_titular_vis>quantile(valor_equipe_titular_vis,0.25) &
      valor_equipe_titular_vis<=quantile(valor_equipe_titular_vis,0.5)~"val_int_baixos",
    valor_equipe_titular_vis>quantile(valor_equipe_titular_vis,0.5) &
      valor_equipe_titular_vis<=quantile(valor_equipe_titular_vis,0.75)~"val_int_altos",
    valor_equipe_titular_vis>quantile(valor_equipe_titular_vis,0.75)~"val_maiores"))

#idade da equipe mandante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(idade_media_titular_man=case_when(
    idade_media_titular_man<=quantile(idade_media_titular_man,0.25)~"id_menores",
    idade_media_titular_man>quantile(idade_media_titular_man,0.25) &
      idade_media_titular_man<=quantile(idade_media_titular_man,0.5)~"id_int_baixos",
    idade_media_titular_man>quantile(idade_media_titular_man,0.5) &
      idade_media_titular_man<=quantile(idade_media_titular_man,0.75)~"id_int_altos",
    idade_media_titular_man>quantile(idade_media_titular_man,0.75)~"id_maiores"))

#idade da equipe visitante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(idade_media_titular_vis=case_when(
    idade_media_titular_vis<=quantile(idade_media_titular_vis,0.25)~"id_menores",
    idade_media_titular_vis>quantile(idade_media_titular_vis,0.25) &
      idade_media_titular_vis<=quantile(idade_media_titular_vis,0.5)~"id_int_baixos",
    idade_media_titular_vis>quantile(idade_media_titular_vis,0.5) &
      idade_media_titular_vis<=quantile(idade_media_titular_vis,0.75)~"id_int_altos",
    idade_media_titular_vis>quantile(idade_media_titular_vis,0.75)~"id_maiores" ))

#Aproveitamento da última rodada do mandante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(Aprov_1_man=case_when(
    Aprov_1_man<=quantile(Aprov_1_man,0.25)~"apr_menores",
    Aprov_1_man>quantile(Aprov_1_man,0.25) &
      Aprov_1_man<=quantile(Aprov_1_man,0.5)~"apr_int_baixos",
    Aprov_1_man>quantile(Aprov_1_man,0.5) &
      Aprov_1_man<=quantile(Aprov_1_man,0.75)~"apr_int_altos",
    Aprov_1_man>quantile(Aprov_1_man,0.75)~"apr_maiores"))

#Aproveitamento das 3 últimas rodadas do mandante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(Aprov_3_man=case_when(
    Aprov_3_man<=quantile(Aprov_3_man,0.25)~"apr_menores",
    Aprov_3_man>quantile(Aprov_3_man,0.25) &
      Aprov_3_man<=quantile(Aprov_3_man,0.5)~"apr_int_baixos",
    Aprov_3_man>quantile(Aprov_3_man,0.5) &
      Aprov_3_man<=quantile(Aprov_3_man,0.75)~"apr_int_altos",
    Aprov_3_man>quantile(Aprov_3_man,0.75)~"apr_maiores"))

#Aproveitamento das 5 últimas rodadas do mandante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(Aprov_5_man=case_when(
    Aprov_5_man<=quantile(Aprov_5_man,0.25)~"apr_menores",
    Aprov_5_man>quantile(Aprov_5_man,0.25) &
      Aprov_5_man<=quantile(Aprov_5_man,0.5)~"apr_int_baixos",
    Aprov_5_man>quantile(Aprov_5_man,0.5) &
      Aprov_5_man<=quantile(Aprov_5_man,0.75)~"apr_int_altos",
    Aprov_5_man>quantile(Aprov_5_man,0.75)~"apr_maiores"))

#Aproveitamento da última rodada do visitante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(Aprov_1_vis=case_when(
    Aprov_1_vis<=quantile(Aprov_1_vis,0.25)~"apr_menores",
    Aprov_1_vis>quantile(Aprov_1_vis,0.25) &
      Aprov_1_vis<=quantile(Aprov_1_vis,0.5)~"apr_int_baixos",
    Aprov_1_vis>quantile(Aprov_1_vis,0.5) &
      Aprov_1_vis<=quantile(Aprov_1_vis,0.75)~"apr_int_altos",
    Aprov_1_vis>quantile(Aprov_1_vis,0.75)~"apr_maiores"))

#Aproveitamento das 3 últimas rodadas do visitante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(Aprov_3_vis=case_when(
    Aprov_3_vis<=quantile(Aprov_3_vis,0.25)~"apr_menores",
    Aprov_3_vis>quantile(Aprov_3_vis,0.25) &
      Aprov_3_vis<=quantile(Aprov_3_vis,0.5)~"apr_int_baixos",
    Aprov_3_vis>quantile(Aprov_3_vis,0.5) &
      Aprov_3_vis<=quantile(Aprov_3_vis,0.75)~"apr_int_altos",
    Aprov_3_vis>quantile(Aprov_3_vis,0.75)~"apr_maiores"))

#Aproveitamento das 5 últimas rodadas do visitante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(Aprov_5_vis=case_when(
    Aprov_5_vis<=quantile(Aprov_5_vis,0.25)~"apr_menores",
    Aprov_5_vis>quantile(Aprov_5_vis,0.25) &
      Aprov_5_vis<=quantile(Aprov_5_vis,0.5)~"apr_int_baixos",
    Aprov_5_vis>quantile(Aprov_5_vis,0.5) &
      Aprov_5_vis<=quantile(Aprov_5_vis,0.75)~"apr_int_altos",
    Aprov_5_vis>quantile(Aprov_5_vis,0.75)~"apr_maiores"))

#Média acumulada de gols marcados pelo mandante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(med_acum_gols_m_man=case_when(
    med_acum_gols_m_man<=quantile(med_acum_gols_m_man,0.25)~"med_menores",
    med_acum_gols_m_man>quantile(med_acum_gols_m_man,0.25) &
      med_acum_gols_m_man<=quantile(med_acum_gols_m_man,0.5)~"med_int_baixos",
    med_acum_gols_m_man>quantile(med_acum_gols_m_man,0.5) &
      med_acum_gols_m_man<=quantile(med_acum_gols_m_man,0.75)~"med_int_altos",
    med_acum_gols_m_man>quantile(med_acum_gols_m_man,0.75)~"med_maiores"))

#Média acumulada de gols marcados pelo visitante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(med_acum_gols_m_vis=case_when(
    med_acum_gols_m_vis<=quantile(med_acum_gols_m_vis,0.25)~"med_menores",
    med_acum_gols_m_vis>quantile(med_acum_gols_m_vis,0.25) &
      med_acum_gols_m_vis<=quantile(med_acum_gols_m_vis,0.5)~"med_int_baixos",
    med_acum_gols_m_vis>quantile(med_acum_gols_m_vis,0.5) &
      med_acum_gols_m_vis<=quantile(med_acum_gols_m_vis,0.75)~"med_int_altos",
    med_acum_gols_m_vis>quantile(med_acum_gols_m_vis,0.75)~"med_maiores"))

#Média acumulada de gols sofridos pelo mandante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(med_acum_gols_s_man=case_when(
    med_acum_gols_s_man<=quantile(med_acum_gols_s_man,0.25)~"med_menores",
    med_acum_gols_s_man>quantile(med_acum_gols_s_man,0.25) &
      med_acum_gols_s_man<=quantile(med_acum_gols_s_man,0.5)~"med_int_menores",
    med_acum_gols_s_man>quantile(med_acum_gols_s_man,0.5) &
      med_acum_gols_s_man<=quantile(med_acum_gols_s_man,0.75)~"med_int_maiores",
    med_acum_gols_s_man>quantile(med_acum_gols_s_man,0.75)~"med_maiores"))

#Média acumulada de gols sofridos pelo visitante
BD_AnaCor<-BD_AnaCor %>% 
  mutate(med_acum_gols_s_vis=case_when(
    med_acum_gols_s_vis<=quantile(med_acum_gols_s_vis,0.25)~"med_menores",
    med_acum_gols_s_vis>quantile(med_acum_gols_s_vis,0.25) &
      med_acum_gols_s_vis<=quantile(med_acum_gols_s_vis,0.5)~"med_int_menores",
    med_acum_gols_s_vis>quantile(med_acum_gols_s_vis,0.5) &
      med_acum_gols_s_vis<=quantile(med_acum_gols_s_vis,0.75)~"med_int_maiores",
    med_acum_gols_s_vis>quantile(med_acum_gols_s_vis,0.75)~"med_maiores"))


#---------------------------------------#
#---Conversão do BD_AnaCor em Factor----#
#---------------------------------------#
BD_AnaCor <- as.data.frame(unclass(BD_AnaCor), stringsAsFactors=TRUE)

BD_AnaCor<-BD_AnaCor %>% mutate(
  gols_man=as.factor(gols_man),
  gols_vis=as.factor(gols_vis),
  rodada=as.factor(rodada),
  colocacao_man=as.factor(colocacao_man),
  colocacao_vis=as.factor(colocacao_vis),
  ano_campeonato=as.factor(ano_campeonato))


#---------------------------------------#
#---Conversão do BD_AnaCor em Factor----#
#---------------------------------------#

P_Value_GM<-as.data.frame(P_Value_GM<-0)
P_Value_GV<-as.data.frame(P_Value_GV<-0)
Variavel<-as.data.frame(Variavel<-0)

#listar as variaveis de entrada do dataset que serão comparada com as variáveis de saída
for (i in 3:dim(BD_AnaCor)[2]){
  Variavel[i-2,]<-names(BD_AnaCor)[i]
  i<-i+1}

#Listar o P_Value da AnaCor comparada com os Gols do Mandante
for (i in 3:dim(BD_AnaCor)[2]){
  tabela_contingencia<-table(BD_AnaCor$gols_man,BD_AnaCor[,i])
  P_Value_GM[i-2,]<-round(chisq.test(x = tabela_contingencia)$p.value,4)
  i<-i+1}

#Listar o P_Value da AnaCor comparada com os Gols do Visitante
for (i in 3:dim(BD_AnaCor)[2]){
  tabela_contingencia<-table(BD_AnaCor$gols_vis,BD_AnaCor[,i])
  P_Value_GV[i-2,]<-round(chisq.test(x = tabela_contingencia)$p.value,4)
  i<-i+1}

#Unificar os datasets
P_Value_AnaCor<-bind_cols(Variavel,P_Value_GM,P_Value_GV)
names(P_Value_AnaCor)<-c("Variavel_Entrada","P_Value_G_Man","P_Value_G_Vis")

#Avaliar os resultados
P_Value_AnaCor<-mutate(P_Value_AnaCor,Analise=case_when(
  P_Value_AnaCor$P_Value_G_Man>=0.05 & P_Value_AnaCor$P_Value_G_Vis>=0.05~"Variável pouco significativa"))


rm(i,tabela_contingencia,P_Value_GV,P_Value_GM,Variavel,BD_AnaCor,BD_Amostra)



####################################################################################################################
#################################Final da Análise de Correspondência################################################
####################################################################################################################
#Salvar em Excel
write.xlsx(P_Value_AnaCor, "P_Value_AnaCor.xlsx",sheetName = "P_Value")













