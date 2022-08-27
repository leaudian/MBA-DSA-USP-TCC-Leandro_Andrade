#---------------------------------------#
#----Correção de nomes dos clubes-------#
#---------------------------------------#

#Padrão de nome de time
#1) Sigla do estado será sempre representada por letras maíusculas antecedidas por "-", exemplo: "América-MG"
#2) Clubes que possuam nomes compostos deverão ser representados por "." entre cada nome, exemplo: "Vasco.da.Gama" e "São.Paulo"
#3) Siglas que são complementares aos nomes dos clubes serão removidas, exemplo: "Santos FC" / "Santos"


nomes_libertadores<-names(BD_Libertadores)[order(names(BD_Libertadores))]
nomes_libertadores_df<-as.data.frame(nomes_libertadores)
write.xlsx(nomes_libertadores_df,sheetName="Libertadores",file ="nomes_libertadores_df.xlsx")

nomes_amostra<-unique(BD_Amostra$time_man)[order(unique(BD_Amostra$time_man))]
nomes_amostra_df<-as.data.frame(nomes_amostra)
write.xlsx(nomes_amostra_df, "nomes_amostra_df.xlsx",sheetName = "Amostra") 

nomes_copa_do_brasil_campeao<-unique(BD_Copa_do_Brasil$Campeão)[order((unique(BD_Copa_do_Brasil$Campeão)))]
nomes_copa_do_brasil_vice<-unique(BD_Copa_do_Brasil$Vice)[order(unique(BD_Copa_do_Brasil$Vice))]
nomes_copa_do_brasil_campeao<-as.data.frame(nomes_copa_do_brasil_campeao)
nomes_copa_do_brasil_vice<-as.data.frame(nomes_copa_do_brasil_vice)
write.xlsx(nomes_copa_do_brasil_vice,sheetName="Vices","copa_vices.xlsx")
write.xlsx(nomes_copa_do_brasil_campeao,sheetName="Campeões","copa_campeos.xlsx")




#---------------------------------------#
#----------Tratativa de NAs-------------#
#---------------------------------------#
#Avaliação ano a ano das variáveis que possuam NA

#2008
BD_2008<-BD_Amostra %>% filter(ano_campeonato==2008)
summary(BD_2008)
View(BD_2008 %>% filter(is.na(valor_equipe_titular_man)|is.na(valor_equipe_titular_vis)|is.na(idade_media_titular_man)|is.na(idade_media_titular_vis)))
times<-as.data.frame(BD_2008 %>% filter(is.na(valor_equipe_titular_man)|is.na(valor_equipe_titular_vis)|is.na(idade_media_titular_man)|is.na(idade_media_titular_vis)))






#---------------------------------------#
#--Aproveitamento das ultimas rodadas---#
#---------------------------------------#
#Verificação sobre o funcionamento do algorítimo
verificacao<-BD_Amostra %>% select(rodada,time_man,gols_man,gols_vis,time_vis,Aprov_1_man,Aprov_3_man,Aprov_5_man,Aprov_1_vis,Aprov_3_vis,Aprov_5_vis)
view(verificacao %>% filter(time_man=="Náutico"|time_vis=="Náutico"))



####################################################################################################################
#################################BACKUP - Retomar do ponto em que parou#############################################
####################################################################################################################
#Salvar Backup
write.xlsx(BD_Amostra, "BD_Amostra_Backup.xlsx",sheetName = "Amostra")


#1) Carregar o arquivo de forma manual "BD_Amostra_Backup.xlsx"
#2) Executar o comando abaixo
BD_Amostra<-BD_Amostra_Backup
rm(BD_Amostra_Backup)
####################################################################################################################










####################################################################################################################
#########################################Em desenvolvimento#########################################################
####################################################################################################################



####################################################################################################################




