
####################################################################################################################
#################################BACKUP - Retomar do ponto em que parou#############################################
####################################################################################################################
#Salvar Backup
write.xlsx(BD_Amostra, "BD_Amostra_Backup.xlsx",sheetName = "Amostra")

write.xlsx(P_Value_AnaCor, "P_Value_AnaCor.xlsx",sheetName = "P_Value")

write.xlsx(BD_Neural_Networks, "BD_Neural_Networks_Backup.xlsx",sheetName = "Neural_Networks")
write.xlsx(BD_NN_Normalizada, "BD_NN_Normalizada_Backup.xlsx",sheetName = "BD_NN_Normalizada")


#1) Carregar o arquivo de forma manual "BD_Amostra_Backup.xlsx"
#2) Executar o comando abaixo
BD_Amostra<-BD_Amostra_Backup
rm(BD_Amostra_Backup)
Caminho=paste0(getwd(),"/")
####################################################################################################################










####################################################################################################################
#########################################Em desenvolvimento#########################################################
####################################################################################################################



####################################################################################################################




