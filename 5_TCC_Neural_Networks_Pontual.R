# Pacotes -----------------------------------------------------------------
library(dplyr)
library("tidyverse")
library(openxlsx)
library(readxl)
library(neuralnet)
library(rpart)





# Banco de dados ----------------------------------------------------------
#Caminho
Caminho=paste0(getwd(),"/")

#Carregar arquivo resultante da etapa anterior
BD_Normalizado<-read_excel(paste0(Caminho,"BD_Normalizado_Backup.xlsx"))
BD_Amostra<-read_excel(paste0(Caminho,"BD_Amostra_Backup.xlsx"))

#Remoção das variáveis times (resultado da etapa de PCA)
BD_Neural_Networks<-BD_Normalizado[,1:19]
rm(BD_Normalizado)




# Variáveis ---------------------------------------------------------------
set.seed(0)
MSE<-0
lr<-0.1
neuronios<-
camadas<-
rede<-rep(neuronios,each=camadas)
funcao_ativacao<-function(x) ifelse(x>=0,x,0)#ReLU




# Rede --------------------------------------------------------------------
for(m in 1:5){ 
  if (m==1)
  {BD_Treino<-BD_Neural_Networks %>% filter(!BD_Neural_Networks$Batch_Fold_Index=="A")
  BD_Teste<-BD_Neural_Networks %>% filter(BD_Neural_Networks$Batch_Fold_Index=="A")}
  if (m==2)
  {BD_Treino<-BD_Neural_Networks %>% filter(!BD_Neural_Networks$Batch_Fold_Index=="B")
  BD_Teste<-BD_Neural_Networks %>% filter(BD_Neural_Networks$Batch_Fold_Index=="B")}
  if (m==3)
  {BD_Treino<-BD_Neural_Networks %>% filter(!BD_Neural_Networks$Batch_Fold_Index=="C")
  BD_Teste<-BD_Neural_Networks %>% filter(BD_Neural_Networks$Batch_Fold_Index=="C")}
  if (m==4)
  {BD_Treino<-BD_Neural_Networks %>% filter(!BD_Neural_Networks$Batch_Fold_Index=="D")
  BD_Teste<-BD_Neural_Networks %>% filter(BD_Neural_Networks$Batch_Fold_Index=="D")}
  if (m==5)
  {BD_Treino<-BD_Neural_Networks %>% filter(!BD_Neural_Networks$Batch_Fold_Index=="E")
  BD_Teste<-BD_Neural_Networks %>% filter(BD_Neural_Networks$Batch_Fold_Index=="E")}
  
  #Simplificação da formula da rede
  var_explicativas<-names(BD_Treino)[4:dim(BD_Treino)[2]]
  equacao<-as.formula(paste("gols_man+gols_vis~",paste(var_explicativas,collapse = "+")))
  
  #Elaboração da rede
  tempo<-Sys.time()
  modelo_rna<-neuralnet(equacao,
                        data = BD_Treino,
                        hidden = rede,
                        act.fct = funcao_ativacao,
                        learningrate = lr,
                        threshold = 0.1,
                        linear.output = F)
  
  
  previsao_normalizada<-neuralnet::compute(modelo_rna,BD_Teste[,4:dim(BD_Teste)[2]])
  
  previsao_gm<-previsao_normalizada$net.result[,1]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
  previsao_gv<-previsao_normalizada$net.result[,2]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
  
  resultado_real_gm<-BD_Teste[,2]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
  resultado_real_gv<-BD_Teste[,3]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
  
  desempenho_gm<-(previsao_gm-resultado_real_gm)^2
  desempenho_gv<-(previsao_gv-resultado_real_gv)^2
  
  MSE_gm<-mean(desempenho_gm$gols_man)
  MSE_gv<-mean(desempenho_gv$gols_vis)
  
  MSE[[m]]<-mean(c(MSE_gm,MSE_gv))
  
  
  #Executa na última rodada de kfold
  if(m==5){
    tempo<-Sys.time()-tempo
    MSE<-mean(MSE)
    
    
    MSE<-0
  } #Retorna o MSE para o valor original
}#Fim do Laço cross validation

rm(var_explicativas,
   previsao_normalizada,
   previsao_gm,
   previsao_gv,
   resultado_real_gm,
   resultado_real_gv,
   desempenho_gm,
   desempenho_gv,
   MSE_gm,
   MSE_gv,
   tempo,
   camadas,
   neuronios,
   rede,
   lr,i,i,j,m)


MSE
tempo