
# Bibliotecas -----------------------------------------------------------------
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


# Redes Neurais -----------------------------------------------------------
set.seed(0)
MSE<-0
lr<-0.1

#Vetores e contador para armazenar os resultados
lista_MSE<-rep(NA, each=27)
lista_funcao_ativacao<-rep(NA, each=27)
lista_camadas<-rep(NA, each=27)
lista_neuronios<-rep(NA, each=27)
lista_tempo<-rep(NA, each=27)
n<-1 #contador a ser utilizado para armazenar as informações coletadas

#Laço com a função de ativação (sigmoide / softplus / relu)
for (i in 1:3){
if (i==1){funcao_ativacao<-"logistic"} #Sigmoide
if (i==2){funcao_ativacao<-function(x) log(1+exp(x))} #Softplus
if (i==3){funcao_ativacao<-function(x) ifelse(x>=0,x,0)} #ReLU

#Laço com a quantidade de camadas
for(j in 1:3){
  if (j==1){camadas<-1}
  if (j==2){camadas<-2}
  if (j==3){camadas<-3}
  
  #Laço com a quantidade de neuronios
  for(k in 1:3){
    if (k==1){neuronios<-5}
    if (k==2){neuronios<-10}
    if (k==3){neuronios<-15}
    
    rede<-rep(neuronios,each=camadas)
    
    #Laço com a taxa de aprendizado
    #for (l in 1:2) {
      #if (l==1){lr<-0.1}
      #if (l==3){lr<-0.01}
      
      #Laço com o cross validation
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

          lista_MSE[[n]]<-MSE
          lista_funcao_ativacao[[n]]<-i
          lista_camadas[[n]]<-camadas
          lista_neuronios[[n]]<-neuronios
          lista_tempo[[n]]<-tempo
          
          MSE<-0
          n<-n+1
          } #Retorna o MSE para o valor original
        }#Fim do Laço cross validation
      #}#Fim do Laço taxa de aprendizado
    }#Fim do Laço quantidade de neuronios
  }#Fim do Laço quantidade de camadas
}#Fim do Laço da função de ativação

Resultado<-cbind.data.frame(lista_funcao_ativacao,
                            lista_camadas,
                            lista_neuronios,
                            lista_tempo,
                            lista_MSE)


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



