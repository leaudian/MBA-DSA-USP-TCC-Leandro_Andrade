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
funcao_ativacao<-function(x) log(1+exp(x))
camadas<-1
neuronios<-5
rede<-rep(neuronios,each=camadas)





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
  } #Retorna o MSE para o valor original
}#Fim do Laço cross validation




MSE
tempo


# Matriz de Confusão ------------------------------------------------------
#Agrupamento dos resultados
BD_Matriz_de_Confusao<-cbind(resultado_real_gm,resultado_real_gv,previsao_gm,previsao_gv)

#Arredondamento dos valores previstos
BD_Matriz_de_Confusao<-BD_Matriz_de_Confusao %>% mutate(
  previsao_gm=round(BD_Matriz_de_Confusao$previsao_gm),
  previsao_gv=round(BD_Matriz_de_Confusao$previsao_gv))

#Classificação dos resultados
BD_Matriz_de_Confusao<- BD_Matriz_de_Confusao %>% mutate(
    Resultado_Real=case_when(
  BD_Matriz_de_Confusao$gols_man>BD_Matriz_de_Confusao$gols_vis~"Vitoria_Mandante",
  BD_Matriz_de_Confusao$gols_man==BD_Matriz_de_Confusao$gols_vis~"Empate",
  BD_Matriz_de_Confusao$gols_man<BD_Matriz_de_Confusao$gols_vis~"Vitoria_Visitante"),
    Resultado_Previsto=case_when(
  BD_Matriz_de_Confusao$previsao_gm>BD_Matriz_de_Confusao$previsao_gv~"Vitoria_Mandante",
  BD_Matriz_de_Confusao$previsao_gm==BD_Matriz_de_Confusao$previsao_gv~"Empate",
  BD_Matriz_de_Confusao$previsao_gm<BD_Matriz_de_Confusao$previsao_gv~"Vitoria_Visitante"
  ))

BD_Matriz_de_Confusao$Resultado_Real<-as.factor(BD_Matriz_de_Confusao$Resultado_Real)
BD_Matriz_de_Confusao$Resultado_Previsto<-as.factor(BD_Matriz_de_Confusao$Resultado_Previsto)

BD_Matriz_de_Confusao %>% conf_mat(truth = Resultado_Real,estimate = Resultado_Previsto,dnn=c("previsto","real"))
BD_Matriz_de_Confusao %>% accuracy(truth = Resultado_Real,estimate = Resultado_Previsto)
BD_Matriz_de_Confusao %>% sens(truth = Resultado_Real,estimate = Resultado_Previsto)
BD_Matriz_de_Confusao %>% spec(truth = Resultado_Real,estimate = Resultado_Previsto)



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
   camadas,
   neuronios,
   rede,
   lr,i,i,j,m,
   modelo_rna)