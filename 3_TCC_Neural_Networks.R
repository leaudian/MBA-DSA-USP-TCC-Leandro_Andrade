#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(openxlsx)
library(readxl)
#library(MASS)
#library(ISLR)
#library(mlbench)
library(neuralnet)
library(rpart)


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

rm(BD_Amostra)

#Alterando a ordem das colunas no dataset (Batch / Var de Saída / Var Cat / Var Cont)
BD_Neural_Networks<-BD_Neural_Networks %>% 
  relocate(Finalista_Brasileiro_vis,.after = time_vis) %>% 
  relocate(Finalista_Brasileiro_man,.after = time_vis) %>% 
  relocate(Finalista_Copa_Brasil_man,.after = time_vis) %>% 
  relocate(Libertadores_vis,.after = time_vis) %>% 
  relocate(Libertadores_man,.after = time_vis)

#----------------------------------------#
#--Normalização das variáveis contínuas--#
#----------------------------------------#

BD_NN_Normalizada<-BD_Neural_Networks %>% 
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
#--?--#
#----------------------------------------#

#Laço com a função de ativação (sigmoide / softplus / relu)
for (i in 1:3){
  if (i==1){funcao_ativacao<-"logistic"}
  if (i==2){funcao_ativacao<-function(x) log(1+exp(x))}
  if (i==3){funcao_ativacao<-function(x) ifelse(x>=0,x,0)}
  
  #Laço com a quantidade de camadas
  for(j in 1:3){
    if (j==1){camadas<-5}
    if (j==2){camadas<-10}
    if (j==3){camadas<-30}
    
    #Laço com a quantidade de neuronios
    for(k in 1:3){
      if (k==1){neuronios<-10}
      if (k==2){neuronios<-50}
      if (k==3){neuronios<-100}
      rede<-rep(neuronios,each=camadas)
      
      #Laço com a taxa de aprendizado
      for (l in 1:3) {
        if (l==1){lr<-0.1}
        if (l==2){lr<-0.05}
        if (l==3){lr<-0.01}
        
        #Laço com o cross validation
        for(m in 1:10){
          if (m==1)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="A")
            BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="A")}
          if (m==2)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="B")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="B")}
          if (m==3)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="C")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="C")}
          if (m==4)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="D")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="D")}
          if (m==5)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="E")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="E")}          
          if (m==6)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="F")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="F")}
          if (m==7)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="G")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="G")}
          if (m==8)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="H")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="H")}
          if (m==9)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="I")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="I")}
          if (m==10)
          {BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="J")
          BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="J")}
          
          nn<-neuralnet(gols_man+gols_vis~
                          ano_campeonato +
                          time_man +
                          time_vis +
                          Libertadores_man +
                          Libertadores_vis +
                          Finalista_Copa_Brasil_man +
                          Finalista_Brasileiro_man +
                          Finalista_Brasileiro_vis +
                          colocacao_man +
                          colocacao_vis +
                          valor_equipe_titular_man +
                          valor_equipe_titular_vis +
                          Aprov_1_man +
                          Aprov_5_vis +
                          med_acum_gols_m_man +
                          med_acum_gols_m_vis +
                          med_acum_gols_s_man +
                          med_acum_gols_s_vis,
                        data = BD_Treino,
                        hidden = rede,
                        act.fct = funcao_ativacao,
                        learningrate = lr)
          
          
          
          
          
          
          
          m<-m+1}#Fim do Laço cross validation
        
        
        
        #---------------------------Modelo-----------------------
        #nn <- neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                        #data=data_treino,
                        #hidden=c(5,4,3),
                        #act.fct = fx,
                        #learningrate = lr)
        #
        
        
        
        l<-l+1}#Fim do Laço taxa de aprendizado
      k<-k+1}#Fim do Laço quantidade de neuronios
    j<-j+1}#Fim do Laço quantidade de camadas
  i<-i+1}#Fim do Laço da função de ativação
rm(i,k,k,l,m,funcao_ativacao,camadas,neuronios,lr,rede,BD_Treino,BD_Teste)
















