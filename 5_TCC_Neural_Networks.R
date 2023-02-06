#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(openxlsx)
library(readxl)
library(neuralnet)
library(rpart)


#----------------------------------------#
#--------Carregar banco de dados---------#
#----------------------------------------#
#Caminho
Caminho=paste0(getwd(),"/")

#Carregar arquivo resultante da etapa anterior
BD_Normalizado<-read_excel(paste0(Caminho,"BD_Normalizado_Backup.xlsx"))

#Remoção das variáveis times (resultado da etapa de PCA)
BD_Neural_Networks<-BD_Normalizado[,1:19]


#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#.........................BKP...................................................
#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
melhor_MSE<-1e20
MSE<-0
tempo_algoritmo<-Sys.time()
contador<-0

#Laço com a função de ativação (sigmoide / softplus / relu)
for (i in 1:3){ tempo<-Sys.time()
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
      for(m in 1:5){ 
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
        
        #Simplificação da formula da rede
        var_explicativas<-names(BD_Treino)[4:dim(BD_Treino)[2]]
        equacao<-as.formula(paste("gols_man+gols_vis~",paste(var_explicativas,collapse = "+")))
        
        melhor_tempo<-Sys.time()
        
        #Elaboração da rede
        nn<-neuralnet(equacao,
                      data = BD_Treino,
                      hidden = rede,
                      act.fct = funcao_ativacao,
                      learningrate = lr)
        previsao_normalizada<-neuralnet::compute(nn,BD_Teste[,4:dim(BD_Teste)[2]])
        
        previsao_gm<-previsao_normalizada$net.result[,1]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
        previsao_gv<-previsao_normalizada$net.result[,2]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
        
        resultado_real_gm<-BD_Teste[,2]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
        resultado_real_gv<-BD_Teste[,3]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
        
        desempenho_gm<-(previsao_gm-resultado_real_gm)^2
        desempenho_gv<-(previsao_gv-resultado_real_gv)^2
        
        MSE_gm<-mean(desempenho_gm)
        MSE_gv<-mean(desempenho_gv)
        
        MSE_kfold<-mean(c(MSE_gm,MSE_gv))
        
        MSE<-c(MSE,MSE_kfold)
        
        contador<-contador+1
        
        #Executa na última rodada de kfold
        if(m==5){
          #Elimina o primeiro valor originalmente contido no MSE
          MSE<-MSE[2:6]
          MSE<-mean(MSE)
          
          #Avalia o MSE atual com o melhor resultado parcial (Armazenar a melhor arquitetura de rede encontrada)
          if(MSE<=melhor_MSE){
            melhor_MSE<-MSE
            melhor_rede<-rede
            melhor_funcao_ativacao<-funcao_ativacao
            melhor_lr<-lr
            melhor_tempo<-Sys.time()-melhor_tempo}
          #Retorna o MSE para o valor original
          MSE<-0
        }
        m<-m+1}#Fim do Laço cross validation
      l<-l+1}#Fim do Laço taxa de aprendizado
    k<-k+1}#Fim do Laço quantidade de neuronios
  j<-j+1}#Fim do Laço quantidade de camadas
i<-i+1}#Fim do Laço da função de ativação

tempo_algoritmo<-Sys.time()-tempo_algoritmo




rm(i,k,k,l,m,funcao_ativacao,camadas,neuronios,lr,
   rede,BD_Treino,BD_Teste,var_explicativas,equacao)
rm(previsao_gm,previsao_gv,resultado_real_gm,resultado_real_gv,
   desempenho_gm,desempenho_gv,MSE_gm,MSE_gv,MSE)



#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#......................Salve-se quem puder.....................................
#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////

#Aquecimento
library(dplyr)
library("tidyverse")
library(openxlsx)
library(readxl)
library(DataEditR)
library(fastDummies)
library(neuralnet)
library(rpart)
Caminho=paste0(getwd(),"/")
BD_NN_Normalizada<-read_excel(paste0(Caminho,"BD_NN_Normalizada_Backup.xlsx"))
#Vars
funcao_ativacao<-"logistic"
camadas<-5
neuronios<-10
rede<-rep(neuronios,each=camadas)
lr<-0.1
var_explicativas<-names(BD_Treino)[4:22]

#Teste variáveis com pau e executar até o compute (linha x)
#Inicio do Teste - Converter as unidades de int para num
BD_NN_Normalizada<-BD_NN_Normalizada %>% mutate(
    time_man_America.MG=as.numeric(time_man_America.MG),
    time_man_Athletico.PR=as.numeric(time_man_Athletico.PR),
    time_man_Atletico.GO=as.numeric(time_man_Atletico.GO),
    time_man_Atletico.MG=as.numeric(time_man_Atletico.MG),
    time_man_Avai=as.numeric(time_man_Avai),
    time_man_Bahia=as.numeric(time_man_Bahia),
    time_man_Barueri=as.numeric(time_man_Barueri),
    time_man_Botafogo=as.numeric(time_man_Botafogo),
    time_man_Ceara=as.numeric(time_man_Ceara),
    time_man_Chapecoense=as.numeric(time_man_Chapecoense),
    time_man_Corinthians=as.numeric(time_man_Corinthians),
    time_man_Coritiba=as.numeric(time_man_Coritiba),
    time_man_Criciuma=as.numeric(time_man_Criciuma),
    time_man_Cruzeiro=as.numeric(time_man_Cruzeiro),
    time_man_CSA=as.numeric(time_man_CSA),
    time_man_Figueirense=as.numeric(time_man_Figueirense),
    time_man_Flamengo=as.numeric(time_man_Flamengo),
    time_man_Fluminense=as.numeric(time_man_Fluminense),
    time_man_Fortaleza=as.numeric(time_man_Fortaleza),
    time_man_Goias=as.numeric(time_man_Goias),
    time_man_Gremio=as.numeric(time_man_Gremio),
    time_man_Guarani=as.numeric(time_man_Guarani),
    time_man_Internacional=as.numeric(time_man_Internacional),
    time_man_Ipatinga=as.numeric(time_man_Ipatinga),
    time_man_Joinville=as.numeric(time_man_Joinville),
    time_man_Nautico=as.numeric(time_man_Nautico),
    time_man_Palmeiras=as.numeric(time_man_Palmeiras),
    time_man_Parana=as.numeric(time_man_Parana),
    time_man_Ponte.Preta=as.numeric(time_man_Ponte.Preta),
    time_man_Portuguesa=as.numeric(time_man_Portuguesa),
    time_man_RB.Bragantino=as.numeric(time_man_RB.Bragantino),
    time_man_Santa.Cruz=as.numeric(time_man_Santa.Cruz),
    time_man_Santo.Andre=as.numeric(time_man_Santo.Andre),
    time_man_Santos=as.numeric(time_man_Santos),
    time_man_Sao.Paulo=as.numeric(time_man_Sao.Paulo),
    time_man_Sport=as.numeric(time_man_Sport),
    time_man_Vasco.da.Gama=as.numeric(time_man_Vasco.da.Gama),
    time_vis_America.MG=as.numeric(time_vis_America.MG),
    time_vis_Athletico.PR=as.numeric(time_vis_Athletico.PR),
    time_vis_Atletico.GO=as.numeric(time_vis_Atletico.GO),
    time_vis_Atletico.MG=as.numeric(time_vis_Atletico.MG),
    time_vis_Avai=as.numeric(time_vis_Avai),
    time_vis_Bahia=as.numeric(time_vis_Bahia),
    time_vis_Barueri=as.numeric(time_vis_Barueri),
    time_vis_Botafogo=as.numeric(time_vis_Botafogo),
    time_vis_Ceara=as.numeric(time_vis_Ceara),
    time_vis_Chapecoense=as.numeric(time_vis_Chapecoense),
    time_vis_Corinthians=as.numeric(time_vis_Corinthians),
    time_vis_Coritiba=as.numeric(time_vis_Coritiba),
    time_vis_Criciuma=as.numeric(time_vis_Criciuma),
    time_vis_Cruzeiro=as.numeric(time_vis_Cruzeiro),
    time_vis_CSA=as.numeric(time_vis_CSA),
    time_vis_Figueirense=as.numeric(time_vis_Figueirense),
    time_vis_Flamengo=as.numeric(time_vis_Flamengo),
    time_vis_Fluminense=as.numeric(time_vis_Fluminense),
    time_vis_Fortaleza=as.numeric(time_vis_Fortaleza),
    time_vis_Goias=as.numeric(time_vis_Goias),
    time_vis_Gremio=as.numeric(time_vis_Gremio),
    time_vis_Guarani=as.numeric(time_vis_Guarani),
    time_vis_Internacional=as.numeric(time_vis_Internacional),
    time_vis_Ipatinga=as.numeric(time_vis_Ipatinga),
    time_vis_Joinville=as.numeric(time_vis_Joinville),
    time_vis_Nautico=as.numeric(time_vis_Nautico),
    time_vis_Palmeiras=as.numeric(time_vis_Palmeiras),
    time_vis_Parana=as.numeric(time_vis_Parana),
    time_vis_Ponte.Preta=as.numeric(time_vis_Ponte.Preta),
    time_vis_Portuguesa=as.numeric(time_vis_Portuguesa),
    time_vis_RB.Bragantino=as.numeric(time_vis_RB.Bragantino),
    time_vis_Santa.Cruz=as.numeric(time_vis_Santa.Cruz),
    time_vis_Santo.Andre=as.numeric(time_vis_Santo.Andre),
    time_vis_Santos=as.numeric(time_vis_Santos),
    time_vis_Sao.Paulo=as.numeric(time_vis_Sao.Paulo),
    time_vis_Sport=as.numeric(time_vis_Sport),
    time_vis_Vasco.da.Gama=as.numeric(time_vis_Vasco.da.Gama))


#----------------------------------------#
#-------Elaboração da Rede Neural--------#
#----------------------------------------#
melhor_MSE<-1e20
MSE<-0
#tempo_algoritmo<-Sys.time()

contador<-0 #/////////////////////////////////////////Deletar parâmetro de teste

#Laço com a função de ativação (sigmoide / softplus / relu)
for (i in 1:3){ #tempo<-Sys.time()
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
        for(m in 1:5){ 
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
          
          #Simplificação da formula da rede
          var_explicativas<-names(BD_Treino)[4:dim(BD_Treino)[2]]
          equacao<-as.formula(paste("gols_man+gols_vis~",paste(var_explicativas,collapse = "+")))

          melhor_tempo<-Sys.time()
          
          #Elaboração da rede
          nn<-neuralnet(equacao,
                        data = BD_Treino,
                        hidden = rede,
                        act.fct = funcao_ativacao,
                        learningrate = lr)
          
          melhor_tempo<-Sys.time()-melhor_tempo #/////Deletar parâmetro de teste
          
          previsao_normalizada<-neuralnet::compute(nn,BD_Teste[,4:dim(BD_Teste)[2]])
          
          previsao_gm<-previsao_normalizada$net.result[,1]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
          previsao_gv<-previsao_normalizada$net.result[,2]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
          
          resultado_real_gm<-BD_Teste[,2]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
          resultado_real_gv<-BD_Teste[,3]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
          
          desempenho_gm<-(previsao_gm-resultado_real_gm)^2
          desempenho_gv<-(previsao_gv-resultado_real_gv)^2
          
          MSE_gm<-mean(desempenho_gm)
          MSE_gv<-mean(desempenho_gv)
          
          MSE_kfold<-mean(c(MSE_gm,MSE_gv))
          
          MSE<-c(MSE,MSE_kfold)
          
          contador<-contador+1
          
            #Executa na última rodada de kfold
            if(m==5){
              #Elimina o primeiro valor originalmente contido no MSE
              MSE<-MSE[2:6]
              MSE<-mean(MSE)
              
              #Avalia o MSE atual com o melhor resultado parcial (Armazenar a melhor arquitetura de rede encontrada)
              if(MSE<=melhor_MSE){
                melhor_MSE<-MSE
                melhor_rede<-rede
                melhor_funcao_ativacao<-funcao_ativacao
                melhor_lr<-lr
                melhor_tempo<-Sys.time()-melhor_tempo}
              #Retorna o MSE para o valor original
              MSE<-0
            }
          m<-m+1}#Fim do Laço cross validation
        l<-l+1}#Fim do Laço taxa de aprendizado
      k<-k+1}#Fim do Laço quantidade de neuronios
    j<-j+1}#Fim do Laço quantidade de camadas
  i<-i+1}#Fim do Laço da função de ativação

#tempo_algoritmo<-Sys.time()-tempo_algoritmo




rm(i,k,k,l,m,funcao_ativacao,camadas,neuronios,lr,
   rede,BD_Treino,BD_Teste,var_explicativas,equacao)
rm(previsao_gm,previsao_gv,resultado_real_gm,resultado_real_gv,
   desempenho_gm,desempenho_gv,MSE_gm,MSE_gv,MSE)
















