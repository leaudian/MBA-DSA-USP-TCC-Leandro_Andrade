
# Teste aleatório para criação de vetor -----------------------------------
teste<-0
for(i in 1:10)
{teste[[i]]<-i}


#-------**********Bagunça**********------------- 
Inicial#Ajustes iniciais
library("MASS")
library("rpart")
library("neuralnet")
set.seed(0)
data <- Boston

tempo<-Sys.time()

#Funções de ativação
softplus<-function(x) log(1+exp(x))
relu<-function(x) ifelse(x>=0,x,0)
#"logistic"
#softplus
#relu

#*******************************#
#*******************************#
#*******************************#
lr<-0.01

neuronios<-5
camadas<-3
rede<-rep(neuronios,each=camadas)

fx<-function(x) log(1+exp(x))


#neuronios_camadas<-c(5,4,3,2)

#data <- Boston
#n<-names(data)
#f<-as.formula(paste("Private~",paste(n[!n%in%"Private"],collapse = "+")))

#*******************************#
#*******************************#
#*******************************#


#Normalização das variáveis
max_data <- apply(data, 2, max) 
min_data <- apply(data, 2, min)
scaled <- scale(data,center = min_data, scale = max_data - min_data)

#Definição do grupo de treine e teste
index = sample(1:nrow(data),round(0.70*nrow(data)))
data_treino <- as.data.frame(scaled[index,])
data_teste <- as.data.frame(scaled[-index,])

rm(scaled,max_data,min_data,index)

#Aplicação da rede neural
nn <- neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                data=data_treino,
                hidden=c(5,4,3),
                act.fct = fx,
                learningrate = lr)


pr.nn <- neuralnet::compute(nn,data_teste[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (data_teste$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE_nn <- mean((pr.nn_ - test.r)^2)

rm(pr.nn,pr.nn_,test.r)
tempo<-Sys.time()-tempo


rm(MSE_nn,nn,data,data_teste,data_treino)



funcao_ativacao<-"logistic"
#function(x) log(1+exp(x))
#unction(x) ifelse(x>=0,x,0)
lr<-0.1
rede<-rep(30,each=5)
BD_Treino<-BD_NN_Normalizada %>% filter(!BD_NN_Normalizada$Batch_Fold_Index=="A")
BD_Teste<-BD_NN_Normalizada %>% filter(BD_NN_Normalizada$Batch_Fold_Index=="A")


#var_explicativas<-names(BD_Treino)[4:dim(BD_Treino)[2]]
var_explicativas<-names(BD_Treino)[4:19]
equacao<-as.formula(paste("gols_man+gols_vis~",paste(var_explicativas,collapse = "+")))


tempo<-Sys.time()
nn<-neuralnet(equacao,
              data = BD_Treino,
              hidden = rede,
              act.fct = funcao_ativacao,
              learningrate = lr)
tempo<-Sys.time()-tempo
tempo



#Plots aleatórios
plot(nn)
plot(test_data$medv,type = 'l',col="red",xlab = "x", ylab = "Valor Residencia")
lines(pr.nn$net.result,col = "blue")






#-----*******Teste com operação com soma de datasets*****************---------------
teste1<-BD_NN_Normalizada[1:10,2:3]
teste2<-BD_NN_Normalizada[11:20,2:3]
teste<-teste1-teste2
teste_final_m<-mean(teste$gols_man)
teste_final_v<-mean(teste$gols_vis)
teste_final<-mean(c(teste_final_m,teste_final_v))
teste<-1
teste<-c(teste,1)
teste2<-teste[2:3]
teste<-c(2,4,6)
teste<-mean(teste)




#-----*******Check na estrutura lógica do for*******--------
Caminho=paste0(getwd(),"/")
BD_Normalizado<-read_excel(paste0(Caminho,"BD_Normalizado_Backup.xlsx"))
BD_Amostra<-read_excel(paste0(Caminho,"BD_Amostra_Backup.xlsx"))
BD_Neural_Networks<-BD_Normalizado[,1:19]
rm(BD_Normalizado)

set.seed(0)

camadas<-3
neuronios<-5
rede<-rep(neuronios,each=camadas)
lr<-0.1
funcao_ativacao<-"logistic"
#threshold<-0.01
#linear.output=F

melhor_MSE<-1e20




#Laço com a função de ativação (sigmoide / softplus / relu)
#for (i in 1:3){
  #if (i==1){funcao_ativacao<-"logistic"}
  #if (i==2){funcao_ativacao<-function(x) log(1+exp(x))}
  #if (i==3){funcao_ativacao<-function(x) ifelse(x>=0,x,0)}
  
  #Laço com a quantidade de camadas
  #for(j in 1:3){
    #if (j==1){camadas<-5}
    #if (j==2){camadas<-10}
    #if (j==3){camadas<-30}
    
    #Laço com a quantidade de neuronios
    #for(k in 1:3){
      #if (k==1){neuronios<-10}
      #if (k==2){neuronios<-50}
      #if (k==3){neuronios<-100}
      #rede<-rep(neuronios,each=camadas)
      
      #Laço com a taxa de aprendizado
      for (l in 1:3) {
        if (l==1){lr<-0.1}
        if (l==2){lr<-0.05}
        if (l==3){lr<-0.01}
        
        MSE<-0
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
          
          tempo<-Sys.time()
          #Elaboração da rede
          nn<-neuralnet(equacao,
                        data = BD_Treino,
                        hidden = rede,
                        act.fct = funcao_ativacao,
                        learningrate = lr)
          tempo<-Sys.time()-tempo
          tempo
          
          previsao_normalizada<-neuralnet::compute(nn,BD_Teste[,4:dim(BD_Teste)[2]])
          
          previsao_gm<-previsao_normalizada$net.result[,1]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
          previsao_gv<-previsao_normalizada$net.result[,2]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
          
          resultado_real_gm<-BD_Teste[,2]*(max(BD_Amostra$gols_man)-min(BD_Amostra$gols_man))+min(BD_Amostra$gols_man)
          resultado_real_gv<-BD_Teste[,3]*(max(BD_Amostra$gols_vis)-min(BD_Amostra$gols_vis))+min(BD_Amostra$gols_vis)
          
          desempenho_gm<-(previsao_gm-resultado_real_gm)^2
          desempenho_gv<-(previsao_gv-resultado_real_gv)^2
          
          MSE_gm<-mean(desempenho_gm$gols_man)
          MSE_gv<-mean(desempenho_gv$gols_vis)
          
          teste<-map("10",safe_log) %>% transpose() %>%  simplify_all()
          
          MSE_kfold<-mean(c(MSE_gm,MSE_gv))
          
          MSE<-c(MSE,MSE_kfold)
          
          rm(MSE_kfold)
          
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
              #melhor_tempo<-Sys.time()-melhor_tempo
              }
            }#Última rodada do kfold
          m<-m+1}#Fim do Laço cross validation
        l<-l+1}#Fim do Laço taxa de aprendizado
      #k<-k+1}#Fim do Laço quantidade de neuronios
    #j<-j+1}#Fim do Laço quantidade de camadas
  #i<-i+1}#Fim do Laço da função de ativação








rm(i,k,k,l,m,funcao_ativacao,camadas,neuronios,lr,
   rede,BD_Treino,BD_Teste,var_explicativas,equacao)
rm(previsao_gm,previsao_gv,resultado_real_gm,resultado_real_gv,
   desempenho_gm,desempenho_gv,MSE_gm,MSE_gv,MSE)