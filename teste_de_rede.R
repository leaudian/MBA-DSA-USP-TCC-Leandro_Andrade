#Ajustes iniciais
library("MASS")
library("rpart")
library("neuralnet")
set.seed(0)
data <- Boston

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


pr.nn <- compute(nn,data_treino[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (data_teste$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE_nn <- mean((pr.nn_ - test.r)^2)

rm(pr.nn,pr.nn_,test.r)



rm(MSE_nn,nn,data,data_teste,data_treino)




#Plots aleatórios
plot(nn)
plot(test_data$medv,type = 'l',col="red",xlab = "x", ylab = "Valor Residencia")
lines(pr.nn$net.result,col = "blue")





