#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(xlsx)
library(openxlsx)

#----------------------------------------#
#--------Carregar banco de dados---------#
#----------------------------------------#
#Caminho
Caminho=paste0(getwd(),"/")



#Banco de dados - Campeonato Brasileiro
BD_Amostra<-read.csv(paste0(Caminho,"brasileirao_serie_a.csv"),
                     encoding = "UTF-8")%>% filter(ano_campeonato>=2008) %>% select(everything(),
                                                                                    -data,-horario,-estadio,
                                                                                    -arbitro,-publico_max,
                                                                                    -tecnico_man,-tecnico_vis,
                                                                                    -gols_1_tempo_man,-gols_1_tempo_vis,
                                                                                    -escanteios_man,-escanteios_vis,
                                                                                    -faltas_man,-faltas_vis,
                                                                                    -chutes_bola_parada_man,-chutes_bola_parada_vis,
                                                                                    -defesas_man,-defesas_vis,
                                                                                    -impedimentos_man,-impedimentos_vis,
                                                                                    -chutes_man,-chutes_vis,
                                                                                    -chutes_fora_man,-chutes_fora_vis,
                                                                                    -publico)

#Banco de dados – Historico de brasileiros
BD_Historico_Brasileiro<-read.csv(paste0(Caminho,"Campeoes_brasileiros.csv"),encoding = "ASCII",header = T,sep = ";")

#Banco de dados – Estados dos clubes
BD_Times_Estados<-read.csv(paste0(Caminho,"Times_e_Estados.csv"),encoding = "ASCII",header = T,sep = ";")

#Banco de dados – Posicao no campeonato estadual
BD_Historico_Estadual<-read.csv(paste0(Caminho,"estaduais.csv"),encoding = "ASCII",header = T,sep = ";")

#Banco de dados - Libertadores
BD_Libertadores<-read.csv(paste0(Caminho,"Libertadores.csv"),
                          encoding = "UTF-8",header = T,sep = ";")

#Banco de dados - Copa do Brasil
BD_Copa_do_Brasil<-read.csv(paste0(Caminho,"copa_do_brasil.csv"),
                            encoding = "ASCII",sep = ";")

#----------------------------------------#
#------------Data Cleaning---------------#
#----------------------------------------#

#******Uniformizar nomes de times********#
BD_Libertadores<-rename(BD_Libertadores, "America-MG"="América.MG","America-RN"="América.RN","Athletico-PR"="Athletico.PR",
                        "Atletico-GO"="Atlético.GO","Atletico-MG"="Atlético.MG","Avai"="Avaí.FC","Brasiliense"="Brasiliense.DF",
                        "Ceara"="Ceará.SC","Coritiba"="Coritiba.FC","Criciuma"="Criciúma.EC","Bahia"="EC.Bahia","Vitoria"="EC.Vitória",
                        "Figueirense"="Figueirense.FC","Goias"="Goiás.EC","Ipatinga"="Ipatinga.FC","Joinville"="Joinville.SC",
                        "Paysandu"="Paysandu.SC","Santos"="Santos.FC","Sport"="Sport.Recife","Ano"="X.U.FEFF.Ano")

BD_Amostra<-mutate(BD_Amostra,
                   time_man=replace(time_man,time_man=="Atlético-PR","Athletico-PR"),
                   time_man=replace(time_man,time_man=="Atlético-MG","Atletico-MG"),
                   time_man=replace(time_man,time_man=="Atlético-GO","Atletico-GO"),
                   time_man=replace(time_man,time_man=="América-MG","America-MG"),
                   time_man=replace(time_man,time_man=="Avaí FC","Avai"),
                   time_man=replace(time_man,time_man=="Ceará SC","Ceara"),
                   time_man=replace(time_man,time_man=="Coritiba FC","Coritiba"),
                   time_man=replace(time_man,time_man=="Criciúma EC","Criciuma"),
                   time_man=replace(time_man,time_man=="EC Bahia","Bahia"),
                   time_man=replace(time_man,time_man=="EC Vitória","Vitoria"),
                   time_man=replace(time_man,time_man=="Figueirense FC","Figueirense"),
                   time_man=replace(time_man,time_man=="Goiás EC","Goias"),
                   time_man=replace(time_man,time_man=="Grêmio","Gremio"),
                   time_man=replace(time_man,time_man=="Ipatinga FC","Ipatinga"),
                   time_man=replace(time_man,time_man=="Joinville-SC","Joinville"),
                   time_man=replace(time_man,time_man=="Náutico","Nautico"),
                   time_man=replace(time_man,time_man=="Paraná","Parana"),
                   time_man=replace(time_man,time_man=="Ponte Preta","Ponte.Preta"),
                   time_man=replace(time_man,time_man=="RB Bragantino","RB.Bragantino"),
                   time_man=replace(time_man,time_man=="Santa Cruz","Santa.Cruz"),
                   time_man=replace(time_man,time_man=="Santo André","Santo.Andre"),
                   time_man=replace(time_man,time_man=="Santos FC","Santos"),
                   time_man=replace(time_man,time_man=="São Paulo","Sao.Paulo"),
                   time_man=replace(time_man,time_man=="Sport Recife","Sport"),
                   time_man=replace(time_man,time_man=="Vasco da Gama","Vasco.da.Gama"),
                   time_vis=replace(time_vis,time_vis=="Atlético-PR","Athletico-PR"),
                   time_vis=replace(time_vis,time_vis=="Atlético-MG","Atletico-MG"),
                   time_vis=replace(time_vis,time_vis=="Atlético-GO","Atletico-GO"),
                   time_vis=replace(time_vis,time_vis=="América-MG","America-MG"),
                   time_vis=replace(time_vis,time_vis=="Avaí FC","Avai"),
                   time_vis=replace(time_vis,time_vis=="Ceará SC","Ceara"),
                   time_vis=replace(time_vis,time_vis=="Coritiba FC","Coritiba"),
                   time_vis=replace(time_vis,time_vis=="Criciúma EC","Criciuma"),
                   time_vis=replace(time_vis,time_vis=="EC Bahia","Bahia"),
                   time_vis=replace(time_vis,time_vis=="EC Vitória","Vitoria"),
                   time_vis=replace(time_vis,time_vis=="Figueirense FC","Figueirense"),
                   time_vis=replace(time_vis,time_vis=="Goiás EC","Goias"),
                   time_vis=replace(time_vis,time_vis=="Grêmio","Gremio"),
                   time_vis=replace(time_vis,time_vis=="Ipatinga FC","Ipatinga"),
                   time_vis=replace(time_vis,time_vis=="Joinville-SC","Joinville"),
                   time_vis=replace(time_vis,time_vis=="Náutico","Nautico"),
                   time_vis=replace(time_vis,time_vis=="Paraná","Parana"),
                   time_vis=replace(time_vis,time_vis=="Ponte Preta","Ponte.Preta"),
                   time_vis=replace(time_vis,time_vis=="RB Bragantino","RB.Bragantino"),
                   time_vis=replace(time_vis,time_vis=="Santa Cruz","Santa.Cruz"),
                   time_vis=replace(time_vis,time_vis=="Santo André","Santo.Andre"),
                   time_vis=replace(time_vis,time_vis=="Santos FC","Santos"),
                   time_vis=replace(time_vis,time_vis=="São Paulo","Sao.Paulo"),
                   time_vis=replace(time_vis,time_vis=="Sport Recife","Sport"),
                   time_vis=replace(time_vis,time_vis=="Vasco da Gama","Vasco.da.Gama"))

BD_Copa_do_Brasil<-mutate(BD_Copa_do_Brasil,
                          Campeao=replace(Campeao,Campeao=="Santo André","Santo.Andre"),
                          Campeao=replace(Campeao,Campeao=="Vasco da Gama","Vasco.da.Gama"),
                          Vice=replace(Vice,Vice=="Atlético-PR","Athletico-PR"),
                          Vice=replace(Vice,Vice=="Ceará (CE)","Ceara"),
                          Vice=replace(Vice,Vice=="Figueirense (SC)","Figueirense"),
                          Vice=replace(Vice,Vice=="São Paulo","Sao.Paulo"),
                          Vice=replace(Vice,Vice=="Vasco da Gama","Vasco.da.Gama"))

BD_Historico_Brasileiro<-mutate(BD_Historico_Brasileiro,Campeao=replace(Campeao,Campeao=="Atlético Mineiro","Atletico-MG"),
                                Campeao=replace(Campeao,Campeao=="Atletico Paranaense","Athletico-PR"),
                                Campeao=replace(Campeao,Campeao=="São Paulo","Sao.Paulo"),
                                Campeao=replace(Campeao,Campeao=="Vasco da Gama","Vasco.da.Gama"),
                                Vice=replace(Vice,Vice=="Atlético Mineiro","Atletico-MG"),
                                Vice=replace(Vice,Vice=="Atlético Paranaense","Athletico-PR"),
                                Vice=replace(Vice,Vice=="São Paulo","Sao.Paulo"),
                                Vice=replace(Vice,Vice=="Grêmio","Gremio"),
                                Vice=replace(Vice,Vice=="Vasco da Gama","Vasco.da.Gama"))

BD_Historico_Estadual<-mutate(BD_Historico_Estadual,Campeao=replace(Campeao,Campeao=="São Paulo","Sao.Paulo"),
                              Vice=replace(Vice,Vice=="São Paulo","Sao.Paulo"),
                              Campeao=replace(Campeao,Campeao=="AD São Caetano","Sao.Caetano"),
                              Vice=replace(Vice,Vice=="AD São Caetano","Sao.Caetano"),
                              Campeao=replace(Campeao,Campeao=="Athletico","Athletico-PR"),
                              Vice=replace(Vice,Vice=="Athletico","Athletico-PR"),
                              Campeao=replace(Campeao,Campeao=="Paraná Clube","Parana"),
                              Vice=replace(Vice,Vice=="Paraná Clube","Parana"),
                              Campeao=replace(Campeao,Campeao=="Atletico","Atletico-MG"),
                              Vice=replace(Vice,Vice=="Atletico","Atlitico-MG"),
                              Campeao=replace(Campeao,Campeao=="America","America-MG"),
                              Vice=replace(Vice,Vice=="America","America-MG"),
                              Campeao=replace(Campeao,Campeao=="Juventude (Caxias do Sul)","Juventude"),
                              Vice=replace(Vice,Vice=="Juventude (Caxias do Sul)","Juventude"),
                              Campeao=replace(Campeao,Campeao=="Santa Cruz","Santa.Cruz"),
                              Vice=replace(Vice,Vice=="Santa Cruz","Santa.Cruz"),
                              Campeao=replace(Campeao,Campeao=="Sport (Invicto)","Sport"),
                              Campeao=replace(Campeao,Campeao=="Vila Nova","Vila.Nova"),
                              Vice=replace(Vice,Vice=="Vila Nova","Vila.Nova"),
                              Campeao=replace(Campeao,Campeao=="Ceará","Ceara"),
                              Vice=replace(Vice,Vice=="Ceará","Ceara"),
                              Campeao=replace(Campeao,Campeao=="Clube do Remo","Remo"),
                              Vice=replace(Vice,Vice=="Clube do Remo","Remo"),
                              Campeco=replace(Campeao,Campeao=="America-RN","America-RN"),
                              Vice=replace(Vice,Vice=="America-RN","America-RN"))


#Eliminacao da partida Chapecoense x Atletico-MG (ultima rodada do campeonato de 2016) - WO duplo
BD_Amostra<-BD_Amostra %>% filter(! is.na(gols_man))
 
#----------------------------------------#
#------------Data Wrangling--------------#
#----------------------------------------#

#-----------------------#
#Ajustes para valores NA#
#-----------------------#


#For com base nos anos do campeonato
for (i in min(BD_Amostra$ano_campeonato):max(BD_Amostra$ano_campeonato)) {
  times_man<-as.data.frame(BD_Amostra %>% filter(ano_campeonato==i) %>% filter(is.na(valor_equipe_titular_man)|
                                                                                 is.na(idade_media_titular_man)))
  times_vis<-as.data.frame(BD_Amostra %>% filter(ano_campeonato==i) %>% filter(is.na(valor_equipe_titular_vis)|
                                                                                 is.na(idade_media_titular_vis)))
  lista<-c(unique(times_man$time_man),unique(times_vis$time_vis))
  
  if(length(lista)>=2){
    for (j in 1:length(lista)) {
      #Calcular a mediana para os times pertencentes a lista
      
      #Valor
      times_man<-BD_Amostra %>% filter(ano_campeonato==i & time_man==lista[j] & ! is.na(valor_equipe_titular_man))
      times_vis<-BD_Amostra %>% filter(ano_campeonato==i & time_vis==lista[j] & ! is.na(valor_equipe_titular_vis))
      BD_Amostra$valor_equipe_titular_man[BD_Amostra$ano_campeonato==i & 
                                            BD_Amostra$time_man==lista[j] & 
                                            is.na(BD_Amostra$valor_equipe_titular_man)]<-median(c(
                                              times_man$valor_equipe_titular_man,
                                              times_vis$valor_equipe_titular_vis))
      BD_Amostra$valor_equipe_titular_vis[BD_Amostra$ano_campeonato==i & 
                                            BD_Amostra$time_vis==lista[j] & 
                                            is.na(BD_Amostra$valor_equipe_titular_vis)]<-median(c(
                                              times_man$valor_equipe_titular_man,
                                              times_vis$valor_equipe_titular_vis))
      
      #Idade
      times_man<-BD_Amostra %>% filter(ano_campeonato==i & time_man==lista[j] & ! is.na(idade_media_titular_man))
      times_vis<-BD_Amostra %>% filter(ano_campeonato==i & time_vis==lista[j] & ! is.na(idade_media_titular_vis))
      BD_Amostra$idade_media_titular_man[BD_Amostra$ano_campeonato==i &
                                           BD_Amostra$time_man==lista[j] &
                                           is.na(BD_Amostra$idade_media_titular_man)]<-median(c(
                                             times_man$idade_media_titular_man,
                                             times_vis$idade_media_titular_vis))
      BD_Amostra$idade_media_titular_vis[BD_Amostra$ano_campeonato==i &
                                           BD_Amostra$time_vis==lista[j] &
                                           is.na(BD_Amostra$idade_media_titular_vis)]<-median(c(
                                             times_man$idade_media_titular_man,
                                             times_vis$idade_media_titular_vis))
    }    
  }
}
rm(times_man,times_vis,i,j,lista)

#Falta de dados de idade e valor da equipe referente ao clube Figueirense na competicao de 2011
#A referencia de informacao mais proxima e a competicao de 2012, visto que nos anos de 2010,2009 e 2008 o Figueirense nao disputou a primeira divisao
#Valor
times_man<-BD_Amostra %>% filter(ano_campeonato==2012 & time_man=="Figueirense" & ! is.na(valor_equipe_titular_man))
times_vis<-BD_Amostra %>% filter(ano_campeonato==2012 & time_vis=="Figueirense" & ! is.na(valor_equipe_titular_vis))
BD_Amostra$valor_equipe_titular_man[BD_Amostra$ano_campeonato==2011 & 
                                      BD_Amostra$time_man=="Figueirense" & 
                                      is.na(BD_Amostra$valor_equipe_titular_man)]<-median(c(
                                        times_man$valor_equipe_titular_man,
                                        times_vis$valor_equipe_titular_vis))
BD_Amostra$valor_equipe_titular_vis[BD_Amostra$ano_campeonato==2011 & 
                                      BD_Amostra$time_vis=="Figueirense" & 
                                      is.na(BD_Amostra$valor_equipe_titular_vis)]<-median(c(
                                        times_man$valor_equipe_titular_man,
                                        times_vis$valor_equipe_titular_vis))

#Idade
times_man<-BD_Amostra %>% filter(ano_campeonato==2012 & time_man=="Figueirense" & ! is.na(idade_media_titular_man))
times_vis<-BD_Amostra %>% filter(ano_campeonato==2012 & time_vis=="Figueirense" & ! is.na(idade_media_titular_vis))
BD_Amostra$idade_media_titular_man[BD_Amostra$ano_campeonato==2011 &
                                     BD_Amostra$time_man=="Figueirense" &
                                     is.na(BD_Amostra$idade_media_titular_man)]<-median(c(
                                       times_man$idade_media_titular_man,
                                       times_vis$idade_media_titular_vis))
BD_Amostra$idade_media_titular_vis[BD_Amostra$ano_campeonato==2011 &
                                     BD_Amostra$time_vis=="Figueirense" &
                                     is.na(BD_Amostra$idade_media_titular_vis)]<-median(c(
                                       times_man$idade_media_titular_man,
                                       times_vis$idade_media_titular_vis))
rm(times_man,times_vis)

#-------------------------------------#
#Normalizacao dos valores de jogadores#
#-------------------------------------#
for (i in 1:dim(BD_Amostra)[1]) {
  valor_total=BD_Amostra$valor_equipe_titular_man[i]+BD_Amostra$valor_equipe_titular_vis[i]
  BD_Amostra$valor_equipe_titular_man[i]<-BD_Amostra$valor_equipe_titular_man[i]/valor_total
  BD_Amostra$valor_equipe_titular_vis[i]<-BD_Amostra$valor_equipe_titular_vis[i]/valor_total
}
rm(i,valor_total)

#---------------------------------------#
#--Aproveitamento das ultimas rodadas---#
#---------------------------------------#
#Inclusao das novas variaveis
BD_Amostra<-mutate(BD_Amostra,
                   Aprov_1_man=0,
                   Aprov_3_man=0,
                   Aprov_5_man=0,
                   Aprov_1_vis=0,
                   Aprov_3_vis=0,
                   Aprov_5_vis=0)


#Laco Ano
for(i in min(BD_Amostra$ano_campeonato):max(BD_Amostra$ano_campeonato)){
  lista_ano<-BD_Amostra %>% filter(ano_campeonato==i)
  lista_clube<-unique(lista_ano$time_man)
  
  #Laco Clube
  for(j in 1:length(lista_clube)){
    #Zerar os parametros de PO para a rodada inicial
    PO1<-3
    PO2<-3
    PO3<-3
    PO4<-3
    PO5<-3
    
    #Laco Rodada
    for(k in min(lista_ano$rodada):max(lista_ano$rodada)){
      
      #TIME MANDANTE
      if(length(BD_Amostra$time_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_man==lista_clube[j]])!=0){
        #Atualizar o banco de dados
        BD_Amostra$Aprov_1_man[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_man==lista_clube[j]]<-PO1/3
        BD_Amostra$Aprov_3_man[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_man==lista_clube[j]]<-(PO1+PO2+PO3)/9        
        BD_Amostra$Aprov_5_man[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_man==lista_clube[j]]<-(PO1+PO2+PO3+PO4+PO5)/15
        #Redistribuir os POs
        PO5<-PO4
        PO4<-PO3
        PO3<-PO2
        PO2<-PO1
        #MANDANTE(EMPATE)
        if(BD_Amostra$gols_man[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_man==lista_clube[j]]==BD_Amostra$gols_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_man==lista_clube[j]]){
          PO1<-1
        }else{#MANDANTE(EMPATE)
          #MANDANTE(VITORIA/DERROTA)
          if(BD_Amostra$gols_man[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_man==lista_clube[j]]>BD_Amostra$gols_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_man==lista_clube[j]]){
            PO1<-3
          }else{#MANDANTE(VITORIA/DERROTA)
            PO1<-0
          }#Fim IF/ELSE - MANDANTE(VITORIA/DERROTA)
        }#Fim IF/ELSE - MANDANTE(EMPATE)
        
        
      }else{#TIME VISITANTE
        #Verificar se de fato o time ?? visitante (Poka Yoke WO)
        if(length(BD_Amostra$time_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_vis==lista_clube[j]])!=0){
          #Atualizar o banco de dados
          BD_Amostra$Aprov_1_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_vis==lista_clube[j]]<-PO1/3
          BD_Amostra$Aprov_3_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_vis==lista_clube[j]]<-(PO1+PO2+PO3)/9        
          BD_Amostra$Aprov_5_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada==k & BD_Amostra$time_vis==lista_clube[j]]<-(PO1+PO2+PO3+PO4+PO5)/15
          #Redistribuir os POs
          PO5<-PO4
          PO4<-PO3
          PO3<-PO2
          PO2<-PO1
          #VISITANTE(EMPATE)
          if(BD_Amostra$gols_man[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_vis==lista_clube[j]]==BD_Amostra$gols_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_vis==lista_clube[j]]){
            PO1<-1
          }else{#VISITANTE(EMPATE)
            #VISITANTE(VITORIA/DERROTA)
            if(BD_Amostra$gols_man[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_vis==lista_clube[j]]<BD_Amostra$gols_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$rodada== k & BD_Amostra$time_vis==lista_clube[j]]){
              PO1<-3
            }else{#VISITANTE(VITORIA/DERROTA)
              PO1<-0
            }
          }#Fim IF/ELSE - VISITANTE(VITORIA/DERROTA)
        }# Fim IF/ELSE - VISITANTE Empate
      }# Fim IF/ELSE - MANDANTE / VISITANTE
    }#Fim Laco Rodada
  }#Fim Laco Clube
}#Fim Laco ano

rm(lista_ano,i,j,k,lista_clube,PO1,PO2,PO3,PO4,PO5)

#---------------------------------------#
#-----Participacao na Libertadores------#
#---------------------------------------#
#Inclusao da nova variavel
BD_Amostra<-mutate(BD_Amostra,Libertadores_man="Nao",Libertadores_vis="Nao")

#Laco Clube na BD_Libertadores
for (i in 2:dim(BD_Libertadores)[2]){
  
  #Laco Ano na BD_Libertadores
  for (j in min(BD_Amostra$ano_campeonato):max(BD_Amostra$ano_campeonato)){
    
    if(BD_Libertadores[BD_Libertadores$Ano==j,i]=="Sim"){
      
      #Atualizacao para "Sim" considerando Ano / Time / Man 
      BD_Amostra$Libertadores_man[BD_Amostra$ano_campeonato==j & BD_Amostra$time_man==names(BD_Libertadores)[i]]<-"Sim"
      
      #Atualizacao para "Sim" considerando Ano / Time / Man
      BD_Amostra$Libertadores_vis[BD_Amostra$ano_campeonato==j & BD_Amostra$time_vis==names(BD_Libertadores)[i]]<-"Sim"
      
    }#Fim IF/ELSE - SIM NO BD_Libertadores
  }#Fim Laco Ano na BD_Libertadores
}#Fim Laco Clube na BD_Libertadores
rm(i,j,BD_Libertadores)

#---------------------------------------#
#--Posicao Copa do Brasil ano anterior--#
#---------------------------------------#

#Inclusao da nova variavel
BD_Amostra<-mutate(BD_Amostra,Finalista_Copa_Brasil_man="Nao",Finalista_Copa_Brasil_vis="Nao")

#Laco Ano na BD_Copa_do_Brasil
for (i in (min(BD_Amostra$ano_campeonato)-1):(max(BD_Amostra$ano_campeonato)-1)){
  #Campeao-mandante
  BD_Amostra$Finalista_Copa_Brasil_man[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_man==BD_Copa_do_Brasil[BD_Copa_do_Brasil==i,2]]<-"Campeao"
  #Campeao-visitante
  BD_Amostra$Finalista_Copa_Brasil_vis[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_vis==BD_Copa_do_Brasil[BD_Copa_do_Brasil==i,2]]<-"Campeao"
  #Vice-mandante
  BD_Amostra$Finalista_Copa_Brasil_man[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_man==BD_Copa_do_Brasil[BD_Copa_do_Brasil==i,3]]<-"Vice"
  #Vice-visitante
  BD_Amostra$Finalista_Copa_Brasil_vis[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_vis==BD_Copa_do_Brasil[BD_Copa_do_Brasil==i,3]]<-"Vice"
}#Fim Laco Ano na BD_Copa_do_Brasil
rm(i,BD_Copa_do_Brasil)

#---------------------------------------#
#---Posicao Brasileirco ano anterior----#
#---------------------------------------#

#Inclusao da nova variavel
BD_Amostra<-mutate(BD_Amostra,Finalista_Brasileiro_man="Nao",Finalista_Brasileiro_vis="Nao")

for (i in (min(BD_Amostra$ano_campeonato)-1):(max(BD_Amostra$ano_campeonato)-1)){
  #Campeao Mandante
  BD_Amostra$Finalista_Brasileiro_man[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_man==BD_Historico_Brasileiro$Campeao[BD_Historico_Brasileiro$Ano==i]]<-"Campeao"
  #Campeao Visitante
  BD_Amostra$Finalista_Brasileiro_vis[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_vis==BD_Historico_Brasileiro$Campeco[BD_Historico_Brasileiro$Ano==i]]<-"Campeao"
  #Vice Mandante
  BD_Amostra$Finalista_Brasileiro_man[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_man==BD_Historico_Brasileiro$Vice[BD_Historico_Brasileiro$Ano==i]]<-"Vice"
  #Vice Visitante
  BD_Amostra$Finalista_Brasileiro_vis[BD_Amostra$ano_campeonato==(i+1) & BD_Amostra$time_vis==BD_Historico_Brasileiro$Vice[BD_Historico_Brasileiro$Ano==i]]<-"Vice"
}
rm(i,BD_Historico_Brasileiro)

#---------------------------------------#
#-----------Estado dos clubes-----------#
#---------------------------------------#

#Inclusao da nova variavel
BD_Amostra<-mutate(BD_Amostra,Estado_man="Nao",Estado_vis="Nao")


#Laco Clubo no BD_Times_Estados
for(i in 1:dim(BD_Times_Estados)[1]){
  #Estado_time_mandante
  BD_Amostra$Estado_man[BD_Amostra$time_man==BD_Times_Estados$time_man[i]]<-BD_Times_Estados$Estado[i]
  #Estado_time_visitante
  BD_Amostra$Estado_vis[BD_Amostra$time_vis==BD_Times_Estados$time_man[i]]<-BD_Times_Estados$Estado[i]
}#Fim Laco Clube no BD_Times_Estados
rm(i,BD_Times_Estados)

#---------------------------------------#
#---Posicao no Campeonato Estadual----#
#---------------------------------------#

#Inclusco da nova variavel
BD_Amostra<-mutate(BD_Amostra,Finalista_Estadual_man="Nao",Finalista_Estadual_vis="Nao")


#Laco Ano
for (i in (min(BD_Amostra$ano_campeonato):max(BD_Amostra$ano_campeonato))) {
  
  lista_ano<-BD_Historico_Estadual %>% filter(Ano==i)
  lista_camp<-lista_ano$Campeao
  lista_vice<-lista_ano$Vice
  
  #Laco Campeao
  for (j in 1:length(lista_camp)) {
    #Campeao Mandante
    BD_Amostra$Finalista_Estadual_man[BD_Amostra$ano_campeonato==i & BD_Amostra$time_man==lista_camp[j]]<-"Campeao"
    #Campeao Visitante
    BD_Amostra$Finalista_Estadual_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$time_vis==lista_camp[j]]<-"Campeao"
  }#Fim Laco Campeao
  
  #Laco Vice
  for (k in 1:length(lista_vice)) {
    #Vice Mandante
    BD_Amostra$Finalista_Estadual_man[BD_Amostra$ano_campeonato==i & BD_Amostra$time_man==lista_vice[k]]<-"Vice"
    #Vice Visitante
    BD_Amostra$Finalista_Estadual_vis[BD_Amostra$ano_campeonato==i & BD_Amostra$time_vis==lista_vice[j]]<-"Vice"
  }#Fim Laco Vice
}#Fim Laco ano

rm(i,j,k,lista_ano,lista_camp,lista_vice,BD_Historico_Estadual)
