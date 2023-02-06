#----------------------------------------#
#----------Carregar bibliotecas----------#
#----------------------------------------#
library(dplyr)
library("tidyverse")
library(plotly)
library(openxlsx)
library(readxl)
library(Hmisc)
library(ltm)
library(psych)
library(PerformanceAnalytics)
library(ggrepel)
library(reshape2)


#----------------------------------------#
#--------Carregar banco de dados---------#
#----------------------------------------#
#Caminho
Caminho=paste0(getwd(),"/")

#Carregar arquivo resultante da etapa anterior
BD_Normalizado<-read_excel(paste0(Caminho,"BD_Normalizado_Backup.xlsx"))


#----------------------------------------#
#--------Coeficiente de  Pearson---------#
#----------------------------------------#
# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(BD_Normalizado[,20:dim(BD_Normalizado)[2]]), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes
diag(corr_sig)<-1


#----------------------------------------#
#------------Análises--------------------#
#----------------------------------------#

# Teste de esfericidade de Bartlett nas variáveis Dummy
cortest.bartlett(BD_Normalizado[,20:93])

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(BD_Normalizado[, 20:93],
                      nfactors = length(BD_Normalizado[, 20:93]),
                      rotate = "none",
                      scores = TRUE)
fatorial


# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues
autovalores<-eigenvalues[1:38]
#Dados para 38 Autovalores
#SS loadings=1.00
#Proportion Var=0.01
#Cumulative Var=0.55
#Proportion Explained=0.01 
#Cumulative Proportion=0.55





























#----------------------------------------#
#------------Visibilidades---------------#
#----------------------------------------#
# Mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  NotasFatorial[,2:5] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())


# Histogramas e Dispersões unificados
chart.Correlation(NotasFatorial[, 2:5], histogram = TRUE, pch = "+")
