######################################################
##################Trabalho Final######################
############Projetos de análise usando R##############

###Instalando Pacotes###
install.packages(c('raster','rgdal','dismo','maptools','sp','rgeos','rJava','ecospat','ecodist','usdm',"sdm"))
install.packages("sdm", repos="http://R-Forge.R-project.org")
install.packages("envirem", dependencies = TRUE)
install.packages("maxnet", dependencies = TRUE)
install.packages ("tidyverse")
install.packages ("vegan")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("readxl")
install.packages("ape")
install.packages("rgdal")
install.packages("tidyr")
install.packages("qualityTools")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages('ggfortify')
install.packages("ggplot")
install.packages("maptools")



###Carregando os pacotes###
library(sp)
library(raster)
library(rgdal)
library(dismo)
library(maptools)
library(rgeos)
library(sdm)
library(envirem)
library(maxnet)
library(sf)
library(ggplot2)
library(pbapply)
library(dplyr)
library('ecodist')
library('ecospat')
library(usdm) 
library(BBmisc)
library(vegan)
library(rmarkdown)
library(knitr)
library(readxl)
library(ape)
library(rgdal)
library(tidyr)
library("qualityTools")
library(FactoMineR)
library(factoextra)
install.packages('ggfortify')
library(ggplot)
library(maptools)



######Iniciando trabalho######

read.csv2("Data/Dados_Fic.csv") #Carregando os dados e lendo os dados#
Dados <- read.csv2("Data/Dados_Fic.csv") #Mudando nome raiz#
summary(Dados) #resumindo Dados com média das variaveis#

###Anova#######
resultado <- aov(Dados$Vent~Dados$Sub + Dados$Ctot)
summary(resultado)
resultado <- aov(Dados$Vent~Dados$Ctot + Dados$CRC)
summary(resultado)
resultado <- aov(Dados$Sub~Dados$CRC + Dados$Vent)
summary(resultado)

data("anscombe")
dim(anscombe)
head(anscombe)
class(anscombe)
str(anscombe)


        #####Explorando os dados######
dim(Dados)
head(Dados)
class(Dados)
str(Dados)

#####Médias das variaveis#####
mean(Dados$Vent)
mean(Dados$Sub)
mean(Dados$CRC)
mean(Dados$CC)
mean(Dados$Ctot)


nrow(Dados)
mean(Dados$Vent)
mean(Dados$Sub)
mean(Dados$Ctot)

# Alálise de PCA
## Criando subconjuntos de linhas e colunas para a análise
Dados_PCA <- Dados[1:38, 3:7]

# Visualizar os dados
View(Dados_PCA)

# Gerando PCA
resu.pca <- prcomp(Dados_PCA)

summary(resu.pca)

# Extrair a proporção de variância dos valores de componentes principais
eig.val <- get_eigenvalue(resu.pca)
eig.val

# Plotar no gráfico mostrando a proporção de variância de cada variavel
fviz_eig(resu.pca, addlabels = T, ylim = c(0,90))

# Extrair os resultados das variaveis do PCA para plotar no gráfico
var <- get_pca_var(resu.pca)
ind <- get_pca_ind(resu.pca)

# Plotar gráfico de PCA
fviz_pca_var(resu.pca, col.var = "blue")

# Criando grupo para cluster
especie <- as.factor(Dados[ ,1])

# Plotando com grupos
fviz_pca(resu.pca, habillage = especie, title = )

####Box plot#####



par(mfrow=c(1, 1))        
hist(Dados$Vent,main=NULL, las = 1, breaks = 4, ylab="Freqência", xlab="Ventrais")

plot(Dados$Vent, ylab="CompTotal", Dados$Ctot, xlab="Ventral")
plot(Dados$Sex, ylab="CompTotal", Dados$Ctot, xlab="Ventral")
plot(Dados$Sex, ylab="Ventrais", Dados$Vent, xlab="Sexo")
plot(Dados$SP, ylab="ventrais", Dados$Vent, xlab="Especies")

#########plots cantinho do R#######
head(Dados)
summary(Dados)
plot(Dados$Vent, Dados$Ctot, las = 1)
plot(Dados$Vent, Dados$Ctot, las = 1, ylab="Comprimento Total",
     xlab="Nº Ventrais")
plot(Dados$Vent, Dados$Ctot, las = 1, ylab="Comprimento Total",
     xlab="Nº Ventrais", pch=16)


#####mapa######
data(wrld_simpl)
read.csv2("Data/Dados_Map.csv")
Map <- read.csv2("Data/Dados_Map.csv") #Mudando nome raiz#
summary(Map)
bradypus<-Map[,9:8]
summary(bradypus)

windows(width=6, height=6, rescale="fixed")

plot(wrld_simpl, xlim=c(-90, -30), ylim=c(-60, 20),
     axes=T, col="gray")

points(bradypus, pch=16, col="black", cex=0.5)


