#-----------------------------------------------------------------
# Análisis correlación entre variables
#-----------------------------------------------------------------


library(tidyverse)
library(raster)
library(sp)
library(terra)
library(MASS)
library(corrplot)
library(dplyr)
library(readxl)
library(agricolae)
library(combinat)
library(utils)
library(rgl)
library(tidyr)

#*chama tabela
setwd("D:/GRTACNA/2025/DEPOSITO_BOT/FROM_BRYAN/dist_spp/input/datos")
data <-read_excel("datos_carzo_23ene26.xlsx")
data <- data %>% replace(is.na(.), 0)

#View(data)
str(data)
summary(data)
names(data)
table(data$ZONE)
table(data$CODE_T)
attach(data)
head(data)

#-----------------------------------------------------------------
#transformacion: variables a vector
#-----------------------------------------------------------------

CODE_T <- factor(CODE_T)
ZONE <- factor(ZONE)
SEX <- factor(SEX)
POS_COPA <- factor(POS_COPA)
FOR_COPA <- factor(FOR_COPA)
EST_SA <- factor(EST_SA)
CAL_FUSTE <- factor(CAL_FUSTE)
MORT <- factor(MORT)
FEN <- factor(FEN)
DIST_REG <- factor(DIST_REG)

str(data)
#-----------------------------------------------------------------
#resumen/sumatoria de algunos datos
#-----------------------------------------------------------------

summary(data)

table(data$CODE_T)
table(data$ZONE)
table(data$SEX)
table(data$POS_COPA)
table(data$FOR_COPA)
table(data$CODE_T,data$ZONE)
table(data$CODE_T,data$SEX)

#-----------------------------------------------------------------
# SEPARAR VAR.NUMERICAS
#-----------------------------------------------------------------
#quitar columna no numericas, matriz para determinar correlacion
head(data)
data_cor1=data[,-c(1,2,3,4,9,10,11,12,13,14,15)]
head(data_cor1)
names(data_cor1) = c("H","AA_m","DAP_m","COB_T_m2","DIST_REG","REG_N_P","REG_N_B","BIO01", "BIO02", "BIO03", "BIO04", "BIO05", "BIO06", "BIO07", "BIO08", "BIO09", "BIO10", "BIO11",  "BIO12", "BIO13", "BIO14", "BIO15", "BIO16","BIO17","BIO18", "BIO19")
names(data_cor1)

#-----------------------------------------------------------------
#  CORRELACION ENTRE VARIABLES NUMERICAS
#-----------------------------------------------------------------

#crear matriz de correlacion para variables numericas seleccionadas
matriz_cor<-round(cor(x = data_cor1, method = "pearson"), 2)
matriz_cor
#graficar matrix de correlacion con 24 var
corrplot(matriz_cor, order= "original")
corrplot(matriz_cor, order= "AOE")
corrplot(matriz_cor, order= "hclust")
corrplot(matriz_cor, order= "FPC")
#order = c("original","AOE", "FPC", "hclust", "alphabet"),
col<- colorRampPalette(c("white", "green"))
corrplot(matriz_cor, method="shade", sade.col= NA, tl.col="black", tl.srt=45, tl.cex = 0.6, cl.cex = 0.6, number.cex = 0.6, col= col(200), addCoef.col = "black", addcolorlabel= "no", order= "original", type= "lower", diag= F, addshade= "all")

#graficar correlacion + regresion linear multiple
library(dplyr)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
rcorr(as.matrix(data_cor1,  method = "pearson"))
chart.Correlation(data_cor1, histogram = F, pch = 17)
#crear matriz de correlacion + coef. de correlacion para todas variables 
library(psych)
matriz_coef_cor<-corr.test(x = data_cor1, method = "pearson")#evalua coef. spearman multiple al 95% osea p<0.05
matriz_coef_cor

class(matriz_coef_cor)
