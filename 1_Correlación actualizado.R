# 1_Correlación actualizado:

library(dplyr)
library(tidyr)
library(readxl)
library(corrplot)
#-----------------------------------------------------------------
# Limpieza y transformacion: variables a vector
#-----------------------------------------------------------------
data <- read_excel("input/datos_carzo_23ene26.xlsx") %>%
  replace_na(list()) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
  mutate(
    across(
      c(CODE_T, ZONE, SEX, POS_COPA, FOR_COPA,
        EST_SA, CAL_FUSTE, MORT, FEN),
      as.factor
    )
  )

#View(data)
str(data)
summary(data)
names(data)
table(data$ZONE)
table(data$CODE_T)
attach(data)
head(data)

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

str(data)

#-----------------------------------------------------------------
# SEPARAR VAR.NUMERICAS
#-----------------------------------------------------------------
#quitar columna no numericas, matriz para determinar correlacion

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
library(corrplot)

# Paleta de colores adecuada
colores <- colorRampPalette(c("blue", "white", "red"))(200)

corrplot(
  matriz_cor,
  method = "shade",
  shade.col = "NA",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.6,
  cl.cex = 0.6,
  number.cex = 0.6,
  col = colores,
  addCoef.col = "black",
  order = "original",
  type = "lower",
  diag = FALSE,
  addshade = "all"
)


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
