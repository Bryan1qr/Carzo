#-----------------------------------------------------------------
#INTEGRAR VARIABLES A MODELOS
#-----------------------------------------------------------------
#*Primero analizar CON TODAS VAR. BIOCLIM. Luego seleccionar variables


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

#*tABLA
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


#ALTURA
modelo0 <- lm(AA_m ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO07 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(modelo0)

#DIAMETRO
modelo1 <- lm(DAP_m ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO07 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(modelo1)

#COBERTURA
modelo2 <- lm(COB_T_m2 ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO07 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(modelo2)

#DISTANCIA A REGENERACIÓN
modelo3 <- lm(DIST_REG ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO07 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(modelo3)

#ABUNDANCIA PLANTULAS - REGENERACIÓN
modelo4 <- lm(REG_N_P ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO07 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(modelo4)

#ABUNDANCIA BRINZALES - REGENERACIÓN
modelo5 <- lm(REG_N_B ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO07 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(modelo5)

#imprimir sumario de modelos
library(stargazer)
stargazer(modelo0,modelo1, modelo2, modelo3, modelo4, type="text", df=FALSE)

#sumario para los modelos evaluados, con titulos
stargazer(modelo0,modelo1, modelo2, modelo3, modelo4, modelo5,type="text", title="Comparación de modelos FINALES")

# Analizar el criterio de AIC
AIC(modelo0,modelo1, modelo2, modelo3, modelo4, modelo5)


#-----------------------------------------------------------
#Seleccion de variables para encontrar el mejor modelo
#-----------------------------------------------------------


# 0. modelo 0 = AA_m ALTURA
full.model0 <- lm(AA_m ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(full.model0)
library(MASS)  # Para poder usar la funcion stepAIC
modback <- stepAIC(full.model0, trace=TRUE, direction="backward")
#para seleccionar modelo final
modback$anova

# 1. modelo 1 = DIAMETRO A ALTURA DE PECHO
full.model01 <- lm(DAP_m ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO09 + BIO10 + + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(full.model01)
library(MASS)  # Para poder usar la funcion stepAIC
modback01 <- stepAIC(full.model01, trace=TRUE, direction="backward")
#para seleccionar modelo final
modback01$anova

# 2. modelo 2 = COBERTURA VEGETAL
full.model2 <- lm(COB_T_m2 ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(full.model2)
library(MASS)  # Para poder usar la funcion stepAIC
modback2 <- stepAIC(full.model2, trace=TRUE, direction="backward")
#para seleccionar modelo final
modback2$anova

# 3. modelo 3 = DISTANCIA A REGENERACIÓN
full.model3 <- lm(DIST_REG ~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(full.model3)
library(MASS)  # Para poder usar la funcion stepAIC
modback3 <- stepAIC(full.model3, trace=TRUE, direction="backward")
#para seleccionar modelo final
modback3$anova


# 4. modelo 4 = ABUNDANCIA PLANTULAS
full.model4 <- lm(REG_N_P~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(full.model4)
library(MASS)  # Para poder usar la funcion stepAIC
modback4 <- stepAIC(full.model4, trace=TRUE, direction="backward")
#para seleccionar modelo final
modback4$anova

# 5. modelo 5 = ABUNDANCIA BRINZALES
full.model5 <- lm(REG_N_B~ H + BIO01 + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO16 + BIO17 + BIO18 + BIO19, data = data_cor1)
summary(full.model5)
library(MASS)  # Para poder usar la funcion stepAIC
modback5 <- stepAIC(full.model5, trace=TRUE, direction="backward")
#para seleccionar modelo final
modback5$anova

#------------------------------------------------------------------------
#Usar variables sugeridas por analisis de deviance para optimizar modelo
#-----------------------------------------------------------------------

mod0 <- lm(AA_m ~  H + BIO01 + BIO02 + BIO03 + BIO05 + BIO06 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO15 + BIO16 + BIO18, data = data_cor1)
summary(mod0)

mod1 <- lm(DAP_m ~ H + BIO01 + BIO02 + BIO03 + BIO05 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO18, data = data_cor1)
summary(mod1)

mod2 <- lm(COB_T_m2 ~ H + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO10 + BIO14 + BIO15 + BIO16 + BIO18, data = data_cor1)
summary(mod2)

mod3 <- lm(DIST_REG ~ H + BIO04 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO16 + BIO18, data = data_cor1)
summary(mod3)

mod4 <- lm(REG_N_P ~ BIO02 + BIO03 + BIO04 + BIO05 + BIO10 + BIO12 + BIO13 + BIO14 + BIO15 + BIO18, data = data_cor1)
summary(mod4)

mod5 <- lm(REG_N_B ~ H + BIO01 + BIO04 + BIO09 + BIO11 + BIO12 + BIO13 + BIO16, data = data_cor1)
summary(mod5)


library(stargazer)
stargazer(mod0,mod1, mod2, mod3, mod4, mod5, type="text", df=FALSE)
stargazer(mod0,mod1, mod2, mod3, mod4, mod5, type="text", title="Comparación de modelos finales")
# Analizar el criterio de AIC
AIC(mod0,mod1, mod2, mod3, mod4, mod5)

#GRAFICO CORRELACIONES-MODELOS FINALES

mf0 <- data_cor1[,-c(3:7,11,14,15,21,24,26)]
chart.Correlation(mf0, histogram = F, pch = 17)
m0 <- lm(AA_m ~  H + BIO01 + BIO02 + BIO03 + BIO05 + BIO06 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO15 + BIO16 + BIO18, data = mf0)
summary(m0)

mf1 <- data_cor1[,-c(2,4:7,11,15,16,23,24,26)]
chart.Correlation(mf1, histogram = F, pch = 17)
m1 <- lm(DAP_m ~ H + BIO01 + BIO02 + BIO03 + BIO05 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO15 + BIO18, data = mf1)
summary(m1)

mf2 <- data_cor1[,-c(2:3,5:8,14,16,18:20,24,26)]
chart.Correlation(mf2, histogram = F, pch = 17)
m2 <- lm(COB_T_m2 ~ H + BIO02 + BIO03 + BIO04 + BIO05 + BIO06 + BIO08 + BIO10 + BIO14 + BIO15 + BIO16 + BIO18, data = mf2)
summary(m2)

mf3 <- data_cor1[,-c(2:4,6:10,12:14,24,26)]
chart.Correlation(mf3, histogram = F, pch = 17)
m3 <- lm(DIST_REG ~ H + BIO04 + BIO08 + BIO09 + BIO10 + BIO11 + BIO12 + BIO13 + BIO14 + BIO16 + BIO18, data = mf3)
summary(m3)

mf4 <- data_cor1[,-c(1:5,7:8,13:16,23:24,26)]
chart.Correlation(mf4, histogram = F, pch = 17)
m4 <- lm(REG_N_P ~ BIO02 + BIO03 + BIO04 + BIO05 + BIO10 + BIO12 + BIO13 + BIO14 + BIO15 + BIO18, data = mf4)
summary(m4)

mf5 <- data_cor1[,-c(2:6,9:10,12:15,17,21:22,24:26)]
chart.Correlation(mf4, histogram = F, pch = 17)
m5 <- lm(REG_N_B ~ H + BIO01 + BIO04 + BIO09 + BIO11 + BIO12 + BIO13 + BIO16, data = mf5)
summary(m5)

AIC(m0,m1, m2, m3, m4, m5)


############################################################
