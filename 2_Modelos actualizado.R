#2_Modelos actualizado:

library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(corrplot)
library(MASS)
library(PerformanceAnalytics)
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
head(data)

#-----------------------------------------------------------------
# SEPARAR VAR.NUMERICAS
#-----------------------------------------------------------------
#quitar columna no numericas, matriz para determinar correlacion
head(data)
data_cor1 <- data[,-c(1,2,3,4,9,10,11,12,13,14,15)]
head(data_cor1)
names(data_cor1) <- c(
  "H","AA_m","DAP_m","COB_T_m2","DIST_REG","REG_N_P","REG_N_B","BIO01",
  "BIO02", "BIO03", "BIO04", "BIO05", "BIO06", "BIO07", "BIO08", "BIO09",
  "BIO10", "BIO11",  "BIO12", "BIO13", "BIO14", "BIO15", "BIO16","BIO17",
  "BIO18", "BIO19")

names(data_cor1)

# Preparación para modelos:
## simplificación de variables:
predictoras <- predictoras <- data_cor1 %>%
  select(H, starts_with("BIO")) %>%
  names()

respuestas <- c("AA_m", # Altura
                "DAP_m", # Diámetro
                "COB_T_m2", # Cobertura
                "DIST_REG", # Distancia a regeneración
                "REG_N_P", # Abundancia plántulas - regeneración
                "REG_N_B") # Abundancia brinzales - regeneración

#-----------------------------------------------------------
# Creación de modelos
#-----------------------------------------------------------

modelos <- respuestas %>% set_names() %>% 
  map(~ lm(
    reformulate(predictoras, response = .x),
    data = data_cor1))

summary(modelos$AA_m)
summary(modelos$DAP_m)
summary(modelos$COB_T_m2)
summary(modelos$DIST_REG)
summary(modelos$REG_N_P)
summary(modelos$REG_N_B)

#imprimir sumario de modelos
stargazer::stargazer(
  modelos, type="text", title = "Comparación de modelos finales", df = FALSE)

# Analizar el criterio de AIC
lapply(modelos, FUN = AIC)

# Con indicadores automatizados:
# Ordenado desde el menor al mayor AIC:
library(broom)
metricas_fun <- function(x){
  map_dfr(x, function(mod){
  tibble(
    AIC  = AIC(mod),
    BIC  = BIC(mod),
    R2   = summary(mod)$r.squared,
    R2_adj = summary(mod)$adj.r.squared,
    RMSE = sqrt(mean(residuals(mod)^2))
   )
  }, .id = "modelo") %>% arrange(AIC)
}
metricas_fun(modelos)

print(metricas_modelos)

#-----------------------------------------------------------
#Seleccion de variables para encontrar el mejor modelo
#-----------------------------------------------------------
modbacks <- lapply(modelos, stepAIC)

# Seleccion de modelos finales:
modbacks$AA_m$anova
modbacks$DAP_m$anova
modbacks$COB_T_m2$anova
modbacks$DIST_REG$anova
modbacks$REG_N_P$anova
modbacks$REG_N_B$anova

# Extracción de variables:
seleccion_variables <- map(modbacks, ~ attr(terms(.x), "term.labels"))

seleccion_variables


# Utilizando las nuevas variables en nuevos modelos:
modelos_finales <- imap(
  seleccion_variables,
  ~ lm(reformulate(.x, response = .y),
       data = data_cor1)
)

# Análisis de indicadores:
stargazer::stargazer(modelos_finales, type = "text", df = FALSE)
metricas_fun(modelos_finales)

# Gráficos automatizados:

Graficos <- function(lista_modelos){

  imap(lista_modelos, function(modelo, nombre){

    vars <- attr(terms(modelo), "term.labels")

    datos_sub <- data_cor1 %>%
      dplyr::select(dplyr::all_of(c(nombre, vars)))

    png(paste0("correlaciones/Correlacion_", nombre, ".png"),
        width = 1200, height = 1200, res = 150)

    chart.Correlation(
      datos_sub,
      histogram = FALSE,
      pch = 17,
      main = paste("Correlaciones -", nombre)
    )

    dev.off()
  })
}

Graficos(modelos_finales)
#? Visualizar resultados en la carpeta correlaciones!!
