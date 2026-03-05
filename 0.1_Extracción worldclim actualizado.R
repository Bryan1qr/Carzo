#* Extracción de valores puntuales de variables bio-worldClim V2.1
library(tidyverse)
library(terra)
library(geodata)

#* Cargamos la tabla de coordenadas EPSG 4326:
xy <- read.csv("input/xy_anual_carzo2024.csv", sep = ";")

#* Renombramos
rownames(xy) <- xy$CODE
xy <- xy[,c(2,3)]
names(xy) <- c('x','y')
head(xy)

#* Método 1: Utilizando el paquete geodata:
bio1 <- worldclim_country(var = "bio",  res = 0.5, country = "PER")
srad <- worldclim_country(var = "srad",  res = 0.5, country = "PER")
wind <- worldclim_country(var = "wind",  res = 0.5, country = "PER")
#! No existe aún en el respistorio de worldclim la variable vapr como TILE!
#! Entonces usamos otra función más pesada:
dir.create("vapor") # Creamos una carpeta para guardado:
vapr <- worldclim_global(var = "vapr", res = 0.5, path = "vapor") # Pesa 1.2 GB!!!!!!
prec <- worldclim_country(var = "prec",  res = 0.5, country = "PER")
tmax <- worldclim_country(var = "tmax",  res = 0.5, country = "PER")
tmin <- worldclim_country(var = "tmin",  res = 0.5, country = "PER")
tavg <- worldclim_country(var = "tavg",  res = 0.5, country = "PER")

# Extracción de los valores puntuales de las variables bioclimáticas
# sobre los puntos de interés:
df_bio <- raster::extract(bio1, xy, df = T)
df_srad <- raster::extract(srad, xy, df = T)
df_wind <- raster::extract(wind, xy, df = T)
df_vapr <- raster::extract(vapr, xy, df = T)
df_prec <- raster::extract(prec, xy, df = T)
df_tmax <- raster::extract(tmax, xy, df = T)
df_tmin <- raster::extract(tmin, xy, df = T)
df_tavg <- raster::extract(tavg, xy, df = T)

df_clim <- df_bio %>%
  inner_join(df_srad, by = "ID") %>%
  inner_join(df_wind, by = "ID") %>%
  inner_join(df_vapr, by = "ID") %>%
  inner_join(df_prec, by = "ID") %>%
  inner_join(df_tmax, by = "ID") %>%
  inner_join(df_tmin, by = "ID") %>%
  inner_join(df_tavg, by = "ID") %>%
  cbind(xy)

str(df_clim)
#* Guardamos
write.csv (df_clim, "world_clim_data_frame.csv")
