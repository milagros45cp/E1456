library(tidyverse)
library(rio)
library(dplyr)
library(readr)

polizas<-read.csv("base_polizas.csv")
polizas <- mutate_all(polizas,~replace(., is.na(.), "sin dato"))

polizas <- polizas %>%
  mutate(
    fecha_inicio = as.Date(fecha_inicio, format = "%Y-%m-%d"),
    fecha_fin = as.Date(fecha_fin, format = "%Y-%m-%d"),
    antiguedad = ifelse(is.na(fecha_fin), NA, as.integer(difftime(fecha_fin, fecha_inicio, units = "days") / 365))
  )

polizas_activas<- polizas %>%
  mutate(fecha_inicio = as.Date(fecha_inicio, format = "%Y-%m-%d"),
         fecha_fin = as.Date(fecha_fin, format = "%Y-%m-%d"))

polizas_activas <- polizas %>%
  mutate(fecha_fin = ifelse(estado == "Activa", NA, fecha_fin))

polizas_solactivas <- polizas %>%
  filter(!is.na(fecha_fin) & 
           fecha_fin >= fecha_inicio & 
           estado == "Activa")

is_valid_date <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}

fecha_incorrectas <- polizas %>%
  filter(!is_valid_date(fecha_inicio) | !is_valid_date(fecha_fin))

polizas <- polizas %>%
  filter(is_valid_date(fecha_inicio) & is_valid_date(fecha_fin))

registros_no_encontrados <- polizas %>%
  filter(nombre_cliente %in% c("", "sin dato") | fecha_fin %in% c("", "sin dato")| monto_cobertura %in% c("", "sin dato") 
         | estado %in% c("","sin dato"))

valores_negativos <- polizas %>%
  filter(id_poliza< 0)
print(valores_negativos)

polizas<- polizas[polizas$id_poliza>0,]

polizas_fecha_mal<- polizas %>%
  filter(fecha_inicio >= fecha_fin | is.na(fecha_fin))

polizas<- polizas %>%
  filter(fecha_fin >= fecha_inicio | is.na(fecha_fin))

polizas <- polizas %>%
  mutate(
    fecha_inicio = as.Date(fecha_inicio, format = "%Y-%m-%d"),
    fecha_fin = as.Date(fecha_fin, format = "%Y-%m-%d"),
    antiguedad = ifelse(is.na(fecha_fin), NA, as.integer(difftime(fecha_fin, fecha_inicio, units = "days") / 365))
  )

polizas_activas<- polizas %>%
  mutate(fecha_inicio = as.Date(fecha_inicio, format = "%Y-%m-%d"),
         fecha_fin = as.Date(fecha_fin, format = "%Y-%m-%d"))

polizas_activas <- polizas %>%
  mutate(fecha_fin = ifelse(estado == "Activa", NA, fecha_fin))
  
polizas_solactivas <- polizas %>%
  filter(!is.na(fecha_fin) & 
           fecha_fin >= fecha_inicio & 
           estado == "Activa")
valores_incorrectos <- bind_rows(
  valores_negativos,
  registros_no_encontrados,
  fecha_incorrectas,
  polizas_fecha_mal
)

print(polizas)
polizas %>% str()