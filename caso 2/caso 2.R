library(tidyverse)
library(rio)
library(dplyr)
library(readr)

envios<-read.csv("base_envÌos.csv")

envios$fecha_entrega <- as.Date(envios$fecha_entrega, origin = "1969-12-31")

valores_negativos <- envios %>%
  filter(id_env√.o < 0)
print(valores_negativos)

registros_na <- envios %>%
  filter(origen %in% c("", "sin dato") |destino %in% c("", "sin dato") | fecha_entrega %in% c("", "Sin dato")
        | monto_env√.o %in% c("", "Sin dato"))


is_valid_date <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}
fechas_invalidas <- envios %>%
  filter(!is_valid_date(fecha_env√.o))

duracion <- envios %>%
  filter(is_valid_date(fecha_env√.o) & is_valid_date(fecha_entrega)) %>%
  mutate(
    fecha_env√.o = as.Date(fecha_env√.o),
    fecha_entrega = as.Date(fecha_entrega)
  )
envios_otros <- duracion %>%
  mutate(diferencia_dias = as.numeric(fecha_entrega - fecha_env√.o))

valores_negativos <- mutate_all(valores_negativos, as.character)
registros_na <- mutate_all(registros_na, as.character)
fechas_invalidas <- mutate_all(fechas_invalidas, as.character)

valores_incoherentes <- bind_rows(
  fechas_invalidas,
  registros_na,
  valores_negativos,
)