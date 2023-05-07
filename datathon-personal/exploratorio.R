# Analisis estadistico de los tipo de comercios 
library(tidyverse)
library(dplyr)

dir <- "/Users/alexa/Carpetas locales/Datathon/datathon-personal"

datos <- read_csv(paste0(dir,"/dataset_original.csv"))%>%
  janitor::clean_names()
  
library(lubridate)
date_check<- datos %>% 
  dplyr::mutate(Date2 = dplyr::case_when(
    grepl('-',fecha_transaccion) ~ as.POSIXct(fecha_transaccion,format = '%m-%d-%y %H:%M'),
    TRUE ~ as.POSIXct(fecha_transaccion,format = '%m/%d/%Y %I:%M:%S %p')
  ),
  mutate(
    #fecha_hora = dmy_hm(Date2),
    mes= month(Date2)
    )
  
  
  )

date_check2<- datos %>% 
  mutate(
    fecha_hora = ymd_hms(fecha_transaccion),
    mes = month(fecha_hora),
    dia_sem = wday(fecha_hora),
    hora = hour(fecha_hora)
  )

agregador <- date_check2%>%
  filter(giro_nombre == "AGREGADOR")%>%
  group_by(dia_sem,tipo_transaccion,hora,mcc_nombre,edad_cliente,giro_nombre)%>%
  summarise(gasto = sum(monto_transaccion))


retail <- date_check2%>%
  filter(giro_nombre == "RETAIL")%>%
  group_by(dia_sem,tipo_transaccion,hora,mcc_nombre,edad_cliente,giro_nombre)%>%
  summarise(gasto = sum(monto_transaccion))


unclass(date_check$Date2)

table(datos$tipo_transaccion)

summary(datos)

datos_c <- datos%>%
  filter(tipo_transaccion == "TDC")%>%
  group_by(giro_nombre)%>%
  summarize(total = sum(monto_transaccion))

datos_d <- datos%>%
  filter(tipo_transaccion == "TDD")%>%
  group_by(giro_nombre)%>%
  summarize(total = sum(monto_transaccion))

