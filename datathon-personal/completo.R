# Limpieza y acomodo de los datos

library(tidyverse)
library(dplyr)
library(tidytext)
library(tm)
library(stopwords)
library(topicmodels)
library(gtools)


dir <- "/Users/alexa/Carpetas locales/Datathon/dsc-datathon/datathon-personal"

datos <- read_csv(paste0(dir,"/dataset_original.csv"))%>%
  janitor::clean_names()%>%
  # Numero de fila para identificar cada transaccion
  mutate(fila = row_number())%>%
  # Agregar fechas 
  mutate(
    fecha_hora = ymd_hms(fecha_transaccion),
    mes = month(fecha_hora),
    dia_sem = wday(fecha_hora),
    hora = hour(fecha_hora)
  )


# Filtrar los rubro que son gastos escenciales o NO hormiga
esenciales <- c('GOBIERNO','FARMACIAS','MEDICOS Y DENTISTAS',
                'SUPERMERCADOS','GASOLINERAS','TELECOMUNICACIONES',
                'COLEGIOS Y UNIVERSIDADES','HOSPITALES','EDUCACIN BASICA',
                'ASEGURADORAS','PEAJE','GUARDERIAS','REFACCIONES Y FERRETERIA',
                'TRANSPORTE AEREO')

no_esenciales <- datos%>%
  filter(!giro_nombre %in% esenciales)%>%
  # Nos quedamos solo con el nombre del comercio
  select(nombre_comercio)

# Tabla completa con los posibles hormiga 
datos_tot_no_ese <- datos%>%
  filter(!giro_nombre %in% esenciales)%>%
  mutate(id = row_number())%>%
  as.tibble()


# Palabras de paro
stop_words = tibble(palabra = c(stopwords('es'),'c','remite','presentado',
                                'manera','considera','inició','12r','así',
                                'cuenta','n','mismo','méxico','cada',"toda",'CIUDAD',
                                'MEX001','VERACRUZ','SANPEDRO','GARN','MORELIA','MICH',
                                'GARZA','GARCIA','NL','877-SEPHORA','SOLIDARIDAD','QQR',
                                'SAN','FRANCISCO','QQU','BCS','BS','GOMEZ','PALACIO010',
                                'MATAMOROS','ACACAPULCO','MICMI','PARROQVERACRUZ',
                                'VER','030','SN','PEDRO','GARZ001','CELAYA','GTO','GU',
                                'MONTERREY','NL','XALAPA','ciudad','mex001','nl','df',
                                'mx','monterrey','bcn','bcn','000','mex09','sn','snp',
                                'd','em','jal','com','001','019','pedro','guadalajara',
                                'zapopan','650','2530000','16','00','ca','1','mexdf','pending',
                                'mexico','ca','7','mexcmx','tijuana','stripe',
                                'sc','str','morelia','mich','mexcmx','san','apodaca',
                                'yuc','mty','amsterdam','nle','guadalupe','nicolas','ztl','s',
                                'santiago','ver','juarez','merida','j','mex','pue','coah',
                                'london','ja','benito','santa','miguel','conekta','ii',
                                'chihuahua','jja','j','mex '))



# Bolsa de palabras
data_words = datos_tot_no_ese%>%
  # Tokenizar
  unnest_tokens(
    output = 'palabra',
    token = 'words',
    input = nombre_comercio
  )%>%
  anti_join(
    stop_words
  )%>%
  count(id,palabra)

# Identificar los numericos
numericos <- data_words%>%
  group_by(palabra)%>%
  summarise(total = n())%>%
  mutate(palabra = as.numeric(palabra))%>%
  na.omit()%>%
  mutate(palabra = as.character(palabra))%>%
  pull(palabra)


# Eliminar los numericos y solo quedarse con las palabras que se repiten
# mas de 50 veces
total_words <- data_words%>%
  group_by(palabra)%>%
  summarise(total = n())%>%
  filter(!palabra %in% numericos,
         total > 50)

# Quitar los espacios en planco y hacer un vector con las palabras de interes
sin_num <- total_words%>%
  mutate(palabra = removeNumbers(palabra))%>%
  filter(!palabra == "")%>%
  pull(palabra)

# Unir en el formato correcto
pat <- paste0(sin_num, collapse = '|')


library("quanteda")

# Hacer corpus con los nombres de los comercios 
corpus <- corpus(datos_tot_no_ese$nombre_comercio)
summary(corpus)


data_corpus_inauguralsents <-
  corpus_reshape(corpus, to = "sentences")

data_corpus_inauguralsents

# Indentificar los que contienen las palabras que nos interesan
containstarget <- 
  stringr::str_detect((corpus), pat)

summary(containstarget)

data_corpus_inauguralsentssub <- corpus_subset(data_corpus_inauguralsents, containstarget)

vector <- as.vector(data_corpus_inauguralsentssub)


# Filtrar la base de datos con todas las columns y 
# las transacciones de los negocios con los nombres que nos 
# interesan 
primer_filtro_p <- datos_tot_no_ese%>%
  filter(nombre_comercio %in% vector)



# Vamos a comenzar a definir gastos hormiga por reglas de decision 
#nombre_comercio <- c('oxxo','eats')
#giro_com <- c('MISCELANEAS','COMIDA RAPIDA')

library(sjmisc)

# Primer NO gastos hormiga 
datos_1 <- datos%>%
  # Marcar como no hormiga a los que son escenciales segun su giro
  mutate(hormiga = ifelse(giro_nombre %in% esenciales,0,
                          # Marcar como hormiga a los casinos 
                          ifelse(mcc_nombre == "CASINOS CASAS DE JUEGO (APUESTAS INCLUYE BILLETES)",1,
                                 
                                 NA)),
         # Definir columna tipo de acuerdo a comercio o giro
         tipo = ifelse(str_detect(nombre_comercio,"OXXO"),"OXXO",
                       ifelse(str_detect(nombre_comercio,"EATS"),"UBER",
                              ifelse(giro_nombre ==  "MISCELANEAS","MISCELANEAS",
                                     ifelse(giro_nombre == "COMIDA RAPIDA","COMIDA RAPIDA",
                                            NA))))
  )%>%
  filter(!is.na(tipo))%>%
  mutate(tipo = as.factor(tipo))




# Segundo paso consiste en clasificar a ciertos tipo de gastos 
# mensualmente 

# La regla consiste en que se considera hormiga a los gastos 
# Identificar id del cliente
id_persona <- datos_1%>%
  pull(id_cliente)%>%
  unique()

# Identificar clientes
# meses <- datos_1%>%
#   pull(mes)%>%
#   unique()

# Crear base que se va a ir llenando 
mat <- matrix(0, ncol = 19)
data2 <- as.data.frame(mat)
data3 <- data2  
names_col <- names(datos_1)
colnames(data3) <- names_col    # Create data frame
data3 


for (persona in id_persona){
  #for (mesi in meses){
    
    datos_p <- datos_1%>%
      filter(id_cliente == persona)
    #%>%
      #group_by(mes)
      
    
    # Total oxxo
    tot_oxxo <- datos_p%>%
      group_by(tipo)%>%
      filter(tipo == "OXXO")%>%
      nrow()
    
    # Total uber
    tot_uber <- datos_p%>%
      group_by(tipo)%>%
      filter(tipo == "UBER")%>%
      nrow()
    
    # Total miscelaneas
    tot_mis <- datos_p%>%
      group_by(tipo)%>%
      filter(tipo == "MISCELANEAS")%>%
      nrow()
    
    # Total comida rapida
    tot_com <- datos_p%>%
      group_by(tipo)%>%
      filter(tipo == "COMIDA RAPIDA")%>%
      nrow()
    
    base1 <- datos_p%>%
      mutate(hormiga = ifelse(tipo == "OXXO" && tot_oxxo > 10,1,
                              ifelse(tipo == "UBER" && tot_uber > 10,1,
                                     ifelse(tipo == "MISCELANEAS" && tot_mis > 10,1,
                                            ifelse(tipo == "COMIDA RAPIDA" && tot_com > 10,1,
                                                   0
                                                   ))))
      )%>%
      as.data.frame()
    
    
    
    
  #}
  
  data3 <- smartbind(data3,base1)
  
}



