# Analisis de texto de los nombres de los comercios 
library(tidyverse)
library(dplyr)
library(tidytext)
library(tm)
#library(tidyverse)
library(stopwords)
library(topicmodels)

#dir <- "datathon-personal"

dir <- "/Users/alexa/Carpetas locales/Datathon/dsc-datathon/datathon-personal"

datos <- read_csv(paste0(dir,"/dataset_original.csv"))%>%
  janitor::clean_names()%>%
  mutate(fila = row_number())%>%
  mutate(
    fecha_hora = ymd_hms(fecha_transaccion),
    mes = month(fecha_hora),
    dia_sem = wday(fecha_hora),
    hora = hour(fecha_hora)
  )


#names(datos)
# Filtrar los rubro que son gastos escenciales 

table(datos$giro_nombre)

esenciales <- c('GOBIERNO','FARMACIAS','MEDICOS Y DENTISTAS',
                 'SUPERMERCADOS','GASOLINERAS','TELECOMUNICACIONES',
                 'COLEGIOS Y UNIVERSIDADES','HOSPITALES','EDUCACIN BASICA',
                 'ASEGURADORAS','PEAJE','GUARDERIAS','REFACCIONES Y FERRETERIA',
                'TRANSPORTE AEREO')

no_esenciales <- datos%>%
  filter(!giro_nombre %in% esenciales)%>%
  select(nombre_comercio)

datos_tot_no_ese <- datos%>%
  filter(!giro_nombre %in% esenciales)



analisis <- datos_tot_no_ese%>%
  mutate(id = row_number()
         )%>%
  
  #select(-row_num)%>%
  as_tibble()

# Extraer bigramas para encontrar nombres propios
# palabras = analisis%>%
#   unnest_tokens(
#     input = propuestas,
#     output = palabras,
#     token = 'words', 
#     #to_lower = FALSE
#   )

  # Filtramos con una expression regular para obtener solo nombres propios
  # filter(
  #   str_detect(
  #     string = palabras,
  #     pattern = '[A-ZAÉÍÓÚÑ]\\w+ [A-ZAÉÍÓÚÑ\\d][\\w]*'
  #   )
  # ) %>%
  # mutate(
  #   ners = str_replace_all(palabras,' ','_')
  # )%>%
  # select(
  #   palabras,ners)


# Crear diccionario
#bigramas_dict = setNames(palabras$ners,palabras$bigramas)


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
data_words = analisis%>%
  # mutate(
  #   # Sustituir los bigramas en el texto completo 
  #   propuestas = str_replace_all(propuestas,bigramas_dict)
  # )%>%
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



numericos <- data_words%>%
  group_by(palabra)%>%
  summarise(total = n())%>%
  mutate(palabra = as.numeric(palabra))%>%
  na.omit()%>%
  mutate(palabra = as.character(palabra))%>%
  pull(palabra)


total_words <- data_words%>%
  group_by(palabra)%>%
  summarise(total = n())%>%
  filter(!palabra %in% numericos,
         total > 50)


sin_num <- total_words%>%
  mutate(palabra = removeNumbers(palabra))%>%
  filter(!palabra == "")%>%
  pull(palabra)
  
pat <- paste0(sin_num, collapse = '|')


library("quanteda")

corpus <- corpus(datos_tot_no_ese$nombre_comercio)
summary(corpus)

# data_corpus_inauguralsents <- 
#    corpus_reshape(corpus, to = "sentences")

data_corpus_inauguralsents <-
  corpus_reshape(corpus, to = "sentences")

data_corpus_inauguralsents


containstarget <- 
  stringr::str_detect((corpus), pat)

summary(containstarget)

data_corpus_inauguralsentssub <- corpus_subset(data_corpus_inauguralsents, containstarget)

vector <- as.vector(data_corpus_inauguralsentssub)


# Esta base es con la que hacemos otros filtros posteriores
primer_filtro_p <- datos_tot_no_ese%>%
  filter(nombre_comercio %in% vector)%>%
  mutate()
  
nombre_comercio <- c('oxxo','eats')
giro_com <- c('MISCELANEAS','COMIDA RAPIDA')

library(sjmisc)



# Primer NO gastos hormiga 
datos_1 <- datos%>%
  mutate(hormiga = ifelse(giro_nombre %in% esenciales,0,
                          ifelse(mcc_nombre == "CASINOS CASAS DE JUEGO (APUESTAS INCLUYE BILLETES)",1,
                                 
                                        NA)),
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
id_persona <- datos_1%>%
  pull(id_cliente)%>%
  unique()

meses <- datos_1%>%
  pull(mes)%>%
  unique()

#base2 <- data.frame()

mat <- matrix(0, ncol = 19)
data2 <- as.data.frame(mat)
data3 <- data2                                     # Duplicate data frame
colnames(data3) <- names_col    # Create data frame
data3 


for (persona in id_persona){
  for (mesi in meses){
    
    datos_p <- datos_1%>%
      filter(id_cliente == persona)%>%
      filter(mes == mesi)
      
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
        mutate(hormiga = ifelse(tipo == "OXXO" && tot_oxxo > 4,1,
                                ifelse(tipo == "UBER" && tot_uber > 4,1,
                                       ifelse(tipo == "MISCELANEAS" && tot_mis > 4,1,
                                              ifelse(tipo == "COMIDA RAPIDA" && tot_com > 4,1,
                                                     0)
                                              
                                              )))
               )
      
      
      
    
  }
  
  data3 <- smartbind(data3,as.data.frame(base1))
  
}
























# Otras revisiones 
trans_cliente <- primer_filtro_p%>%
  group_by(id_cliente)%>%
  summarise(total_trans = n())


trans_cliente_AGREGADOR <- primer_filtro_p%>%
  group_by(id_cliente)%>%
  filter(giro_nombre == "AGREGADOR")%>%
  summarise(total_trans = n())








