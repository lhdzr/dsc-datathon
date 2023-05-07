# Analisis de texto de los nombres de los comercios 
library(tidyverse)
library(dplyr)
library(tidytext)
library(tm)
library(tidyverse)
library(stopwords)
library(topicmodels)

dir <- "datathon-personal"

datos <- read_csv(paste0(dir,"/dataset_original.csv"))%>%
  janitor::clean_names()

#names(datos)
# Filtrar los rubro que son gastos escenciales 

#table(datos$giro_nombre)

esenciales <- c('GOBIERNO','FARMACIAS','MEDICOS Y DENTISTAS',
                 'SUPERMERCADOS','GASOLINERAS','TELECOMUNICACIONES',
                 'COLEGIOS Y UNIVERSIDADES','HOSPITALES','EDUCACIN BASICA',
                 'ASEGURADORAS','PEAJE','GUARDERIAS','REFACCIONES Y FERRETERIA')

no_esenciales <- datos%>%
  filter(!giro_nombre %in% esenciales)%>%
  select(nombre_comercio)

datos_tot_no_ese <- datos%>%
  filter(!giro_nombre %in% esenciales)



analisis = datos_tot_no_ese%>%
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
#   corpus_reshape(corpus, to = "sentences")
# data_corpus_inauguralsents


containstarget <- 
  stringr::str_detect((corpus), pat)

summary(containstarget)

data_corpus_inauguralsentssub <- 
  corpus_subset(data_corpus_inauguralsents, containstarget)


vector <- as.vector(data_corpus_inauguralsentssub)

primer_filtro_p <- datos_tot_no_ese%>%
  filter(nombre_comercio %in% vector)
  
trans_cliente <- primer_filtro_p%>%
  group_by(id_cliente)%>%
  summarise(total_trans = n())


trans_cliente_AGREGADOR <- primer_filtro_p%>%
  group_by(id_cliente)%>%
  filter(giro_nombre == "AGREGADOR")%>%
  summarise(total_trans = n())








