# Leer datos
library(gtools)
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidytext)
library(tm)
library(stopwords)
library(topicmodels)



dir <- "/Users/alexa/Carpetas locales/Datathon/dsc-datathon/Datos/"



leon <- read_csv(paste0(dir,"leon.csv"))%>%
  filter(!is.na(hormiga))

alexa <- read_csv(paste0(dir,"alexa.csv"))%>%
  filter(!is.na(hormiga))

# Las transacciones que se evaluaron manualmente
todos_muestra <- smartbind(as.data.frame(leon),as.data.frame(alexa))%>%
  filter(!hormiga == "9")%>%
  mutate(giro_nombre = as.factor(giro_nombre),
         mcc_nombre = as.factor(mcc_nombre),
         entry_mode = as.factor(entry_mode),
         sexo_cliente = as.factor(sexo_cliente),
         nombre_comercio = as.factor(nombre_comercio),
         hormiga = as.factor(hormiga)
  )%>%
  as.data.frame()


# Los clientes que clasificamos manualmente 
muestra_clas <- todos_muestra%>%
  pull(id_cliente)%>%
  unique()


dir_p <- "/Users/alexa/Carpetas locales/Datathon/dsc-datathon/datathon-personal"

# La base sin lo que clasificamos manualmene 
v_datos_no_clas <- read_csv(paste0(dir_p,"/dataset_original.csv"))%>%
   filter(!id_cliente %in% muestra_clas)%>%
   mutate(giro_nombre = as.factor(giro_nombre),
          mcc_nombre = as.factor(mcc_nombre),
          entry_mode = as.factor(entry_mode),
          sexo_cliente = as.factor(sexo_cliente),
          #id_cliente = as.factor(id_cliente),
          #hormiga = as.factor(hormiga)
   )%>%
  pull(id_cliente)%>%
  unique()

# Elegir 25 id al azar
muestra <- sample(v_datos_no_clas,size = 25, replace = FALSE)


# Filtrar los 25 al azar 
base_comp_test <- read_csv(paste0(dir_p,"/dataset_original.csv"))%>%
  filter(id_cliente %in% muestra)%>%
  mutate(giro_nombre = as.factor(giro_nombre),
         mcc_nombre = as.factor(mcc_nombre),
         entry_mode = as.factor(entry_mode),
         sexo_cliente = as.factor(sexo_cliente),
         hormiga = "NA",
         hormiga = as.character(hormiga)
         )%>%
  filter(id_cliente %in% muestra)%>%
  as.data.frame()%>%
  # Unir la base con los 25 clasificados
  smartbind(todos_muestra)




# RANDOM FOREST ------------------------------------------------
# Bosques aleatorios -----------------------------------------------------------
# Instalación y carga de paqueterías
# Cargar ranger                                                       
if(require(ranger) == FALSE){                                                
  install.packages('ranger')                                                 
}


# Instalación y carga de paqueterías
# Instalar - Cargar tidymodels                                                       
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}

# Instalar - Cargar patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}

# Instalación y carga de paqueterías
# Instalar - Cargar vip                                                       
if(require(vip) == FALSE){                                                
  install.packages('vip')                                                 
  library(vip)                                                            
}else{                                                                          
  library(vip)                                                            
}

# Instalación y carga de paqueterías
# Instalar glmnet                                                       
if(require(glmnet) == FALSE){                                                
  install.packages('glmnet')                                                 
}

# Instalación y carga de paqueterías
# Instalar GGally                                                       
if(require(GGally) == FALSE){                                                
  install.packages('GGally')                                                 
}

#dir_p <- "/Users/alexa/Carpetas locales/Datathon/dsc-datathon/datathon-personal"



library(lubridate)


# Separa en conjunto de prueba y entrenamiento
data_split = todos_muestra %>% 
  initial_split(
    strata = hormiga
  )

entrenamiento = training(data_split)
prueba = testing(data_split)


nrow(entrenamiento)
nrow(prueba)

# Crea una receta

# Para random forest no es necesario normalizar pues no se basa en 
# distancias (en knn o k means si es necesario normalizar)
receta = recipe(formula = hormiga ~ giro_nombre+ mcc_nombre +
                  monto_transaccion + tipo_transaccion, data = entrenamiento) 

# Construye las muestras de validación cruzada con los datos de entrenamiento
validacion_cruzada = vfold_cv(entrenamiento, v = 5)



# Fija una semilla
set.seed(123)

# Creamos el cascarón del modelo de bosques aleatorios
ajuste_bosque = rand_forest() %>% 
  # Fija el motor
  # The basic idea of the permutation variable importance approach 
  # is to consider a variable important if it has a positive effect on the prediction performance.
  # https://github.com/imbs-hl/ranger/issues/237
  set_engine("ranger", importance = "permutation") %>% 
  # Fijo el modo, en este caso queremos hacer clasificacion
  set_mode("classification") %>% 
  # Fija los argumentos
  set_args( 
    # Numero de predictores por muestra
    mtry = tune(),
    # Numero de árboles
    trees = 100,
    # Minimo de observaciones
    min_n = tune()
  )

# Define el flujo de trabajo
flujo_bosque = workflow() %>% 
  # Agrega la receta
  add_recipe(receta) %>% 
  # Agrega el modelo 
  add_model(ajuste_bosque) 


# Genera una muestra aleatoria de parametros
bosque_grid = grid_regular(
  mtry(range = c(2, ncol(entrenamiento) - 1)),
  min_n(range = c(1, 6)), levels = 3)

# Agrega elementos de control
ctrl = control_grid(verbose = TRUE)

# Entrenamiento con validacion cruzada
bosque_tunning = flujo_bosque %>% 
  tune_grid(
    # Usa las muestras de validación cruzada
    resamples = validacion_cruzada,
    # Recorre la maya de hyperparametros
    grid = bosque_grid,
    # Agrega los elementos de control
    control = ctrl,
    # Define las métricas
    metrics = metric_set(accuracy, sens, spec, roc_auc, kap))

# Imprime las métricas del modelo bosque     
metricas_bosque = bosque_tunning %>% 
  collect_metrics() 

print(metricas_bosque)

# Mejor modelo bosque
mejor_bosque = bosque_tunning %>% 
  select_best('roc_auc')

print(mejor_bosque)

# Selecciona el mejor modelo
ajuste_bosque_final = flujo_bosque %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(mejor_bosque) 

# Revisa como se comporta el conjunto de entrenamiento
ajuste_bosque_final %>% 
  # Ajusta sobre el conjunto
  fit(entrenamiento) %>% 
  # Extrae el ajuste
  extract_fit_parsnip()  %>% 
  # Revisa la importancia de variables
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    # Ordena por importancia
    Variable = fct_reorder(Variable, Importance),
  ) %>%
  # Grafica de importancia
  ggplot(aes(x = Importance, y = Variable)) +
  # Agrega columnas
  geom_col() +
  # Modifica el eje x
  scale_x_continuous(
    # Ajusta los márgenes
    expand = c(0, 0), 
  ) +
  # Modifica las etiquetas
  labs(x = 'Importancia') +
  # Agrega títulos
  ggtitle('Importancia de variables en el modelo bosque aleatorio') +
  # Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Arial'),
    # Cambia los títulos
    axis.title.y = element_blank(),
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Las variables más importantes 
# mcc nombre , monto transaccion, giro nombre, tipo transaccion


# Realizamos el ajuste final
ajuste_bosque_final_test = ajuste_bosque_final%>% 
  # Realiza el último ajuste con los datos de prueba que no habiamos usado
  last_fit(data_split)

# Matriz de confusión bosque 
matriz_bosque= ajuste_bosque_final_test %>% 
  collect_predictions() %>% 
  conf_mat(truth = hormiga, estimate = .pred_class) 

matriz_bosque %>% 
  autoplot(type='heatmap')



# Estas son con las predicciones para los de prueba
predicciones <- ajuste_bosque_final_test%>%
  collect_predictions()%>%
  rename("n_row" = ".row",
         "hormiga_prediccion" = ".pred_class")%>%
  select(n_row,hormiga_prediccion)



final <- todos_muestra%>%
  mutate(n_row = row_number())%>%
  inner_join(predicciones, by= "n_row")
  
  
  
#write.csv(final, "final.csv", row.names = FALSE)
  
