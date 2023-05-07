# Modelos de prediccion 

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

dir <- "/Users/alexa/Carpetas locales/Datathon/dsc-datathon/datathon-personal"

  

library(lubridate)

datos <- read_csv(paste0(dir,"/simulacion_hormiga.csv"))%>%
  mutate(giro_nombre = as.factor(giro_nombre),
         mcc_nombre = as.factor(mcc_nombre),
         entry_mode = as.factor(entry_mode),
         sexo_cliente = as.factor(sexo_cliente),
         id_cliente = as.factor(id_cliente),
         hormiga = as.factor(hormiga)
         )%>%
  mutate(
    fecha_hora = dmy_hm(fecha_transaccion),
    mes = month(fecha_hora),
    dia_sem = wday(fecha_hora),
    dia_num = day(fecha_hora),
    hora = hour(fecha_hora),
    ano = year(fecha_hora),
    
  )


# Separa en conjunto de prueba y entrenamiento
data_split = datos %>% 
  initial_split(
    strata = hormiga
  )

entrenamiento = training(data_split)
prueba = testing(data_split)

nrow(datos)

nrow(entrenamiento)
nrow(prueba)

# Crea una receta
# tipo_gasto si es hormiga o no 

# Para random forest no es necesario normalizar pues no se basa en 
# distancias (en knn o k means si es necesario normalizar)
receta = recipe(formula = hormiga ~ giro_nombre+ mcc_nombre +
                  monto_transaccion + mes +dia_sem+
                hora, data = entrenamiento) 
#%>%
  # Remueve las variables sin varianza
  #step_zv(all_numeric(), -all_outcomes()) %>%
  # Normaliza las variables numericas
  #step_normalize(all_numeric(), -all_outcomes())

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

# Realizamos el ajuste final
ajuste_bosque_final = ajuste_bosque_final%>% 
  # Realiza el último ajuste
  last_fit(data_split)

# Matriz de confusión bosque 
matriz_bosque= ajuste_bosque_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = hormiga, estimate = .pred_class) 

matriz_bosque %>% 
  autoplot(type='heatmap')


# Ajuste completo
ajuste_bosque_final = ajuste_bosque_final%>% 
  # Realiza el último ajuste
  last_fit(data_split)

predicciones <- ajuste_bosque_final%>%
  collect_predictions()



# Procedimiento para 


# Los bosques aleatorios son el mejor modelo hasta ahora
# print(matriz_arbol)
# print(matriz_bosque)
# print(matriz_de_confusion_ml)
