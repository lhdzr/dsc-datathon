library(lubridate)


#### K MEANS CLUSTERING

primer_filtro_p$fecha_transaccion <- as.Date(primer_filtro_p$fecha_transaccion)
fecha_referencia <- as.Date(min(primer_filtro_p$fecha_transaccion))
primer_filtro_p$tiempo <- as.numeric(difftime(primer_filtro_p$fecha_transaccion,fecha_referencia,units = "days"))
datos_cluster <- primer_filtro_p[, c("monto_transaccion", "tiempo")]

# resultado_kmeans <- kmeans(datos_cluster, centers = 2,nstart = 1)
# resultado_kmeans$tot.withinss
# resultado_kmeans <- kmeans(datos_cluster, centers = 2,nstart = 20)
# resultado_kmeans$tot.withinss
# resultado_kmeans <- kmeans(datos_cluster, centers = 2,nstart = 50)
# resultado_kmeans$tot.withinss
# 
# resultado_kmeans <- kmeans(datos_cluster, centers = 3,nstart = 1)
# resultado_kmeans$tot.withinss
# resultado_kmeans <- kmeans(datos_cluster, centers = 3,nstart = 20)
# resultado_kmeans$tot.withinss
# resultado_kmeans <- kmeans(datos_cluster, centers = 3,nstart = 50)
# resultado_kmeans$tot.withinss
# 
# resultado_kmeans <- kmeans(datos_cluster, centers = 4,nstart = 1)
# resultado_kmeans$tot.withinss
# resultado_kmeans <- kmeans(datos_cluster, centers = 4,nstart = 20)
# resultado_kmeans$tot.withinss
resultado_kmeans <- kmeans(datos_cluster, centers = 4,nstart = 50)
resultado_kmeans$tot.withinss

# resultado_kmeans <- kmeans(datos_cluster, centers = 5,nstart = 1)
# resultado_kmeans$tot.withinss
# resultado_kmeans <- kmeans(datos_cluster, centers = 5,nstart = 20)
# resultado_kmeans$tot.withinss
# resultado_kmeans <- kmeans(datos_cluster, centers = 5,nstart = 50)
# resultado_kmeans$tot.withinss


primer_filtro_p <- primer_filtro_p %>%
  mutate(cluster=resultado_kmeans$cluster)


primer_filtro_p %>%
  group_by(cluster) %>%
  summarise(cantidad_transacciones = n(),
            tiempo_promedio = mean(tiempo),
            monto_promedio = mean(monto_transaccion))

# primer_filtro_p %>%
#   group_by(giro_nombre) %>%
#   summarise(
#     total_transacciones = n(),
#     monto_promedio = mean(monto_transaccion),
#     monto_total = sum(monto_transaccion)
#   ) %>%
#   arrange(cluster, desc(total_transacciones))


View(primer_filtro_p %>%
  group_by(giro_nombre, cluster) %>%
  count())


#write.csv(primer_filtro_p,"Datos/transacciones_clusters.csv")
set.seed(123)
leon<- primer_filtro_p %>% sample_n(300,replace = FALSE)
alexa <- primer_filtro_p %>% anti_join(leon) %>% sample_n(300,replace = FALSE)
write.csv(leon,"Datos/leon.csv")
write.csv(alexa,"Datos/alexa.csv")
