library(readxl)
library(DBI)
library(RSQLite)
library(ggplot2)
library(stringr)
library(dplyr)
library(NbClust)
library(cluster)
library(glmnet)
# Cargar los datos del archivo de Excel en R

municipios <- "C:/Users/deiby/OneDrive/Escritorio/municipios.xlsx"
datos_municipio <- read_excel(municipios)

# Filtrar filas con valores no faltantes en la columna de Superficie
datos_municipio_sin_na_superficie <- datos_municipio[!is.na(datos_municipio$Superficie), ]

# Filtrar filas con valores no faltantes en la columna de Población
datos_municipio_sin_na_poblacion <- datos_municipio[!is.na(datos_municipio$Poblacion), ]
conexion <- dbConnect(SQLite(), "municipios.db") # crea la conexión de SQL a R
# crea la tabla de datos de SQL y carga los datos en ella
dbWriteTable(conexion, "tabla_municipios", datos_municipio, overwrite = TRUE)
# Escribir y ejecutar la consulta SQL para realizar el conteo por código de Dep
query <- "SELECT Dep, COUNT(*) AS Conteo FROM tabla_municipios GROUP BY Dep;"
# Calcula el rango intercuartílico (IQR) de los datos de población
# Realizar cálculo del IQR y límites para población
Q1 <- quantile(datos_municipio_sin_na_poblacion$Poblacion, 0.25)
Q3 <- quantile(datos_municipio_sin_na_poblacion$Poblacion, 0.75)
IQR <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Filtrar los datos de población excluyendo los valores atípicos
datos_poblacion_sin_atipicos <- datos_municipio_sin_na_poblacion[datos_municipio_sin_na_poblacion$Poblacion >= limite_inferior & datos_municipio_sin_na_poblacion$Poblacion <= limite_superior, ]

# Realizar cálculo del IQR y límites para superficie
Q1_superficie <- quantile(datos_municipio_sin_na_superficie$Superficie, 0.25)
Q3_superficie <- quantile(datos_municipio_sin_na_superficie$Superficie, 0.75)
IQR_superficie <- Q3_superficie - Q1_superficie
limite_inferior_superficie <- Q1_superficie - 1.5 * IQR_superficie
limite_superior_superficie <- Q3_superficie + 1.5 * IQR_superficie

# Filtrar los datos de superficie excluyendo los valores atípicos
datos_superficie_sin_atipicos <- datos_municipio_sin_na_superficie[datos_municipio_sin_na_superficie$Superficie >= limite_inferior_superficie & datos_municipio_sin_na_superficie$Superficie <= limite_superior_superficie, ]

#Realizar prueba t para determinar si puedo eliminar los valores atipicos de poblacion
prueba_t_poblacion <- t.test(datos_municipio_sin_na_poblacion$Poblacion, datos_poblacion_sin_atipicos$Poblacion)
# Realizar prueba t para determinar si puedes eliminar los valores atípicos de la superficie
prueba_t_superficie <- t.test(datos_municipio_sin_na_superficie$Superficie, datos_superficie_sin_atipicos$Superficie)

#son significativamente diferentes, por lo que se procedió a eliminar los datos atipicos

# Calcular la media, mediana, mínimo, máximo y desviación estándar para población y superficie
media_superficie_sin_atipicos <- mean(datos_superficie_sin_atipicos$Superficie)
media_poblacion_sin_atipicos <- mean(datos_poblacion_sin_atipicos$Poblacion)
mediana_superficie_sin_atipicos <- median(datos_superficie_sin_atipicos$Superficie)
mediana_poblacion_sin_atipicos <- median(datos_poblacion_sin_atipicos$Poblacion)
min_superficie_sin_atipico <- min(datos_superficie_sin_atipicos$Superficie)
min_poblacion_sin_atipicos <- min(datos_poblacion_sin_atipicos$Poblacion)
max_superficie_sin_atipicos <- max(datos_superficie_sin_atipicos$Superficie)
max_poblacion_sin_atipicos <- max(datos_poblacion_sin_atipicos$Poblacion)
desviacion_superficie_sin_atipicos <- sd(datos_superficie_sin_atipicos$Superficie)
desviacion_poblacion_sin_atipicos <- sd(datos_poblacion_sin_atipicos$Poblacion)

#Visualizacion de datos totales

#Agrupar datos por Dep, eliminando la caracteres diferentes a letras

# Limpiar los nombres de los departamentos
datos_municipio_limpio <- datos_municipio %>%
  mutate(Departamento_Limpio = gsub("[^a-zA-ZáéíóúÁÉÍÓÚüÜñÑ\\s]", "", Departamento)) %>% #elimina los caracteres diferentes
  # Convertir a minúsculas
  mutate(Departamento_Limpio = tolower(Departamento_Limpio))

# Realizar el conteo por Departamento_Limpio
conteo_departamentos <- datos_municipio_limpio %>%
  group_by(Departamento_Limpio) %>%
  summarise(Count = n())

# Ordenar los datos por Count en orden ascendente
conteo_departamentos <- conteo_departamentos %>%
  arrange(Count)

#grafica de mayor a menor
ggplot(data = conteo_departamentos, aes(x = reorder(Departamento_Limpio, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Histograma de Departamentos",
       x = "Departamento",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar e ordena departamentos

# comparar la media y mediana con y sin valores atipicos para poblacion

# Crear un dataframe con las medias y medianas de población con y sin valores atípicos
poblacion_simplificado <- data.frame(
  Categoria = c("Población con atípicos", "Población sin atípicos"),
  Media = c(mean(datos_municipio$Poblacion), mean(datos_poblacion_sin_atipicos$Poblacion)),
  Mediana = c(median(datos_municipio$Poblacion), median(datos_poblacion_sin_atipicos$Poblacion))
)

# Transformar el dataframe de formato largo a formato ancho para la visualización
poblacion_simplificado_long <- tidyr::pivot_longer(poblacion_simplificado, cols = c(Media, Mediana), names_to = "Estadística", values_to = "Valor")

# Asignar etiquetas a las categorías
poblacion_simplificado_long$Categoria <- factor(poblacion_simplificado_long$Categoria, levels = c("Población con atípicos", "Población sin atípicos"))

# Crear el diagrama de barras
ggplot(poblacion_simplificado_long, aes(x = Categoria, y = Valor, fill = Estadística)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Media y Mediana de Población",
       x = "Categoría",
       y = "Valor",
       fill = "Estadística") +
  scale_fill_manual(values = c("Media" = "blue", "Mediana" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
####

datos_agrupados_poblacion <- datos_poblacion_sin_atipicos %>%
  group_by(Dep, Superficie, Poblacion)

datos_agrupados_superficie <- datos_superficie_sin_atipicos %>%
  group_by(Dep, Superficie, Poblacion)


# Combinar los datos de población y superficie
datos_combinados_sin_atipicos <- merge(datos_agrupados_poblacion, datos_agrupados_superficie, by = c("Dep", "Superficie", "Poblacion"))

# Graficar población y superficie con nombres de Dep y diferentes colores por Dep
ggplot(datos_combinados_sin_atipicos, aes(x = Superficie, y = Poblacion, color = as.factor(Dep))) +
  geom_point() +
  geom_text(aes(label = as.character(Dep)), vjust = 1.5, hjust = -0.5) +  # Agregar etiquetas de Dep
  labs(title = "Superficie vs. Población por Departamento",
       x = "Superficie",
       y = "Población") +
  scale_color_manual(values = rainbow(length(unique(datos_combinados_sin_atipicos$Dep))))  # Colores por Dep



####
# Verificar la longitud de los vectores
length_superficie <- length(datos_superficie_sin_atipicos$Superficie)
length_poblacion <- length(datos_poblacion_sin_atipicos$Poblacion)

# Si los vectores tienen longitudes diferentes, tomar la longitud mínima para ambos
if (length_superficie != length_poblacion) {
  min_length <- min(length_superficie, length_poblacion)
  datos_superficie_sin_atipicos <- datos_superficie_sin_atipicos[1:min_length, ]
  datos_poblacion_sin_atipicos <- datos_poblacion_sin_atipicos[1:min_length, ]
}

# Calcular la correlación entre superficie y población
correlation <- cor(datos_superficie_sin_atipicos$Superficie, datos_poblacion_sin_atipicos$Poblacion)

# Graficar diagrama de dispersión entre superficie y población sin valores atípicos
ggplot(data = NULL, aes(x = datos_superficie_sin_atipicos$Superficie, y = datos_poblacion_sin_atipicos$Poblacion)) +
  geom_point(color = "blue") +
  labs(title = paste("Diagrama de Dispersión: Superficie vs. Población sin Valores Atípicos\nCoeficiente de correlación: ", round(correlation, 2)),
       x = "Superficie (km²)",
       y = "Población") +
  theme_minimal()

#no se observa una relacion lineal entre la poblacion y superficie

poblacion_superficie_suma <- datos_municipio %>%
  mutate(Departamento_Limpio = gsub("[^a-zA-ZáéíóúÁÉÍÓÚüÜñÑ\\s]", "", Departamento)) %>%
  mutate(Departamento_Limpio = tolower(Departamento_Limpio)) %>%
  group_by(Departamento_Limpio) %>%
  summarise(Suma_Poblacion = sum(Poblacion), Suma_Superficie = sum(Superficie)) %>%
  mutate(Densidad_Poblacion = Suma_Poblacion / Suma_Superficie)


# Calcular la densidad de población (población por km²)
poblacion_superficie_suma <- datos_municipio %>%
  mutate(Departamento_Limpio = gsub("[^a-zA-ZáéíóúÁÉÍÓÚüÜñÑ\\s]", "", Departamento)) %>%
  mutate(Departamento_Limpio = tolower(Departamento_Limpio)) %>%
  group_by(Departamento_Limpio) %>%
  summarise(Suma_Poblacion = sum(Poblacion), Suma_Superficie = sum(Superficie)) %>%
  mutate(Densidad_Poblacion = Suma_Poblacion / Suma_Superficie)

# Reordenar el factor Departamento_Limpio por densidad de población de mayor a menor
poblacion_superficie_suma$Departamento_Limpio <- factor(poblacion_superficie_suma$Departamento_Limpio, 
                                                        levels = poblacion_superficie_suma$Departamento_Limpio[order(poblacion_superficie_suma$Densidad_Poblacion, decreasing = TRUE)])

# Graficar el histograma con nombres de departamentos en el eje x
ggplot(data = poblacion_superficie_suma, aes(x = Departamento_Limpio, y = Densidad_Poblacion)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Densidad de Población por Departamento",
       x = "Departamento",
       y = "Densidad de Población (hab/km²)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_to_title(x))


# Seleccionar solo las variables relevantes para el clustering
datos_clustering <- poblacion_superficie_suma %>%
  select(Suma_Poblacion, Suma_Superficie)

# Eliminar filas con valores faltantes
datos_clustering <- na.omit(datos_clustering)

# Estandarizar las variables
datos_clustering <- scale(datos_clustering)

# Calcular la suma de los cuadrados intra-cluster para diferentes números de clusters (método del codo)
wss <- sapply(1:10, function(k) {
  kmeans(datos_clustering, k, nstart=10)$tot.withinss
})
# Graficar el método del codo
plot(1:10, wss, type="b", pch = 19, frame = FALSE, 
     xlab="Número de clusters",
     ylab="Suma de cuadrados intra-cluster")
# Realizar clustering con K-means y 3 clusters
kmeans_model <- kmeans(datos_clustering, centers = 3, nstart = 10)

# Visualizar los clusters en un gráfico de dispersión
plot(datos_clustering, col = kmeans_model$cluster, main = "Clustering con K-means (3 clusters)")
points(kmeans_model$centers, col = 1:3, pch = 8, cex = 2)

# Agrupar los datos por regiones y departamentos, y contar el número de departamentos en cada región
datos_suma <- datos_municipio_limpio %>%
  group_by(Region, Departamento_Limpio) %>%
  summarise(Count = n_distinct(Departamento_Limpio)) %>%
  ungroup() %>%
  group_by(Region) %>%
  summarise(Total_Departamentos = sum(Count),
            Departamentos = paste(Departamento_Limpio, collapse = "\n"))  # Usamos "\n" para separar los nombres

# Graficar el histograma
ggplot(data = datos_suma, aes(x = Region, y = Total_Departamentos, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Departamentos por Región",
       x = "Región",
       y = "Total de Departamentos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x para mejor visualización
  geom_text(aes(label = Departamentos), vjust = 1.5)  # Ajustar más la posición vertical de los nombres

#Datos por región

datos_region <- datos_municipio%>%
  group_by(Region) %>%
  summarise(Suma_Poblacion = sum(Poblacion),
            Suma_Superficie = sum(Superficie))
datos_region <- datos_region %>%
  na.omit()

# Graficar el diagrama de barras por region
ggplot(datos_region, aes(x = Region)) +
  geom_bar(aes(y = Suma_Poblacion, fill = "Población"), stat = "identity", alpha = 0.7) +
  geom_bar(aes(y = Suma_Superficie, fill = "Superficie"), stat = "identity", alpha = 0.7) +
  labs(title = "Suma de Población y Superficie por Región",
       y = "Superficie o Población",
       fill = "Variable") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("Población" = "skyblue", "Superficie" = "lightgreen"),
                    name = "Variable", labels = c("Población", "Superficie"))

datos_densidad_region <- datos_region %>% #Toma los datos y transforma en densidad por region
  mutate(Densidad_Poblacion_por_km2 = Suma_Poblacion / Suma_Superficie)
#grafica densidad por regiones
ggplot(datos_densidad_region, aes(x = Region, y = Densidad_Poblacion_por_km2, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Densidad_Poblacion_por_km2, 2)), vjust = -0.5, size = 3) + # Agregar etiquetas de densidad
  labs(title = "Densidad de Personas por km² por Región",
       x = "Región",
       y = "Densidad de Personas por km²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Girar etiquetas del eje x
###


