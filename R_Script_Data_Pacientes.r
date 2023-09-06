### Grupo X
### Integrantes:
###   - Aboulafia Gerardo
###   - Barquet Amelie
###   - David Santiago
###   - Fernandez Ignacio
###   - Vasquez Agustina

# Carga del dataset y librerías
data <- read.csv("/Users/gerardoaboulafia/Documents/Exploración_de_datos/Datasets/Datos trabajo 1.csv", sep=";", dec = ",")
library(ggplot2)

# -------------Limpieza de datos y transformación de variables------------------

#Eliminamos el punto que separa los miles
data$Calorías <- gsub("\\.", "", data$Calorías)

#Verificamos qué tipo de dato tiene cada columna del dataset
str(data)

# Reemplazamos el valor 999,99 por NA (como indica la consigna)
data <- replace(data, data == 999.99, NA)

#Transformamos la columna Calorías a numérica
data$Calorías <- as.numeric(data$Calorías)

# -------- Análisis exploratorio de datos y visualización --------
colores <- c("CATE 1" = "#065893", "CATE 2" = "#179CBC", "CATE 3" = "#47C0A7")

# Medidas de posición Grasas_sat
media_grasa <- mean(data$Grasas_sat, na.rm = TRUE)  # na.rm = TRUE para omitir NA
mediana_grasa <- median(data$Grasas_sat, na.rm = TRUE)
valores_grasa <- c(media_grasa, mediana_grasa)
barplot(valores_grasa, names.arg = c("Media Grasa", "Mediana Grasa"), 
        main = "Medidas de posición Grasas_sat", col = "#065893",
        ylim = c(0, max(valores_grasa) * 1.3))

# Calcular la desviación estándar Grasas_sat
desviacion_estandar_grasa <- sd(data$Grasas_sat, na.rm = TRUE)

# Crear un histograma Grasas_sat
hist(data$Grasas_sat, main = "Histograma de Datos de Grasas Saturadas", 
     xlab = "Valores", ylab = "Frecuencia", col = "#065893",
     ylim = c(0, max(hist(data$Grasas_sat)$counts) * 1.2))

# Crear un boxplot Grasas_sat
boxplot(data$Grasas_sat, main = "Boxplot de Grasas Saturadas", ylab = "Valores", col="#065893")

# Medidas de posición alcohol
media_alcohol <- mean(data$Alcohol, na.rm = TRUE)  # na.rm = TRUE para omitir NA
mediana_alcohol <- median(data$Alcohol, na.rm = TRUE)
valores_alcohol <- c(media_alcohol, mediana_alcohol)

barplot(c(media_alcohol, mediana_alcohol), names.arg = c("Media alcohol", "Mediana alcohol"), 
        main = "Medidas de posición Alcohol", col="#065893",
        ylim = c(0, max(valores_alcohol) * 1.3))

# Calcular la desviación estándar alcohol
desviacion_estandar_alcohol <- sd(data$Alcohol, na.rm = TRUE)

# Crear un histograma alcohol
hist(data$Alcohol, main = "Histograma de Datos Alcohol", xlab = "Valores", 
     ylab = "Frecuencia", col="#065893",
     ylim = c(0, max(hist(data$Alcohol)$counts) * 1.2))

# Crear un boxplot alcohol
boxplot(data$Alcohol, main = "Boxplot de Alcohol", ylab = "Valores", col="#065893")

# Medidas de posición calorias
media_calorías <- mean(data$Calorías, na.rm = TRUE)  # na.rm = TRUE para omitir NA
mediana_calorías <- median(data$Calorías, na.rm = TRUE)
valores_calorias <- c(media_calorías, mediana_calorías)

barplot(c(media_calorías, mediana_calorías), names.arg = c("Media calorias", "Mediana calorias"), 
          main = "Medidas de posición Calorias", col="#065893",
          ylim = c(0, max(valores_calorias) * 1.3))

# Calcular la desviación estándar calorias
desviacion_estandar_calorias <- sd(data$Calorías, na.rm = TRUE)

# Crear un histograma calorias
hist(data$Calorías, main = "Histograma de Datos Calorias", xlab = "Valores", 
     ylab = "Frecuencia", col="#065893",
     ylim = c(0, max(hist(data$Calorías)$counts) * 1.2))

# Crear un boxplot calorias
boxplot(data$Calorías, main = "Boxplot de Calorias", ylab = "Valores", col="#065893")

#Desviacion estandar completo
barplot(c(desviacion_estandar_grasa,desviacion_estandar_alcohol,desviacion_estandar_calorias), 
        names.arg = c("Desviacion estandar Grasas", "Desviacion estandar Alcohol", "Desviacion estandar Calorías"),  
        main = "Desviacion estandar", col=colores)

# -------- Analizamos si el sexo del paciente influye en el comportamiento de las variables --------

# Resumen estadístico para Grasas sat por Sexo
summary(data$Grasas_sat[data$Sexo == "M"])
summary(data$Grasas_sat[data$Sexo == "F"])

# Resumen estadístico para Alcohol por Sexo
summary(data$Alcohol[data$Sexo == "M"])
summary(data$Alcohol[data$Sexo == "F"])

# Resumen estadístico para Calorías por Sexo
summary(data$Calorías[data$Sexo == "M"])
summary(data$Calorías[data$Sexo == "F"])

# Boxplot para Grasas sat por Sexo
boxplot(Grasas_sat ~ Sexo, data = data, 
        xlab = "Sexo", ylab = "Grasas sat", 
        main = "Distribución de Grasas sat por Sexo",
        col=colores)

# Boxplot para Alcohol por Sexo
boxplot(Alcohol ~ Sexo, data = data, 
        xlab = "Sexo", ylab = "Alcohol", 
        main = "Distribución de Alcohol por Sexo",
        col=colores)

# Boxplot para Calorías por Sexo
boxplot(Calorías ~ Sexo, data = data, 
        xlab = "Sexo", ylab = "Calorías", 
        main = "Distribución de Calorías por Sexo",
        col=colores)

# -------- Analizamos el comportamiento de los datos para la variable Alcohol de acuerdo con la cantidad de Calorías consumidas --------

# Recategorizamos la variable Calorías en 3 categorías (CATE 1, CATE 2, CATE 3) de acuerdo con los valores de la variable
data$Categoria_Calorías <- cut(data$Calorías, breaks=c(0, 1100, 1700, Inf), labels=c("CATE 1", "CATE 2", "CATE 3"))

# Visualizamos la distribución de la variable Calorías por los grupos creados

ggplot(data, aes(x = Categoria_Calorías, y = Alcohol)) +
  geom_boxplot(fill = colores) +
  labs(
    title = "Consumo de Alcohol en función de la Categoría de Calorías",
    x = "Categoría de Calorías",
    y = "Consumo de Alcohol"
  ) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12, face = "bold", angle = 45),
  )
