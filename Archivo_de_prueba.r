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
library(dplyr)
library(tidyr)

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

#Acá va la parte de Amelie

# Convertir la columna "Grasas_sat" a números decimales
data_pacientes$Grasas_sat <- as.numeric(data_pacientes$Grasas_sat)

# Medidas de posición Grasas_sat
media_grasa <- mean(data_pacientes$Grasas_sat, na.rm = TRUE)  # na.rm = TRUE para omitir NA
mediana_grasa <- median(data_pacientes$Grasas_sat, na.rm = TRUE)
barplot(c(media, mediana), names.arg = c("Media Grasa", "Mediana Grasa"))

# Calcular la desviación estándar Grasas_sat
desviacion_estandar_grasa <- sd(data_pacientes$Grasas_sat, na.rm = TRUE)

# Crear un histograma Grasas_sat
hist(data_pacientes$Grasas_sat, main = "Histograma de Datos", xlab = "Valores", ylab = "Frecuencia")

# Crear un boxplot Grasas_sat
boxplot(data_pacientes$Grasas_sat, main = "Gráfico de Caja", ylab = "Valores")

# Convertir la columna "Alcohol" a números decimales
data_pacientes$Alcohol <- as.numeric(data_pacientes$Alcohol)

# Medidas de posición alcohol
media_alcohol <- mean(data_pacientes$Alcohol, na.rm = TRUE)  # na.rm = TRUE para omitir NA
mediana_alcohol <- median(data_pacientes$Alcohol, na.rm = TRUE)
barplot(c(media, mediana), names.arg = c("Media alcohol", "Mediana alcohol"))

# Calcular la desviación estándar alcohol
desviacion_estandar_alcohol <- sd(data_pacientes$Alcohol, na.rm = TRUE)

# Crear un histograma alcohol
hist(data_pacientes$Alcohol, main = "Histograma de Datos Alcohol", xlab = "Valores", ylab = "Frecuencia")

# Crear un boxplot alcohol
boxplot(data_pacientes$Alcohol, main = "Gráfico de Caja Alcohol", ylab = "Valores")

# Convertir la columna "calorias" a números decimales
data_pacientes$Calorías <- as.numeric(data_pacientes$Calorías)

# Medidas de posición calorias
media_calorías <- mean(data_pacientes$Calorías, na.rm = TRUE)  # na.rm = TRUE para omitir NA
mediana_calorías <- median(data_pacientes$Calorías, na.rm = TRUE)
barplot(c(media, mediana), names.arg = c("Media calorias", "Mediana calorias"))

# Calcular la desviación estándar calorias
desviacion_estandar_calorias <- sd(data_pacientes$Calorías, na.rm = TRUE)

# Crear un histograma calorias
hist(data_pacientes$Calorías, main = "Histograma de Datos Calorias", xlab = "Valores", ylab = "Frecuencia")

# Crear un boxplot calorias
boxplot(data_pacientes$Calorías, main = "Gráfico de Caja Calorias", ylab = "Valores")

#Desviacion estandar completo
barplot(c(desviacion_estandar_grasa,desviacion_estandar_alcohol,desviacion_estandar_calorias), names.arg = c("Desviacion estandar Grasas", "Desviacion estandar Alcohol", "Desviacion estandar Calorías"))

View(data_pacientes)



# -------- Analizamos si el sexo influye en el comportamiento de las variables --------

#Acá va la parte de Agus




# -------- Analizamos el comportamiento de los datos para la variable Alcohol de acuerdo con la cantidad de Calorías consumidas --------

# Recategorizamos la variable Calorías en 3 categorías (CATE 1, CATE 2, CATE 3) de acuerdo con los valores de la variable
data$Categoria_Calorías <- cut(data$Calorías, breaks=c(0, 1100, 1700, Inf), labels=c("CATE 1", "CATE 2", "CATE 3"))

# Visualizamos la distribución de la variable Calorías por los grupos creados
colores <- c("CATE 1" = "#065893", "CATE 2" = "#179CBC", "CATE 3" = "#47C0A7")


ggplot(data, aes(x = Categoria_Calorías, y = Alcohol)) +
  geom_bar(stat = "summary", fun = "mean", fill = colores) +
  labs(
    title = "Consumo de Alcohol en función de la Categoría de Calorías",
    x = "Categoría de Calorías",
    y = "Consumo de Alcohol (media)"
  ) + 
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
      axis.title.x = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5),
      axis.text.x = element_text(size = 12, face = "bold", angle = 45),
    )
