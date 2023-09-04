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