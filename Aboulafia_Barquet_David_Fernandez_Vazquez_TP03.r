### Trabajo Práctico N°3 - Exploración de Datos
### Dataset: Predicción de calidad de vino
### Integrantes:
###   - Aboulafia Gerardo
###   - Barquet Amelie
###   - David Santiago
###   - Fernandez Ignacio
###   - Vazquez Agustina

# Carga del dataset y librerías
WineQT <- read.csv("/Users/gerardoaboulafia/Documents/WineQT.csv")
libs <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "pheatmap",
  "MASS",
  "lmtest",
  "generalhoslem",
  "caTools",
  "ROCR",
  "caret",
  "bestglm",
  "pROC",
  "glmnet"
)

installed_libs <- libs %in% rownames(installed.packages())

if(any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

invisible(
  lapply(libs, library, character.only = TRUE)
)

#Definimos paleta de colores
colores <- c(
  "#065893",
  "#430079",
  "#FFD700",
  "#FF6F61",
  "#D3D3D3",
  "#98FF98",
  "#ab87f4",
  "#F08080",
  "#32CD32",
  "#00CED1",
  "#FFA07A"
)


# -------------Limpieza de datos y transformación de variables------------------#
#Verificamos qué tipo de dato tiene cada columna del dataset
str(WineQT)

# Verificamos que no haya valores duplicados
sum(duplicated(WineQT))

#Verificamos que no haya valores nulos
sum(is.na(WineQT))

#Eliminamos la columna Id ya que no aporta información
WineQT <- subset(WineQT, select = -Id)

#Normalización de los datos
WineQT_scaled <- WineQT %>%
  mutate_if(is.numeric, scale)

# -------------Aplicación de modelo geométrico de clasificación (K-means)------------------#
# Iteramos sobre el modelo para obtener el mejor valor de clusters
n_clusters <- 10

#Instanciamos WSS para evaluar la cantidad óptima de clusters
wss <- numeric(n_clusters)

# Iteramos sobre el modelo para obtener el mejor valor de clusters
for (i in 1:n_clusters) {
  # Ajustamos el modelo: km_model
  km_model <- kmeans(WineQT_scaled, centers = i, nstart = 20)
  # Guardamos el resultado de WSS
  wss[i] <- km_model$tot.withinss
}

# Creamos un dataframe con los resultados y los visualizamos
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab("Número de clusters")

scree_plot

scree_plot +
  geom_hline(
    yintercept = wss,
    linetype = "dashed",
)

# -------------Aplicación de modelo probabilístico de clasificación (Regresión Logística)------------------#
# Fijamos los valores categóricos de la variable 'quality'
c <- ifelse(WineQT$quality >= 6, 1, 0)
WineQT$quality <- c
# Modelo de ML probabilístico usando logit:
# ('quality' es una variable binaria)
WineQT$quality <- as.factor(WineQT$quality)

# Seteamos una seed para poder reproducir los resultados
set.seed(123)

samp <- sample(1:nrow(WineQT), 0.7 * nrow(WineQT))
train <- WineQT[samp, ]
test <- WineQT[-samp, ]

# Instanciamos el modelo de regresion logística
model <- glm(
  quality ~ .,
  data = train,
  family = binomial(link = "logit")
)

# Instanciamos el mejor modelo
best_model <- bestglm(
  train,
  IC = "AIC",
  family = binomial,
  method = "exhaustive"
)

best_model$Subsets

best_train <- glm(quality ~ fixed.acidity + volatile.acidity + citric.acid + total.sulfur.dioxide + sulphates + alcohol, 
  data = train, 
  family = binomial)

best_pred <- predict(best_train, test, type = "response")

# Convertir las probabilidades en etiquetas binarias (0 o 1) usando un umbral (por ejemplo, 0.5)
test_pred_binary <- ifelse(best_pred >= 0.5, 1, 0)

# Crear una matriz de confusión
confusion_matrix <- confusionMatrix(data = factor(test_pred_binary), reference = factor(test$quality))

# Mostrar la matriz de confusión
print(confusion_matrix)

ROC_best_glm <- roc(test$quality, best_pred)
auc(ROC_best_glm)
plot(ROC_best_glm, col = "red")

# -------------Aplicación de modelo de lógico de clasificación (Decision Tree Classifier)------------------#
library(rpart)
library(tidymodels)

# Separamos los datos en datos de entrenamiento y de prueba
#(70% entrenamiento, 30% prueba)
samp <- sample(1:nrow(WineQT), 0.7 * nrow(WineQT))
train <- WineQT[samp, ]
test <- WineQT[-samp, ]

data_split <- initial_split(WineQT, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Instanciamos el modelo
tree_class <- rpart(
  quality ~ .,
  data = train_data,
  method = "class"
)

tree_class
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Graficamos el árbol
fancyRpartPlot(tree_class)

# Evaluamos el modelo
important_variables <- tree_class$variable.importance
important_variables <- as.data.frame(important_variables)
important_variables
printcp(tree_class)

# Usamos el modelo para predecir
test_data$quality_pred <- predict(tree_class, test_data, type = "class")
test_data$quality_pred_prob <- predict(tree_class, test_data, type = "prob")
test_data

# Generamos una matriz de confusión
confusionMatrix(
  test_data$quality,
  test_data$quality_pred,
  positive = "1"
)

# Hacer curva ROC
library(ROCR)
ROC_best_DT <- roc(test_data$quality, test_data$quality_pred_prob[, 2])

# Calcular AUC
auc(ROC_best_DT)


### -------------Selección del mejor modelo de clasificación------------------#
# Plot de las curvas ROC de los modelos
plot(ROC_best_glm, col = "red") # Regresión Logística
plot(ROC_best_DT, col = "blue", add = TRUE) # Decision Tree
