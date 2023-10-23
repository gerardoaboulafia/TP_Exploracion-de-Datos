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
  "dplyr"
  ,"tidyr"
  ,"ggplot2"
  ,"MASS"
  ,"generalhoslem"
  ,"caTools"
  ,"ROCR"
  ,"caret"
  ,"bestglm"
  ,"pROC"
  ,"glmnet"
  ,"rattle"
  ,"rpart"
  ,"rpart.plot"
  ,"RColorBrewer"
)

installed_libs <- libs %in% rownames(installed.packages())

if(any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

invisible(
  lapply(libs, library, character.only = TRUE)
)

# Se definen la paleta de colores
colores <- c(
  "#FFC20A"
  ,"#0C7BDC"
  ,"#4B0092"
  ,"#DC3220"
)

# -------------Limpieza de datos y transformación de variables------------------#
# Se verifica qué tipo de dato tiene cada columna del dataset
str(WineQT)

# Se verifica que no haya valores duplicados
sum(duplicated(WineQT))

# Se verifica que no haya valores nulos
sum(is.na(WineQT))

# Se elimina la columna Id ya que no aporta información
WineQT <- subset(WineQT, select = -Id)

# Normalización de los datos
WineQT_scaled <- WineQT %>%
  mutate_if(is.numeric, scale)

# -------------Aplicación de modelo geométrico de clasificación (K-means)------------------#
# Se iteran sobre el modelo para obtener el mejor valor de clusters
n_clusters <- 10

# Se instancia WSS para evaluar la cantidad óptima de clusters
wss <- numeric(n_clusters)

# Se itera sobre el modelo para obtener el mejor valor de clusters
for (i in 1:n_clusters) {
  # Ajustamos el modelo: km_model
  km_model <- kmeans(WineQT_scaled, centers = i, nstart = 20)
  # Guardamos el resultado de WSS
  wss[i] <- km_model$tot.withinss
}

# Se crea un dataframe con los resultados y los visualizamos
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab("Número de clusters")

scree_plot +
  geom_hline(
    yintercept = wss,
    linetype = "dashed",
  )
scree_plot

# -------------Aplicación de modelo probabilístico de clasificación (Regresión Logística)------------------#
# Se fijan los valores categóricos de la variable 'quality'
c <- ifelse(WineQT$quality >= 6, 1, 0)
WineQT$quality <- c

# 'quality' es una variable binaria, por lo que se convierte en factor
WineQT$quality <- as.factor(WineQT$quality)

# Se setea una seed para poder reproducir los resultados
set.seed(123)

samp_logreg <- sample(1:nrow(WineQT), 0.7 * nrow(WineQT))
train_logreg <- WineQT[samp_logreg, ]
test_logreg <- WineQT[-samp_logreg, ]

# Se instancia el modelo de regresion logística
model <- glm(
  quality ~ .,
  data = train_logreg,
  family = binomial(link = "logit")
)

model

# Se instancia el mejor modelo
best_model <- bestglm(
  train_logreg,
  IC = "AIC",
  family = binomial,
  method = "exhaustive"
)

# Se determina las mejores características para el modelo
best_model$Subsets

# Se instancia el modelo con las mejores características
best_train <- glm(
  quality ~ fixed.acidity +
    volatile.acidity +
    citric.acid +
    total.sulfur.dioxide + sulphates + alcohol,
  data = train_logreg,
  family = binomial
)

best_train

# Se predice las probabilidades de la variable de respuesta
best_pred <- predict(best_train, test_logreg, type = "response")
best_pred

# Se convierte las probabilidades en etiquetas binarias usando un umbral (por ejemplo, 0.5)
test_pred_binary <- ifelse(best_pred >= 0.5, 1, 0)
test_pred_binary

# Se crea una matriz de confusión
confusion_matrix <- confusionMatrix(
  data = factor(test_pred_binary),
  reference = factor(test_logreg$quality),
)

# Se muestra la matriz de confusión
print(confusion_matrix)

# Se calcula la curva ROC
ROC_best_glm <- roc(test_logreg$quality, best_pred)
auc(ROC_best_glm)
plot(ROC_best_glm, col = colores)

# -------------Aplicación de modelo de lógico de clasificación (Decision Tree Classifier)------------------#
# Se separa los datos en datos de entrenamiento y de prueba
samp_DTC <- sample(1:nrow(WineQT), 0.7 * nrow(WineQT))
train_DTC <- WineQT[samp_DTC, ]
test_DTC <- WineQT[-samp_DTC, ]

# Instanciamos el modelo
tree_class <- rpart(
  quality ~ .,
  data = train_DTC,
  method = "class"
)

tree_class

# Se grafica el árbol
fancyRpartPlot(tree_class)

# Se evalúa el modelo
important_variables <- tree_class$variable.importance
important_variables <- as.data.frame(important_variables)
important_variables
printcp(tree_class)

# Se usa el modelo para predecir
test_DTC$quality_pred <- predict(tree_class, test_DTC, type = "class")
test_DTC$quality_pred_prob <- predict(tree_class, test_DTC, type = "prob")
test_DTC

# Se genera una matriz de confusión
confusionMatrix(
  test_DTC$quality,
  test_DTC$quality_pred,
  positive = "1"
)

# Se hace la curva ROC
ROC_best_DT <- roc(test_DTC$quality, test_DTC$quality_pred_prob[, 2])

# Se calcula AUC
auc(ROC_best_DT)


### -------------Selección del mejor modelo de clasificación------------------#
# Plot de las curvas ROC de los modelos
plot(ROC_best_glm, col = colores[1]) # Regresión Logística
plot(ROC_best_DT, col = colores[2], add = TRUE) # Decision Tree
