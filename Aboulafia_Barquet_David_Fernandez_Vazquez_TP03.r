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
set.seed(123)
samp_DTC <- sample(1:nrow(WineQT), 0.7 * nrow(WineQT))
train_DTC <- WineQT[samp_DTC, ]
test_DTC <- WineQT[-samp_DTC, ]

# Instanciamos el modelo
tree_class <- rpart(
  quality ~ .,
  data = train_DTC,
  method = "class"
)

# Se visualiza el árbol
rpart.plot(
  tree_class, type = 4,
  extra = 108, under = TRUE,
  cex = 0.8, box.palette = "auto"
)

# Se evalúa el modelo
important_variables <- tree_class$variable.importance
important_variables <- as.data.frame(important_variables)
important_variables
printcp(tree_class)

# Se predice las probabilidades de la variable de respuesta
pred_DTC <- predict(tree_class, test_DTC, type = "prob")
pred_DTC

# Se convierte las probabilidades en etiquetas binarias usando un umbral (por ejemplo, 0.5)
test_pred_binary_DTC <- ifelse(pred_DTC >= 0.5, 1, 0)
test_pred_binary_DTC

# Se genera una matriz de confusión
confusion_matrix_DTC <- confusionMatrix(
  data = factor(test_pred_binary_DTC[, 2]),
  reference = factor(test_DTC$quality),
)

print(confusion_matrix_DTC)

# Se calcula la curva ROC
ROC_best_DT <- roc(test_DTC$quality, pred_DTC[, 2])

# Se visualiza la curva ROC
plot(ROC_best_DT, col = colores)

# Se calcula AUC
auc(ROC_best_DT)

# Se poda el árbol
pruned_tree <- prune(
  tree_class,
  cp = tree_class$cptable[which.min(tree_class$cptable[,"xerror"]), "CP"]
)

# Se visualiza el árbol podado
rpart.plot(
  pruned_tree, type = 4,
  extra = 108, under = TRUE,
  cex = 0.8, box.palette = "auto"
)

# Se evalúa el modelo
important_variables <- pruned_tree$variable.importance
important_variables <- as.data.frame(important_variables)
important_variables

# Se predice las probabilidades de la variable de respuesta
pred_pruned_tree <- predict(pruned_tree, test_DTC, type = "prob")
pred_pruned_tree

# Se convierte las probabilidades en etiquetas binarias usando un umbral (por ejemplo, 0.5)
test_pred_binary_pruned_tree <- ifelse(pred_pruned_tree >= 0.5, 1, 0)
test_pred_binary_pruned_tree

# Se genera una matriz de confusión
confusion_matrix_pruned_tree <- confusionMatrix(
  data = factor(test_pred_binary_pruned_tree[, 2]),
  reference = factor(test_DTC$quality),
)

print(confusion_matrix_pruned_tree)

# Se calcula la curva ROC
ROC_best_pruned_tree <- roc(test_DTC$quality, pred_pruned_tree[, 2])

# Se visualiza la curva ROC
plot(ROC_best_pruned_tree, col = colores)

# Se calcula AUC
auc(ROC_best_pruned_tree)

### -------------Selección del mejor modelo de clasificación------------------#
# Plot de las curvas ROC de los modelos
plot(ROC_best_glm, col = colores[1]) # Regresión Logística
plot(ROC_best_pruned_tree, col = colores[2], add = TRUE) # Decision Tree Classifier
legend(
  "bottomright",
  legend = c("Regresión Logística", "Decision Tree Classifier"),
  col = colores[1:2],
  lty = 1,
  cex = 0.8
)