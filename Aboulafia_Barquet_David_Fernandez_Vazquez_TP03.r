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
  "InformationValue",
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
WineQT <- WineQT %>%
  mutate_if(is.numeric, scale)

# -------------Aplicación de modelo geométrico de clasificación (K-means)------------------#
# Instanciamos el modelo con 3 clusters
set.seed(123)
km_inicial <- kmeans(WineQT, centers = 3, nstart = 20)
km_inicial

# Iteramos sobre el modelo para obtener el mejor valor de clusters
n_clusters <- 10

#Instanciamos WSS para evaluar la cantidad óptima de clusters
wss <- numeric(n_clusters)

# Iteramos sobre el modelo para obtener el mejor valor de clusters
for (i in 1:n_clusters) {
  # Ajustamos el modelo: km_model
  km_model <- kmeans(WineQT, centers = i, nstart = 50)
  # Guardamos el resultado de WSS
  wss[i] <- km_model$tot.withinss
}

# Creamos un dataframe con los resultados y los visualizamos
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab("Number of clusters")

scree_plot

scree_plot +
  geom_hline(
    yintercept = wss,
    linetype = "dashed",
)


# -------------Aplicación de modelo probabilístico de clasificación (Bayes)------------------#
# Fijamos los valores categóricos de la variable 'quality'
c <- ifelse(WineQT$quality >= 6, 1, 0)
WineQT$quality <- c
# Modelo de ML probabilístico usando logit:
# ('quality' es una variable binaria)
df$quality <- as.factor(df$quality)

# Separamos los datos en datos de entrenamiento y de prueba
#(70% entrenamiento, 30% prueba)

# Seteamos una seed para poder reproducir los resultados
set.seed(123)

split <- sample.split(df$quality, SplitRatio = 0.7)
train_data <- subset(df, split == TRUE)
test_data <- subset(df, split == FALSE)

# Instanciamos el modelo
logreg_model <- glm(
  quality ~ .,
  data = train_data,
  family = binomial(link = "logit")
)

# Generamos las predicciones sobre los datos de testeo
predictions <- predict(logreg_model, newdata = test_data, type = "response")

# Evaluación del modelo
# Empleamos ROC y AUC
pred <- prediction(predictions, test_data$quality)
perf <- performance(pred, "tpr", "fpr")
auc <- as.numeric(performance(pred, "auc")@y.values)

# AUC value
cat("AUC:", auc, "\n")


# -------------Aplicación de modelo de lógico de clasificación (Decision Tree Classifier)------------------#
samp <- sample(1:nrow(WineQT), 0.7 * nrow(WineQT))
train <- WineQT[samp, ]
test <- WineQT[-samp, ]

# Instanciamos el modelo de regresion logística
model <- glm(
  quality ~ .,
  data = train,
  family = binomial(link = "logit")
)

best_model <- bestglm(
  train,
  IC = "AIC",
  family = binomial,
  method = "exhaustive"
)

best_model$Subsets
