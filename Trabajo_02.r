### Trabajo Práctico N°2 - Exploración de Datos
### Dataset: Predicción de calidad de vino
### Integrantes:
###   - Aboulafia Gerardo
###   - Barquet Amelie
###   - David Santiago
###   - Fernandez Ignacio
###   - Vazquez Agustina

# Carga del dataset y librerías
WineQT <- read.csv("/Users/gerardoaboulafia/Documents/WineQT.csv") 
library(dplyr)
library(tidyr)
library(ggplot2)
library(pheatmap)
library(MASS)
library(lmtest)
library(generalhoslem)


# -------------Limpieza de datos y transformación de variables------------------#
#Verificamos qué tipo de dato tiene cada columna del dataset
str(WineQT)

#Eliminamos la columna Id ya que no aporta información
WineQT <- subset(WineQT, select = -Id)

#Verificamos que no haya valores nulos
sum(is.na(WineQT))

#-------------  Análisis exploratorio de datos y visualización -------------#
# Visualizamos el count de quality para ver la distribución de los datos con color #065893
ggplot(WineQT, aes(x = as.factor(quality))) +
  geom_bar(fill = "#065893") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Quality Count", x = "Quality", y = "Count")

# Group by de quality y visualizamos la media de las variables
WineQT %>%
  group_by(quality) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(cols = -quality, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(quality), y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(title = "Quality Mean", x = "Quality", y = "Mean") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Análisis de correlación entre las variables
pheatmap(cor(WineQT), display_numbers = TRUE, cluster_cols = FALSE,
         cluster_rows = FALSE)

#------------- Aplicación de Modelo de Regresión Lineal Múltiple -------------#
lm(alcohol ~ ., data = WineQT) %>% summary()

# Instanciamos el modelo 
m1 <- lm(alcohol ~ ., data = WineQT)

# Optimización del modelo por feature selection
step <- stepAIC(m1, direction = "both")
step$anova

# Verificamos supuestos del modelo lineal

# 1. Normalidad de los residuos
hist(rstandard(m1))

# 2. Homocedasticidad
par(mfrow = c(2, 2))
plot(m1)
lmtest::bptest(m1)

#------------- Aplicación de Modelo de Regresión Logística -------------#

qplot(WineQT$quality, bins = 50)
qplot(WineQT$alcohol, bins = 50)

expit <- plogis

# Vemos la funcion logistica
x <- seq(-10, 10, length.out = 1e5)
plot(x,
     expit(x),
     type = "l",
     main = "Understanding the effect of expit")


## Fijamos los valores categóricos de la variable 'quality'
c <- ifelse(WineQT$quality >= 6, 1, 0)
WineQT$quality <- c
WineQT <- data.frame(WineQT)


model_glm_original <- glm(quality ~ ., family = binomial(link = "logit"),
                          data = WineQT)
model_glm_original

# Chequeamos los coeficientes del modelo: beta_0,...,beta_11
coef(model_glm_original) 

# Response nos da los datos en formato de probabilidad
predict(model_glm_original, type = "response")


# Optimización del modelo por feature selection
# Instanciamos el stepwise
model_glm_step <- stepAIC(model_glm_original)

# Verificamos qué variables no son relevantes para el modelo
model_glm_step$anova

# Chequeamos los nuevos coeficientes del modelo: beta_0,...,beta_6
coef(model_glm_step)

# Response nos da los datos en formato de probabilidad
predict(model_glm_step, type = "response")


#Aplicamos el test de Hosmer Lemeshow para evauar la bondad de ajuste del modelo
quantile(predict(model_glm_step, type = "response"),
         probs = seq(0, 1, 0.1))

goodness_of_fit <- logitgof(WineQT$quality,
                            predict(model_glm_step, type = "response"))

# Obtenemos un p-valor y un valor del test X^2
print(goodness_of_fit)
print(cbind(goodness_of_fit$observed,
            goodness_of_fit$expected))

# Agregamos la columna de predicción al dataset
WineQT$predicted_quality <- predict(model_glm_step, type = "response")
