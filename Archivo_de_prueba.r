### Grupo X
### Integrantes:
###   - Aboulafia Gerardo
###   - Barquet Amelie
###   - David Santiago
###   - Fernandez Ignacio
###   - Vasquez Agustina

# Carga del dataset
data_pacientes <- read.csv("/Users/gerardoaboulafia/Documents/Exploración_de_datos/Datasets/Datos trabajo 1.csv", sep=";")

# Reemplazar espacios en blanco por NA
data_pacientes <- replace(data_pacientes, data_pacientes == "999,99", NA)

