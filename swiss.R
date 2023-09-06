### Grupo X
### Integrantes:
###   - Aboulafia Gerardo
###   - Barquet Amelie
###   - David Santiago
###   - Fernandez Ignacio
###   - Vasquez Agustina

#Carga del dataset y librerias
library(ggplot2)
library(tidyr)
library(lattice)
data(swiss)

#-------------Instanciamos las variables y sus intervalos para utilizarlos más adelante-----------------#
#FERTILIDAD
fertilidad<-as.numeric(unlist(c(swiss[1])))
intervalosFertilidad<-cut(fertilidad,
                          breaks=seq(35.0,92.5,length=nclass.Sturges(fertilidad)),
                          include.lowest=TRUE)

#EDUCACION
educacion<-as.numeric(unlist(c(swiss[4])))
intervalosEducacion<-cut(educacion,breaks=seq(
  range(educacion)[1],range(educacion)[2],
  length=nclass.Sturges(educacion)),include.lowest=TRUE)

#MORT.INFANTIL
mort_inf<-as.numeric(unlist(c(swiss[6])))
intervalosMortInf<-cut(mort_inf,breaks=seq(
  range(mort_inf)[1],range(mort_inf)[2],
  length=nclass.Sturges(mort_inf)),include.lowest=TRUE)

#CATOLICOS
catolicos<-as.numeric(unlist(c(swiss[5])))
intervalosCatolicos<-cut(catolicos,breaks=seq(
  range(catolicos)[1],range(catolicos)[2],
  length=nclass.Sturges(catolicos)),include.lowest=TRUE)

#AGRICULTURA
agricultura<-as.numeric(unlist(c(swiss[2])))
intervalosAgricultura<-cut(agricultura,breaks=seq(1.2,89.7,length=nclass.Sturges(
  agricultura)),include.lowest=TRUE)


#ALTA CALIFICACION
alta_calificacion<-as.numeric(unlist(c(swiss[3])))
intervalosAltaCalificacion<-cut(alta_calificacion,breaks=seq(
  range(alta_calificacion)[1],range(alta_calificacion)[2],
  length=nclass.Sturges(alta_calificacion)),include.lowest=TRUE)

#-----------------Tablas de contingencia-----------------#
#Creamos los labels para los intervalos de la tabla de contingencia
agricultura_titulos<-c('Muy Bajo','Bajo','Medio','Alto','Muy Alto','Máximo')
fertilidad_titulos<-c('Muy Pobre','Pobre','Medio','Bueno','Muy Bueno','Excelente')
catolicos_titulos<-c('Muy Bajo','Bajo','Medio','Alto','Muy Alto','Máximo')
educacion_titulos<-c('Muy Bajo','Bajo','Medio','Alto','Muy Alto','Máximo')


#Creamos la tabla de contingencia para fertilidad y agricultura
      #FERTILIDAD/AGRICULTURA

tabla_fert_agr<-table(intervalosFertilidad,intervalosAgricultura)


rownames(tabla_fert_agr)<-fertilidad_titulos
colnames(tabla_fert_agr)<-agricultura_titulos


barchart(tabla_fert_agr,horizontal=FALSE,stack=TRUE,auto.key=TRUE)
barplot(table(intervalosFertilidad,intervalosAgricultura)) #faltan los 
                                                           #colores y labels

chisq.test(tabla_fert_agr)  #opcion =>,simulate.p.values=TRUE

#CORRELACION
cor(swiss)
cor(fertilidad,agricultura)

#PLOTS (muestran graficamente los valores de las correlaciones)
plot(agricultura,fertilidad)
plot(catolicos,educacion)
plot(educacion,agricultura)