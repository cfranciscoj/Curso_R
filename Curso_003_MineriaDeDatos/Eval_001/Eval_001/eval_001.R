library("dplyr")
library("ggplot2")
library("magrittr")
setwd("/home/carlos/repos/Curso_R/Curso_003_MineriaDeDatos/Eval_001")
DatosViajes <- read.csv("DatosViajes.csv", sep=";",header=TRUE)


#Summary para generar un resumen estadistico de la base de datos
summary(DatosViajes)

#Ingreso por comuna
tapply(DatosViajes$Ingreso, DatosViajes$Comuna, mean)

tapply(DatosViajes$Ingreso, DatosViajes$Comuna, sd)

tapply(DatosViajes$Ingreso, DatosViajes$Comuna, var)

#Personas por familia por comuna
tapply(DatosViajes$Personas, DatosViajes$Comuna, mean)

tapply(DatosViajes$Personas, DatosViajes$Comuna, sd)

tapply(DatosViajes$Personas, DatosViajes$Comuna, var)


cor(DatosViajes$Ingreso,DatosViajes$Personas)

DatosViajesProm <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), mean)

DatosViajesVar <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), var)

DatosViajesDest <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), sd)

DatosViajesMed <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), median)

DatosViajesSum <- aggregate(DatosViajes[, 3:13], list(DatosViajes$Comuna), sum)

##Correlación Jovenes
cor(DatosViajes$Viajes, DatosViajes$Jovenes)
cor(DatosViajesProm$Viajes, DatosViajesProm$Jovenes)
cor(DatosViajesVar$Viajes, DatosViajesVar$Jovenes)
cor(DatosViajesDest$Viajes, DatosViajesDest$Jovenes)
cor(DatosViajesMed$Viajes, DatosViajesMed$Jovenes)
cor(DatosViajesSum$Viajes, DatosViajesSum$Jovenes)




