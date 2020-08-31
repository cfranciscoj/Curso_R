
#Indicar el directorio de trabajo: Dirección donde se encuentra la carpeta.
setwd("C:/Users/Sebastian/Documents/R")

#Cargar la base de datos: Indicar el nombre del archivo, el tipo de separacion de columnas.
datos <- read.csv("DatosTrafico.csv", sep=";",header=TRUE)

#Head para visualizar las primeras 6 filas de la base de datos
head(datos)

#Summary para generar un resumen estadistico de la base de datos
summary(datos)

#Promedio de una variable 
mean(datos$Trafico)

#Mediana de una variable 
median(datos$Trafico)

#Desviacion estandar y varianza de una variable 
sd(datos$Trafico)
var(datos$Trafico)

#Obtener la desviacion estandar como la raiz de la varianza
sqrt(var(datos$Trafico))

#Histogramas para los datos de una variable de la base
hist(datos$Trafico)

#Promedio de trafico en cada dia de la semana
tapply(datos$Trafico, datos$Dia, mean)

#Correlacio entre dos variables 
cor(datos$Trafico,datos$Ventas)

#Grafico de dispersion entre dos variables
plot(datos$Trafico,datos$Ventas)

#Correlacion con variablee categorica (no funciona)
cor(datos$Trafico,datos$Dia)

#Se crea una nueva columna de variable binaria para identificar si es un dia de fin de semana
datos$FinDeSemana <- (datos$Dia == "Sabado") + (datos$Dia == "Domingo")

#Se crea una nueva columna de variable binaria para identificar si es un dia laboral
datos$Laboral <- 1 - datos$FinDeSemana

#Correlacion entre el trafico y si es un dia laboral
cor(datos$Trafico,datos$Laboral)

#Promedio de trafico en dia laboral (1) y en no laboral (0)
tapply(datos$Trafico, datos$Laboral, mean)


#Nueva columna que contiene la diferencia absoluta entre el trafico y la mediana de trafico en la base de datos
datos$Error <- abs(datos$Trafico-median(datos$Trafico))

#Nueva columna que registra el Valor-Z como la razon entre el error y el promedio del error en la base de datos
datos$ValorZ <- datos$Error/median(datos$Error)

#Se crea una nueva base de datos a partir de las observaciones que tienen un Valor-Z menor a 4.5 y se mantienen los nombres de las columnas
datos2 <- datos[which(datos$ValorZ < 4.5),names(datos)]

#Resumen estadistico de la nueva base de datos
summary(datos2)

#Correlacion
cor(datos2$Trafico,datos2$Ventas)

#Grafico de dispersion entre dos variables
plot(datos2$Trafico,datos2$Ventas)

#Histograma de una variable
hist(datos2$Trafico)