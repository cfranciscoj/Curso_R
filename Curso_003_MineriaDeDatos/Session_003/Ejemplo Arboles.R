# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
#install.packages("rpart")
#install.packages("rpart.plot")

# Cargamos las librerias que utilizaremos
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

# Indicamos el directorio de trabajo
setwd("H:/repos/Curso_R/Curso_003_MineriaDeDatos/Session_003")


# Cargamos la base de datos de entrenamiento
datos <- read.csv("Datos Tenis.csv", sep=";",header=TRUE)


# ?rbol de clasificaci?n
modelo1 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

# Especificamos la variable endogena (Jugar) como funci?n de las
# las variables exogenas (Clima, Temperatura, Humedad y Viento)

# Metodos:
# "class" para arboles de clasificacion
# "anova" para arboles de regresi?n

# Par?metro de division para arboles de clasificacion:
# "information" para usar ganancia de informacion (entropia)
# "gini" para usar el indice de impuridad de Gini

# Parametros opcionales de control:
# minsplit determina la cantidad minima de observaciones para intentar dividir un nodo
# minbucket determina la cantidad minima de observaciones a tener en un nodo terminal
# maxdepth determina la profundidad maxima del arbol (el nodo raiz cuenta como 0)
# cp es una version escalada del parametro lambda asociado al costo de complejidad


# Arbol resultante
print(modelo1)

# Graficamos el arbol
rpart.plot(modelo1)

plot(modelo1)

prp(modelo1)

rxDTree(modelo1)

fancyRpartPlot(modelo1)
# Probemos otras combinaciones de parametros de control:
modelo2 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

rpart.plot(modelo2)


# Tabla de cp muestra la mejora en costo de complejidad en cada nodo
printcp(modelo1)

# Representacion grafica
plotcp(modelo1)


# Cambiemos el criterio de division:
modelo3 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

rpart.plot(modelo3)


# Opciones adicionales para la representacion visual del arbol:
# 0 - Sin informaci?n adicional
# 1 - Mostrar cantidad de observaciones por nodo
# 2 - Tasa de clasificaci?n del nodo (correctos/total)
# 3 - Tasa de error de clasificaci?n del nodo (incorrectos/tota)
# 4 - Probabilidad de clasificaci?n del nodo, condicionada al nodo
# 5 - Igual que 4, pero sin mostrar la clase ajustada
# 6 - Probabilidad de la segunda clase ?nicamente
# 7 - Igual que 6, pero sin mostrar la clase ajustada
# 8 - Probabilidad de la clase ajustada
# 9 - Probabilidad de clasificaci?n del nodo, relativa a todas las observaciones
# 10 - Igual que 9, pero con la probababilidad de la segunda clase
# 11 - Igual que 10, pero sin mostrar la clase ajustada
# +100 - Muestra el porcentaje de observaviones del nodo

rpart.plot(modelo3, extra = 0)


# Probemos arboles de regresion
modelo4 <- rpart(Jugadores ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "anova",
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))
rpart.plot(modelo4)


modelo5 <- rpart(Jugadores ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "anova",
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0.03))
rpart.plot(modelo5)


modelo6 <- rpart(Jugadores ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "anova",
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0.18))
rpart.plot(modelo6)


# Usemos los modelos para predecir
# No tenemos una base de datos de validacion distinta a la de entrenamiento
# Asi que usaremos los mismos datos para entrenar y validar (no recomendado en la practica)

# Modelo discreto (informacion)
datos$prediccionB <- predict(modelo1, datos)


# "Empeoremos" un poco el modelo
modelo7 <- rpart(Jugar ~ Clima + Temperatura + Humedad + Viento,
                 data = datos,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 2,
                                         cp = 0))

rpart.plot(modelo7)

datos$prediccionB <- predict(modelo7, datos)

datos$prediccionB <- predict(modelo7, datos, type = "class")

plot(datos$Jugadores,datos$prediccionB)
# Matriz de confusion
table(datos$Jugar,datos$prediccionB)



# Modelo continuo (anova)
datos$prediccionC <- predict(modelo6, datos)

plot(datos$Jugadores,datos$prediccionC)
cor(datos$Jugadores,datos$prediccionC)
