library(rpart)
library(rpart.plot)
library(dplyr)

setwd("C:/Estudios/Diplomado Big Data y Data Science/Curso 3 - Ciencia de datos y sus aplicaciones/Proyectos-20200910/Proyecto 1")

# Cargamos la base de datos de entrenamiento
datosFugas <- read.csv("Proyecto1_Base_analisis_de_fuga.csv", sep=",", header=TRUE)

str(datosFugas)
head(datosFugas,30)

# No se observan valores NA, no hay valores por convertir a factor.

#Dividiremos el conjunto de datos en entrenamiento y test.
data_train <- datosFugas %>% dplyr::sample_frac(.8)
data_test  <- dplyr::anti_join(datosFugas, data_train, by = 'ï..Id_Cliente') # se debe tener un id
data_train <- dplyr::select(data_train, -ï..Id_Cliente)
data_test <- dplyr::select(data_test, -ï..Id_Cliente)

modelo <- rpart(Fuga ~ .,
                 data = data_train,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0.046429))

rpart.plot(modelo, extra = 106)

printcp(modelo)
plotcp(modelo)

#prediccion <- predict(modelo, data_test, type = 'class')

datosFugas$prediccion1 <- predict(modelo, data_test, type = 'class')

# Contabilizamos la coincidencia entre las observaciones de test y predecidos.
# matriz de confusión.
table_mat <- table(data_test$Fuga, prediccion)
table_mat

exactitud_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Exactitud para el test', exactitud_Test))
