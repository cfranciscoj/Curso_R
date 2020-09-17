# Cargamos las librerias que utilizaremos
install.packages("rattle")

install.packages("tidyverse")

install.packages("caret")
install.packages("lattice")
install.packages("e1071")

install.packages("kernlab")

library("rpart")
library("rpart.plot")
library("rattle")
library("RColorBrewer")
library("dplyr")
library("magrittr")

# Indicamos el directorio de trabajo
setwd("H:/repos/Curso_R/Curso_003_MineriaDeDatos/Eval_002")

datos_brutos <- read.csv("titanic.csv", sep=",",header=TRUE)

datos_brutos <- datos_brutos %>%
                  mutate(Sobrvivio = ifelse(Survived == 1,
                              "SI",
                              "NO"))  


datos_limpios <- datos_brutos %>%
                   filter(Age != "NA")
  

datos_modelo <- datos_limpios %>%
                  head(500)
  
datos_control <- datos_limpios %>%
                   tail(214) 


datos_modelo %>%
  filter(Sobrvivio == "SI") %>%
  count()

datos_modelo %>%
  filter(Sobrvivio == "NO") %>%
  count()


#Modelo Base
modelo_info_000 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "information"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 maxdepth = 30,
                                                 cp = 0))


rpart.plot(modelo_info_000,extra = 108)

plotcp(modelo_info_000)

# Cambio en el maxdepth
modelo_info_001 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "information"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 maxdepth = 3,
                                                 cp = 0))


rpart.plot(modelo_info_001)

plotcp(modelo_info_001)

# Cambio en el minbucket
modelo_info_002 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "information"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 5,
                                                 maxdepth = 3,
                                                 cp = 0))
rpart.plot(modelo_info_002, extra = 102)

plotcp(modelo_info_002)

# Cambio en el minsplit
modelo_info_003 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "information"),
                         control = rpart.control(minsplit = 5,
                                                 minbucket = 5,
                                                 maxdepth = 3,
                                                 cp = 0))
rpart.plot(modelo_info_003, extra = 108)

plotcp(modelo_info_003)


# Cambio en el CP
modelo_info_004 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "information"),
                         control = rpart.control(minsplit = 5,
                                                 minbucket = 5,
                                                 maxdepth = 5,
                                                 cp = 0.005))
rpart.plot(modelo_info_004, extra = 108)

plotcp(modelo_info_004)

# Cambio en el CP
modelo_info_005 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "information"),
                         control = rpart.control(minsplit = 5,
                                                 minbucket = 5,
                                                 maxdepth = 5,
                                                 cp = 0))
rpart.plot(modelo_info_005, extra = 108)

plotcp(modelo_info_005)


# gini base
modelo_gini_000 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 maxdepth = 5,
                                                 cp = 0))
rpart.plot(modelo_gini_000, extra = 108)

plotcp(modelo_gini_000)




# gini cp
modelo_gini_004 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_modelo,
                         method = "class",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 5,
                                                 minbucket = 5,
                                                 maxdepth = 5,
                                                 cp = 0.005))
rpart.plot(modelo_gini_004, extra = 108)

plotcp(modelo_gini_004)


# gini cp
control_gini_004 <- rpart(Sobrvivio ~ Pclass + Sex + Age,
                         data = datos_control,
                         method = "class",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 5,
                                                 minbucket = 5,
                                                 maxdepth = 5,
                                                 cp = 0.005))
rpart.plot(control_gini_004, extra = 108)

plotcp(control_gini_004)

# Modelo discreto (informacion)
datos$prediccionB <- predict(modelo1, datos)
