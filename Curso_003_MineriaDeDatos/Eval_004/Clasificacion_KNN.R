
install.packages("factoextra")
# Cargamos las librerias que utilizaremos
library(class)
library(caret)
library(e1071)
library(gtools)
library(tidyverse)  
library(dplyr)
library(magrittr)

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

agrega_coma <- function(var_arreglo) {
  pegado <- var_arreglo[1]
  if(length(var_arreglo) > 1){
    j <- 2
    while(j <= length(var_arreglo)){
      #print(length(var_arreglo))
      pegado <- paste(pegado,";",var_arreglo[j])  
      j = j + 1
    }
  }
  return(pegado)
}
                          

# Indicamos el directorio de trabajo
setwd("/home/carlos/repos/Curso_R/Curso_003_MineriaDeDatos/Eval_004")
datos <- read.csv("/home/carlos/Nextcloud/Cursos/BigData/Curso 3 Mineria de Datos/Evaluaciones/eval 004/SouthGermanCredit.csv", sep = ";",header=TRUE)






datos_todos <- as.data.frame(cbind(status_SinCuenta        = (datos$status == 1),
                                   status_CuentaConDeuda   = (datos$status == 2),
                                   status_AhorroHasta200   = (datos$status == 3),
                                   status_AhorraDesde200   = (datos$status == 4),
                                   history_RetrasosPasados = (datos$history == 0),
                                   history_CuentaOtroBanco = (datos$history == 1),
                                   history_SinCreditoAnter = (datos$history == 2),
                                   history_CredVigentesDia = (datos$history == 3),
                                   history_TodosCredPasado = (datos$history == 4),
                                   amount   = datos$amount,
                                   savings  = datos$savings,
                                   employed = datos$employed,
                                   rate     = datos$rate,
                                   personal_HombreDivorciado    = (datos$personal == 1),
                                   personal_HombreSolMujerNoSol = (datos$personal == 2),
                                   personal_HombreCasadoViudo   = (datos$personal == 3),
                                   personal_MujerSoltera        = (datos$personal == 4),
                                   residence = datos$residence,
                                   property  = datos$property,
                                   age       = datos$age,
                                   housing_Gratuita  = (datos$housing == 1),
                                   housing_Arrendada = (datos$housing == 2),
                                   housing_Propia    = (datos$housing == 3),
                                   job       = datos$job,
                                   persons   = datos$persons,
                                   telephone = datos$telephone,
                                   foreign   = datos$foreign,
                                   credit    = datos$credit))


#Largo de registro del Dataframe con todos las variables
largo_todos <- length(datos_todos) - 1

# Datos normalizados, entre 1 y 0
datos_norm <- as.data.frame(lapply(datos_todos, normalize))

# División entre los datos en entrenamiento (75%) y validacion (25%)
subset <- sample(1:nrow(datos), size = 0.75 * nrow(datos), replace = FALSE)



# Calculo de Accuracy de cada combinación


resultados <- data.frame("nro_registro" = integer(),
                         "nro_combi"    = integer(),
                         "knn"          = integer(), 
                         "colunnas"     = character(), 
                         "accuracy"     = double())

combinaciones_00 <- c(1:largo_todos)
i <- 1
while( i <= largo_todos){
  combinaciones <- combinations(largo_todos,  i, combinaciones_00)
  j <- 1
  while(j < nrow(combinaciones)){
    datos_train <- datos_norm[subset, c(combinaciones[j,])]
    label_train <- datos_norm[subset, largo_todos + 1 ]
    datos_test <- datos_norm[-subset, c(combinaciones[j,])]
    label_test <- datos_norm[-subset, largo_todos + 1 ]
    colunas <- agrega_coma(combinaciones[j,])
    k.optm = 1
    n <- 1
    while(n <= 25){
      knn.mod <- knn(datos_train,
                     datos_test,
                     label_train,
                     k = n)
      k.optm <- sum(label_test == knn.mod)/length(label_test)
      z <- data.frame("nro_registro" = i      , "nro_combi" = j, "knn" = n, 
                      "colunnas"     = colunas, "accuracy"  = k.optm)
        
      resultados <-  rbind(resultados,z)
      n <- n + 1
    }
    j <- j + 1
  }
  i <- i + 1
}



res_final <- resultados %>%
                 filter(accuracy == max(resultados$accuracy, na.rm = FALSE)) %>%
                 select(nro_registro ,
                        nro_combi    ,
                        knn          , 
                        colunnas     , 
                        accuracy     ) %>%
                 head(1)
  

combinaciones_final <- combinations(largo_todos,  res_final$nro_registro, combinaciones_00)

datos_train_final <- datos_norm[subset, c(combinaciones_final[res_final$nro_combi,])]
label_train_final <- datos_norm[subset, largo_todos + 1 ]
datos_test_final <- datos_norm[-subset, c(combinaciones_final[res_final$nro_combi,])]
label_test_final <- datos_norm[-subset, largo_todos + 1 ]
colunas_final <- agrega_coma(combinaciones_final[res_final$nro_combi,])




modelo_k_final <- knn(datos_train_final,
                      datos_test_final,
                      label_train_final,
                      k = res_final$knn)

k.optm_final <- sum(label_test_final == modelo_k_final)/length(label_test_final)
confusionMatrix(modelo_k_final,as.factor(label_test_final), positive = "1")

k_graf_final <- t(resultados %>%
             filter(nro_registro == res_final$nro_registro & nro_combi == res_final$nro_combi) %>%
             select(accuracy))



png(filename = "grafico_004.png", width = 500, height = 250)
  plot(k_graf_final[1,])
dev.off()
