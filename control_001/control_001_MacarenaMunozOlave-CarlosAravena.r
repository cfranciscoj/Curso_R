### Control 1: Sintaxis básica y manipulación de objetos simples en R ###
#
# Nombre Integrantes : Macarena Muñoz Olave
#                      Carlos Aravena De Los Ríos
# Curso              : Introduccion a R 2020-2
# Sigla              : DBDN-R-C1

####### Librerias a utilizar
library("dplyr")

# Sección 1
#
# La siguiente línea de código permite cargar en memoria, un objeto de extensión *.rds que está almacenado en el disco de la máquina en la ruta actual de trabajo.
#
# Este archivo contiene la información de los contagios por COVID-19 en nuestsro país, según las cifras oficiales entregadas por el ministerio de salud actualizadas al 08 de mayo. En el archivo encontrará, para cada mes, el total de casos diarios desglosados según:
#
#     'casos_nuevos_con_sintomas': casos nuevos con síntomas del día.
#     'casos_totales' : casos acumulados a la fecha.
#     'casos_recuperados': casos recuperados acumulados a la fecha.
#     'fallecidos': casos fallecidos acumulados.
#     'casos_activos': casos activos a la fecha.
#     'casos_nuevos_sin_sintomas': casos asintomáticos del día.
#
#     'casos_nuevos_totales': suma de casos asintomáticos más casos con síntomas del día.
#
#     Si bien no será utilizados en este trabajo, también econtrán nuevos registros de acuerdo a los últimos cambios en la metodología de conteo. Para más info pueden referirse al siguiente link: https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto5
#
#     'casos_activos_por_fd'
#     'casos_activos_por_fis'
#     'casos_recuperados_por_fis'
#     'casos_recuperados_por_fd'
#     'casos_confirmados_recuperados'
#     'casos_activos_confirmados'
#     'casos_probables_acumulados'
#     'casos_activos_probables'

# Carga de casos
#
RutaBase = getwd()
RutaCovid <- paste(RutaBase, "/covid.rds", sep = "")

casos <- readRDS(RutaCovid)

# Pregunta 1.1
#
# 1a) (1pt) ¿Qué clase de objetos es casos?
# 1b) (1pt) ¿Cuántos elementos posee este objeto?
# 1c) (1pt) ¿Cuáles son los nombres de este objeto?
# 1d) (2pt) ¿Qué clase de objeto está almacenado en sl slot correspondiente al
#            mes de junio ("jun") y cuántos elementos posee?
# 1e) (2pt) Para el mes de junio, ¿a qué clase de objeto corresponde
#           casos_totales y cuántos elementos posee?

# P1.1
## respuestas
## 1a) (1pt) ¿Qué clase de objetos es casos?
print(paste("1a) La Clase de 'casos' es:", class(casos)))
# R: La variable "casos" es de clase "Lista (list)"

## 1b) (1pt) ¿Cuántos elementos posee este objeto?
print(paste("1b) La cantidad de elementos de 'casos' es:", length(casos)))
# R: La variable "casos", tiene 5 elementos

## 1c) (1pt) ¿Cuáles son los nombres de este objeto?
Nombres <- names(casos)
LargoNombres <- length(Nombres)
NombresElementos <- ""
i <- 1
while (i <= LargoNombres) {
  if (i == 1){
    NombresElementos <- paste(NombresElementos, Nombres[i], sep= "")
  } else {
    NombresElementos <- paste(NombresElementos, Nombres[i], sep= ", ")
  }
  i <- i + 1
}
print(paste("1c) Los nombres de los elementos de 'casos' son:", NombresElementos))
# R: Los nombres de los elementos de 'casos' son: mar, abr, may, jun, jul"
# names(casos) # R: Los nombre son:
#              #        1) "mar"
#              #        2) "abr"
#              #        3) "may"
#              #        4) "jun"
#              #        5) "jul"



## 1d) (2pt) ¿Qué clase de objeto está almacenado en sl slot correspondiente al
##            mes de junio ("jun") y cuántos elementos posee?
print(paste("1d) La Clase de 'casos$jun' es:", class(casos$jun)))
# R: El slot "jun" es de clase "Lista (list)"

## 1e) (2pt) Para el mes de junio, ¿a qué clase de objeto corresponde
##           casos_totales y cuántos elementos posee?
# class(casos$jun$casos_totales)  # R: La clase para el objeto casos_totales es "data.frame"
# length(casos$jun$casos_totales) # R: La cantidad de elementos es 2
print(paste("1d) La Clase de 'casos$jun$casos_totales' es:", class(casos$jun$casos_totales)))
# R: La clase del objeto "casos_totales", para el mes de junio es "data.frame"
print(paste("1d) La cantidad de elementos de 'casos$jun$casos_totales' es:", length(casos$jun$casos_totales)))
# R: La cantidad de elmentos que posee el objeto "casos_totales" es "2"


# Pregunta 1.2
#
# 2a) (2pts) ¿Cuántos casos activos existían al 25 de marzo? (indique claramente
#             la expresión de R para obtener el valor solicitado) .
# 2b) (1pts) Genere cinco nuevas variables llamadas totales_marzo,totales_abril,
#            totales_mayo, totales_junio y totales_julio, que contengan los
#            dataframes correspondientes al slot casos_totales del mes
#            respectivo, (por ejemplo, la variable totales_julio debe contener
#            una dataframe de dimensión 8×2
# 2c) (2pts) Para cada dataframe del paso anterior, genere una nueva columna
#            llamada mes, que repita el nombre del mes correspondiente por cada fila.
# 2d) (3pts) En cada data.frame, agregue una nueva columna llamada casos_diarios
#            que contanga la información correspontiende al total de casos
#            nuevos diarios(casos_nuevos_totales).

# P1.2
## Respusta
## 2a) (2pts) ¿Cuántos casos activos existían al 25 de marzo? (indique claramente
##             la expresión de R para obtener el valor solicitado) .
print(paste("2a) Los casos activos que existian al 25 de marzo:", casos$mar$casos_activos$total[casos$mar$casos_activos$dia_del_mes == "25"]))
# R: Los casos activos que existian al 25 de marzo es "1117"

## 2b) (1pts) Genere cinco nuevas variables llamadas totales_marzo,totales_abril,
##            totales_mayo, totales_junio y totales_julio, que contengan los
##            dataframes correspondientes al slot casos_totales del mes
##            respectivo, (por ejemplo, la variable totales_julio debe contener
##            una dataframe de dimensión 8×2
totales_marzo <- casos$mar$casos_totales
totales_abril <- casos$abr$casos_totales
totales_mayo  <- casos$may$casos_totales
totales_junio <- casos$jun$casos_totales
totales_julio <- casos$jul$casos_totales



## 2c) (2pts) Para cada dataframe del paso anterior, genere una nueva columna
##            llamada mes, que repita el nombre del mes correspondiente por cada fila.
mes <- rep("marzo", length.out = length(totales_marzo$dia_del_mes))
totales_marzo <- cbind(totales_marzo, mes)

mes <- rep("Abril", length.out = length(totales_abril$dia_del_mes))
totales_abril <- cbind(totales_abril, mes)

mes <- rep("Mayo", length.out = length(totales_mayo$dia_del_mes))
totales_mayo <- cbind(totales_mayo, mes)

mes <- rep("Junio", length.out = length(totales_junio$dia_del_mes))
totales_junio <- cbind(totales_junio, mes)

mes <- rep("Julio", length.out = length(totales_julio$dia_del_mes))
totales_julio <- cbind(totales_julio, mes)


## 2d) (3pts) En cada data.frame, agregue una nueva columna llamada casos_diarios
##            que contanga la información correspontiende al total de casos
##            nuevos diarios(casos_nuevos_totales).

casos_diarios <- casos$mar$casos_nuevos_totales$total

str(casos$mar)
# Pregunta 1.3
#
# 3a) (1pts) Con la ayuda de lafunción rbind() , una las filas de las 5 tablas
#            generadas en la pregunta anterior y guarde la tabla resultante en
#            una variable llamada casos_hist.
# 3b) (1pt)  En promedio, ¿Cuántos casos hay diariamente desde el 03 de marzo
#            hasta el 08 de julio?
# 3c) (2pts) En promedio, ¿Cuántos casos hay diariamente en cada mes ?.
# P1.3
## Respusta
# 3a) (1pts) Con la ayuda de lafunción rbind() , una las filas de las 5 tablas
#            generadas en la pregunta anterior y guarde la tabla resultante en
#            una variable llamada casos_hist.

# 3b) (1pt)  En promedio, ¿Cuántos casos hay diariamente desde el 03 de marzo
#            hasta el 08 de julio?

# 3c) (2pts) En promedio, ¿Cuántos casos hay diariamente en cada mes ?.

################################################################################
# Sección 2
#
# La siguiente celda de código permitirá cargar en su sesión de trabajo
# los siguientes vectores:
#
#   temp_hombre: Contiene el registro de la temperatura para una muestra de 650
#                hombres escogidos al azar.
#   temp_hombre: Contiene el registro de la temperatura para una muestra de 580
#                mujeres escogidas al azar.
#
# En ambos vectores, los identificadores (id) de cada persona están contenidos
# en el atributo names() del vector respectivo.

# temp_h <- readRDS("temp_h.rds")
# temp_m <- readRDS("temp_m.rds")


# Pregunta 2.1
#
# P1) (4pts) A partir de los dos vectores cargados temp_h y temp_m ustred deberá
#            crear un data.frame llamado casos con tres columnas y un total de
#            1230 filas. Las columnas deben ser las siguientes; una con el
#            id del registro, otra con el género y otra con la temperatura
#            registrada. A modo de ejemplo, a continuación se muestra como
#            debería quedar el data.frame:
#
#                    id     genero  temperatura
#                    h_162 	hombre  36.8
#                    h_140  hombre  38.7
#                    h_13   hombre  36.1
#                     ⋮      ⋮      ⋮
#                    m_16   mujer   39.1
#                    m_502  mujer   40.1
#                    m_189  mujer   36.8
# P2.1. ---------------------------------------------------------------------
## Respuesta:


# Pregunta 2.2
#
# Adicionalmente, se cuenta con la información de síntomas presentados
# por cada presona registrada. Esta información se encuentra disponible
# en el archivo sintomas.rds.
#
# 2a) (1pt) Cargue el archivo sintomas.rds en una variable llamada sintomas.
# 2b) (1pt) ¿Cuál es la clase de este objeto?.
# 2c) (1pt) Note que cada fila undica con un 1 si la persona presentó el
#           respectivo síntoma y 0 si no. ¿Qué sintomas presentó la
#           persona ubicada en el registro 450?.
# 2d) (2pts) ¿Qué síntomas y qué temperatura presentó la persona con id "h_537"?
#            Si es le es de utilidad puede utilizar la función rownames(),
#            la cual permite obtener los nombres (id) de las filas de una matriz.
# P2.2
## Respuestas:
# 2a) (1pt) Cargue el archivo sintomas.rds en una variable llamada sintomas.


# 2b) (1pt) ¿Cuál es la clase de este objeto?.


# 2c) (1pt) Note que cada fila undica con un 1 si la persona presentó el
#           respectivo síntoma y 0 si no. ¿Qué sintomas presentó la
#           persona ubicada en el registro 450?.


# 2d) (2pts) ¿Qué síntomas y qué temperatura presentó la persona con id "h_537"?
#            Si es le es de utilidad puede utilizar la función rownames(),
#            la cual permite obtener los nombres (id) de las filas de una matriz


# # Nota de utilidad
# # Ya sea trabajando con un vector, matriz, data.frame o lista, es posible omitir
# # la selección de alguna(s) de sus posiciones (filas/columnas o slots).
# # Esto se puede llevar a cabo de diversas maneras, por ejemplo:
#
# # Primero creamos un vector, una matriz, un data.frame  y una lista
# vec <- c(v1 = 1, v2 = 2, v3 = 3,v4 = 4)
# mat <- cbind(c(1,1),c(2,2),c(3,3),c(4,4) ) ; rownames(mat) <- c("f1","f2"); colnames(mat) <- c("c1","c2","c3","c4")
# dat <- as.data.frame(mat)
# lis <- list(s1 = c(1,1), s2 = c(2,2), s3 = c(3,3) , s4 = c(4,4))
#
# # Omisión por posición
# # Si queremos omitir el primer y tercer elmento del vector y la lista
# vec[-c(1,3)]
# lis[-c(1,3)]
#
# # Misma idea si queremos omitir columnas de una matriz (o data.frame)
# mat[,-c(1,3)]
# dat[,-c(1,3)]
#
# # Omisión por nombres
# # Por ahora tendremos que ubicar las posiciones de los nombres deseados.
# pos_vec <- which(names(vec) %in% c("v1","v3"))
# pos_lis <- which(names(lis) %in% c("s1","s3"))
# pos_mat <- which(colnames(mat) %in% c("c1","c3"))
# pos_dat <- which(names(dat) %in% c("c1","c3"))
#
# vec[-pos_vec]
# lis[-pos_lis]
#
# # Misma idea si queremos omitir columnas de una matriz (o data.frame)
# mat[,-pos_mat]
# dat[,-pos_dat]
#
# # Más adelante veremos otras alternivas, como por ejemplo, utilizando
# # funciones del package dplyr.


# Pregunta 2.3
# Con la ayuda de las funciones rowSums() y colSums(), identifique lo siguiente.
#
# 3a) (3pt) ¿Cuántos registros presentaron exactamente los 4 síntomas?
# 3b) (1pt) ¿Cúantas personas presentaron tos?
# 3c) (2pt) ¿Cuál es el síntoma más frecuente?
# P3
##Respuestas:
# 3a) (3pt) ¿Cuántos registros presentaron exactamente los 4 síntomas?

# 3b) (1pt) ¿Cúantas personas presentaron tos?

# 3c) (2pt) ¿Cuál es el síntoma más frecuente?


# Pregunta 2.4
#
# 4a) (1pts) La función as.data.frame() permite coercionar un objeto de otra
#            clase a data.frame. cree una variable llamada sintomas_df
#            con la información de sintomas pero como data.frame.
# 4b) (1pts) Se puede observar que los ids solo están disponibles en los
#            nombres de las filas. Sin embargo esútil tener esta información
#            como una columna más de la tabla. Para ello cree una nueva columna
#            en la tabla sintomas_df llamada id que contenga los ids de cada
#            registro.
#
# NOTA: El orden de las filas no ha variado por lo que podemos añadir
#       la columna sin mayores precauciones.
#
# 4c) (2pts) ¿Cuántas personas sólo presentaron tos y dolor de cabeza?
# 4d) (1pt) Se sabe que una persona tiene fiebre cuando su temperatura es
#           mayor o igual a los 37.2°. Cree una nueva columna en la tabla
#           sintomas_df llamada tiene_fiebre que contenga TRUE cuando tiene
#           fiebre y FALSE en caso contrario.
# P4
#Respuestas:
# 4a) (1pts) La función as.data.frame() permite coercionar un objeto de otra
#            clase a data.frame. cree una variable llamada sintomas_df
#            con la información de sintomas pero como data.frame.


# 4b) (1pts) Se puede observar que los ids solo están disponibles en los
#            nombres de las filas. Sin embargo esútil tener esta información
#            como una columna más de la tabla. Para ello cree una nueva columna
#            en la tabla sintomas_df llamada id que contenga los ids de cada
#            registro.

# 4c) (2pts) ¿Cuántas personas sólo presentaron tos y dolor de cabeza?


# 4d) (1pt) Se sabe que una persona tiene fiebre cuando su temperatura es
#           mayor o igual a los 37.2°. Cree una nueva columna en la tabla
#           sintomas_df llamada tiene_fiebre que contenga TRUE cuando tiene
#           fiebre y FALSE en caso contrario.
