### Control 3: Evaluación Final ###
#
# Nombre Integrantes : Macarena Muñoz Olave
#                      Carlos Aravena De Los Ríos
# Curso              : Introduccion a R 2020-2
# Sigla              : DBDN-R-C3

####### Librerias a utilizar
library("gapminder")
library("dplyr")
library("ggplot2")
library("magrittr")
library("corrplot")

# #############################################################################
# data(gapminder)
# head(gapminder)
# ##############################################################################


# ################################ Sección 1 ###################################
#
# ##############
# Introducción
# #############
#
# En esta sección usted generará un breve análisis descriptivo de la evolución de
# la expectativa de vida de diferentes países. Para ello usted utilizará la
# información disponible en el dataset gapminder del package del mismo nombre.
# La información contenida en este dataset corresponde a la siguiente:
#
#     - country: Factor con 142 niveles
#     - continent: Factor con 5 niveles
#     - year: Rangos de años desde 1952 to 2007 en incrementos de 5 años
#     - lifeExp: Esperanza de vida al nacer, en años
#     - pop: Población
#     - gdpPercap: GDP per capita (US$, ajustado por inflación)
#
# P1) (2pts) El siguiente representa la relación entre el el ingreso GDP y
#            la esperanza de vida para todos los países a lo largo de todos
#            los años, adicionalmente el tamaño de cada punto está en proporción
#            con la poblción total de cada país. Adicionalmente, el color varía
#            en función del año del registro. En el gráfico se pueden apreciar
#            observaciones con alto GDP (aquellas encerradas en el recuadro rojo).
#            Identifíque dichas observaciones e indique claramente a qué
#            país(es) y año(s) corresponden.
#
# ##############################################################################
# NOTA: Usted tiene libertad de escoger el método con el cual identificar
#       dichas observaciones.
#       Gráfico 001
# ##############################################################################
#
# R:

gapminder %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color=year,
             size = pop))  +
  labs(colour="year") +
  geom_point(fill="#9DB9E3",
             alpha = 0.5) +
  #geom_hline(aes()) +
  geom_rect(aes(xmin = 57000,
                xmax = 115000,
                ymin = 50,
                ymax = 71,
                fill = c()),
            color="red",
            size = 0.5,
            alpha = 0) +
  xlab("gdpPercap") +
  ylab("lifeExp") 

gapminder %>%
  filter(gdpPercap >= 57000) %>%
  distinct(country) %>%
  group_by(country) %>%
  select(country)

# P2) (3pts) Mediante un gráfico de puntos, visualice una comparativa entre
#            la relación de ingresos y expectativa de vida, para los
#            años 1952 y 2007. Para ello usted deberá replicar el siguiente
#            gráfico, donde el color representa a un continente distinto y
#            el tamaño está dado por el total de población.
#            Grafico 002
#
# R:
Var1952_2007 <- gapminder %>%
  filter(year == 1952 | year == 2007)

Var1952_2007 %>%
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color=continent,
             size = pop))  +
  labs(colour="Continet") +
  geom_point(fill="#9DB9E3",
             alpha = 0.5) +
  facet_wrap(~ year) +
  xlab("gdpPercap") +
  ylab("lifeExp") 


# P3) (2pts) Determine el nivel de correlación de spearman, entre las variables
#            gdpPercap y lifeExp para cada uno de los años registrados.
#            ¿En qué año se observa el mayor nivel de correlación entre
#            ambas variables?
#
# R:



Correlacionados <- data.frame(Agno  =  integer(),
                              Corr  =  double()
                             )

AuxCorre <- data.frame(Agno  =  integer(),
                       Corr  =  double()
                      )

Agnos <- gapminder %>%
  distinct(year) %>%
  select(year)

LargoAgnos <- as.integer(length(Agnos$year))
j <- 1
AgnoRev <- 0
AgnoCorr <- 0.0
while (j <= LargoAgnos) {
  AgnoRev <- as.integer(Agnos$year[j])
  
  AgnoGap <- gapminder %>%
    filter(year == AgnoRev)
  
  
  AgnoCorr <- cor(x = AgnoGap$gdpPercap, y = AgnoGap$lifeExp, method = "spearman")
  AuxCorreAgno  <- data.frame(AgnoRev, AgnoCorr) 
  
  Correlacionados <- rbind(Correlacionados,AuxCorreAgno)

  
  j <- j + 1
  AgnoRev <- 0
  AgnoCorr <- 0.0
  
}

Correlacionados %>%
  arrange(desc(AgnoCorr)) %>%
  head(1)
#   1992 0.8972412

# P4) Para el año obtenido en la pregunta anterior, realice una breve
#     descrición de la distribución de la expectativa de vida lifeExp.
#     E indique lo siguiente:
# a) (1pt) ¿Cuál fue la esperanza de vida promedio considerando todos
#          los países registrados?
#
# R:



# b) (1pt) ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?
#
# R:


# c) (1pt) ¿Cuál es el país que en dicho año tuvo la mayor esperanza de vida?
#
# R:


# ###########################
# En busca de la normalidad
# ##########################
#
# Se sabe que una manera de estabilizar la variabilidad presente en una variable,
# es estudiando su logaritmo. A continuación usted deberá estudiar el
# comportamiento ddistribucional del loagritmo de la expectativa de vida.
#
# P5) Considerando todos los años de observación, determine lo sguiente:
# a) (2pts) Mediante el test de shapiro, indique los dos continentes que
#           presentan un comportamiento normal en la distribución del
#           logaritmo de lifeExp.
#
# R:


# b) (3pts) Para los continentes encontrados en a), grafique los histogramas
#           para el logaritmo de lifeExp. Considere añadir estimaciones de las
#           densidades, dadas por geom_density así como una densidad normal con
#           parámetros de media y vrianza igual a la media y desviación estandar
#           muestral. ¿Qué opina sobre el histograma de Oceanía?
#           Comente sobre posibles causas de su aspecto.
#
# R:


# c) (2pts) Complemente lo anterior, visualizando los qqplots para el logarimo
#           de lifeExp. Considere la utilización de las funciones
#           qqnorm() y qqline() para el contraste contra una distribución normal.
#
# R:


# P6) (3pts) Considerando el continente de África, y asumiendo normalidad en el
#            logaritmo de lifeExp. Independiente del año,
#            ¿cuál es la probabilidad de que la expectativa de vida (lifeExp)
#            sea superior a 54 años?
#
# R:


# ################################ Sección 2 ###################################
#
library("arules")
library("tidyverse")


# ###########
# Cafetería
# ##########
#
# A usted se le solicita analizar las ventas de los distintos productos
# ofrecidos por una cafetería. Para ello usted deberá generar una descripción
# de dichas ventas,utilizando estadísticos descriptivos, gráficos y además
# deberá incluir un análisis de la venta cruzada de los productos en cuestión.
#
#
#
# El siguiente archivo contiene los registros de las ventas de una cafetería en
# un determinado período. Ustede deberá responder las siguientes preguntas con
# el objetivo de generar recomendaciones de ventas en distintos períodos de tiempo.
#
# trans_original <- read.csv("cafeteria.csv")
#
# P1) (2pts) Genere tres nuevas columnas, que contengan la hora, minutos y
#            segundos de la transacción registrada. A modo de referencia,
#            su tabla debería contener al menos los siguientes campos.
#
#
#     Date          hora  min   seg   Transaction   Item
#     ----------    ----  ---   ---   -----------   --------------
#     2016-10-30      09   58    11             1   Bread
#     2016-10-30      10   05    34             1   Scandinavian
#     2016-10-30      10   05    24             1   Scandinavian
#     2016-10-30      10   07    57             3   Hot chocolate
#     2016-10-30      10   07    57             3   Jam
#     2016-10-30      10   07    57             3   Cookies
#
# De ser de utilidad, puede consultar la documentación de las funciones
# substr y separate de los packages base y tidyr respectivamente.
#
#
# R:


# P2) (4pts) Genere una tabla resumen que contenga la siguiente información.
#
#     hora: Hora donde se registraron las transacciones. por ejemplo, el valor
#           09 indica el bloque horario comprendido entre las 09:00 y 09:59 hrs.
#     total_trx : total de transacciones distintas generadas en el bloque horario
#                 respectivo.
#     total_items: total de items vendidos en el bloque horario respectivo.
#     total_items_unicos: total de items únicos venidos en el bloque horario
#                         respectivo.
#
# R:


# P3) Con base en la tabla anterior, diremos que una hora pertenece al horario
#     punta si la cantidad de transacciones distintas generadas en dicho
#     bloque supera las 1000 transacciones.
#
# a) (1pt) ¿Qué horas comprende el horario punta?
#
# R:


# b) (2pt) En promedio, ¿cuántas transacciones distintas por hora se dieron
#          en horario punta?¿y en horario no punta?
#
# R:



# P4) Se sabe que el total de personal disponible es capaz de atender como
#     máximo, 1300 transacciones por hora, de modo que no se "sature"
#     el sistema y que los tiempos de espera de los clientes sean razonables.
#     Asumiendo que la cantidad de transacciones por hora tiene una
#     distribución Poisson con parámetro $\lambda$ igual al estimado
#     en la pregunta 3.b responda lo siguiente:
#
# a) (2pt) ¿Cuál es la probabilidad de que en horario punta se den más de
#           1300 transacciones en una hora? ¿Cómo interpretaría este valor?
#           Comente.
#
# R:


# b) (1pt) Con el objetivo de reducir costos, se propone limitar el personal
#          disponible a modo de poder atender como máximo 1250 transacciones
#          por hora. ¿Que tan probable es que se supere este máximo de
#          transacciones por hora ? ¿Recomendaría usted esta medida?
#
# R:


# c) (2pt) Usted sugiere modificar la cantidad de personal pero teniendo en
#          cuenta de que se garantice la atención de al menos un 95% de las
#          transacciones por hora. ¿Cuántas transacciones por hora se deberían
#          poder gestionar en este escenario?
#
# R:


# ####################################
# Análisis de los productos vendidos.
# ####################################
#
# Con el objetivo de aumentar las ventas, se le solicita a usted analizar
# los itmes y las ventas cruzadas entre los productos ofrecidos. Para ello
# usted guía su análisis en función de las siguientes preguntas.
#
# P5) (2pt) ¿Cuáles son los 5 items más vendidos? Ilustre mediante un
#            gráfico de barras o una tabla.
#
# R:



# P6) (4pts) ¿Cambian estos 5 ítems según el horario de atención? Para ello
#             muestre los 5 items más vendidos en los siguientes horarios.
#
#             - 7:00-11:59
#             - 12:00-16:59
#             - 17:00-23:59
#
#
# R:



# #######################
# Genereación de reglas
# ######################
# P7) Considerando un support mínimo de 0.02 , un confidence mínimo de 0.1 y
#     teniendo en cuenta que no se deben considerar reglas de asociación cuyo
#     antecedente o consecuente sean vacíos,
#
# a) (2pts) ¿Cuál es la regla de asociación más frecuente en cada uno de
#            los horarios indicados en P3)?
#
# R:



# b) (2pts) ¿Cuál es la regla de asociación con mayor confidence en cada uno
#            de los horarios indicados en P3)?
#
# R:


# c) (2pts) ¿Cuál es la regla de asociación con mayor lift en cada uno de los
#            horarios indicados en P3)?
#
#
# ##############################################################################
# NOTA: En caso de no obtener reglas con los parámetros indicados, modifíquelos,
#       pero tenga en consideración este hecho al momento de argumentar
#       las preguntas posteriores.
# ##############################################################################
#
#
# R:


# P8) Se quiere potenciar un segundo producto por la compra de un café en los
#     tres horarios definidos previamente.
#
# a) (3pts) Genere tres listados (uno por cada rango horario) con todas las
#           reglas que contengan el producto Coffee en el antecedente.
#
# R:


# b) (2pts) ¿Qué promoción recomendaría en cada horario por la compra de un
#           café? Justifique su respuesta basándose en los
#           indicadores support, confidence y lift.
#
# R:
