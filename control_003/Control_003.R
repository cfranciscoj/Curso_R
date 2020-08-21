### Control 3: EvaluaciÃ³n Final ###
#
# Nombre Integrantes : Macarena MuÃ±oz Olave
#                      Carlos Aravena De Los RÃ?os
# Curso              : Introduccion a R 2020-2
# Sigla              : DBDN-R-C3

####### Librerias a utilizar
library("gapminder")
library("dplyr")
library("ggplot2")
library("arules")
library("tidyverse")

# ################################ SecciÃ³n 1 ###################################
#
# ##############
# IntroducciÃ³n
# #############
#
# En esta seciÃ³n usted generarÃ¡ un breve anÃ¡isis descriptivo de la evoluciÃ³n de
# la expectativa de vida de diferentes paÃ?ses. Para ello usted utilizarÃ¡ la
# informaciÃ³n disponible en el dataset gapminder del package del mismo nombre.
# La informaciÃ³n contenida en este dataset corresponde a la siguiente:
#
#     - country: Factor con 142 niveles
#     - continent: Factor con 5 niveles
#     - year: Rangos de aÃ±os desde 1952 to 2007 en incrementos de 5 aÃ±os
#     - lifeExp: Esperanza de vida al nacer, en aÃ±os
#     - pop: PoblaciÃ³n
#     - gdpPercap: GDP per capita (US$, ajustado por inflaciÃ³n)
#

#Carga de data
data(gapminder)
head(gapminder)

# P1) (2pts) El siguiente representa la relaciÃ³n entre el el ingreso GDP y
#            la esperanza de vida para todos los paÃ?ses a lo largo de todos
#            los aÃ±os, adicionalmente el tamaÃ±o de cada punto estÃ¡ en proporciÃ³n
#            con la poblciÃ³n total de cada paÃ?s. Adicionalmente, el color varÃ?a
#            en funciÃ³n del aÃ±o del registro. En el grÃ¡fico se pueden apreciar
#            observaciones con alto GDP (aquellas encerradas en el recuadro rojo).
#            IdentifÃ?que dichas observaciones e indique claramente a quÃ©
#            paÃ?s(es) y aÃ±o(s) corresponden.
#
# ##############################################################################
# NOTA: Usted tiene libertad de escoger el mÃ©todo con el cual identificar
#       dichas observaciones.
#       GrÃ¡fico 001
# ##############################################################################
#
# R:
gapminder%>%
        filter(gdpPercap>55000 & lifeExp > 50 )


# P2) (3pts) Mediante un grÃ¡fico de puntos, visualice una comparativa entre
#            la relaciÃ³n de ingresos y expectativa de vida, para los
#            aÃ±os 1052 y 2007. Para ello usted deberÃ¡ replicar el siguiente
#            grÃ¡fico, donde el color representa a un continente distinto y
#            el tamaÃ±o estÃ¡ dado por el total de poblaciÃ³n.
#            Grafico 002
#
# R:

#View(gapminder)

gapminder %>%
        filter((year == 1952) | (year==2007))%>%
        ggplot()+
        aes(x=gdpPercap,y=lifeExp,color=continent)+
        geom_point(mapping = aes(x=gdpPercap,y=lifeExp,size=pop))+
        facet_wrap(~year)


#write.csv2(gapminder, "gapminderoriginal.csv", row.names = F)

# P3) (2pts) Determine el nivel de correlaciÃ³n de spearman, entre las variables
#            gdpPercap y lifeExp para cada uno de los aÃ±os registrados.
#            Â¿En quÃ© aÃ±o se observa el mayor nivel de correlaciÃ³n entre
#            ambas variables?
#
# R:
library(stats)
#library(corrplot)

gapminder%>%
        group_by(year)%>%
        summarise(correlacion=cor(gdpPercap,lifeExp,method = "spearman"))%>%
        arrange(-correlacion)%>%
        head(1)
       
#R: El año 1992 se observa el mayor nivel de correlación entre ambas
#variables, alcanzado un valor de 0.897


# P4) Para el aÃ±o obtenido en la pregunta anterior, realice una breve
#     descriciÃ³n de la distribuciÃ³n de la expectativa de vida lifeExp.
#     E indique lo siguiente:
# a) (1pt) Â¿CuÃ¡l fue la esperanza de vida promedio considerando todos
#          los paÃ?ses registrados?
#
# R:

vida<-gapminder%>%
        filter(year==1992)

hist(x=vida$lifeExp,freq = T,main = "Histograma de edad",
     xlab = "Edad",ylab = "Frecuencia",
     col="gray")


mean(vida$lifeExp)

#R: La esperanza de vida es 64.16


# b) (1pt) Â¿CuÃ¡l es el paÃ?s que en dicho aÃ±o tuvo la mayor esperanza de vida?
#
# R:
gapminder%>%
        filter(year==1992)%>%
        group_by(country)%>%
        summarise(esperanza=mean(lifeExp))%>%
        arrange(-esperanza)%>%
        head(1)


#R: El país con la mayor esperanza de vida fue Japón

# ###########################
# En busca de la normalidad
# ##########################
#
# Se sabe que una manera de estabilizar la variabilidad presente en una variable,
# es estudiando su logaritmo. A continuaciÃ³n usted deberÃ¡ estudiar el
# comportamiento ddistribucional del loagritmo de la expectativa de vida.
#
# P5) Considerando todos los aÃ±os de observaciÃ³n, determine lo sguiente:
# a) (2pts) Mediante el test de shapiro, indique los dos continentes que
#           presentan un comportamiento normal en la distribuciÃ³n del
#           logaritmo de lifeExp.
#
# R:

gapminder%>%
        group_by(continent)%>%
        summarise(W=shapiro.test(log(lifeExp))$statistic,
                 valor_p=shapiro.test(log(lifeExp))$p.value)

      
#R: Los dos continentes que presentan una distribución normal son: África y 
#Oceanía, donde el valor p del test de shapiro-Wilk es mayor a 0.05, por lo
#tanto no existe evidencia estadísticamente significativa para rechazar la
#hipótesis nula, esto es que la variable en estudio tenga un comportamiento
#normal.


# b) (3pts) Para los continentes encontrados en a), grafique los histogramas
#           para el logaritmo de lifeExp. Considere aÃ±adir estimaciones de las
#           densidades, dadas por geom_density asÃ? como una densidad normal con
#           parÃ¡metros de media y vrianza igual a la media y desviaciÃ³n estandar
#           muestral. Â¿QuÃ© opina sobre el histograma de OceanÃ?a?
#           Comente sobre posibles causas de su aspecto.
#
# R:


aux_africa<-gapminder%>%
        filter(continent=="Africa")

aux_africa%>%
        ggplot()+
        aes(x=log(lifeExp))+
        geom_histogram(aes(y=..density..))+
        stat_function(fun=function(x){
                dnorm(x,mean=mean(log(aux_africa$lifeExp)),
                      sd=sd(log(aux_africa$lifeExp)))
        },col="red")+
        geom_density(col="blue")


aux_oceania<-gapminder%>%
        filter(continent=="Oceania")


        
aux_oceania%>%
        ggplot()+
        aes(x=log(lifeExp))+
        geom_histogram(aes(y=..density..))+
        stat_function(fun=function(x){
                dnorm(x,mean=mean(log(aux_oceania$lifeExp)),
                      sd=sd(log(aux_oceania$lifeExp)))
        },col="red")+
        geom_density(col="blue")



#Adicional
aux_oceania%>%
        group_by(country)%>%
        summarise(n=n())

aux_africa%>%
        group_by(country)%>%
        summarise(n=n())

#R: El historgrama de Oceanía visualmente no parece tener una distribución
#normal, sin embargo esto puede ser a causa de los pocos países considerados
#en la data.

# c) (2pts) Complemente lo anterior, visualizando los qqplots para el logarimo
#           de lifeExp. Considere la utilizaciÃ³n de las funciones
#           qqnorm() y qqline() para el contraste contra una distribuciÃ³n normal.
#
# R:

#África
qqnorm(log(aux_africa$lifeExp))
qqline(log(aux_africa$lifeExp))

#Oceanía
qqnorm(log(aux_oceania$lifeExp))
qqline(log(aux_oceania$lifeExp))

#R: En el gráfico de qqnorm y qqline de África se observa una gran cantidad de
#datos y visualmente se podría decir que el logaritmo de lifeExp tiene una
#una distribución normal. 
#En el gráfico qqnorm y qqline de Oceanía se observan pocos datos y además
#algunos posibles puntos atípicos, no es posible afirmar visualmente que
# el logaritmo de la variable lifeExp se distribuya normal, sin embargo
#al realizar el test de shapiro, el valor p no es menor a 0.05, por lo que
#no existe evidencia estadísticamente significativa para rechazar la hipótesis
#nula, entonces podemos asumir que esta variable tiene una disribución normal.


# P6) (3pts) Considerando el continente de Ãfrica, y asumiendo normalidad en el
#            logaritmo de lifeExp. Independiente del aÃ±o,
#            Â¿cuÃ¡l es la probabilidad de que la expectativa de vida (lifeExp)
#            sea superior a 54 aÃ±os?
#
# R:

africa<- gapminder%>%
        filter(continent=="Africa")

1-pnorm(log(54),mean=mean(log(africa$lifeExp)),sd=sd(log(africa$lifeExp)))




# ################################ SecciÃ³n 2 ###################################
#
# ###########
# CafeterÃ?a
# ##########
#
# A usted se le solicita analizar las ventas de los distintos productos
# ofrecidos por una cafeterÃ?a. Para ello usted deberÃ¡ generar una descripciÃ³n
# de dichas ventas,utilizando estadÃ?sticos descriptivos, grÃ¡ficos y ademÃ¡s
# deberÃ¡ incluir un anÃ¡lisis de la venta cruzada de los productos en cuestiÃ³n.
#
#
#
# El siguiente archivo contiene los registros de las ventas de una cafeterÃ?a en
# un determinado perÃ?odo. Ustede deberÃ¡ responder las siguientes preguntas con
# el objetivo de generar recomendaciones de ventas en distintos perÃ?odos de tiempo.
#
# trans_original <- read.csv("cafeteria.csv")
#

#Carga de archivo
ruta<-getwd()
rutaC<-paste(ruta,"/datasets/cafeteria.csv", sep = "")
trans_original <- read.csv(rutaC)
View(trans_original)

#Limpieza de items "NONE" en trans_original, debido a que no aportan información
#relevante al análisis
trans_original<-trans_original%>%
        filter(Item != "NONE")

View(trans_original)

# P1) (2pts) Genere tres nuevas columnas, que contengan la hora, minutos y
#            segundos de la transacciÃ³n registrada. A modo de referencia,
#            su tabla deberÃ?a contener al menos los siguientes campos.
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
# De ser de utilidad, puede consultar la documentaciÃ³n de las funciones
# substr y separate de los packages base y tidyr respectivamente.
#
#
# R:

trans_original<-trans_original%>%
        separate(Time,c("hora","min","seg"),sep = ":",convert = F)

head(trans_original)

# P2) (4pts) Genere una tabla resumen que contenga la siguiente informaciÃ³n.
#
#     hora: Hora donde se registraron las transacciones. por ejemplo, el valor
#           09 indica el bloque horario comprendido entre las 09:00 y 09:59 hrs.
#     total_trx : total de transacciones distintas generadas en el bloque horario
#                 respectivo.
#     total_items: total de items vendidos en el bloque horario respectivo.
#     total_items_unicos: total de items Ãºnicos venidos en el bloque horario
#                         respectivo.
#
# R:

A<-trans_original%>%
        group_by(hora)%>%
        distinct(Transaction)%>%
        summarise(total_trx=n())%>%
        arrange((hora))


B<-trans_original%>%
        group_by(hora)%>%
        summarise(total_items=n())%>%
        arrange((hora))

C<-trans_original%>%
        group_by(hora)%>%
        distinct(Item)%>%
        summarise(total_items_unicos=n())%>%
        arrange((hora))


resumen<-A%>%
        left_join(B,by=c("hora"))%>%
        left_join(C,by=c("hora"))

names(resumen)=c("hora","total_trx","total_items","total_items_unicos") 

head(resumen)


# P3) Con base en la tabla anterior, diremos que una hora pertenece al horario
#     punta si la cantidad de transacciones distintas generadas en dicho
#     bloque supera las 1000 transacciones.
#
# a) (1pt) Â¿QuÃ© horas comprende el horario punta?
#
# R:

resumen%>%
        filter(total_trx>1000)%>%
        select(hora)%>%
        arrange((hora))

#R: El horio punta comprende entre 9 y 14 horas.


# b) (2pt) En promedio, Â¿cuÃ¡ntas transacciones distintas por hora se dieron
#          en horario punta?Â¿y en horario no punta?
#
# R:

trx_horario_punta<-resumen%>%
        filter(total_trx>1000)%>%
        select(total_trx)

trx_horario_no_punta<-resumen%>%
        filter(total_trx<=1000)%>%
        select(total_trx)

mean(trx_horario_punta$total_trx) #promedio en horario punta
mean(trx_horario_no_punta$total_trx) #promedio en horario no punta

#R: El promedio de transacciones en horario punta es 1216.5 transacciones, 
#en tanto que el horario no punta alcanza un promedio de 180.5 transacciones.


# P4) Se sabe que el total de personal disponible es capaz de atender como
#     mÃ¡ximo, 1300 transacciones por hora, de modo que no se "sature"
#     el sistema y que los tiempos de espera de los clientes sean razonables.
#     Asumiendo que la cantidad de transacciones por hora tiene una
#     distribuciÃ³n Poisson con parÃ¡metro $\lambda$ igual al estimado
#     en la pregunta 3.b responda lo siguiente:
#
# a) (2pt) Â¿CuÃ¡l es la probabilidad de que en horario punta se den mÃ¡s de
#           1300 transacciones en una hora? Â¿CÃ³mo interpretarÃ?a este valor?
#           Comente.
#
# R:
#Sea X igual a la cantidad de transacciones que se procesan en una hora
#lambda igual a la cantidad de transacciones procesadas en hora punta
#entonces la pregunta: P(X>1300) = 1- P(X<=1300)

#View(resumen)
lambda_punta=mean(trx_horario_punta$total_trx)
1-ppois(1300,lambda = lambda_punta)


#R: La probabilidad de que el número de transacciones por hora sea mayor
# a 1300 y que por tanto el sistema se sature y que los tiempos de espera
# de los clientes ya no sean razonabes es de un 0.008512964


# b) (1pt) Con el objetivo de reducir costos, se propone limitar el personal
#          disponible a modo de poder atender como mÃ¡ximo 1250 transacciones
#          por hora. Â¿Que tan probable es que se supere este mÃ¡ximo de
#          transacciones por hora ? Â¿RecomendarÃ?a usted esta medida?
#
# R:
# La pregunta es interpretada como: P(X>1250)=1-P(X<=1250)

#En horario punta se tiene:
1-ppois(1250,lambda = lambda_punta)

#En horario no punta se tiene:
lambda_no_punta=mean(trx_horario_no_punta$total_trx)
1-ppois(1250,lambda = lambda_no_punta)

#R: La probabilidad de que el número de transacciones por hora sea superior
#a 1250 transacciones por hora es de un 16,476% en horario punta, en tanto que
#en el horario no punta la probabilidad es 0%
#Con la información disponible, no recomendaría la medida, 
#ya que existe una probabilidad de un 16,476%
#de que el número de transacciones por hora sea superior y no tendríamos la 
#capacidad de procesar estas transacciones, lo que impactaría en 
#la calidad del servicio prestado a nuestros clientes.


# c) (2pt) Usted sugiere modificar la cantidad de personal pero teniendo en
#          cuenta de que se garantice la atenciÃ³n de al menos un 95% de las
#          transacciones por hora. Â¿CuÃ¡ntas transacciones por hora se deberÃ?an
#          poder gestionar en este escenario?
#
# R:

#En horario punta se tiene:
ppois(1274,lambda = lambda_punta)

#En horario no punta se tiene:
ppois(203,lambda = lambda_no_punta)

#Mediante prueba y error, se garantiza que se gestiorarían como máximo
#1274 transacciones por hora en horario punta y 564 transacciones por hora
#en horario no punta.


# ####################################
# AnÃ¡lisis de los productos vendidos.
# ####################################
#
# Con el objetivo de aumentar las ventas, se le solicita a usted analizar
# los itmes y las ventas cruzadas entre los productos ofrecidos. Para ello
# usted guÃ?a su anÃ¡lisis en funciÃ³n de las siguientes preguntas.
#
# P5) (2pt) Â¿CuÃ¡les son los 5 items mÃ¡s vendidos? Ilustre mediante un
#            grÃ¡fico de barras o una tabla.
#
# R:

trans_original %>%
        group_by(Item) %>%
        summarise(cant = n()) %>%
        arrange(desc(cant)) %>%
        head(5) %>%
        ggplot() +
        aes(x = reorder(Item,-cant), 
            y = cant, 
            fill = cant) +
        geom_bar(stat='identity')+
        labs(
                x="Item",
                y="Cantidad",
                fill="Cantidad",
                title="Top 5 items más vendidos",
                subtitle = "Considera todo el horario de operación"
        )



# P6) (4pts) Â¿Cambian estos 5 Ã?tems segÃºn el horario de atenciÃ³n? Para ello
#             muestre los 5 items mÃ¡s vendidos en los siguientes horarios.
#
#             - 7:00-11:59
#             - 12:00-16:59
#             - 17:00-23:59
#
#
# R:

trans_original$hora<-as.numeric(trans_original$hora)

#Sea:
#             - 7:00-11:59      ---> tramoI
#             - 12:00-16:59     ---> tramoII
#             - 17:00-23:59     ---> tramoIII

tramoI<-trans_original%>%
        filter(hora>=7 & hora<12)%>%
        group_by(Item)%>%
        summarise(cant=n())%>%
        arrange(desc(cant))%>%
        head(5)

tramoII<-trans_original%>%
        filter(hora>=12 & hora<17)%>%
        group_by(Item)%>%
        summarise(cant=n())%>%
        arrange(desc(cant))%>%
        head(5)

tramoIII<-trans_original%>%
        filter(hora>=17 & hora<=23)%>%
        group_by(Item)%>%
        summarise(cant=n())%>%
        arrange(desc(cant))%>%
        head(5)

#R: 
#Entre 7:00 y 11:59 los 5 items más vendidos son:
tramoI
#Entre 12:00 y 16:59 los 5 items más vendidos son:
tramoII
#Entre 17:00 y 23:59 los 5 items más vendidos son:
tramoIII

# Sí, cambian los ítems, específicamente desde la tercera posición, 
#para el tramo I, en el tramo II y III sólo cambia la quinta posición,
#ingresa "Sandwich" y "Hot chocolate" en vez de "Pastry", respectivamente.
#Cabe indicar que para todos los tramos las dos primeras posiciones se 
#mantienen con "Coffee" y "Bread".



# #######################
# GenereaciÃ³n de reglas
# ######################
#
#Para realizar un análisis de las reglas de asociación, se procede
#a transformar el "dataframe" trans_original a uno de tipo "transactional",
#como sigue:
# Registros en el tramo: 7:00-11:59
rango_1<-trans_original%>%
        filter(hora>=7 & hora<12)
trans_1<-as(split(rango_1[,"Item"],rango_1[,"Transaction"]),"transactions")


# Registros en el tramo: 12:00-16:59
rango_2<-trans_original%>%
        filter(hora>=12 & hora<17)
trans_2<-as(split(rango_2[,"Item"],rango_2[,"Transaction"]),"transactions")


# Registros en el tramo: 17:00-23:59
rango_3<-trans_original%>%
        filter(hora>=17 & hora<=23)
trans_3<-as(split(rango_3[,"Item"],rango_3[,"Transaction"]),"transactions")


# P7) Considerando un support mÃ?nimo de 0.02 , un confidence mÃ?nimo de 0.1 y
#     teniendo en cuenta que no se deben considerar reglas de asociaciÃ³n cuyo
#     antecedente o consecuente sean vacÃ?os,
#
# a) (2pts) Â¿CuÃ¡l es la regla de asociaciÃ³n mÃ¡s frecuente en cada uno de
#            los horarios indicados en P6)?
#
# R:
#Generación de reglas según tramo.
# Registros en el tramo: 7:00-11:59
regla_1 <- apriori(
        data = trans_1,
        parameter = list(
                supp = 0.02,
                conf = 0.1,
                minlen = 2 #se excluyen aquellos que no tienen antecedente o consecuente
                ),
        control = list (verbose = F)
        )
top_1_rango_1_supp<-sort(regla_1,by="support",decreasing = T)[1:2]
#inspect(regla_1)


# Registros en el tramo: 12:00-16:59
regla_2 <- apriori(
        data = trans_2,
        parameter = list(
                supp = 0.02,
                conf = 0.1,
                minlen=2 #se excluyen aquellos que no tienen antecedente o consecuente
                ),
        control = list (verbose = F)
        )
top_1_rango_2_supp<-sort(regla_2,by="support",decreasing = T)[1:2]

#inspect(regla_2)

# Registros en el tramo: 17:00-23:59
regla_3 <- apriori(
        data = trans_3,
        parameter = list(
                supp = 0.02,
                conf = 0.1,
                minlen=2 #se excluyen aquellos que no tienen antecedente o consecuente
                ),
        control = list (verbose = F)
        )
top_1_rango_3_supp<-sort(regla_3,by="support",decreasing = T)[1:2]
inspect(regla_3)

# Registros en el tramo: 7:00-11:59
inspect(top_1_rango_1_supp)
#La regla más frecuente en este tramo es: {Bread}->{Coffee} y {Coffee}->{Bread}
#ambas con el mismo support

# Registros en el tramo: 12:00-16:59
inspect(top_1_rango_2_supp)
#La regla más frecuente en este tramo es: {Bread}->{Coffee} y {Coffee}->{Bread}
#ambas con el mismo support

# Registros en el tramo: 17:00-23:59
inspect(top_1_rango_3_supp)
#La regla más frecuente en este tramo es: {Cake}->{Coffee} y {Coffee}->{Cake}
#ambas con el mismo support

# b) (2pts) Â¿CuÃ¡l es la regla de asociaciÃ³n con mayor confidence en cada uno
#            de los horarios indicados en P6)?
#
# R:
# Registros en el tramo: 7:00-11:59
top_1_rango_1_conf<-sort(regla_1,by="confidence",decreasing = T)[1]
inspect(top_1_rango_1_conf)
#La regla con mayor confidence es: {Toast}->{Coffee}
#con un confidence de 0.72

# Registros en el tramo: 12:00-16:59
top_1_rango_2_conf<-sort(regla_2,by="confidence",decreasing = T)[1]
inspect(top_1_rango_2_conf)
#La regla con mayor confidence es: {Pastry}->{Coffee}
#con un confidence de 0.558

# Registros en el tramo: 17:00-23:59
top_1_rango_3_conf<-sort(regla_3,by="confidence",decreasing = T)[1]
inspect(top_1_rango_3_conf)
#La regla con mayor confidence es: {Postcard}->{Tshirt}
#con un confidence de 0.60


# c) (2pts) Â¿CuÃ¡l es la regla de asociaciÃ³n con mayor lift en cada uno de los
#            horarios indicados en P6)?
#
#
# ##############################################################################
# NOTA: En caso de no obtener reglas con los parÃ¡metros indicados, modifÃ?quelos,
#       pero tenga en consideraciÃ³n este hecho al momento de argumentar
#       las preguntas posteriores.
# ##############################################################################
#
#
# R:
# Registros en el tramo: 7:00-11:59
top_1_rango_1_lift<-sort(regla_1,by="lift",decreasing = T)[1]
inspect(top_1_rango_1_lift)
#La regla con mayor lift es: {Toast}->{Coffee}
#con un lift de: 1.399

# Registros en el tramo: 12:00-16:59
top_1_rango_2_lift<-sort(regla_2,by="lift",decreasing = T)[1:2]
inspect(top_1_rango_2_lift)
#La regla con mayor lift es: {Cake}->{Tea} y {Tea}->{Cake}
#amba con un lift de: 1,405

# Registros en el tramo: 17:00-23:59
top_1_rango_3_lift<-sort(regla_3,by="lift",decreasing = T)[1:2]
inspect(top_1_rango_3_lift)
#La regla con mayor lift es: {Tshirt}->{Postcard} y {Postcard}->{Tshirt}
#ambas con un lift de: 7.800


# P8) Se quiere potenciar un segundo producto por la compra de un cafÃ© en los
#     tres horarios definidos previamente en P6.
#
# a) (3pts) Genere tres listados (uno por cada rango horario) con todas las
#           reglas que contengan el producto Coffee en el antecedente.
#
# R:

regla_1_coffee <- apriori(
        data = trans_1,
        parameter = list(
                supp = 0.0,
                conf = 0.0,
                minlen = 2
                ),
        appearance = list(lhs = "Coffee"),
        control = list (verbose = F)
        )

#inspect(regla_1_coffee)

regla_2_coffee <- apriori(
        data = trans_2,
        parameter = list(
                supp = 0.0,
                conf = 0.0,
                minlen = 2
                ),
        appearance = list(lhs = "Coffee"),
        control = list (verbose = F)
        )

regla_3_coffee <- apriori(
        data = trans_3,
        parameter = list(
                supp = 0.0,
                conf = 0.0,
                minlen = 2
                ),
        appearance = list(lhs = "Coffee"),
        control = list (verbose = F)
        )
# Registros en el tramo: 7:00-11:59
view(inspect(regla_1_coffee))

# Registros en el tramo: 12:00-16:59
view(inspect(regla_2_coffee))

# Registros en el tramo: 12:00-16:59
view(inspect(regla_3_coffee))



# b) (2pts) Â¿QuÃ© promociÃ³n recomendarÃ?a en cada horario por la compra de un
#           cafÃ©? Justifique su respuesta basÃ¡ndose en los
#           indicadores support, confidence y lift.
#
# R:

# Registros en el tramo: 7:00-11:59
#Considerando un support mínimo de 0.02 y un confidence mínimo de 0.1, donde con 
#el primer parámetro controlamos la frecuencia relativa de la asociación, en tanto
#que con el parámetro confidence controlamos que la probabilidad de que ocurra
#el consecuente dado que ocurrió el antecedente, entonces se tiene:

regla_1_coffee <- apriori(
        data = trans_1,
        parameter = list(
                supp = 0.02,
                conf = 0.1,
                minlen = 2
                ),
        appearance = list(lhs = "Coffee"),
        control = list (verbose = F)
        )

#Luego ordenamos por lift
inspect(sort(regla_1_coffee,by="lift",decreasing = T))

#Revisión de support de item medialuna
regla_1_coffee_rev <- apriori(
        data = trans_1,
        parameter = list(
                supp = 0.01,
                conf = 0.01,
                maxlen = 1
                ),
        appearance = list(rhs = "Medialuna"),
        control = list (verbose = F)
        )

inspect(regla_1_coffee_rev)


#En este horario recomendaría una promoción de Coffee y Medialuna, debido a que
#el lift alcanzado por esta regla es de un 1.144 esto significa que la probabilidad
#de comprar medialuna se ve aumentada dado que ya compraron café, específicamente
#la probabilidad de comprar medialuna aumenta desde un 0.09263774 a un 0.1060104.


# Registros en el tramo: 12:00-16:59
#Considerando un support mínimo de 0.02 y un confidence mínimo de 0.1, donde con 
#el primer parámetro controlamos la frecuencia relativa de la asociación, en tanto
#que con el parámetro confidence controlamos que la probabilidad de que ocurra
# el consecuente dado que ocurrió el antecedente, entonces se tiene:
regla_2_coffee <- apriori(
        data = trans_2,
        parameter = list(
                supp = 0.02,
                conf = 0.1,
                minlen = 2
                ),
        appearance = list(lhs = "Coffee"),
        control = list (verbose = F)
        )

#Luego ordenamos por lift
inspect(sort(regla_2_coffee,by="lift",decreasing = T))

#Revisión de support de item sandwich
regla_2_coffee_rev <- apriori(
        data = trans_2,
        parameter = list(
                supp = 0.01,
                conf = 0.01,
                maxlen = 1
                ),
        appearance = list(rhs = "Sandwich"),
        control = list (verbose = F)
        )

inspect(regla_2_coffee_rev)

#En este horario recomendaría una promoción de Coffee y Sandwich, debido a que
#el lift alcanzado por esta regla es de un 1.1818 esto significa que la probabilidad
#de comprar sandwich se ve aumentada dado que ya compraron café, específicamente
#la probabilidad de comprar sandwich aumenta desde un 0.1159363 a un 0.1354701.


# Registros en el tramo: 17:00-23:59
#Considerando un support mínimo de 0.02 y un confidence mínimo de 0.1, donde con 
#el primer parámetro controlamos la frecuencia relativa de la asociación, en tanto
#que con el parámetro confidence controlamos que la probabilidad de que ocurra
# el consecuente dado que ocurrió el antecedente, entonces se tiene:
regla_3_coffee <- apriori(
        data = trans_3,
        parameter = list(
                supp = 0.02,
                conf = 0.1,
                minlen = 2
                ),
        appearance = list(lhs = "Coffee"),
        control = list (verbose = F)
        )

#Luego ordenamos por lift
inspect(sort(regla_3_coffee,by="lift",decreasing = T))

#Revisión de support de item cake
regla_3_coffee_rev <- apriori(
        data = trans_3,
        parameter = list(
                supp = 0.01,
                conf = 0.01,
                maxlen = 1
                ),
        appearance = list(rhs = "Cake"),
        control = list (verbose = F)
        )

inspect(regla_3_coffee_rev)

#En este horario recomendaría una promoción de Coffee y Cake, debido a que
#el lift alcanzado por esta regla es de un 2.0800 esto significa que la probabilidad
#de comprar cake se ve aumentada dado que ya compraron café, específicamente
#la probabilidad de comprar cake aumenta desde un 0.1025641 a un 0.2133333.



