### Control 3: Evaluación Final ###
#
# Nombre Integrantes : Macarena Muñoz Olave
#                      Carlos Aravena De Los R�?os
# Curso              : Introduccion a R 2020-2
# Sigla              : DBDN-R-C3

####### Librerias a utilizar
library("gapminder")
library("dplyr")
library("ggplot2")
library("arules")
library("tidyverse")

# ################################ Sección 1 ###################################
#
# ##############
# Introducción
# #############
#
# En esta seción usted generará un breve anáisis descriptivo de la evolución de
# la expectativa de vida de diferentes pa�?ses. Para ello usted utilizará la
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

#Carga de data
data(gapminder)
head(gapminder)

# P1) (2pts) El siguiente representa la relación entre el el ingreso GDP y
#            la esperanza de vida para todos los pa�?ses a lo largo de todos
#            los años, adicionalmente el tamaño de cada punto está en proporción
#            con la poblción total de cada pa�?s. Adicionalmente, el color var�?a
#            en función del año del registro. En el gráfico se pueden apreciar
#            observaciones con alto GDP (aquellas encerradas en el recuadro rojo).
#            Identif�?que dichas observaciones e indique claramente a qué
#            pa�?s(es) y año(s) corresponden.
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

gapminder%>%
        filter(gdpPercap>57000 & lifeExp > 50 & lifeExp <71)

#R: Las observaciones corresponde al pa�s Kuwait y los a�os corresponden a:
#1952,1957, 1962, 1967, 1972, 1977.


# P2) (3pts) Mediante un gráfico de puntos, visualice una comparativa entre
#            la relación de ingresos y expectativa de vida, para los
#            años 1952 y 2007. Para ello usted deberá replicar el siguiente
#            gráfico, donde el color representa a un continente distinto y
#            el tamaño está dado por el total de población.
#   
#
# R:

gapminder %>%
        filter((year == 1952) | (year==2007))%>%
        ggplot()+
        aes(x=gdpPercap,y=lifeExp,color=continent)+
        geom_point(mapping = aes(x=gdpPercap,y=lifeExp,size=pop))+
        facet_wrap(~year)



# P3) (2pts) Determine el nivel de correlación de spearman, entre las variables
#            gdpPercap y lifeExp para cada uno de los años registrados.
#            ¿En qué año se observa el mayor nivel de correlación entre
#            ambas variables?
#
# R:
library(stats)

gapminder%>%
        group_by(year)%>%
        summarise(correlacion=cor(gdpPercap,lifeExp,method = "spearman"))%>%
        arrange(-correlacion)%>%
        head(1)
       
#R: El a�o 1992 se observa el mayor nivel de correlaci�n entre ambas
#variables, alcanzado un valor de 0.897


# P4) Para el año obtenido en la pregunta anterior, realice una breve
#     descrición de la distribución de la expectativa de vida lifeExp.
#     E indique lo siguiente:
# a) (1pt) ¿Cuál fue la esperanza de vida promedio considerando todos
#          los pa�?ses registrados?
#
# R:

vida<-gapminder%>%
        filter(year==1992)
mean(vida$lifeExp)

# #Adicional, para visualizar como se distribuye la variable
# hist(x=vida$lifeExp,freq = T,main = "Histograma de edad a�o 1992",
#      xlab = "Edad",ylab = "Frecuencia",
#      col="gray")


#R: La esperanza de vida es 64.16034


# b) (1pt) ¿Cuál es el pa�?s que en dicho año tuvo la mayor esperanza de vida?
#
# R:
gapminder%>%
        filter(year==1992)%>%
        group_by(country)%>%
        summarise(esperanza=mean(lifeExp))%>%
        arrange(-esperanza)%>%
        head(1)


#R: El pa�s con la mayor esperanza de vida fue Jap�n, alcanzando un valor de 79.4



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

gapminder%>%
        group_by(continent)%>%
        summarise(W=shapiro.test(log(lifeExp))$statistic,
                 valor_p=shapiro.test(log(lifeExp))$p.value)%>%
        filter(valor_p>=0.05)

      
#R: Los dos continentes que presentan una distribuci�n normal son: �frica y 
#Ocean�a, donde el valor p del test de shapiro-Wilk es mayor a 0.05, por lo
#tanto no existe evidencia estad�sticamente significativa para rechazar la
#hip�tesis nula, esto es que la variable en estudio tenga una distribuci�n
#normal.


# b) (3pts) Para los continentes encontrados en a), grafique los histogramas
#           para el logaritmo de lifeExp. Considere añadir estimaciones de las
#           densidades, dadas por geom_density as�? como una densidad normal con
#           parámetros de media y vrianza igual a la media y desviación estandar
#           muestral. ¿Qué opina sobre el histograma de Ocean�?a?
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


#R: El historgrama de Ocean�a visualmente no parece tener una distribuci�n
#normal, sin embargo esto puede ser a causa de los pocos pa�ses considerados
#en la data.


# c) (2pts) Complemente lo anterior, visualizando los qqplots para el logaritmo
#           de lifeExp. Considere la utilización de las funciones
#           qqnorm() y qqline() para el contraste contra una distribución normal.
#
# R:

#�frica
qqnorm(log(aux_africa$lifeExp))
qqline(log(aux_africa$lifeExp))

#Ocean�a
qqnorm(log(aux_oceania$lifeExp))
qqline(log(aux_oceania$lifeExp))

#R: En el gr�fico de qqnorm y qqline de �frica se observa una gran cantidad de
#datos y visualmente se podr�a decir que el logaritmo de lifeExp tiene una
#una distribuci�n normal. 
#En el gr�fico qqnorm y qqline de Ocean�a se observan pocos datos y adem�s
#algunos posibles puntos at�picos, no es posible afirmar visualmente que
# el logaritmo de la variable lifeExp se distribuya normal, sin embargo
#al realizar el test de shapiro, el valor p no es menor a 0.05, por lo que
#no existe evidencia estad�sticamente significativa para rechazar la hip�tesis
#nula, entonces podemos asumir que esta variable tiene una disribuci�n normal.


# P6) (3pts) Considerando el continente de África, y asumiendo normalidad en el
#            logaritmo de lifeExp. Independiente del año,
#            ¿cuál es la probabilidad de que la expectativa de vida (lifeExp)
#            sea superior a 54 años?
#
# R:

africa<- gapminder%>%
        filter(continent=="Africa")

1-pnorm(log(54),mean=mean(log(africa$lifeExp)),sd=sd(log(africa$lifeExp)))


#R: La probabilidad de que la expectativa de vida (lifeExp)
#sea superior a 54 a�os es de 0.2634829.



# ################################ Sección 2 ###################################
#
# ###########
# Cafeter�?a
# ##########
#
# A usted se le solicita analizar las ventas de los distintos productos
# ofrecidos por una cafeter�?a. Para ello usted deberá generar una descripción
# de dichas ventas,utilizando estad�?sticos descriptivos, gráficos y además
# deberá incluir un análisis de la venta cruzada de los productos en cuestión.
#
#
#
# El siguiente archivo contiene los registros de las ventas de una cafeter�?a en
# un determinado per�?odo. Ustede deberá responder las siguientes preguntas con
# el objetivo de generar recomendaciones de ventas en distintos per�?odos de tiempo.
#
# trans_original <- read.csv("cafeteria.csv")
#

#Carga de archivo
ruta<-getwd()
rutaC<-paste(ruta,"/datasets/cafeteria.csv", sep = "")
trans_original <- read.csv(rutaC)
#View(trans_original)

#Limpieza de items "NONE" en trans_original, debido a que no aportan informaci�n
#relevante al an�lisis

trans_original<-trans_original%>%
        filter(Item != "NONE")

#View(trans_original)

# P1) (2pts) Genere tres nuevas columnas, que contengan la hora, minutos y
#            segundos de la transacción registrada. A modo de referencia,
#            su tabla deber�?a contener al menos los siguientes campos.
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

trans_original<-trans_original%>%
        separate(Time,c("hora","min","seg"),sep = ":",convert = F)

head(trans_original)

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
# a) (1pt) ¿Qué horas comprende el horario punta?
#
# R:

resumen%>%
        filter(total_trx>1000)%>%
        select(hora)%>%
        arrange((hora))

#R: El horio punta comprende entre 9 y 14 horas.


# b) (2pt) En promedio, ¿cuántas transacciones distintas por hora se dieron
#          en horario punta?¿y en horario no punta?
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
#     máximo, 1300 transacciones por hora, de modo que no se "sature"
#     el sistema y que los tiempos de espera de los clientes sean razonables.
#     Asumiendo que la cantidad de transacciones por hora tiene una
#     distribución Poisson con parámetro $\lambda$ igual al estimado
#     en la pregunta 3.b responda lo siguiente:
#
# a) (2pt) ¿Cuál es la probabilidad de que en horario punta se den más de
#           1300 transacciones en una hora? ¿Cómo interpretar�?a este valor?
#           Comente.
#
# R:
#Sea X igual a la cantidad de transacciones que se procesan en una hora
#lambda igual a la cantidad de transacciones procesadas en hora punta
#entonces la pregunta: P(X>1300) = 1- P(X<=1300)

lambda_punta=mean(trx_horario_punta$total_trx)
1-ppois(1300,lambda = lambda_punta)


#R: La probabilidad de que el n�mero de transacciones por hora sea mayor
# a 1300 y que por tanto el sistema se sature y que los tiempos de espera
# de los clientes ya no sean razonabes es de un 0.008512964


# b) (1pt) Con el objetivo de reducir costos, se propone limitar el personal
#          disponible a modo de poder atender como máximo 1250 transacciones
#          por hora. ¿Que tan probable es que se supere este máximo de
#          transacciones por hora ? ¿Recomendar�?a usted esta medida?
#
# R:
# La pregunta es interpretada como: P(X>1250)=1-P(X<=1250)


#En horario no punta se tiene:
lambda_no_punta=mean(trx_horario_no_punta$total_trx)
1-ppois(1250,lambda = lambda_no_punta)

#En horario punta se tiene:
1-ppois(1250,lambda = lambda_punta)


#R: La probabilidad de que el n�mero de transacciones por hora sea superior
#a 1250 transacciones por hora es de un 16,476% en horario punta, en tanto que
#en el horario no punta la probabilidad es 0%
#Con la informaci�n disponible, no recomendar�a la medida, 
#ya que existe una probabilidad de un 16,476%
#de que el n�mero de transacciones por hora sea superior y no tendr�amos la 
#capacidad de procesar estas transacciones, lo que impactar�a en 
#la calidad del servicio prestado a nuestros clientes.


# c) (2pt) Usted sugiere modificar la cantidad de personal pero teniendo en
#          cuenta de que se garantice la atención de al menos un 95% de las
#          transacciones por hora. ¿Cuántas transacciones por hora se deber�?an
#          poder gestionar en este escenario?
#
# R:

#En horario punta se tiene:
ppois(1274,lambda = lambda_punta)

#En horario no punta se tiene:
ppois(203,lambda = lambda_no_punta)

#R: Mediante prueba y error, se garantiza que se gestiorar�an 
#1274 transacciones por hora en horario punta y 203 transacciones por hora
#en horario no punta, .


# ####################################
# Análisis de los productos vendidos.
# ####################################
#
# Con el objetivo de aumentar las ventas, se le solicita a usted analizar
# los itmes y las ventas cruzadas entre los productos ofrecidos. Para ello
# usted gu�?a su análisis en función de las siguientes preguntas.
#
# P5) (2pt) ¿Cuáles son los 5 items más vendidos? Ilustre mediante un
#            gráfico de barras o una tabla.
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
                title="Top 5 items m�s vendidos",
                subtitle = "Considera todo el horario de operaci�n"
        )



# P6) (4pts) ¿Cambian estos 5 �?tems según el horario de atención? Para ello
#             muestre los 5 items más vendidos en los siguientes horarios.
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
#Entre 7:00 y 11:59 los 5 items m�s vendidos son:
tramoI
#Entre 12:00 y 16:59 los 5 items m�s vendidos son:
tramoII
#Entre 17:00 y 23:59 los 5 items m�s vendidos son:
tramoIII

# S�, cambian los �tems, espec�ficamente desde la tercera posici�n, 
#para el tramo I, ingresando a la tercera posici�n "Pastry", cuarta
#posici�n "Tea" y quinta posici�n "Medialuna".
#Para el tramo II y III s�lo cambia la quinta posici�n,
#ingresa "Sandwich" y "Hot chocolate" en vez de "Pastry", respectivamente.
#Cabe indicar que para todos los tramos las dos primeras posiciones se 
#mantienen con "Coffee" y "Bread".



# #######################
# Genereación de reglas
# ######################
#
#Para realizar un an�lisis de las reglas de asociaci�n, se procede
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


# P7) Considerando un support m�?nimo de 0.02 , un confidence m�?nimo de 0.1 y
#     teniendo en cuenta que no se deben considerar reglas de asociación cuyo
#     antecedente o consecuente sean vac�?os,
#
# a) (2pts) ¿Cuál es la regla de asociación más frecuente en cada uno de
#            los horarios indicados en P6)?
#
# R:
#Generaci�n de reglas seg�n tramo.
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
                minlen = 2 #se excluyen aquellos que no tienen antecedente o consecuente
                ),
        control = list (verbose = F)
        )
top_1_rango_2_supp<-sort(regla_2,by="support",decreasing = T)[1:2]

#inspect(top_1_rango_2_supp)

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
#inspect(regla_3)

# Registros en el tramo: 7:00-11:59
inspect(top_1_rango_1_supp)
#summary(regla_1)
#La regla m�s frecuente en este tramo es: {Bread}->{Coffee} y {Coffee}->{Bread}
#ambas con el mismo support

# Registros en el tramo: 12:00-16:59
inspect(top_1_rango_2_supp)
#La regla m�s frecuente en este tramo es: {Bread}->{Coffee} y {Coffee}->{Bread}
#ambas con el mismo support

# Registros en el tramo: 17:00-23:59
inspect(top_1_rango_3_supp)
#La regla m�s frecuente en este tramo es: {Cake}->{Coffee} y {Coffee}->{Cake}
#ambas con el mismo support

# b) (2pts) ¿Cuál es la regla de asociación con mayor confidence en cada uno
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
#con un confidence de 0.6


# c) (2pts) ¿Cuál es la regla de asociación con mayor lift en cada uno de los
#            horarios indicados en P6)?
#
#
# ##############################################################################
# NOTA: En caso de no obtener reglas con los parámetros indicados, modif�?quelos,
#       pero tenga en consideración este hecho al momento de argumentar
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


# P8) Se quiere potenciar un segundo producto por la compra de un café en los
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
inspect(regla_1_coffee)

# Registros en el tramo: 12:00-16:59
inspect(regla_2_coffee)

# Registros en el tramo: 12:00-16:59
inspect(regla_3_coffee)



# b) (2pts) ¿Qué promoción recomendar�?a en cada horario por la compra de un
#           café? Justifique su respuesta basándose en los
#           indicadores support, confidence y lift.
#
# R:

#Registros en el tramo: 7:00-11:59
#Considerando un support m�nimo de 0.02 y un confidence m�nimo de 0.1, donde con 
#el primer par�metro controlamos la frecuencia relativa de la asociaci�n, en tanto
#que con el par�metro confidence controlamos la probabilidad de que ocurra
#el consecuente dado que ocurri� el antecedente, entonces se tiene:

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

#Revisi�n de support de item medialuna
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


#En este horario recomendar�a una promoci�n de Coffee y Medialuna, debido a que
#el lift alcanzado por esta regla es de un 1.144 esto significa que la probabilidad
#de comprar medialuna se ve aumentada dado que ya compraron caf�, espec�ficamente
#la probabilidad de comprar medialuna aumenta desde un 0.09263774 a un 0.1060104.


# Registros en el tramo: 12:00-16:59
#Considerando un support m�nimo de 0.02 y un confidence m�nimo de 0.1, donde con 
#el primer par�metro controlamos la frecuencia relativa de la asociaci�n, en tanto
#que con el par�metro confidence controlamos la probabilidad de que ocurra
# el consecuente dado que ocurri� el antecedente, entonces se tiene:
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

#Revisi�n de support de item sandwich
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

#En este horario recomendar�a una promoci�n de Coffee y Sandwich, debido a que
#el lift alcanzado por esta regla es de un 1.168 esto significa que la probabilidad
#de comprar sandwich se ve aumentada dado que ya compraron caf�, espec�ficamente
#la probabilidad de comprar sandwich aumenta desde un 0.1159363 a un 0.1354701.


# Registros en el tramo: 17:00-23:59
#Considerando un support m�nimo de 0.02 y un confidence m�nimo de 0.1, donde con 
#el primer par�metro controlamos la frecuencia relativa de la asociaci�n, en tanto
#que con el par�metro confidence controlamos la probabilidad de que ocurra
# el consecuente dado que ocurri� el antecedente, entonces se tiene:
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

#Revisi�n de support de item cake
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

#En este horario recomendar�a una promoci�n de Coffee y Cake, debido a que
#el lift alcanzado por esta regla es de un 2.0800 esto significa que la probabilidad
#de comprar cake se ve aumentada dado que ya compraron caf�, espec�ficamente
#la probabilidad de comprar cake aumenta desde un 0.1025641 a un 0.2133333.



