### Control 2: Manipulación de tablas y gráficos ###
#
# Nombre Integrantes : Carlos Aravena De Los Ríos
# Curso              : Introduccion a R 2020-2
# Sigla              : DBDN-R-C2

####### Librerias a utilizar
library("dplyr")
library("ggplot2")
library("magrittr")



### Sección 1
# En la ruta ./datasets se encuentran los siguientes arhivos, correspondiente
# a un sondeo de distintos restaurants de EE.UU y sus valoraciones.
# El archivo generalinfo.csv contiene información propia del restaurant,
# como tipo de comida ofrecida y valoración de los clientes entre otros.
#     - id_restaurant: Identificador del restaurant valorado.
#     - label: Nombre del restaurant.
#     - food_type: Tipo de comida ofrecida por el restaurant.
#     - review: Calificación promedio del restaurant otorgada por los usuarios.
#       (valor entre 0 y 1)
#
# Por otra parte, la información contenida en el archivo location.csv
# es la siguiente:
#     - id_rest: Identificador del restaurant valorado.
#     - street_num: Numeración de la ubicación del restaurant.
#     - street_name: Nombre de la calle donde está ubicado el restaurant.
#     - city: Ciudad donde se ubica el restaurant.
#
# Note que ambas tablas están relacionadas a través de los campos
# id_restaurant y id_rest.
#
#
### Preliminar
# Cargue los archivos indicados previamente en dos variables, una llamada
# general y otra location para generalinfo.csv y location.csv respectivamente.
RutaBase = "/home/carlos/repos/Curso_R/control_002" #getwd()
RutaGeneral <- paste(RutaBase, "/datasets/generalinfo.csv", sep = "")
general <- read.csv(RutaGeneral)
RutaLocation <- paste(RutaBase, "/datasets/location.csv", sep = "")
location <- read.csv(RutaLocation)



############################## Sección 1 ##############################
### Preguntas 1.1
# P1a) (1pt) Basándose en a tabla general, ¿cuántos restaurants distintos hay en total?
CantRestoranesDist <- general %>%
                        distinct(label) %>%
                        count()

print(paste("1a) La cantidad de restaurants distintos es: ", CantRestoranesDist))
#R: "1a) La cantidad de restaurants distintos es:  7606"

# P1b) (1pt) ¿En cuántos tipos de comida diferentes se clasifican los restaurants?
TipoComida <- general %>%
                distinct(food_type) %>%
                count()

print(paste("1b) La Cantidad de tipos de comida diferentes que se clasifican los restaurants son: ", TipoComida))
# R: "1b) La Cantidad de tipos de comida diferentes que se clasifican los restaurants son:  145"


# P1c) (2pt) ¿Cuántas ciudades distintas considera el sondeo?
CantCiudadesDist <- location %>%
                      distinct(city) %>%
                      count()

print(paste("1c) La cantidad de ciudades distintas considera el sondeo es: ", CantCiudadesDist))
# R: "1c) La cantidad de ciudades distintas considera el sondeo es:  167"


# P1d) (2pt) ¿Indique el tipo de comida y las ciudades donde se encuentra el restaurant
#             "great wall restaurant"?
print("1d) El tipo de comida y ciudad de restaurant 'great wall restaurant' es: ")
general %>%
  inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
  filter(label == "great wall restaurant") %>%
  select(food_type, city)
# R:  food_type    city
# 1   chinese      san francisco
# 2   chinese      san leandro

# P1e) (2pt) ¿Cuántos restaurantes de la ciudad de san francisco tienen calificación
#             mayor o igual a 3.8 y venden comida vegetariana (vegetarian) ?
#
CantRestVegan <- general %>%
                   inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                   filter(city == "san francisco" & food_type == "vegetarian" & review >= 3.8) %>%
                   count()
print(paste("1e) La cantidad de restaurantes en la ciudad de san francisco que tienen calificación mayor o igual a 3.8 y venden comida vegetariana es:", CantRestVegan))
# R: 1e) La cantidad de restaurantes en la ciudad de san francisco que tienen calificación mayor o igual a 3.8 y venden comida vegetariana es: 3"


### Preguntas 1.2
# P2a) (2pt) Sin considerar San Francisco, ¿cuál es la ciudad con mayor cantidad
#            de restaurantes sondeados?
CiudadMayorSondeo <- location %>%
                       filter(city != "san francisco") %>%
                       group_by(city) %>%
                       summarise(cantidad = n(), .groups = 'drop') %>%
                       filter(cantidad == max(cantidad))

print(paste("2a) la ciudad con mayor cantidad de restaurantes sondeados, sin considerar San Francisco es:", CiudadMayorSondeo$city))
#R: "2a) la ciudad con mayor cantidad de restaurantes sondeados, sin considerar San Francisco es: san jose"



# P2b) (1pt) ¿Cuáles son los 3 tipos de comida ofrecido más comunes ?
Comunes <- general %>%
             group_by(food_type) %>%
             summarise(cantidad = n(), .groups = 'drop') %>%
             arrange(desc(cantidad))

print("2b) Los 3 tipos de comida ofrecido más comunes son: ")
head(Comunes, 3)
# R: 2b) Los 3 tipos de comida ofrecido más comunes son:
#   food_type cantidad
# 1 cafe      1098
# 2 chinese   1075
# 3 pizza     959


# P2c) (2pt) Sin considerar San Francisco, ¿Cuáles son las 3 ciudades con mayor
#            cantidad de restaurants que ofrecen comido tipo japanese?
TipoJaponesa <- general %>%
                  inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                  filter(city != "san francisco" & food_type == "japanese") %>%
                  group_by(city) %>%
                  summarise(cantidad = n(), .groups = 'drop') %>%
                  arrange(desc(cantidad))

print("2c) Las 3 ciudades con mayor cantidad de restaurants que ofrecen comido tipo japanese son: ")
head(TipoJaponesa, 3)
# R: 2c) Las 3 ciudades con mayor cantidad de restaurants que ofrecen comido tipo japanese son:
#   city       cantidad
# 1 san jose   28
# 2 berkeley   14
# 3 oakland    14

# P2d) (2pt) Usted decide viajar a una de las ciudades en cuestión, para ello
#            calcula el promedio de las valoraciones medias (promedio de review)
#            por cada ciudad, y escoje aquella con mayor review promedio.
#            ¿Qué ciudad escoge?
Promedio <- general %>%
              inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
              group_by(city) %>%
              summarize(promedio = mean(review), .groups = 'drop') %>%
              arrange(desc(promedio))

head(Promedio,1)
print(paste("2d) la ciudad que escojo es:", head(Promedio$city,1)))
# R: "2d) la ciudad que escojo es: cerritos"

# P2e) (2pt) Cuál es la ciudad con mejor valoración promedio de restaurantes
#            tipo "barbeque"
PromedioBarbeque <- general %>%
                      inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                      filter(food_type == "barbeque") %>%
                      group_by(city) %>%
                      summarize(promedio = mean(review), .groups = 'drop') %>%
                      arrange(desc(promedio))

print(paste("2e) la ciudad con mejor valoración promedio de restaurantes tipo barbeque es:", head(PromedioBarbeque$city,1)))
# R: "2e) la ciudad con mejor valoración promedio de restaurantes tipo barbeque es: pleasant hill"

### Preguntas 1.3
# P3a) (4pt) En la pregunta 1d), se pudo observar que un mismo restaurant puede
#            estar presente en más de una ciudad. ¿Cuántos restaurants tienen
#            esta característica, es decir están en más de una ciudad distinta?
#            De ser de utilidad puede investigar y utilizar la función distinct().
CantRestEnMasCiudad <- general %>%
                         inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                         distinct(label, city) %>%
                         group_by(label) %>%
                         summarise(cantidad = n(), .groups = 'drop') %>%
                         filter(cantidad > 1) %>%
                         count()

print(paste("3a) La cantidad de restaurants tienen que están en más de una ciudad distinta es:", CantRestEnMasCiudad))
# R: "3a) La cantidad de restaurants tienen que están en más de una ciudad distinta es: 559"


# P3b) (2pt) ¿Cuál es el restaurant que tiene presencia en la mayor cantidad de
#             ciudades distintas?¿En cuántas ciudades está presente?
RestMasCiudades <- head(general %>%
                          inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                          distinct(label, city) %>%
                          group_by(label) %>%
                          summarise(cantidad = n(), .groups = 'drop') %>%
                          arrange(desc(cantidad)), 1)

print(paste("3b) El restaurant que tiene presencia en la mayor cantidad de ciudades distintas es: ", RestMasCiudades$label))
print(paste("3b) y está presente en", RestMasCiudades$cantidad ,"ciudades"))
# R: "3b) El restaurant que tiene presencia en la mayor cantidad de ciudades distintas es:  baskin robbins"
#    "3b) y está presente en 49 ciudades"


# P3c) (5pt) Diremos que un restaurant posee sucursales si en la tabla general
#            existe más de un registro con el mismo label. Muestre, mediante un
#            gráfico de barras, los 15 restaurants con mayor cantidad de
#            sucursales, donde la altura de la barra representa el total
#            de sucursales de cada restaurant. Su gráfico debe contemplar
#            al menos los siguientes aspectos:
#               - Debe contener un título.
#               - Debe incluir nombres sugerentes en los ejes.
#               - Los nombres de los restaurants deben ser legibles.
#               - Los restaurants deben ir ordenados según su cantidad de sucursales.
#               - Incluya tiquetas en la parte superior de cada barra,
#                 que muestre la cantidad de sucursales respectivas.
#               - Las leyendas de cada gráfico no deben visualizarse
RestMasSuc <- head(general %>%
                     inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                     group_by(label) %>%
                     summarise(cantidad = n(), .groups = 'drop') %>%
                     arrange(desc(cantidad)),15)

# Reordenamiento de los datos de mayor a menor, según cantidad de sucursales
RestMasSuc$label <- reorder(RestMasSuc$label, -RestMasSuc$cantidad) 

# Se despliega el gráfico
RestMasSuc %>%
  ggplot(aes(label, cantidad )) + 
  geom_col(alpha = 0.8) +
  xlab("Restaurants") + 
  ylab("Cantidad de Sucursales") +
  ggtitle("Total de Sucursales por Restaurants") + 
  geom_text(aes(label = cantidad), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust= 1))
  
### Preguntas 1.4
# P4a) (4pts) Genere una tabla llamada resumen, que contenga la siguiente
#             información:
#                - city: Ciudad
#                - food_type: Tipo de comida
#                - n_rest: Cantidad de restaurantes por cada ciudad y tipo de comida.
#                - review_prom: Valoración promedio por cada ciudad y tipo de comida.
#                - total_rest: Total de restaurantes por cada ciudad (se puede
#                  repetir el valor por cada tipo de comida).
#                - review_prom_city: Valoración promedio de los restaurantes por
#                  cada ciudad (se puede repetir el valor por cada tipo de comida).
#
# Su tabla deberá tener la siguiente estructura:
#
#          city   food_type    n_rest   review_prom   total_rest   review_prom_city
# -------------   ---------    ------   -----------   ----------   ----------------
# san francisco     chinese       172      2.345349         1241           2.490250
# san francisco     italian       142      2.641549         1241           2.490250
#      san jose       pizza       126      2.131746          933           2.193569
#      san jose        cafe       121      2.033884          933           2.193569
#       oakland     chinese       112      2.156250          666           2.194745
#       oakland        cafe        90      2.051111          666           2.194745
#
#
#
# Creación de tabla con Cantidad y valoración promedio de restaurantes 
# por cada ciudad y tipo de comida
CantCiudadComida <- general %>%
                      inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                      select(city, food_type, review) %>%
                      group_by(city, food_type) %>%
                      summarise(n_rest = n(), 
                                review_prom = mean(review), 
                                .groups = 'drop')

# Creación de tabla con cantidad total y valoración promedio de los 
# restaurantes por cada ciudad
TotRestPronCiud <- general %>%
                     inner_join(location, by = c("id_restaurant" = "id_rest")) %>%
                     group_by(city) %>%
                     summarise(total_rest = n(), 
                               review_prom_city = mean(review),
                               .groups = 'drop')

# Creación de la tabla resumen
resumen = ""
resumen <- CantCiudadComida %>%
             inner_join(TotRestPronCiud, by =  c("city" = "city")) %>%
             arrange(desc(n_rest))


# P4b) (2pts) Basado en la tabla anterior, construya dos nuevas columnas llamadas
#             density_food_type y ratio_review que contengan la siguiente información:
#                - density_food_type: Representa el cuociente entre le total de
#                  restaurants por tipo de comida y ciudad, respecto del total
#                  de restaurantes de la ciudad. (n_rest/total_rest)
#                - ratio_review: Representa el cociente entre a valoración
#                  del restaurant por tipo de comida y ciudad, respecto de la
#                  valoración promedio de los resturants de la misma ciudad.
#                  (review_prom/review_prom_city)
#

# Se agrega a "resumen" la columna "density_food_type" que corresponde al 
# cuociente entre le total de restaurants por tipo de comida y ciudad, 
# respecto del total de restaurantes de la ciudad.
resumen <- resumen %>%
             mutate(density_food_type = n_rest / total_rest) 

# Se agrega a "resumen" la columna "ratio_review" que representa al cuociente 
# entre la valoración del restaurant por tipo de comida y ciudad, respecto de la
#  valoración promedio de los resturants de la misma ciudad
resumen <- resumen %>%
             mutate(ratio_review = review_prom / review_prom_city) 

#
# P4c) (3pts) Mediante un gráfico de dispersión, muestre la relación entre
#             density_food_type y ratio_review. Investigue sobre el parámetro
#             alpha dentro de la capa geométrica para una mejor visualizaciónd
#             de los puntos. Adicionalmente añada una curva de tendencia y, con
#             base en él, indique si cabe la posibilidad de establecer algún
#             tipo de dependencia entre density_food_type y ratio_review.

# Creación del gráfico
resumen %>%
  ggplot() +
  aes(x = density_food_type, y = ratio_review) +
  xlab("Desnsidad por tipo de comida") +
  ylab("Ratio reseña") +
  ggtitle("Densidad vs. Reseña") +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# Preguntas 1.5
# P5a) (3pts) En la tabla resumen creada en P4a), genere una nueva columna
#             llamada type_review, que contenga "review alto",
#             si ratio_review >= 1 y "review bajo" ratio_review < 1.
#             ¿Qué indica esta variable? Comente.
resumen <- resumen %>%
             mutate(type_review = ifelse(ratio_review >= 1, 
                                         "review alto", 
                                         "review bajo"))
                    

# P5b) (3pts) Para cada type_review, muestre a través de un gráfico de
#             cajas (boxplot), la distribución de la densidad del tipo de
#             comida density_food_type. ¿Qué puede observar?
resumen %>%
  ggplot() +
  aes(x = type_review, y = density_food_type) +
  xlab("Tipo de reseña") +
  ylab("Densidad por tipo de comida") +
  ggtitle("Tipo de Reseña vs. Densidad Tipo comida") +
  geom_boxplot(alpha = 0.1) 

### Pregunta 1.6
# P6a) (2pts) Determine cuáles son las siguientes ciudades:
#
#     Ciudad 1: Ciudad con mayor cantidad de restaurants (posición 1).
#     Ciudad 2: Ciudad ubicada en la posición 5, al ordenar las ciudades
#               de manera decreciente según cantidad de restaurants.
#     Ciudad 3: Ciudad ubicada en la posición 10, al ordenar las ciudades
#               de manera decreciente según cantidad de restaurants.
Ciudades_1_5_10 <- resumen %>%
                     group_by(city) %>%
                     summarise(cantidad = n(), .groups = 'drop') %>%
                     arrange(desc(cantidad)) %>%
                     slice(c(1, 5, 10)) %>%
                     select(city)
#Ciudades_1_5_10
LargoCiudades <- length(Ciudades_1_5_10$city)
i <- 1
while (i <= LargoCiudades) {
  PosicionCiudad <- case_when(
    i == 1 ~ 1,
    i == 2 ~ 5,
    i == 3 ~ 10,
    i > 3 ~ i
   )
  print(paste("6a) La ciudad en la Posición",PosicionCiudad,"es", Ciudades_1_5_10[i,1]))
  i <- i + 1
}

# R: "6a) La ciudad en la Posición 1 es san francisco"
#    "6a) La ciudad en la Posición 5 es palo alto"
#    "6a) La ciudad en la Posición 10 es santa clara"



# P6b) (6pts) Usted deberá graficar la cantidad de restaurants, por cada uno
#             de los 5 tipos de comida más frecuentes dentro de la ciudad
#             respectiva. Las ciudades graficadas deben corresponder, de
#             izquierda a derecha, a las ciudades de posición 1, 5 y 10
#             encontradas en P6a). Al generar el gráfico, deberá tener
#             en cuenta los siguientes elementos:
#                - Investigue sobre el parámetro position dentro de la capa
#                  geométrica que genera el gráfico de barras, para la correcta
#                  presentación de las mismas.
#                - Por cada ciudad, sólo se grafican los 5 tipos de comida más
#                  frecuentes (con mayor cantidad de restaurants de dicho tipo).
#                - EL gráfico deberá mostrar las barras ordenadas por altura tal
#                  cual se muestra en la imagen. De ser de utilidad, puede
#                  investigar el parámetro group dentro de la función aes().
#                - Debe incluir título, nombre de los ejes y nombre de la leyenda,
#                  tal como se muestra en la imagen.
#                - Incluya etiquetas en la parte superior de cada barra. Estas
#                  etiquetas deberán mostrar la densidad del tipo de comida
#                  density_food_type, expresado como porccentaje (ie. density_food_type*100 %).
#                  Para tal efecto, investigue sobre la utilización de las
#                  funciones geom_label() o geom_text().
#
# NOTA: Como referencia, su gráfico debe contener los mismos elementos del cuarto
#       gráfico, "Cantidad de restaurantes por tipo de comida y ciudad", que se
#       muestran en el README.md (no necesariamente los mismos valores de los elementos que lo componen).
#       Por otro lado, no es necesaria la utilización de la misma paleta de
#       colores. En este gráfico se muestran las ciudades de posición 1, 2 y 3.
#       Ustede deberá visualizar las 1, 5 y 10.
