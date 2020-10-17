library(class)
library(caret)
library(e1071)
library(gtools)
library(tidyverse)  
library(dplyr)
library(magrittr)
library(cluster)
library(factoextra)

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

setwd("/home/carlos/repos/Curso_R/Curso_005_CienciaDeDatosysusAplicaciones/Proyecto_002")



# Cargamos la base de datos
datos <- read.csv("datos.csv", sep = ";",header=TRUE)
head(datos)

str(datos)

datos_filtrados <- datos %>%
                     filter(Contactos.con.su.Ejecutivo <= 100)

#datos %>%
#  count(Tiene.Credito.Hipotecario)

#str(datos_filtrados)


datos_todos <- as.data.frame(cbind(Edad                 = datos_filtrados$Edad,
                                   Numero_Cumple        = datos_filtrados$Numero.Cumple,
                                   ActLab_Jubilado      = (datos_filtrados$Actividad.Laboral == "Jubilado"), 
                                   ActLab_Obrero        = (datos_filtrados$Actividad.Laboral == "Obrero"), 
                                   ActLab_Tecnico       = (datos_filtrados$Actividad.Laboral == "Tecnico"), 
                                   ActLab_Dependiente   = (datos_filtrados$Actividad.Laboral == "Trabajador Dependiente"), 
                                   ActLab_Independiente = (datos_filtrados$Actividad.Laboral == "Trabajador Independiente"),
                                   ActLab_Empresario    = (datos_filtrados$Actividad.Laboral == "Empresario") ,
                                   #ActLab_Gerente       = (datos_filtrados$Actividad.Laboral == "Gerente"),}
                                   EstadoCiv_Casado     = (datos_filtrados$Estado.Civil == "Casado"),
                                   EstadoCiv_Divorciado = (datos_filtrados$Estado.Civil == "Divorciado"),
                                   Educacion            = ifelse(datos_filtrados$Nivel.Educacional == "Ensegnanza Basica",1 ,
                                                             ifelse(datos_filtrados$Nivel.Educacional == "Ensegnanza Media",2,
                                                               ifelse(datos_filtrados$Nivel.Educacional == "Tecnico Profesional",3,
                                                                 ifelse(datos_filtrados$Nivel.Educacional == "Universitario",4, 0
                                                                      )
                                                                     )    
                                                                    )
                                                                  ),
                                   Tiene_Mora           = (datos_filtrados$Tiene.Mora =='Si'),
                                   Saldo_Medio_Anual    = datos_filtrados$Saldo.Medio.Anual,
                                   Tiene_Credito_Hipo   = datos_filtrados$Tiene.Credito.Hipotecario,
                                   Tiene_Credito_Cons   = datos_filtrados$Tiene.Credito.de.Consumo,
                                   Medio_Contacto_Part  = (datos_filtrados$Medio.de.Contacto.Preferente == "Fono Particular"), 
                                   Medio_Contacto_Celu  = (datos_filtrados$Medio.de.Contacto.Preferente == "Celular"),
                                   Contacto_Con_Ejecu   = datos_filtrados$Contactos.con.su.Ejecutivo,
                                   Tiene_Inversiones    = datos_filtrados$Tiene.Inversiones,
                                   Numeros_Creditos     = datos_filtrados$Tiene.creditos))
                                   
                                   
                                   
#str(datos_todos)                                   
                                   







datos_norm <- as.data.frame(lapply(datos_todos, normalize))






# Apliquemos el codo
#clusters <- c(1,2,3,4,5,6,7,8,9,10)
#codo <- c(0,0,0,0,0,0,0,0,0,0)
#for (k in 1:10) { 
#  codo[k] <- kmeans(datos_norm,
#                    centers = k,
#                    nstart = 25)$tot.withinss
#}

#plot(clusters, codo, type = "l")

#k2 <- kmeans(datos_norm,
#             centers = 2,
#             nstart = 25)

# Tamano de cada cluster
#k2$size
# Cluster al que pertenece cada observacion
#k2$cluster
# Centros de cada cluster
#k2$centers
# Suma cuadratica inicial de distancias
#k2$totss
# Sumas cuadraticas de distancias dentro de cada cluster
#k2$withinss
#k2$tot.withinss



# Calculemos los valores de silueta para nuestros clusters
#silueta_k2 <- silhouette(k2$cluster, dist(datos_norm))

# Grafiquemos la silueta
#silueta <- c(0,0,0,0,0,0,0,0,0)
#for (k in 2:10) { 
#  modelo_aux <- kmeans(datos_norm,
#                       centers = k,
#                       nstart = 25)
#  silueta_aux <- silhouette(modelo_aux$cluster, dist(datos_norm))
#  silueta[k] <- mean(silueta_aux[, 3])
#}

#plot(clusters, silueta, type = "l")




# Agreguemos el cluster de cada observacion a los datos
#datos_todos$cluster2 <- k2$cluster

# Podemos analizar visualmente los clusteres
#plot(datos$eruptions, datos$waiting, col = datos$cluster3)

# Podemos analizar las variables promedio de cada cluster
#View(datos_todos
#     %>% group_by(cluster2)
#     %>% summarise_all(mean))

###########################
clusters <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)

datos_clasificados <- datos_norm %>%
                          select(Edad                 ,
                                 #Numero_Cumple        ,
                                 ActLab_Jubilado      , 
                                 ActLab_Obrero        , 
                                 #ActLab_Tecnico       , 
                                 ActLab_Dependiente   , 
                                 ActLab_Independiente ,
                                 #ActLab_Empresario    ,
                                 #EstadoCiv_Casado     ,
                                 #EstadoCiv_Divorciado ,
                                 #Educacion            ,
                                 Tiene_Mora           ,
                                 #Saldo_Medio_Anual    ,
                                 #Tiene_Credito_Hipo   ,
                                 #Tiene_Credito_Cons   ,
                                 Medio_Contacto_Part  , 
                                 Medio_Contacto_Celu  ,
                                 Contacto_Con_Ejecu   ,
                                 Tiene_Inversiones    ,
                                 Numeros_Creditos     
                                 )


for (k in 1:10) { 
  codo[k] <- kmeans(datos_clasificados,
                    centers = k,
                    nstart = 25)$tot.withinss
}


plot(clusters, codo, type = "l")

text(clusters,codo,labels = round(codo[c(1:10)],3),cex=0.8,pos=3)


k_final <- kmeans(datos_clasificados,
             centers = 3,
             nstart = 25)

datos_clasificados$cluster_final <- k_final$cluster

# Podemos analizar las variables promedio de cada cluster
View(datos_clasificados
     %>% group_by(cluster_final)
     %>% summarise_all(mean))


registros <- datos_clasificados %>%
  group_by(cluster_final)%>%
  summarise_all(mean)


write.csv2(registros,"registros.csv",row.names = F)

#Prueba dendograma - Yasmin
test <- hclust(dist(datos_clasificados), method = "ward.D")
plot(test)