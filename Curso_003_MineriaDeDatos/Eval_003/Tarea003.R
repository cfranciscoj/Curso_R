#Evaluación 3 - Reglas de asociación
#Curso Minería de datos

library(dplyr)
library(ggplot2)
library(arules)
library(arulesViz)
library(rCBA)


#Carga de archivo
setwd("C:/Users/macar/Documents/MineriaDatos/Ev3")
setwd("/home/carlos/repos/Curso_R/Curso_003_MineriaDeDatos/Eval_003")
getwd()
ruta<-getwd()
rutaC<-paste(ruta,"/lastfm_sel.csv", sep = "")
datos <- read.csv(rutaC,sep = ",")

# Generamos la lista de transacciones a utilizar
write.table(datos, file = tmp <- file(), row.names = FALSE)
listas <- read.transactions(tmp,
                            format = "single",
                            header = TRUE,
                            cols = c("user", "artist"))
close(tmp)
summary(listas)
inspect(head(listas,2))
#class(listas)

write.csv(datos,"tempora.csv")


#Análisis exploratorio de la base de datos

dim(datos)
# [1] 289955      4


#Cantidad de artistas distintos
datos %>%
        distinct(artist) %>%
        count()

#1004

#Cantidad de usuarios   = cantidad de listas     
datos %>%
        distinct(user) %>%
        count()

# 15.000

#promedio de cantidad de artistas por lista
b<-datos %>% 
        group_by(datos$user) %>%
        summarise(c=n())

mean(b$c)
#  19.33033      


#Gráfico tamaño versus cantidad de usuarios
png(filename = "TamañoVSusuarios.png", width = 2000, height = 1000)
datos %>%
        group_by(size) %>%
        summarise(cant= n_distinct(user)) %>%
        arrange(desc(size)) %>%
        head(70) %>%
        ggplot() +
        aes(x = reorder(size,size),
            y = cant,
            fill = cant) +
        geom_bar(stat = 'identity',width = 0.9, col="gray",fill="#2297E6") +
        geom_text(aes(label=cant),size=7,vjust=-0.6)+
        labs(
                x = "Tamaño de lista",
                y = "Cantidad de usuarios",
                fill = "Cantidad",
                title = "Tamaño de lista versus cantidad de usuarios" #,
        )+theme(text=element_text(size=30))
dev.off()
#Fuente: https://www.iteramos.com/pregunta/38614/modificar-fuentes-en-ggplot2

#Gráfico tamaño de listas con mayor cantidad de usuarios
datos %>%
        group_by(size) %>%
        summarise(cant= n_distinct(user)) %>%
        arrange(desc(cant)) %>%
        head(15) %>%
        ggplot() +
        aes(x = reorder(size,-cant),
            y = cant,
            fill = cant) +
        geom_bar(stat = 'identity',width = 0.5,col="gray",fill="#2297E6") +
        geom_text(aes(label=cant),size=3.5,vjust=-0.6)+
        labs(
                x = "Tamaño de lista",
                y = "Cantidad de usuarios",
                fill = "Cantidad",
                title = "Tamaño de Listas con mayor cantidad de usuarios" #,
                #subtitle = "Considera todo el horario de operación"
        )
s<-datos %>%
        group_by(size) %>%
        summarise(cant= n_distinct(user)) %>%
        arrange(desc(cant)) %>%
        head(15)

(sum(s$cant)/15000)*100


# Los 5 usuarios con listas de reproducción con la mayor cantidad de artistas
datos %>%
        group_by(user) %>%
        summarise(cant=n()) %>%
        arrange(desc(cant)) %>%
        head(5) %>%
        ggplot() +
        aes(x = reorder(user,-cant),
            y = cant,
            fill = cant) +
        geom_bar(stat = 'identity',width = 0.5,col="gray",fill="#2297E6") +
        geom_text(aes(label=cant),size=3.5,vjust=-0.6)+
        labs(
                x = "Usuarios",
                y = "Cantidad",
                fill = "Cantidad",
                title = "Top 5 usuarios con listas con la mayor cantidad de artistas" #,
                #subtitle = "Considera todo el horario de operación"
        )

#Número de usuarios con solo 1 artista en la lista
datos %>%
        filter( size == 1) %>%
        count()
#185        


# Los 10 artistas con mayor presencia en listas de reproducción
png(filename = "Top10artistasMayor.png", width = 2000, height = 1000)
datos %>%
        group_by(artist) %>%
        summarise(cant=n()) %>%
        arrange(desc(cant)) %>%
        head(10) %>%
        ggplot() +
        aes(x = reorder(artist,-cant),
            y = cant,
            fill = cant) +
        geom_bar(stat = 'identity',width = 0.9,col="gray",fill="#2297E6") +
        geom_text(aes(label=cant),size=7,vjust=-0.6)+
        labs(
                x = "Artista",
                y = "Cantidad",
                fill = "Cantidad",
                title = "Top 10 artistas con mayor presencia en las listas" #,
                #subtitle = "Considera todo el horario de operación"
        )+
        theme(text=element_text(size=30),axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#text=element_text(size=30)

#Presencia promedio
s1<-datos %>%
        group_by(artist) %>%
        summarise(cant=n()) %>%
        arrange(desc(cant)) 

#Máxima presencia 2704

mean(s1$cant)
#Presencia promedio 288,8

s2<-datos %>%
        group_by(artist) %>%
        summarise(cant=n()) %>%
        arrange(cant)
#min presencia 96

# Los 10 artistas con menor presencia en listas de reproducción
png(filename = "Top10artistasMenor.png", width = 2000, height = 1000)
datos %>%
        group_by(artist) %>%
        summarise(cant=n()) %>%
        arrange(cant) %>%
        head(10) %>%
        ggplot() +
        aes(x = reorder(artist,cant),
            y = cant,
            fill = cant) +
        geom_bar(stat = 'identity',width = 0.9,col="gray",fill="#2297E6") +
        geom_text(aes(label=cant),size=7,vjust=-0.6)+
        labs(
                x = "Artista",
                y = "Cantidad",
                fill = "Cantidad",
                title = "Top 10 artistas con menor presencia en las listas" #,
                #subtitle = "Considera todo el horario de operación"
        )+
        theme(text=element_text(size=30),axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


#Comentarios 
#support (presencia de artistas en listas)
# 96 / 15000 mínimo -> 0,0064   --> support 0,006     100 / 15000 razonable   0,007
# 2704 / 15000 máximo -> 0,180 

# Confidence (largo de las listas)  
#
#15000-185 = 14815 número de clientes con listas de largo >=2
# 61/14815 support de una regla, dividido por el support mínimo 0,007    0,59 aprox

#10/14815 support de una regla, dividido por el support mínimo 0,007    0,59 aprox

#------------------------------------------------------------------------------
# Reglas de asociación --------------------
#------------------------------------------

regla_1<- apriori(
        data = listas,
        parameter = list(
                supp = 0.001, 
                conf = 0.005,
                minlen = 2 #se excluyen aquellos que no tienen antecedente o consecuente
        ),
        control = list (verbose = F)
                )

#summary(regla_1) # Cantidad de reglas, tamaños de itemsets  
#inspect(regla_1)

write(regla_1,
      file = "association_rules.csv",
      sep = ";",
      quote = FALSE,
      row.names = FALSE)





s2<-inspect(head(sort(regla_1,by="lift"),10))
s2<-inspect(head(sort(regla_1,by="lift"),10306))
write.csv2(s2,"Reglas.csv",row.names = F)




#Fuente: https://rstudio-pubs-static.s3.amazonaws.com/205309_df549965a0d04557bd4fab179fa35609.html
#Gráfica de las reglas 
png(filename = "GráficoDispersion.png", width = 500, height = 300)
plot(regla_1,main = paste('Gráfico de dispersión para ',63,' reglas'))
dev.off()

#Fuente http://apuntes-r.blogspot.com/2015/07/reglas-de-asociacion.html
#artistas más recurrentes (realizado antes)
itemFrequencyPlot(listas ,topN=20,type="absolute")

# Grafico de red 20 reglas con mayor confianza


# Grafico de matriz de 10 regla con mayor confianza
png(filename = "GráficoGroup.png", width = 1000, height = 800)
plot(head(regla_1,10), method="grouped")
dev.off()




#Gráfico ordenado por support
top_support<-sort(regla_1,by="support",decreasing = T)[1:10]
inspect(top_support)

png(filename = "GráficoReglasSupport.png", width = 1000, height = 800)
plot(head(sort(regla_1,by="support"),10), method="graph",
     main = paste('Grafo de las',10,' reglas con mayor soporte'))
dev.off()

inspect(head(sort(regla_1,by="support"),10))

#Gráfico ordenado por confidence
top_conf<-sort(regla_1,by="confidence",decreasing = T)[1:10]
inspect(top_conf)

png(filename = "GráficoReglasConfidence.png", width = 1000, height = 800)
plot(head(sort(regla_1,by="confidence"),10), method="graph",
     main = paste('Grafo de las',10,' reglas con mayor confianza'))
dev.off()

png(filename = "GráficoReglasConfidenceX.png", width = 1000, height = 800)
plot(head(sort(regla_1,by="confidence"),10), method="graph", control=list(type="items"))
dev.off()

#Gráfico ordenado por lift
top_lift<-sort(regla_1,by="lift",decreasing = T)[1:10]
inspect(top_lift)

#Visualización de resultados vistos en clases
#plot(regla_1, method = "paracoord")
png(filename = "GráficoReglasLift.png", width = 1000, height = 800)
plot(head(sort(regla_1,by="lift"),10), method = "graph",
     main = paste('Grafo de las',10,' reglas con mayor lift'))
dev.off()

inspect(head(sort(regla_1,by="lift"),10))







#Otras visualizaciones
#plot(regla_1) #Gráfico de dispersión de todas las reglas
#head(quality(regla_1))


plot(regla_1, measure=c("support","lift"), shading="confidence")
plot(head(regla_1,10), method="matrix", measure="lift")
#plot(head(regla_1), method="graph", control=list(type="artist"))

#Ordenar por lift decreciente
#
top_lift<-sort(regla_1,by="lift",decreasing = F)[1:5]
inspect(top_lift)

inspect(regla_1)





regla_2<- apriori(
        data = listas,
        parameter = list(
                supp = 0.01, 
                conf = 0.01,
                maxlen = 1 #se excluyen aquellos que no tienen antecedente o consecuente
        ),
        appearance = list(rhs = "rihanna"),
        control = list (verbose = F)
)
summary(regla_2) # Cantidad de reglas, tamaños de itemsets  
inspect(regla_2)


regla_3<- apriori(
        data = listas,
        parameter = list(
                supp = 0.01, 
                conf = 0.01,
                maxlen = 1 #se excluyen aquellos que no tienen antecedente o consecuente
        ),
        control = list (verbose = F)
)
summary(regla_3) # Cantidad de reglas, tamaños de itemsets  
inspect(regla_3)


#############-------------
#Fuente: https://stackoverflow.com/questions/42844279/subsetting-arules-by-lhs-in-r
#Para buscar un solo artista en el lado izquierdo
regla_2_1<-subset(regla_1,lhs %in% c("coldplay"))
inspect(regla_2_1)
#Para buscar más de un sólo artista a la vez en el lado izquierdo
regla_2_2<-subset(regla_1,lhs %oin% c("coldplay","pink floyd"))
inspect(regla_2_2)


regla_2_2<-subset(regla_1,lhs %ain% c("coldplay","pink floyd"))
inspect(regla_2_2)


