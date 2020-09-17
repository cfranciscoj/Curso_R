library("rpart")
library("rpart.plot")
library("caret")
library("lattice")
library("e1071")
library("dplyr")
library("magrittr")
library("ggplot2")
library("tidyverse")


# Indicamos el directorio de trabajo
setwd("H:/Nextcloud/Cursos/BigData/Curso 5 Ciencia de Datos y sus Aplicaciones/Proyectos")


# Cargamos la base de datos de entrenamiento
datos <- read.csv("fuga.csv", sep=",",header=TRUE)
head(datos)

modelo1 <- rpart(Fuga ~ Llamadasaserviciodeatenci0nalcliente + Total.llamadas.internacionales + Total.llamadas.horario.reducido + Plan.Internacional.Preferente,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 5,
                                         cp = 0))

rpart.plot(modelo1)

any(!complete.cases(datos))

map_dbl(datos,.f=function(x){
  sum(is.na(x))
  
  
  

  
modelo1_2<-rpart(Fuga ~ .,
                 data = datos,
                 method = "class",
                 parms = list(split = "information"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 4,                         
                                         cp = 0))
printcp(modelo1_2)
plotcp(modelo1_2)

rpart.plot(modelo1_2,extra = 108)
  
prediccion1_2<-predict(modelo1_2,newdata=datos,type="class")
cm2<-confusionMatrix(prediccion1_2,datos[["Fuga"]])
cm2$overall["Accuracy"]
1-cm2$overall["Accuracy"]
length(row.names(modelo1_2$frame))  
})

datos %>%
  map_lgl(.f=function(x){
    any(!is.na(x) & x == "")
  })


datos %>%
  group_by(Fuga) %>%
  summarise(n=n()) %>%
  ggplot()+
  aes(x=Fuga, y=n,fill=Fuga)+
  geom_bar(stat = 'identity',width = 0.3)+
  scale_fill_manual(values = c("gray60","orange3"))+
  geom_text(aes(label=n),size=3.5,vjust=-0.6)+
  labs(title = "Variable Fuga",
       x="Fuga",
       y="Cantidad")+
  theme_bw()+
  theme(legend.position = "bottom")

n_obs<-nrow(datos)
prediccion<-rep(x="Fugado", n_obs)
mean(prediccion== datos$Fuga)*100

datos %>%
  filter(!is.na(Llamadasaserviciodeatenci0nalcliente)) %>%
  group_by(Fuga) %>%
  summarise(
    media=mean(Llamadasaserviciodeatenci0nalcliente),
    mediana=median(Llamadasaserviciodeatenci0nalcliente),
    min=min(Llamadasaserviciodeatenci0nalcliente),
    max=max(Llamadasaserviciodeatenci0nalcliente)
  )


datos %>%
  ggplot()+
  aes(x=Llamadasaserviciodeatenci0nalcliente, y=..count..,fill=Fuga)+
  geom_bar(width = 0.3)+
  scale_fill_manual(values = c("gray60","orange3"))+
  labs(title = "Variable Pclass",
       x="Fuga",
       y="Cantidad")+
  theme_bw()+
  theme(legend.position = "bottom")

prop.table(table(datos$Llamadasaserviciodeatenci0nalcliente,datos$Fuga),margin=1) %>%
  round(digits = 2)


#Sex
datos %>%
  ggplot()+
  aes(x=Total.llamadas.internacionales, y=..count..,fill=Fuga)+
  geom_bar(width = 0.3)+
  scale_fill_manual(values = c("gray60","orange3"))+
  labs(title = "Llamadas internacionales",
       x="Llmadas Internacionales",
       y="Cantidad")+
  theme_bw()+
  theme(legend.position = "bottom")

prop.table(table(datos$Total.llamadas.internacionales,datos$Fuga),margin=1) %>%
  round(digits = 2)


