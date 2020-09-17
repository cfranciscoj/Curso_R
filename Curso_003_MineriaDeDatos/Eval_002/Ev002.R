#Evaluación 2
#Curso Minería de datos
#Nombre: Macarena Muñoz Olave


# El objetivo de esta evaluación es utilizar y analizar árboles de clasificación. 
# La base de datos contiene 891 registros de pasajeros del RMS Titanic. 
# Las variables relevantes son:
# • Survived, indica si el pasajero sobrevivió al naufragio (variable endógena)
# • Pclass, indica la clase tarifaria en la que viajaba el pasajero
# • Sex, indica el sexo del pasajero
# • Age, indica la edad del pasajero

# Usted deberá realizar un análisis comparativo en R de los métodos de clasificación 
# vistos en clases. El análisis debe incluir al menos:
# • El impacto del método de división: information vs gini.
# • El impacto del parámetro cp asociado al costo de complejidad
# • El impacto de los parámetros de parada anticipada minsplit, minbucket y maxdepth.

# Cada comparación es independiente entre sí (i.e. no se complique comparando gini 
# con cierto minsplit vs information con otro minsplit). Los criterios de análisis y 
# comparación consisten en una evaluación crítica del árbol resultante y la capacidad 
# predictiva del modelo (obteniendo, por ejemplo, matrices de confusión).


#library("stats")
#library("dplyr")
library("ggplot2")
library("tidyverse")
library("rpart") #el paquete con la implementación de árboles de clasificación que utilizaremos
library("rpart.plot") #para graficar los resultados de rpart.
library("caret")
library("lattice")
library("e1071")
library("dplyr")
library("magrittr")

#"tidyverse": para llamar a la familia de paquetes tidyverse, 
#que nos ayudaran al procesamiento de nuestros datos.

#"caret": un paquete con utilidades para clasificación y regresión. 
# Lo usaremos por su función para crear matrices de confusión


#Carga de archivo
setwd("C:/Users/macar/Documents/MineriaDatos/Ev2")
getwd()
ruta<-getwd()
rutaC<-paste(ruta,"/datasets/titanic.csv", sep = "")
titanic1 <- read.csv(rutaC,sep = ",")

#Seleccino las variables relevantes Survived, Pclass, Sex y Age
titanic2<-titanic1[,c(1:3,5:6)]
write.csv2(titanic2,"titaniclimpia.csv",row.names = F)

head(titanic2)
dim(titanic2)

#Revisión del tipo de datos
glimpse(titanic2)
#str(titanic2)
#Respuesta: todos ok

#Se recodifica la columna survived como factor con "Si" y "No
#para evitar que se calcule su media, ya que no tiene sentido
#su cálculo sobre esta variable
titanic2$Survived<-ifelse(titanic2$Survived==1,"Si","No")
titanic2$Survived<-as.factor(titanic2$Survived)

#La variable Pclass se convierte en factor, debido a que representa
#la clase tarifaria en la que viajaba el pasajero, por lo que
#se manejará como una variable discreta

titanic2$Pclass<-as.factor(titanic2$Pclass)


#Revisión del data frame 
summary(titanic2)

#Número de observaciones y valores ausentes
#Tratamiento de NA
nrow(titanic2)

#Detección de filas incompletas
any(!complete.cases(titanic2))

# Número de datos ausentes por variable
map_dbl(titanic2,.f=function(x){
                        sum(is.na(x))
                        })

#Age tiene 177 valores NA

#Se procede a identificar qué variables contienen valores "".
titanic2 %>%
        map_lgl(.f=function(x){
                any(!is.na(x) & x == "")
                })

#Ninguna variable

#Si existieran "" se procede de la siguiente manera
#datos$Cabin[datos$Cabin == ""] <- NA
#Fuente: www.cienciadedatos.net/documentos/41_machine_learning_con_r_y_caret


#Distribución de variables respuesta, table construye una tabla de frecuencias
table(titanic2$Survived)

prop.table(table(titanic2$Survived)) %>%
                         round(digits = 2)

titanic2 %>%
        group_by(Survived) %>%
        summarise(n=n()) %>%
        ggplot()+
        aes(x=Survived, y=n,fill=Survived)+
        geom_bar(stat = 'identity',width = 0.3)+
        scale_fill_manual(values = c("gray60","orange3"))+
        geom_text(aes(label=n),size=3.5,vjust=-0.6)+
        labs(title = "Variable Survived",
             x="Survived",
             y="Cantidad")+
        theme_bw()+
        theme(legend.position = "bottom")


# Porcentaje de aciertos si se predice para todas las observaciones que no sobrevivieron
n_obs<-nrow(titanic2)
prediccion<-rep(x="No", n_obs)
mean(prediccion== titanic2$Survived)*100




# Estadísticos de la edad de los supervivientes y fallecidos
titanic2 %>%
        filter(!is.na(Age)) %>%
        group_by(Survived) %>%
        summarise(
                media=mean(Age),
                mediana=median(Age),
                min=min(Age),
                max=max(Age)
        )

#Distribución de variables cualitativas
#Pclass
titanic2 %>%
        ggplot()+
        aes(x=Pclass, y=..count..,fill=Survived)+
        geom_bar(width = 0.3)+
        scale_fill_manual(values = c("gray60","orange3"))+
        labs(title = "Variable Pclass",
             x="Pclass",
             y="Cantidad")+
        theme_bw()+
        theme(legend.position = "bottom")

prop.table(table(titanic2$Pclass,titanic2$Survived),margin=1) %>%
        round(digits = 2)


#Sex
titanic2 %>%
        ggplot()+
        aes(x=Sex, y=..count..,fill=Survived)+
        geom_bar(width = 0.3)+
        scale_fill_manual(values = c("gray60","orange3"))+
        labs(title = "Variable Sex",
             x="Sex",
             y="Cantidad")+
        theme_bw()+
        theme(legend.position = "bottom")

prop.table(table(titanic2$Sex,titanic2$Survived),margin=1) %>%
        round(digits = 2)

#Edad
titanic2 %>%
        ggplot()+
        aes(x=Age, y=..count..,fill=Survived)+
        geom_bar(width = 0.3)+
        scale_fill_manual(values = c("gray60","orange3"))+
        labs(title = "Variable Age",
             x="Age",
             y="Cantidad")+
        theme_bw()+
        theme(legend.position = "bottom")

prop.table(table(titanic2$Age,titanic2$Survived),margin=1) %>%
        round(digits = 2)


#Correlación entre variables continuas, no funciona porque deben
#ser variables numéricas
#cor.test(x=titanic2$Age,y=titanic2$Pclass,method = "pearson")

#Todo lo anterior era una análisis exploratorio

#Se opta por eliminar las variables NA de edad, 
# se sabe que el método de árbol de decisión sumará éstos valores
#a la rama que no cumpla con el criterio de división

titanic3<-titanic2 %>%
        filter(!is.na(Age))

dim(titanic3)
write.csv2(titanic3,"titanic3.csv",row.names = F)

#############################################################################
########### Creación de modelo ##############################################
###########################################################################

###################################################
##############División de la data##################
###################################################

#Se crean dos set, uno para entrenamiento y otro para prueba
set.seed(123)
######## Primera forma #############################################
#titanic3_entrenamiento<-sample_frac(titanic3,0.8)
#Obtenemos la subconjunto de datos complementario
#titanic3_prueba<-setdiff(titanic3,titanic3_entrenamiento)
#Nota esta forma de división necesita el ID y no queda con las mismas proporciones
# que la data original.


###### Segunda forma de particionar la data ########################
train<-createDataPartition(y=titanic3$Survived,p=0.8,list = F,times = 1)

titanic3_train<-titanic3[train,]
titanic3_test<-titanic3[-train,]

prop.table(table(titanic3_train$Survived))
prop.table(table(titanic3_test$Survived))
prop.table(table(titanic3$Survived))

write.csv2(titanic3_train,"titanic3_train.csv",row.names = F)
write.csv2(titanic3_test,"titanic3_test.csv",row.names = F)

# Resultado, con ID me aseguro que los datos no se repitan en los set de
#datos.


#################################################################
################### Entrenamiento del modelo###################
##############################################################

#Entrenando el modelo con Information
modelo1_1<-rpart(Survived ~ Pclass + Sex + Age,
               data = titanic3_train,
               method = "class",
               parms = list(split = "information"),
               control = rpart.control(minsplit = 1,
                                       minbucket = 11,
                                       maxdepth = 6,                         #4,0,8126 y 7 dan mismo accuracy y 2 tb
                                       cp = 0)
               )
printcp(modelo1_1)

png(filename = "InformationCP.png", width = 500, height = 300)
plotcp(modelo1_1)
dev.off()

modelo1_1$cptable[which.min(modelo1_1$cptable[,"xerror"]),"CP"]

write.csv2(modelo1_1$cptable,"InformationCP.csv",row.names = F)

#which.min(modelo1_1$cptable[,"rel error"])

png(filename = "Information43nodos.png", width = 1000, height = 800)
rpart.plot(modelo1_1,extra = 103)
dev.off()

png(filename = "Information43nodos.png", width = 1000, height = 800)
rpart.plot(modelo1_1,extra = 1)
dev.off()

prediccion1_1<-predict(modelo1_1,newdata=titanic3_test,type="class")
cm<-confusionMatrix(prediccion1_1,titanic3_test[["Survived"]])
cm$overall["Accuracy"]
1-cm$overall["Accuracy"]
length(row.names(modelo1_1$frame))




##### Prueba del modelo con todo el set de datos
prediccion1_1<-predict(modelo1_1,newdata=titanic3,type="class")
cm<-confusionMatrix(prediccion1_1,titanic3[["Survived"]])
cm$overall["Accuracy"]
1-cm$overall["Accuracy"]
length(row.names(modelo1_1$frame))



#summary(modelo1_1)

#row.names(modelo1_1$frame)

#modelo1_1$variable.importance

#cm
#summary(modelo1_1)
#modelo1_1$where

#varImp(modelo1_1) #Muestra las variables de importancia

##########################################################
###### Prueba SVM #######################################
#########################################################
library(kernlab)
modelo_svmlineal<-train(Survived ~ Pclass + Sex + Age,
                        method="svmLinear",
                        data=titanic3_train)
modelo_svmlineal$finalModel
#22% de error


#######################################################
###### Entrenamiento del modelo con gini #############
#####################################################

modelo1_2<-rpart(Survived ~ Pclass + Sex + Age,
                 data = titanic3_train,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 4,                         #4,0,8126 y 7 dan mismo accuracy y 2 tb
                                         cp = 0)
        )
printcp(modelo1_2)
plotcp(modelo1_2)

rpart.plot(modelo1_2,extra = 103)

png(filename = "Gini1.png", width = 1000, height = 800)
rpart.plot(modelo1_2,extra = 103)
dev.off()


png(filename = "Gini.png", width = 500, height = 300)
rpart.plot(modelo1_2,extra = 1)
dev.off()


prediccion1_2<-predict(modelo1_2,newdata=titanic3_test,type="class")
cm2<-confusionMatrix(prediccion1_2,titanic3_test[["Survived"]])
cm2$overall["Accuracy"]
1-cm2$overall["Accuracy"]
length(row.names(modelo1_2$frame))

#Evaluar qué tan bien se comporta con los datos de entrenamiento
prediccion1_2_train<-predict(modelo1_2,newdata=titanic3_train,type="class")
cm2_train<-confusionMatrix(prediccion1_2_train,titanic3_train[["Survived"]])
cm2_train$overall["Accuracy"]

#cp mínimo, revisar
modelo1_2$cptable[which.min(modelo1_2$cptable[,"xerror"]),"CP"]

modelo1_1$cptable[which.min(modelo1_1$cptable[,"xerror"]),"CP"]



# Metodos:
# "class" para arboles de clasificacion
# "anova" para arboles de regresion

# Parametro de division para arboles de clasificacion:
# "information" para usar ganancia de informacion (entropia)
# "gini" para usar el indice de impuridad de Gini

# Parametros opcionales de control:
# minsplit determina la cantidad minima de observaciones para intentar dividir un nodo
# minbucket determina la cantidad minima de observaciones a tener en un nodo terminal
# maxdepth determina la profundidad maxima del arbol (el nodo raiz cuenta como 0)
# cp es una version escalada del parametro lambda asociado al costo de complejidad

print(modelo1_1)

# Graficamos el arbol
rpart.plot(modelo1_1,extra = 106)

prediccion1_1<-predict(modelo1_1,newdata=titanic3_prueba,type="class")
cm<-confusionMatrix(prediccion1_1,titanic3_prueba[["Survived"]])
Accuracy<-cm$overall["Accuracy"]
Accuracy

printcp(modelo1_1)

png(filename = "cp_Information.png", width = 500, height = 300)
plotcp(modelo1_1)
dev.off()
cp_infor<-modelo1_1$cptable

write.csv2(cp_infor,"cp_infor.csv",row.names = F)


#----------------------------------

modelo1_1$cptable

row.names(modelo1_1$frame) #Número de nodos en el árbol
modelo1_1$frame$var #Entrega las variables usadas en el split 
modelo1_1$variable.importance
modelo1_1$numresp #Número
modelo1_1$ordered
modelo1_1$parms
modelo1_1$control

?rpart

summary(modelo1_1)
printcp(modelo1_1)
plotcp(modelo1_1)

Profundidad<-modelo1_1$numresp
Split<-modelo1_1$parms$split
Minsplit<-modelo1_1$control$minsplit
Minbucket<-modelo1_1$control$minbucket
Cp<-modelo1_1$control$cp
Maxdepth<-modelo1_1$control$maxdepth
Leaf<-modelo1_1$control$val

dataframe<-cbind(Accuracy,Split,Profundidad,Minsplit,Minbucket,Cp,Maxdepth,Leaf)








##############################################
## Base de datos con data balanceada #########
##############################################

modelo1_2<-rpart(Survived ~ Pclass + Sex + Age,
                 data = titanic3_entrenamiento,
                 method = "class",
                 parms = list(split = "gini"),
                 control = rpart.control(minsplit = 1,
                                         minbucket = 1,
                                         maxdepth = 30,
                                         weights = 1.43,#4,0,8126 y 7 dan mismo accuracy y 2 tb
                                         cp = 0)
        )

prediccion1_1<-predict(modelo1_1,newdata=titanic3_prueba,type="class")
cm<-confusionMatrix(prediccion1_1,titanic3_prueba[["Survived"]])
Accuracy<-cm$overall["Accuracy"]
Accuracy



###########################################       
# Cada nodo muestra
# 
# La clase predecida (died o survived),
# La probabilidad predecida de survival,
# El porcentaje de observaciones en el nodo.


#Ver SVM, Random forest y Knn
